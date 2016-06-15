/* Mednafen - Multi-system Emulator
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 */

#include "psx.h"
#include "cpu.h"
#include <setjmp.h>

extern bool psx_cpu_overclock;

/* TODO
	Make sure load delays are correct.

	Consider preventing IRQs being taken while in a branch delay slot, to prevent potential problems with games that like to be too clever and perform
	un-restartable sequences of instructions.
*/

#define BIU_ENABLE_ICACHE_S1	0x00000800	// Enable I-cache, set 1
#define BIU_ENABLE_DCACHE	   0x00000080	// Enable D-cache
#define BIU_TAG_TEST_MODE	   0x00000004	// Enable TAG test mode(IsC must be set to 1 as well presumably?)
#define BIU_INVALIDATE_MODE	0x00000002	// Enable Invalidate mode(IsC must be set to 1 as well presumably?)
#define BIU_LOCK		         0x00000001	// Enable Lock mode(IsC must be set to 1 as well presumably?)

						// Does lock mode prevent the actual data payload from being modified, while allowing tags to be modified/updated???

int32_t gtimestamp;
volatile uint32_t branch_to;
PS_CPU *cpu;

PS_CPU::PS_CPU()
{
   uint64_t a;
   unsigned i;
   Halted = false;

   memset(FastMap, 0, sizeof(FastMap));
   memset(DummyPage, 0xFF, sizeof(DummyPage));	// 0xFF to trigger an illegal instruction exception, so we'll know what's up when debugging.

   for(a = 0x00000000; a < (UINT64_C(1) << 32); a += FAST_MAP_PSIZE)
      SetFastMap(DummyPage, a, FAST_MAP_PSIZE);

   CPUHook = NULL;
   ADDBT = NULL;

   GTE_Init();

   for(i = 0; i < 24; i++)
   {
      uint8 v = 7;

      if(i < 12)
         v += 4;

      if(i < 21)
         v += 3;

      MULT_Tab24[i] = v;
   }

   cpu = this;
}

PS_CPU::~PS_CPU()
{


}

void PS_CPU::SetFastMap(void *region_mem, uint32_t region_address, uint32_t region_size)
{
   uint64_t A;
   // FAST_MAP_SHIFT
   // FAST_MAP_PSIZE

   for(A = region_address; A < (uint64)region_address + region_size; A += FAST_MAP_PSIZE)
      FastMap[A >> FAST_MAP_SHIFT] = ((uint8_t *)region_mem - region_address);
}

void PS_CPU::RecalcIPCache(void)
{
   IPCache = 0;

   if(((CP0.SR & CP0.CAUSE & 0xFF00) && (CP0.SR & 1)))
      IPCache = 0x80;

   if(Halted)
      IPCache = 0x80;
}

void PS_CPU::SetHalt(bool status)
{
   Halted = status;
   RecalcIPCache();
}

void PS_CPU::Power(void)
{
   unsigned i;

   assert(sizeof(ICache) == sizeof(ICache_Bulk));

   memset(GPR, 0, sizeof(GPR));
   memset(&CP0, 0, sizeof(CP0));

   gte_ts_done = 0;
   muldiv_ts_done = 0;

   BACKED_PC = 0xBFC00000;
   BACKED_new_PC = 4;
   BACKED_new_PC_mask = ~0U;

   BACKED_LDWhich = 35;
   BACKED_LDValue = 0;
   LDAbsorb = 0;
   memset(ReadAbsorb, 0, sizeof(ReadAbsorb));
   ReadAbsorbWhich = 0;
   ReadFudge = 0;

   CP0.SR |= (1 << 22);	// BEV
   CP0.SR |= (1 << 21);	// TS

   CP0.PRID = 0x2;

   RecalcIPCache();


   BIU = 0;

   memset(ScratchRAM.data8, 0, 1024);

   // Not quite sure about these poweron/reset values:
   for(i = 0; i < 1024; i++)
   {
      ICache[i].TV = 0x2 | ((BIU & 0x800) ? 0x0 : 0x1);
      ICache[i].Data = 0;
   }

   GTE_Power();
}

int PS_CPU::StateAction(StateMem *sm, int load, int data_only)
{
   SFORMAT StateRegs[] =
   {
      SFARRAY32(GPR, 36),
      SFVAR(BACKED_PC),
      SFVAR(BACKED_new_PC),
      SFVAR(BACKED_new_PC_mask),

      SFVAR(IPCache),
      SFVAR(Halted),

      SFVAR(BACKED_LDWhich),
      SFVAR(BACKED_LDValue),
      SFVAR(LDAbsorb),

      SFVAR(next_event_ts),
      SFVAR(gte_ts_done),
      SFVAR(muldiv_ts_done),

      SFVAR(BIU),
      SFARRAY32(ICache_Bulk, 2048),

      SFARRAY32(CP0.Regs, 32),

      SFARRAY(ReadAbsorb, 0x20),
      SFVARN(ReadAbsorb[0x20], "ReadAbsorbDummy"),
      SFVAR(ReadAbsorbWhich),
      SFVAR(ReadFudge),

      SFARRAY(ScratchRAM.data8, 1024),

      SFEND
   };
   int ret = MDFNSS_StateAction(sm, load, data_only, StateRegs, "CPU");

   ret &= GTE_StateAction(sm, load, data_only);

   if(load)
   {

   }

   return(ret);
}

void PS_CPU::AssertIRQ(unsigned which, bool asserted)
{
   assert(which <= 5);

   CP0.CAUSE &= ~(1 << (10 + which));

   if(asserted)
      CP0.CAUSE |= 1 << (10 + which);

   RecalcIPCache();
}

void PS_CPU::SetBIU(uint32_t val)
{
   const uint32_t old_BIU = BIU;

   BIU = val & ~(0x440);

   if((BIU ^ old_BIU) & 0x800)
   {
      unsigned i;

      if(BIU & 0x800)	// ICache enabled
      {
         for(i = 0; i < 1024; i++)
            ICache[i].TV &= ~0x1;
      }
      else			// ICache disabled
      {
         for(i = 0; i < 1024; i++)
            ICache[i].TV |= 0x1;
      }
   }

   PSX_DBG(PSX_DBG_SPARSE, "[CPU] Set BIU=0x%08x\n", BIU);
}

uint32_t PS_CPU::GetBIU(void)
{
   return BIU;
}

static const uint32_t addr_mask[8] = { 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF,
				     0x7FFFFFFF, 0x1FFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF };

template<typename T>
INLINE T PS_CPU::PeekMemory(uint32_t address)
{
   address &= addr_mask[address >> 29];

   if(address >= 0x1F800000 && address <= 0x1F8003FF)
      return ScratchRAM.Read<T>(address & 0x3FF);

   //assert(!(CP0.SR & 0x10000));

   if(sizeof(T) == 1)
      return PSX_MemPeek8(address);
   else if(sizeof(T) == 2)
      return PSX_MemPeek16(address);
   return PSX_MemPeek32(address);
}

template<typename T>
void PS_CPU::PokeMemory(uint32 address, T value)
{
   address &= addr_mask[address >> 29];

   if(address >= 0x1F800000 && address <= 0x1F8003FF)
      return ScratchRAM.Write<T>(address & 0x3FF, value);

   if(sizeof(T) == 1)
      PSX_MemPoke8(address, value);
   else if(sizeof(T) == 2)
      PSX_MemPoke16(address, value);
   else
      PSX_MemPoke32(address, value);
}

template<typename T>
INLINE T PS_CPU::ReadMemory(uint32_t address, bool DS24, bool LWC_timing)
{
   T ret;

   ReadAbsorb[ReadAbsorbWhich] = 0;
   ReadAbsorbWhich = 0;

   address &= addr_mask[address >> 29];

   if(address >= 0x1F800000 && address <= 0x1F8003FF)
   {
      LDAbsorb = 0;

      if(DS24)
         return ScratchRAM.ReadU24(address & 0x3FF);
      return ScratchRAM.Read<T>(address & 0x3FF);
   }

   gtimestamp += (ReadFudge >> 4) & 2;

   //assert(!(CP0.SR & 0x10000));

   int32_t lts = gtimestamp;

   if(sizeof(T) == 1)
      ret = PSX_MemRead8(gtimestamp, address);
   else if(sizeof(T) == 2)
      ret = PSX_MemRead16(gtimestamp, address);
   else
   {
      if(DS24)
         ret = PSX_MemRead24(gtimestamp, address) & 0xFFFFFF;
      else
         ret = PSX_MemRead32(gtimestamp, address);
   }

   if(LWC_timing)
      gtimestamp += 1;
   else
      gtimestamp += 2;

   LDAbsorb = gtimestamp - lts;

   return(ret);
}

template<typename T>
INLINE void PS_CPU::WriteMemory(uint32_t address, uint32_t value, bool DS24)
{
   if(MDFN_LIKELY(!(CP0.SR & 0x10000)))
   {
      address &= addr_mask[address >> 29];

      if(address >= 0x1F800000 && address <= 0x1F8003FF)
      {
         if(DS24)
            ScratchRAM.WriteU24(address & 0x3FF, value);
         else
            ScratchRAM.Write<T>(address & 0x3FF, value);

         return;
      }

      if(sizeof(T) == 1)
         PSX_MemWrite8(gtimestamp, address, value);
      else if(sizeof(T) == 2)
         PSX_MemWrite16(gtimestamp, address, value);
      else
      {
         if(DS24)
            PSX_MemWrite24(gtimestamp, address, value);
         else
            PSX_MemWrite32(gtimestamp, address, value);
      }
   }
   else
   {
      if(BIU & 0x800)	// Instruction cache is enabled/active
      {
         if(BIU & 0x4)	// TAG test mode.
         {
            // TODO: Respect written value.
            __ICache *ICI = &ICache[((address & 0xFF0) >> 2)];
            const uint8_t valid_bits = 0x00;

            ICI[0].TV = ((valid_bits & 0x01) ? 0x00 : 0x02) | ((BIU & 0x800) ? 0x0 : 0x1);
            ICI[1].TV = ((valid_bits & 0x02) ? 0x00 : 0x02) | ((BIU & 0x800) ? 0x0 : 0x1);
            ICI[2].TV = ((valid_bits & 0x04) ? 0x00 : 0x02) | ((BIU & 0x800) ? 0x0 : 0x1);
            ICI[3].TV = ((valid_bits & 0x08) ? 0x00 : 0x02) | ((BIU & 0x800) ? 0x0 : 0x1);
         }
         else if(!(BIU & 0x1))
         {
            ICache[(address & 0xFFC) >> 2].Data = value << ((address & 0x3) * 8);
         }
      }

      if((BIU & 0x081) == 0x080)	// Writes to the scratchpad(TODO test)
      {
         if(DS24)
            ScratchRAM.WriteU24(address & 0x3FF, value);
         else
            ScratchRAM.Write<T>(address & 0x3FF, value);
      }
      //printf("IsC WRITE%d 0x%08x 0x%08x -- CP0.SR=0x%08x\n", (int)sizeof(T), address, value, CP0.SR);
   }
}

uint32_t PS_CPU::Exception(uint32_t code, uint32_t PC, const uint32 NP, const uint32_t NPM, const uint32_t instr)
{
   const bool AfterBranchInstr = !(NPM & 0x1);
   const bool BranchTaken = !(NPM & 0x3);
   uint32_t handler = 0x80000080;

   assert(code < 16);

   if(code != EXCEPTION_INT && code != EXCEPTION_BP && code != EXCEPTION_SYSCALL)
   {
      PSX_DBG(PSX_DBG_WARNING, "Exception: %08x @ PC=0x%08x(IBDS=%d) -- IPCache=0x%02x -- IPEND=0x%02x -- SR=0x%08x ; IRQC_Status=0x%04x -- IRQC_Mask=0x%04x\n", code, PC, BranchTaken, IPCache, (CP0.CAUSE >> 8) & 0xFF, CP0.SR,
            ::IRQ_GetRegister(IRQ_GSREG_STATUS, NULL, 0), ::IRQ_GetRegister(IRQ_GSREG_MASK, NULL, 0));
   }

   if(CP0.SR & (1 << 22))	// BEV
      handler = 0xBFC00180;

   CP0.EPC = PC;
   if(AfterBranchInstr)
   {
      CP0.EPC -= 4;
      CP0.TAR = (PC & (NPM | 3)) + NP;
   }

#ifdef HAVE_DEBUG
   if(ADDBT)
      ADDBT(PC, handler, true);
#endif

   // "Push" IEc and KUc(so that the new IEc and KUc are 0)
   CP0.SR = (CP0.SR & ~0x3F) | ((CP0.SR << 2) & 0x3F);

   // Setup cause register
   CP0.CAUSE &= 0x0000FF00;
   CP0.CAUSE |= code << 2;

   // If EPC was adjusted -= 4 because we are after a branch instruction, set bit 31.
   CP0.CAUSE |= AfterBranchInstr << 31;
   CP0.CAUSE |= BranchTaken << 30;
   CP0.CAUSE |= (instr << 2) & (0x3 << 28); // CE

   RecalcIPCache();

   return(handler);
}

jmp_buf excjmpenv;

void PS_CPU::Interrupt(uint32_t addr) {
   longjmp(excjmpenv, addr);
}

bool PS_CPU::HandleHalt() {
   while(Halted) {
      gtimestamp = next_event_ts;
      if(!PSX_EventHandler(gtimestamp)) {
         return false;
      }
   }
   return true;
}

int32_t PS_CPU::RunReal(int32_t timestamp_in)
{
   uint32_t PC;
   uint32_t new_PC;
   uint32_t new_PC_mask;

#ifdef RUN_TESTS
   static bool startedTest = false;
   if(!startedTest) {
      startedTest = true;
      BACKED_PC = 0xDEADBEE0;
   }
#endif

   gte_ts_done += timestamp_in;
   muldiv_ts_done += timestamp_in;

   BACKING_TO_ACTIVE;

   gtimestamp = timestamp_in;

   // Used for interrupts/exceptions
   uint32_t temp = setjmp(excjmpenv);
   if(temp != 0) {
      PC = temp;
   }

   while(gtimestamp < next_event_ts || PSX_EventHandler(gtimestamp)) {
#ifdef RUN_TESTS
      if((PC & 0x0FFFFFFF) == 0x0EADBEE0)
         PC = cpuTest();
#endif
      if(IPCache != 0) {
         if(Halted) {
            if(!HandleHalt())
               break;
         } else if((CP0.SR & 1) != 0) {
            PC = Exception(EXCEPTION_INT, PC, PC, 0xFF, 0);
         }
      }

      PC = RunBlock(PC);
   }

   if(gte_ts_done > 0)
      gte_ts_done -= gtimestamp;

   if(muldiv_ts_done > 0)
      muldiv_ts_done -= gtimestamp;

   ACTIVE_TO_BACKING;

   return(gtimestamp);
}

int32_t PS_CPU::Run(int32_t timestamp_in)
{
   return RunReal(timestamp_in);
}

void PS_CPU::SetCPUHook(void (*cpuh)(const int32_t timestamp, uint32_t pc), void (*addbt)(uint32_t from, uint32_t to, bool exception))
{
   ADDBT = addbt;
   CPUHook = cpuh;
}

uint32_t PS_CPU::GetRegister(unsigned int which, char *special, const uint32_t special_len)
{
   if(which >= GSREG_GPR && which < (GSREG_GPR + 32))
      return GPR[which];
   switch(which)
   {
      case GSREG_PC:
         return BACKED_PC;
      case GSREG_PC_NEXT:
         return BACKED_new_PC;
      case GSREG_IN_BD_SLOT:
         return !(BACKED_new_PC_mask & 3);
      case GSREG_LO:
         return GPR[34];
      case GSREG_HI:
         return GPR[33];
      case GSREG_SR:
         return CP0.SR;
      case GSREG_CAUSE:
         return CP0.CAUSE;
      case GSREG_EPC:
         return CP0.EPC;
   }

   return 0;
}

void PS_CPU::SetRegister(unsigned int which, uint32_t value)
{
   if(which >= GSREG_GPR && which < (GSREG_GPR + 32))
   {
      if(which != (GSREG_GPR + 0))
         GPR[which] = value;
   }
   else switch(which)
   {
      case GSREG_PC:
         BACKED_PC = value & ~0x3;	// Remove masking if we ever add proper misaligned PC exception
         break;

      case GSREG_LO:
         GPR[34] = value;
         break;

      case GSREG_HI:
         GPR[33] = value;
         break;

      case GSREG_SR:
         CP0.SR = value;		// TODO: mask
         break;

      case GSREG_CAUSE:
         CP0.CAUSE = value;
         break;

      case GSREG_EPC:
         CP0.EPC = value & ~0x3U;
         break;
   }
}

bool PS_CPU::PeekCheckICache(uint32_t PC, uint32_t *iw)
{
   if(ICache[(PC & 0xFFC) >> 2].TV == PC)
   {
      *iw = ICache[(PC & 0xFFC) >> 2].Data;
      return(true);
   }

   return(false);
}


uint8_t PS_CPU::PeekMem8(uint32_t A)
{
 return PeekMemory<uint8>(A);
}

uint16_t PS_CPU::PeekMem16(uint32_t A)
{
 return PeekMemory<uint16>(A);
}

uint32_t PS_CPU::PeekMem32(uint32_t A)
{
 return PeekMemory<uint32>(A);
}

void PS_CPU::PokeMem8(uint32 A, uint8 V)
{
 PokeMemory<uint8>(A, V);
}

void PS_CPU::PokeMem16(uint32 A, uint16 V)
{
 PokeMemory<uint16>(A, V);
}

void PS_CPU::PokeMem32(uint32 A, uint32 V)
{
 PokeMemory<uint32>(A, V);
}

uint32_t load_memory(int size, uint32_t ptr, uint32_t pc) {
   // Enable full debugging in SOTN
   //if(ptr == 0x80032AB0)
   //   return 1;
   uint32_t val;
   switch(size) {
      case 8:
         val = (uint32_t) cpu->ReadMemory<uint8_t>(ptr);
         break;
      case 16:
         val = (uint32_t) cpu->ReadMemory<uint16_t>(ptr);
         break;
      case 24:
         val = cpu->ReadMemory<uint32_t>(ptr, true);
         break;
      default:
         val = cpu->ReadMemory<uint32_t>(ptr);
   }

   //printf("Loading %i bits of memory from %08x <-- %08x\n", size, ptr, val);

   return val;
}

void store_memory(int size, uint32_t ptr, uint32_t val, uint32_t pc) {
   //printf("Storing %i bits of memory to %08x <-- %08x @ %08x\n", size, ptr, val, pc);

   switch(size) {
      case 8:
         cpu->WriteMemory<uint8_t>(ptr, val);
         break;
      case 16:
         cpu->WriteMemory<uint16_t>(ptr, val);
         break;
      case 24:
         cpu->WriteMemory<uint32_t>(ptr, val, true);
      case 32:
         cpu->WriteMemory<uint32_t>(ptr, val);
         break;
   }
}
