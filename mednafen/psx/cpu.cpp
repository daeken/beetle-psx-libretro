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

bool gdebug = false;

volatile int32_t gtimestamp;
void timestamp_inc(int amt) {
   gtimestamp += amt;
}

void muldiv_delay(uint32_t a, uint32_t b) {
   if(a == 0 && b == 0)
      cpu->muldiv_ts_done = gtimestamp + 37;
   else
      cpu->muldiv_ts_done = gtimestamp + cpu->MULT_Tab24[MDFN_lzcount32((a ^ ((int32_t) b >> 31)) | 0x400)];
}

void absorb_muldiv_delay() {
   if(gtimestamp < cpu->muldiv_ts_done) {
      if(gtimestamp == cpu->muldiv_ts_done - 1)
         cpu->muldiv_ts_done--;
      else
         do {
            if(cpu->ReadAbsorb[cpu->ReadAbsorbWhich])
               cpu->ReadAbsorb[cpu->ReadAbsorbWhich]--;
            gtimestamp++;
         } while(gtimestamp < cpu->muldiv_ts_done);
   }
}

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

   init_decompiler();
   cpu = this;
   LastBlock = NULL;
   memset(BlockPages, 0, sizeof(BlockPages));

   for(i = 0; i < 24; i++)
   {
      uint8 v = 7;

      if(i < 12)
         v += 4;

      if(i < 21)
         v += 3;

      MULT_Tab24[i] = v;
   }
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

INLINE void PS_CPU::RecalcIPCache(void)
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

   BACKED_LDWhich = 0x20;
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

uint32_t load_memory(int size, uint32_t ptr) {
   // Enable full debugging in SOTN
   if(ptr == 0x80032AB0)
      return 1;
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

   if(gdebug)
      printf("Loading %i bits of memory from %08x <-- %08x\n", size, ptr, val);

   return val;
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
      ret = PSX_MemRead8(lts, address);
   else if(sizeof(T) == 2)
      ret = PSX_MemRead16(lts, address);
   else
   {
      if(DS24)
         ret = PSX_MemRead24(lts, address) & 0xFFFFFF;
      else
         ret = PSX_MemRead32(lts, address);
   }

   if(LWC_timing)
      lts += 1;
   else
      lts += 2;

   LDAbsorb = (lts - gtimestamp);
   gtimestamp = lts;

   return(ret);
}

void store_memory(int size, uint32_t ptr, uint32_t val, uint32_t pc) {
   if(gdebug)
      printf("Storing %i bits of memory to %08x <-- %08x @ %08x\n", size, ptr, val, pc);

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

#define BACKING_TO_ACTIVE			\
	PC = BACKED_PC;				\
	new_PC = BACKED_new_PC;			\
	new_PC_mask = BACKED_new_PC_mask;	\
	LDWhich = BACKED_LDWhich;		\
	LDValue = BACKED_LDValue;

#define ACTIVE_TO_BACKING			\
	BACKED_PC = PC;				\
	BACKED_new_PC = new_PC;			\
	BACKED_new_PC_mask = new_PC_mask;	\
	BACKED_LDWhich = LDWhich;		\
	BACKED_LDValue = LDValue;

volatile uint32_t branch_to;
volatile block_t *branch_to_block;
void branch(uint32_t target) {
   //printf("branching to (pc) %08x\n", target);
   branch_to = target;
}

void branch_block(block_t *block) {
   //printf("branching to (bl) %08x\n", block->pc);
   branch_to_block = block;
}

jmp_buf excjmpenv;

void ps_syscall(int code, uint32_t pc, uint32_t instr) {
   branch(cpu->Exception(EXCEPTION_SYSCALL, pc, pc + 4, 0xFF, instr));
}

void break_(int code, uint32_t pc, uint32_t instr) {
   branch(cpu->Exception(EXCEPTION_BP, pc, pc + 4, 0xFF, instr));
}

void overflow(uint32_t a, uint32_t b, int dir, uint32_t pc, uint32_t instr) {
   if(dir) {
      uint32_t r = a + b;
      if(((~(a ^ b)) & (a ^ r)) & 0x80000000)
         longjmp(excjmpenv, cpu->Exception(EXCEPTION_OV, pc, pc, 0xFF, instr));
   } else {
      uint32_t r = a - b;
      if((((a ^ b)) & (a ^ r)) & 0x80000000)
         longjmp(excjmpenv, cpu->Exception(EXCEPTION_OV, pc, pc, 0xFF, instr));
   }
}

void copfun0(int cofun, uint32_t inst) {
   assert(cofun == 0x10);

   cpu->CP0.SR = (cpu->CP0.SR & ~0x0F) | ((cpu->CP0.SR >> 2) & 0x0F);
   cpu->RecalcIPCache();
}

void copfun2(int cofun, uint32_t inst) {
   if(gtimestamp < cpu->gte_ts_done)
      gtimestamp = cpu->gte_ts_done;
   cpu->gte_ts_done = gtimestamp + GTE_Instruction(inst);
}

void copfun(int cop, int cofun, uint32_t inst) {
   switch(cop) {
      case 0:
         copfun0(cofun, inst);
         break;
      case 2:
         copfun2(cofun, inst);
         break;
   }
}

void write_copreg0(int reg, uint32_t val) {
   switch(reg) {
      case CP0REG_BPC:
         cpu->CP0.BPC = val;
         break;

      case CP0REG_BDA:
         cpu->CP0.BDA = val;
         break;

      case CP0REG_TAR:
         cpu->CP0.TAR = val;
         break;

      case CP0REG_DCIC:
         cpu->CP0.DCIC = val & 0xFF80003F;
         break;

      case CP0REG_BDAM:
         cpu->CP0.BDAM = val;
         break;

      case CP0REG_BPCM:
         cpu->CP0.BPCM = val;
         break;

      case CP0REG_CAUSE:
         cpu->CP0.CAUSE &= ~(0x3 << 8);
         cpu->CP0.CAUSE |= val & (0x3 << 8);
         cpu->RecalcIPCache();
         break;

      case CP0REG_SR:
         if((cpu->CP0.SR ^ val) & 0x10000)
            PSX_DBG(PSX_DBG_SPARSE, "[CPU] IsC %u->%u\n", (bool)(cpu->CP0.SR & (1U << 16)), (bool)(val & (1U << 16)));

         cpu->CP0.SR = val & ~( (0x3 << 26) | (0x3 << 23) | (0x3 << 6));
         cpu->RecalcIPCache();
         break;
   }
}

void write_copreg2(int reg, uint32_t val) {
   if(gtimestamp < cpu->gte_ts_done)
      gtimestamp = cpu->gte_ts_done;

   GTE_WriteDR(reg, val);
}

void write_copreg(int cop, int reg, uint32_t val) {
   switch(cop) {
      case 0:
         write_copreg0(reg, val);
         break;
      case 2:
         write_copreg2(reg, val);
         break;
   }
}

uint32_t read_copreg0(int reg) {
   return cpu->CP0.Regs[reg];
}

uint32_t read_copreg2(int reg) {
   if(gtimestamp < cpu->gte_ts_done) {
      cpu->LDAbsorb = cpu->gte_ts_done - gtimestamp;
      gtimestamp = cpu->gte_ts_done;
   } else
      cpu->LDAbsorb = 0;

   return GTE_ReadDR(reg);
}

uint32_t read_copreg(int cop, int reg) {
   switch(cop) {
      case 0:
         return read_copreg0(reg);
      case 2:
         return read_copreg2(reg);
   }
   return 0;
}

void write_copcreg2(int reg, uint32_t val) {
   if(gtimestamp < cpu->gte_ts_done)
      gtimestamp = cpu->gte_ts_done;

   GTE_WriteCR(reg, val);
}

void write_copcreg(int cop, int reg, uint32_t val) {
   switch(cop) {
      case 2:
         write_copcreg2(reg, val);
         break;
      default:
         printf("Invalid copcreg write to cop %i\n", cop);
   }
}

uint32_t read_copcreg2(int reg) {
   if(gtimestamp < cpu->gte_ts_done) {
      cpu->LDAbsorb = cpu->gte_ts_done - gtimestamp;
      gtimestamp = cpu->gte_ts_done;
   } else
      cpu->LDAbsorb = 0;

   return GTE_ReadCR(reg);
}

uint32_t read_copcreg(int cop, int reg) {
   switch(cop) {
      case 2:
         return read_copcreg2(reg);
      default:
         printf("Invalid copcreg read from cop %i\n", cop);
   }
   return 0;
}

void check_irq(uint32_t pc) {
   if(cpu->IPCache != 0 && (cpu->CP0.SR & 1) != 0) {
      cpu->GPR[cpu->LDWhich] = cpu->LDValue;
      cpu->ReadAbsorb[cpu->LDWhich] = cpu->LDAbsorb;
      cpu->ReadFudge = cpu->LDWhich;
      cpu->ReadAbsorbWhich |= cpu->LDWhich & 0x1F;
      cpu->LDWhich = 35;
      longjmp(excjmpenv, cpu->Exception(EXCEPTION_INT, pc, pc, 0xFF, 0));
   }
}

void step(uint32_t arg) {
   // Called for every instruction
   //printf("step... %08x\n", arg);
}

block_t *PS_CPU::GetBlockReference(uint32_t pc) {
   map<uint32_t, block_t *>::iterator iter = BlockCache.find(pc);

   if(iter != BlockCache.end())
      return iter->second;

   block_t *block = new block_t;
   block->pc = pc;
   block->end = pc;
   block->jit_func = NULL;
   block->block = NULL;
   block->cache_iter = BlockCache.end();
   StashBlock(pc, block);
   return block;
}

void PS_CPU::StashBlock(uint32_t pc, block_t *block) {
   if(block->cache_iter == BlockCache.end()) {
      BlockCache[pc] = block;
      // XXX: We need to not do this lookup twice! This is expensive.
      block->cache_iter = BlockCache.find(pc);
   }

   if(pc == block->end)
      return;

   for(uint32_t start = (pc & 0x1FFFFFFF) >> 12, end = (block->end & 0x1FFFFFFF) >> 12; start <= end; ++start) {
      if(BlockPages[start] == NULL)
         BlockPages[start] = new list<block_t *>();
      BlockPages[start]->push_back(block);
   }
}

// Exported for low-level memory invalidation
void invalidate(uint32_t address) {
   cpu->InvalidateBlocks(address);
}

void PS_CPU::InvalidateBlocks(uint32_t addr) {
   addr &= 0x1FFFFFFF;
   uint32_t page = addr >> 12;
   if(BlockPages[page] == NULL || BlockPages[page]->empty())
      return;

   for(list<block_t *>::iterator iter = BlockPages[page]->begin(); iter != BlockPages[page]->end(); ++iter) {
      block_t *block = *iter;
      // XXX: This should delete the jit_function_t and closure
      block->end = block->pc;
      block->jit_func = NULL;
      block->block = NULL;
   }

   BlockPages[page]->clear();
}

void game_printf(char *fmt, uint32_t *GPR) {
   int reg = 5;
   while(*fmt != 0) {
      if(*fmt == '%') {
         fmt++;
         int padding = 0;
         if(*fmt == '0') {
            fmt++;
            while(*fmt >= '0' && *fmt <= '9')
               padding = padding * 10 + (*(fmt++) - '0');
         }
         switch(*fmt) {
            case 's': {
               uint32_t addr = GPR[reg];
               int len = 0;
               while(cpu->PeekMem8(addr + len++) != 0)
                  ;
               char *buf = new char[len+1];
               for(int i = 0; i < len; ++i)
                  buf[i] = cpu->PeekMem8(addr + i);
               buf[len] = 0;
               printf("%s", buf);
               ++reg;
               break;
            }
            case 'c':
               putc(GPR[reg++], stdout);
               break;
            case 'x': {
               uint32_t val = GPR[reg++];
               if(padding == 0)
                  printf("%x", val);
               else
                  while(--padding >= 0)
                     putc("0123456789abcdef"[(val >> (4 * padding)) & 0xF], stdout);
               break;
            }
            case 'd': case 'i':
               printf("%i", GPR[reg++]);
               break;
            default:
               putc(*(fmt++), stdout);
         }
         fmt++;
      } else
         putc(*(fmt++), stdout);
   }
   fflush(stdout);
}

template<bool DebugMode>
int32_t PS_CPU::RunReal(int32_t timestamp_in)
{
   uint32_t PC;
   uint32_t new_PC;
   uint32_t new_PC_mask;

   gte_ts_done += timestamp_in;
   muldiv_ts_done += timestamp_in;

   BACKING_TO_ACTIVE;

   gtimestamp = timestamp_in;

   // Used for per-instruction irq checking
   uint32_t temp = setjmp(excjmpenv);
   if(temp != 0)
      PC = temp;

   //gdebug = true;

   do {
      while(MDFN_LIKELY(gtimestamp < next_event_ts)) {
         if(Halted) {
            gtimestamp = next_event_ts;
            break;
         }

         if(IPCache != 0 && (CP0.SR & 1) != 0) {
            PC = Exception(EXCEPTION_INT, PC, PC, 0xFF, 0);
         }

         uint32_t initPC = PC;
         block_t *block;
         //if(initPC == 0x8001c188) // sotn start 0x80010dfc
         //   gdebug = true;
         //if(LastBlock == NULL || initPC != LastBlock->pc)
         //   printf("block %08x\n", initPC);
         map<uint32_t, block_t *>::iterator iter;
         if((LastBlock == NULL || PC != LastBlock->pc || LastBlock->block == NULL) && ((iter = BlockCache.find(PC)) == BlockCache.end() || *iter->second->block == NULL)) {
            bool branched = false, no_delay = false, uncond = false, did_delay = false;
            jit_function_t func = create_function();
            //printf("recompiling %08x\n", PC);
            while(!did_delay) {
                  uint32_t instr;
                  uint32_t opf;
                  uint32_t startstamp = gtimestamp;

                  if((PC & 0x3) != 0) {
                     PC = Exception(EXCEPTION_ADEL, PC, PC, 0xFF, 0);
                     continue;
                  }

                  instr = ICache[(PC & 0xFFC) >> 2].Data;

                  if(ICache[(PC & 0xFFC) >> 2].TV != PC)
                  {
                     ReadAbsorb[ReadAbsorbWhich] = 0;
                     ReadAbsorbWhich = 0;

                     // FIXME: Handle executing out of scratchpad.
                     if(PC >= 0xA0000000 || !(BIU & 0x800))
                     {
                        instr = LoadU32_LE((uint32_t *)&FastMap[PC >> FAST_MAP_SHIFT][PC]);

                        if (!psx_cpu_overclock)
                        {
                           // Approximate best-case cache-disabled time, per PS1 tests
                           // (executing out of 0xA0000000+); it can be 5 in 
                           // *some* sequences of code(like a lot of sequential "nop"s, 
                           // probably other simple instructions too).
                           gtimestamp += 4;	
                        }
                     }
                     else
                     {
                        __ICache *ICI = &ICache[((PC & 0xFF0) >> 2)];
                        const uint32_t *FMP = (uint32_t *)&FastMap[(PC &~ 0xF) >> FAST_MAP_SHIFT][PC &~ 0xF];

                        // | 0x2 to simulate (in)validity bits.
                        ICI[0x00].TV = (PC &~ 0xF) | 0x00 | 0x2;
                        ICI[0x01].TV = (PC &~ 0xF) | 0x04 | 0x2;
                        ICI[0x02].TV = (PC &~ 0xF) | 0x08 | 0x2;
                        ICI[0x03].TV = (PC &~ 0xF) | 0x0C | 0x2;

                        // When overclock is enabled, remove code cache fetch latency
                        if (!psx_cpu_overclock)
                           gtimestamp += 3;

                        switch(PC & 0xC)
                        {
                           case 0x0:
                              if (!psx_cpu_overclock)
                                 gtimestamp++;
                              ICI[0x00].TV &= ~0x2;
                              ICI[0x00].Data = LoadU32_LE(&FMP[0]);
                           case 0x4:
                              if (!psx_cpu_overclock)
                                 gtimestamp++;
                              ICI[0x01].TV &= ~0x2;
                              ICI[0x01].Data = LoadU32_LE(&FMP[1]);
                           case 0x8:
                              if (!psx_cpu_overclock)
                                 gtimestamp++;
                              ICI[0x02].TV &= ~0x2;
                              ICI[0x02].Data = LoadU32_LE(&FMP[2]);
                           case 0xC:
                              if (!psx_cpu_overclock)
                                 gtimestamp++;
                              ICI[0x03].TV &= ~0x2;
                              ICI[0x03].Data = LoadU32_LE(&FMP[3]);
                              break;
                        }
                        instr = ICache[(PC & 0xFFC) >> 2].Data;
                     }
                  }

                  //call_timestamp_inc(func, gtimestamp - startstamp);
                  gtimestamp = startstamp;

                  if(branched) {
                     did_delay = true;
                     branched = false;
                  }

                  if(!decompile(func, PC, instr, branched, no_delay)) {
                     printf("[CPU] Unknown instruction @%08x = %08x, op=%02x, funct=%02x\n", PC, instr, instr >> 26, (instr & 0x3F));
                     exit(0); // XXX: Handle this properly...
                  }

                  //call_step(func, PC);

                  assert(!branched || (!did_delay && branched)); // No branch in branch delay slot...

                  if(branched && no_delay)
                     did_delay = true;

                  PC += 4;
            }

            //printf("ended at %08x\n", PC);

            if(iter == BlockCache.end())
               block = GetBlockReference(initPC);
            else
               block = iter->second;
            block->end = PC;
            block->jit_func = func;
            block->block = compile_function(func);
            StashBlock(initPC, block);
         } else if(LastBlock->pc == initPC && LastBlock->block != NULL)
            block = LastBlock;
         else
            block = iter->second;

         assert(block->block != NULL);
         assert(block->end != block->pc);

         LastBlock = block;

         branch_to = -1;
         branch_to_block = NULL;

         GPR[32] = initPC;

         if(gdebug)
            printf("Running %08x\n", initPC);

         block->block(
            GPR, ReadAbsorb, &ReadAbsorbWhich, &ReadFudge, 
            &LDWhich, &LDValue, &LDAbsorb
         );

         PC = GPR[32] + 4; // We don't set PC after instructions

         if(branch_to != -1) {
            assert(branch_to_block == NULL);
            PC = branch_to;
         } else if(branch_to_block != NULL) {
            PC = branch_to_block->pc;
            LastBlock = (block_t *) branch_to_block;
         }

         if(PC == 0x80016124) {
            char fmt[4096];
            char *at = fmt;
            uint32_t addr = GPR[4];
            while((*(at++) = PeekMem8(addr++)) != 0) {
            }
            printf("Game log [at %08x]: ", initPC);
            game_printf(fmt, GPR);
         }
         
         if(gdebug)
            fflush(stdout);
      }
   } while(MDFN_LIKELY(PSX_EventHandler(gtimestamp)));

   if(gte_ts_done > 0)
      gte_ts_done -= gtimestamp;

   if(muldiv_ts_done > 0)
      muldiv_ts_done -= gtimestamp;

   ACTIVE_TO_BACKING;

   return(gtimestamp);
}

int32_t PS_CPU::Run(int32_t timestamp_in)
{
#ifdef HAVE_DEBUG
   if(CPUHook || ADDBT)
      return(RunReal<true>(timestamp_in));
#endif
   return(RunReal<false>(timestamp_in));
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

#undef BEGIN_OPF
#undef END_OPF
#undef MK_OPF

#define MK_OPF(op, funct)	((op) ? (0x40 | (op)) : (funct))
#define BEGIN_OPF(op, funct) case MK_OPF(op, funct): {
#define END_OPF } break;

// FIXME: should we breakpoint on an illegal address?  And with LWC2/SWC2 if CP2 isn't enabled?
void PS_CPU::CheckBreakpoints(void (*callback)(bool write, uint32_t address, unsigned int len), uint32_t instr)
{
}
