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
#include <setjmp.h>

extern bool psx_cpu_overclock;

bool gdebug = false;
jmp_buf excjmpenv;
PS_CPU_Recompiler *rcpu;

void timestamp_inc(int amt) {
   gtimestamp += amt;
}

void muldiv_delay(uint32_t a, uint32_t b) {
   if(a == 0 && b == 0)
      rcpu->muldiv_ts_done = gtimestamp + 37;
   else
      rcpu->muldiv_ts_done = gtimestamp + rcpu->MULT_Tab24[MDFN_lzcount32((a ^ ((int32_t) b >> 31)) | 0x400)];
}

void absorb_muldiv_delay() {
   if(gtimestamp < rcpu->muldiv_ts_done) {
      if(gtimestamp == rcpu->muldiv_ts_done - 1)
         rcpu->muldiv_ts_done--;
      else
         do {
            if(rcpu->ReadAbsorb[rcpu->ReadAbsorbWhich])
               rcpu->ReadAbsorb[rcpu->ReadAbsorbWhich]--;
            gtimestamp++;
         } while(gtimestamp < rcpu->muldiv_ts_done);
   }
}

uint32_t load_memory(int size, uint32_t ptr, uint32_t pc) {
   // Enable full debugging in SOTN
   if(ptr == 0x80032AB0)
      return 1;
   uint32_t val;
   switch(size) {
      case 8:
         val = (uint32_t) rcpu->ReadMemory8(ptr);
         break;
      case 16:
         //if((ptr & 0x1) != 0)
         //   longjmp(excjmpenv, rcpu->Exception(EXCEPTION_ADEL, pc, pc, 0xFF, 0));
         val = (uint32_t) rcpu->ReadMemory16(ptr);
         break;
      case 24:
         //if((ptr & 0x3) != 0)
         //   longjmp(excjmpenv, rcpu->Exception(EXCEPTION_ADEL, pc, pc, 0xFF, 0));
         val = rcpu->ReadMemory24(ptr);
         break;
      default:
         //if((ptr & 0x3) != 0)
         //   longjmp(excjmpenv, rcpu->Exception(EXCEPTION_ADEL, pc, pc, 0xFF, 0));
         val = rcpu->ReadMemory32(ptr);
   }

   if(gdebug)
      printf("Loading %i bits of memory from %08x <-- %08x\n", size, ptr, val);

   return val;
}

void store_memory(int size, uint32_t ptr, uint32_t val, uint32_t pc) {
   if(gdebug)
      printf("Storing %i bits of memory to %08x <-- %08x @ %08x\n", size, ptr, val, pc);

   switch(size) {
      case 8:
         rcpu->WriteMemory8(ptr, val);
         break;
      case 16:
         rcpu->WriteMemory16(ptr, val);
         break;
      case 24:
         rcpu->WriteMemory24(ptr, val);
      case 32:
         rcpu->WriteMemory32(ptr, val);
         break;
   }
}

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

void ps_syscall(int code, uint32_t pc, uint32_t instr) {
   branch(rcpu->Exception(EXCEPTION_SYSCALL, pc, pc + 4, 0xFF, instr));
}

void break_(int code, uint32_t pc, uint32_t instr) {
   branch(rcpu->Exception(EXCEPTION_BP, pc, pc + 4, 0xFF, instr));
}

void overflow(uint32_t a, uint32_t b, int dir, uint32_t pc, uint32_t instr) {
   if(dir == 1) {
      uint32_t r = a + b;
      if(((~(a ^ b)) & (a ^ r)) & 0x80000000)
         longjmp(excjmpenv, rcpu->Exception(EXCEPTION_OV, pc, pc, 0xFF, instr));
   } else {
      uint32_t r = a - b;
      if((((a ^ b)) & (a ^ r)) & 0x80000000)
         longjmp(excjmpenv, rcpu->Exception(EXCEPTION_OV, pc, pc, 0xFF, instr));
   }
}

void check_irq(uint32_t pc) {
   if(rcpu->IPCache != 0 && (rcpu->CP0.SR & 1) != 0) {
      rcpu->GPR[rcpu->LDWhich] = rcpu->LDValue;
      rcpu->ReadAbsorb[rcpu->LDWhich] = rcpu->LDAbsorb;
      rcpu->ReadFudge = rcpu->LDWhich;
      rcpu->ReadAbsorbWhich |= rcpu->LDWhich & 0x1F;
      rcpu->LDWhich = 35;
      longjmp(excjmpenv, rcpu->Exception(EXCEPTION_INT, pc, pc, 0xFF, 0));
   }
}

block_t *PS_CPU_Recompiler::GetBlockReference(uint32_t pc) {
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

void PS_CPU_Recompiler::StashBlock(uint32_t pc, block_t *block) {
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
   rcpu->InvalidateBlocks(address);
}

void PS_CPU_Recompiler::InvalidateBlocks(uint32_t addr) {
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
               while(rcpu->PeekMem8(addr + len++) != 0)
                  ;
               char *buf = new char[len+1];
               for(int i = 0; i < len; ++i)
                  buf[i] = rcpu->PeekMem8(addr + i);
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

void step(uint32_t arg) {
   // Called for every instruction
   //printf("step... %08x\n", arg);
}

PS_CPU_Recompiler::PS_CPU_Recompiler() {
   init_decompiler();
   LastBlock = NULL;
   memset(BlockPages, 0, sizeof(BlockPages));

   rcpu = this;
}

int32_t PS_CPU_Recompiler::RunReal(int32_t timestamp_in)
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

void PS_CPU_Recompiler::CheckBreakpoints(void (*callback)(bool write, uint32_t address, unsigned int len), uint32_t instr)
{
}
