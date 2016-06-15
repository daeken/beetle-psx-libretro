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

extern bool psx_cpu_overclock;

PS_CPU_Recompiler *rcpu = NULL;

void timestamp_inc(int amt) {
   gtimestamp += amt;
}

volatile block_t *branch_to_block;

void branch_block(block_t *block) {
   //printf("branching to (bl) %08x\n", block->pc);
   branch_to_block = block;
}

void check_irq(uint32_t pc) {
   if(rcpu->IPCache != 0 && (rcpu->CP0.SR & 1) != 0) {
      rcpu->GPR[rcpu->LDWhich] = rcpu->LDValue;
      rcpu->ReadAbsorb[rcpu->LDWhich] = rcpu->LDAbsorb;
      rcpu->ReadFudge = rcpu->LDWhich;
      rcpu->ReadAbsorbWhich |= rcpu->LDWhich & 0x1F;
      rcpu->LDWhich = 35;
      rcpu->Interrupt(rcpu->Exception(EXCEPTION_INT, pc, pc, 0xFF, 0));
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

   for(uint32_t start = (pc & 0x1FFFFFFF) >> 10, end = (block->end & 0x1FFFFFFF) >> 10; start <= end; ++start) {
      if(BlockPages[start] == NULL)
         BlockPages[start] = new list<block_t *>();
      BlockPages[start]->push_back(block);
   }
}

// Exported for low-level memory invalidation
void invalidate(uint32_t address) {
   if(rcpu != NULL)
      rcpu->InvalidateBlocks(address);
}

void PS_CPU_Recompiler::InvalidateBlocks(uint32_t addr) {
   static uint32_t invali = 0;
   addr &= 0x1FFFFFFF;
   uint32_t page = addr >> 10;
   if((addr == 0x80 || addr == 0xa0 || addr == 0xc0) && invali > 20)
      return;
   if(BlockPages[page] == NULL || BlockPages[page]->empty())
      return;

   for(list<block_t *>::iterator iter = BlockPages[page]->begin(); iter != BlockPages[page]->end(); iter++) {
      block_t *block = *iter;
      if((block->pc & 0x1FFFFFFF) <= addr && (block->end & 0x1FFFFFFF) + 8 > addr) {
         // XXX: This should delete the jit_function_t and closure
         if(addr == 0x80 || addr == 0xa0 || addr == 0xc0)
            printf("%08x [%08x-%08x] %i\n", addr, block->pc, block->end, ++invali);
         block->end = block->pc;
         block->jit_func = NULL;
         block->block = NULL;
         BlockPages[page]->erase(iter);
         break; // Fair assumption that all writes will only affect one block?
      }
   }

   //BlockPages[page]->clear();
}

void step(uint32_t arg) {
}

PS_CPU_Recompiler::PS_CPU_Recompiler() {
   init_decompiler();
   LastBlock = NULL;
   memset(BlockPages, 0, sizeof(BlockPages));

   rcpu = this;
}

inline uint32_t PS_CPU_Recompiler::RunBlock(uint32_t PC) {
   uint32_t initPC = PC;
   block_t *block;
   //if(LastBlock == NULL || initPC != LastBlock->pc)
   //   printf("block %08x\n", initPC);
   map<uint32_t, block_t *>::iterator iter;
   if((LastBlock == NULL || PC != LastBlock->pc || LastBlock->block == NULL) && ((iter = BlockCache.find(PC)) == BlockCache.end() || *iter->second->block == NULL)) {
      bool branched = false, no_delay = false, uncond = false, did_delay = false;
      jit_function_t func = create_function();
      while(!did_delay) {
            uint32_t instr;
            uint32_t opf;
            uint32_t startstamp = gtimestamp;

            if((PC & 0x3) != 0) {
               PC = Exception(EXCEPTION_ADEL, PC, PC, 0xFF, 0);
            }

            instr = ICache[(PC & 0xFFC) >> 2].Data;

            if(ICache[(PC & 0xFFC) >> 2].TV != PC)
            {
               //call_zra(func);

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
            }

            if(!decompile(func, PC, instr, branched, no_delay)) {
               printf("[CPU] Unknown instruction @%08x = %08x, op=%02x, funct=%02x\n", PC, instr, instr >> 26, (instr & 0x3F));
               exit(0); // XXX: Handle this properly...
            }

            //call_step(func, PC);

            //assert(!branched || (!did_delay && branched)); // No branch in branch delay slot...

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

   block->block(
      GPR, ReadAbsorb, &ReadAbsorbWhich, &ReadFudge, 
      &LDWhich, &LDValue, &LDAbsorb
   );

   if(branch_to != -1) {
      assert(branch_to_block == NULL);
      PC = branch_to;
   } else if(branch_to_block != NULL) {
      PC = branch_to_block->pc;
      LastBlock = (block_t *) branch_to_block;
   }
   return PC;
}

void PS_CPU_Recompiler::CheckBreakpoints(void (*callback)(bool write, uint32_t address, unsigned int len), uint32_t instr)
{
}
