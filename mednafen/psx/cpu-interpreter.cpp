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

jmp_buf iexcjmpenv;
PS_CPU_Interpreter *icpu = NULL;

PS_CPU_Interpreter::PS_CPU_Interpreter() {
   icpu = this;
}

void PS_CPU_Interpreter::Interrupt(uint32_t addr) {
   longjmp(iexcjmpenv, addr);
}

uint32_t defer_branch = -1;
int32_t PS_CPU_Interpreter::RunReal(int32_t timestamp_in)
{
   uint32_t PC;
   uint32_t new_PC;
   uint32_t new_PC_mask;

#ifdef RUN_TESTS
   static bool startedTest = false;
   if(!startedTest) {
      startedTest = true;
      BACKED_PC = cpuTest();
   }
#endif

   gte_ts_done += timestamp_in;
   muldiv_ts_done += timestamp_in;

   BACKING_TO_ACTIVE;

   gtimestamp = timestamp_in;

   // Used for interrupts/exceptions
   uint32_t temp = setjmp(iexcjmpenv);
   if(temp != 0) {
      PC = temp;
   }

   do {
#ifdef RUN_TESTS
      if((PC & 0x0FFFFFFF) == 0x0EADBEE0)
         PC = cpuTest();
#endif
      if(Halted) {
         gtimestamp = next_event_ts;
         if(!PSX_EventHandler(gtimestamp))
            break;
      }

      uint32_t instr, before = gtimestamp;

      instr = ICache[(PC & 0xFFC) >> 2].Data;
      
      if(ICache[(PC & 0xFFC) >> 2].TV != PC)
      {
         //ReadAbsorb[ReadAbsorbWhich] = 0;
         //ReadAbsorbWhich = 0;

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

      gtimestamp = before;

      if(ReadAbsorb[ReadAbsorbWhich])
         ReadAbsorb[ReadAbsorbWhich]--;
      else
         gtimestamp++;

      //printf("running %08x\n", PC);
      branch_to = -1;

      GPR[0] = 0;
      if(!interpret(GPR, PC, instr)) {
         printf("[CPU] Unknown instruction @%08x = %08x, op=%02x, funct=%02x\n", PC, instr, instr >> 26, (instr & 0x3F));
         exit(0); // XXX: Handle this properly...
      }

      PC += 4;

      if(branch_to != -1 && defer_branch == -1) {
         defer_branch = branch_to;
      } else if(defer_branch != -1) {
         PC = defer_branch;
         defer_branch = -1;
         if(IPCache != 0 && (CP0.SR & 1) != 0) {
            PC = Exception(EXCEPTION_INT, PC, PC, 0xFF, 0);
         }

         if(gtimestamp >= next_event_ts && !PSX_EventHandler(gtimestamp))
            break;
      }
   } while(true);

   if(gte_ts_done > 0)
      gte_ts_done -= gtimestamp;

   if(muldiv_ts_done > 0)
      muldiv_ts_done -= gtimestamp;

   ACTIVE_TO_BACKING;

   return(gtimestamp);
}

void PS_CPU_Interpreter::CheckBreakpoints(void (*callback)(bool write, uint32_t address, unsigned int len), uint32_t instr)
{
}
