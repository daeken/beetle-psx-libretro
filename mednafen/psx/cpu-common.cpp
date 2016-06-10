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

void branch(uint32_t target) {
   branch_to = target;
}

int32_t signext(int size, uint32_t imm) {
   if(size == 8)
      return (int8_t) ((uint8_t) imm);
   else if(size == 16)
      return (int16_t) ((uint16_t) imm);
   else if(size == 32)
      return (int32_t) imm;
   else if(imm & (1 << (size - 1)))
      return (int32_t) imm - (1 << size);
   else
      return (int32_t) imm;
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

void ps_syscall(int code, uint32_t pc, uint32_t instr) {
   cpu->Interrupt(cpu->Exception(EXCEPTION_SYSCALL, pc, pc + 4, 0xFF, instr));
}

void break_(int code, uint32_t pc, uint32_t instr) {
   cpu->Interrupt(cpu->Exception(EXCEPTION_BP, pc, pc + 4, 0xFF, instr));
}

void overflow(uint32_t a, uint32_t b, int dir, uint32_t pc, uint32_t instr) {
   if(dir == 1) {
      uint32_t r = a + b;
      if(((~(a ^ b)) & (a ^ r)) & 0x80000000)
         cpu->Interrupt(cpu->Exception(EXCEPTION_OV, pc, pc, 0xFF, instr));
   } else {
      uint32_t r = a - b;
      if((((a ^ b)) & (a ^ r)) & 0x80000000)
         cpu->Interrupt(cpu->Exception(EXCEPTION_OV, pc, pc, 0xFF, instr));
   }
}

void alignment(uint32_t addr, int size, int store, uint32_t pc) {
	if((size == 16 && (addr & 1) != 0) || (size == 32 && (addr & 3) != 0)) {
		cpu->Interrupt(cpu->Exception(store ? EXCEPTION_ADES : EXCEPTION_ADEL, pc, pc, 0xFF, 0));
	}
}
