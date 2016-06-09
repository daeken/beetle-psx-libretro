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
