#include "psx.h"

#define REG(gpr) state[gpr]
#define HI() state[33]
#define LO() state[34]
#define TGPR(name, gpr) uint32_t name = REG(gpr)

#define DEP(gpr) do { if(gpr != 0) cpu->ReadAbsorb[gpr] = 0; } while(0)
#define RES(gpr) do { if(gpr != 0) cpu->ReadAbsorb[gpr] = 0; } while(0)

#define DO_LDS() do { \
	state[cpu->LDWhich] = cpu->LDValue; \
	cpu->ReadAbsorb[cpu->LDWhich] = cpu->LDAbsorb; \
	cpu->ReadFudge = cpu->LDWhich; \
	cpu->ReadAbsorbWhich |= (cpu->LDWhich != 35) ? (cpu->LDWhich & 0x1F) : 0; \
	cpu->LDWhich = 35; \
} while(0)

#define DEFER_SET(gpr, val) do { cpu->LDWhich = gpr; cpu->LDValue = val; } while(0)

#define branch_default() do { } while(0)

#define INSNLOG(mnem) printf(#mnem " [%08x]\n", pc);
