#define GPR(gpr) state[gpr]
#define HI state[33]
#define LO state[34]
#define TGPR(name, gpr) uint32_t name = GPR(gpr)
