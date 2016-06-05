#ifndef __MDFN_PSX_DECOMP_H
#define __MDFN_PSX_DECOMP_H

#include <jit/jit.h>
#include <stdint.h>

void init_decompiler();
jit_function_t create_function();
bool decompile(jit_function_t func, uint32_t pc, uint32_t inst, bool &branched);

#endif
