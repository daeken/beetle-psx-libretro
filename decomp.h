#ifndef __MDFN_PSX_DECOMP_H
#define __MDFN_PSX_DECOMP_H

#include <jit/jit.h>
#include <jit/jit-dump.h>
#include <stdint.h>

void init_decompiler();
jit_function_t create_function();
void compile_function(jit_function_t func);
bool decompile(jit_function_t func, uint32_t pc, uint32_t inst, bool &branched);

void call_timestamp_inc(jit_function_t func, uint32_t amount);
void timestamp_inc(int amt);

uint32_t load_memory(int size, uint32_t ptr);
void store_memory(int size, uint32_t ptr, uint32_t val);

void branch(uint32_t target);

#endif
