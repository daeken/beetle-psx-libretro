#ifndef __MDFN_PSX_DECOMP_H
#define __MDFN_PSX_DECOMP_H

#include <jit/jit.h>
#include <jit/jit-dump.h>
#include <stdint.h>

typedef void (*block_t)(
	volatile uint32_t *state, 
	uint8_t *ReadAbsorb, uint8_t *ReadAbsorbWhich, uint8_t *ReadFudge, 
	uint32_t *LDWhich, uint32_t *LDValue, uint32_t *LDAbsorb
);

void init_decompiler();
jit_function_t create_function();
block_t compile_function(jit_function_t func);
bool decompile(jit_function_t func, uint32_t pc, uint32_t inst, bool &branched, bool &no_delay);

void step(uint32_t arg);
void call_step(jit_function_t func, uint32_t arg);

void call_timestamp_inc(jit_function_t func, uint32_t amount);
void timestamp_inc(int amt);

uint32_t load_memory(int size, uint32_t ptr);
void store_memory(int size, uint32_t ptr, uint32_t val);

void branch(uint32_t target);

void syscall(int code, uint32_t pc, uint32_t instr);

void copfun(int cop, int cofun, uint32_t inst);
uint32_t read_copreg(int cop, int reg);
void write_copreg(int cop, int reg, uint32_t val);
uint32_t read_copcreg(int cop, int reg);
void write_copcreg(int cop, int reg, uint32_t val);

#endif
