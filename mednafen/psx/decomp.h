#ifndef __MDFN_PSX_DECOMP_H
#define __MDFN_PSX_DECOMP_H

#include <jit/jit.h>
#include <jit/jit-dump.h>
#include <stdint.h>
#include <list>
#include <map>
using namespace std;

class PS_CPU;

typedef void (*block_func_t)(
	volatile uint32_t *state, 
	uint8_t *ReadAbsorb, uint8_t *ReadAbsorbWhich, uint8_t *ReadFudge, 
	uint32_t *LDWhich, uint32_t *LDValue, uint32_t *LDAbsorb
);

typedef struct block_s {
   uint32_t pc, end;
   jit_function_t jit_func;
   block_func_t block;
   map<uint32_t, struct block_s *>::iterator cache_iter;
} block_t;

void init_decompiler();
jit_function_t create_function();
block_func_t compile_function(jit_function_t func);
bool decompile(jit_function_t func, uint32_t pc, uint32_t inst, bool &branched, bool &no_delay);

void step(uint32_t arg);
void call_step(jit_function_t func, uint32_t arg);

void call_timestamp_inc(jit_function_t func, uint32_t amount);
void timestamp_inc(int amt);

void div_delay();
void mul_delay(uint32_t a, uint32_t b, int signed);
void absorb_muldiv_delay();
void call_zra(jit_function_t func);

void check_irq(uint32_t pc);

uint32_t load_memory(int size, uint32_t ptr, uint32_t pc);
void store_memory(int size, uint32_t ptr, uint32_t val, uint32_t pc);

void branch(uint32_t target);
void branch_block(block_t *target);
void call_branch_block(jit_function_t func, block_t *block);

void ps_syscall(int code, uint32_t pc, uint32_t instr);
void break_(int code, uint32_t pc, uint32_t instr);
void overflow(uint32_t a, uint32_t b, int dir, uint32_t pc, uint32_t instr);
void alignment(uint32_t addr, int size, int store, uint32_t pc);

void copfun(int cop, int cofun, uint32_t inst);
uint32_t read_copreg(int cop, int reg);
void write_copreg(int cop, int reg, uint32_t val);
uint32_t read_copcreg(int cop, int reg);
void write_copcreg(int cop, int reg, uint32_t val);

#endif
