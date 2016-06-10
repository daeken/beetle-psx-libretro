#include "psx.h"

// This is passed as an array of uint32_t
typedef struct state_s {
	uint32_t reg[32];
	uint32_t pc;
	uint32_t hi, lo;
	uint32_t fake; // Used for load delay stuff
} state_t;

jit_value_t _make_ptr(jit_function_t func, void *val) {
	return jit_value_create_nint_constant(func, jit_type_void_ptr, (jit_nint) val);
}
#define make_ptr(val) _make_ptr(func, (val))
jit_value_t _make_uint(jit_function_t func, uint32_t val) {
	return jit_value_create_nint_constant(func, jit_type_uint, val);
}
#define make_uint(val) _make_uint(func, (val))
jit_value_t _make_ubyte(jit_function_t func, uint32_t val) {
	return jit_value_create_nint_constant(func, jit_type_ubyte, val);
}
#define make_ubyte(val) _make_ubyte(func, (val))

#define LOAD(ptr, type) jit_insn_load_relative(func, (ptr), 0, type)
#define STORE(ptr, value) jit_insn_store_relative(func, (ptr), 0, (value))
#define CAST(value, type) jit_insn_convert(func, (value), (type), 0)

#define WGPR(gpr, val) do { if(gpr != 0) jit_insn_store_elem(func, state, make_uint(gpr), (val)); } while(0)
#define WGPR_VAL(gpr, val) jit_insn_store_elem(func, state, gpr, (val))
#define RGPR(gpr) ((gpr == 0) ? make_uint(0) : jit_insn_load_elem(func, state, make_uint(gpr), jit_type_uint))
#define TGPR(name, gpr) jit_value_t name = RGPR(gpr)

#define WPC(val) jit_insn_store_relative(func, state, 32*4, (val));
#define RPC() jit_insn_load_relative(func, state, 32*4, jit_type_uint)
#define WHI(val) jit_insn_store_relative(func, state, 33*4, (val));
#define RHI() jit_insn_load_relative(func, state, 33*4, jit_type_uint)
#define WLO(val) jit_insn_store_relative(func, state, 34*4, (val));
#define RLO() jit_insn_load_relative(func, state, 34*4, jit_type_uint)

#define DEP(gpr) do { if(gpr != 0) WRA(make_ubyte(gpr), make_ubyte(0)); } while(0)
#define RES(gpr) do { if(gpr != 0) WRA(make_ubyte(gpr), make_ubyte(0)); } while(0)

jit_type_t sig_0, sig_1, sig_1_ptr, sig_2, sig_3, sig_4, sig_5;
jit_value_t state, _ReadAbsorb, _ReadAbsorbWhich, _ReadFudge, LDWhich, LDValue, LDAbsorb;

#define WRA(idx, val) jit_insn_store_relative(func, jit_insn_add(func, _ReadAbsorb, idx), 0, (val))

void do_lds(jit_function_t func) {
	jit_value_t ldw = LOAD(LDWhich, jit_type_uint), raw = LOAD(_ReadAbsorbWhich, jit_type_ubyte);
	WGPR_VAL(ldw, LOAD(LDValue, jit_type_uint));
	WRA(ldw, jit_insn_load(func, LDAbsorb));
	STORE(_ReadFudge, CAST(ldw, jit_type_ubyte));
	jit_label_t label = jit_label_undefined;
	jit_insn_branch_if(func, jit_insn_eq(func, raw, make_uint(35)), &label);
	STORE(_ReadAbsorbWhich, CAST(jit_insn_or(func, raw, jit_insn_and(func, ldw, make_uint(0x1F))), jit_type_ubyte));
	jit_insn_label(func, &label);
	STORE(LDWhich, make_uint(35));
}
#define DO_LDS() do_lds(func)

void defer_set(jit_function_t func, int reg, jit_value_t val) {
    STORE(LDWhich, make_uint(reg));
	STORE(LDValue, val);
}

void call_store_memory(jit_function_t func, int size, jit_value_t ptr, jit_value_t val, uint32_t pc) {
	jit_value_t args[] = {make_uint(size), ptr, val, make_uint(pc)};
	jit_insn_call_native(func, 0, (void *) store_memory, sig_4, args, 4, 0);
}

jit_value_t call_load_memory(jit_function_t func, int size, jit_value_t ptr, uint32_t pc) {
	jit_value_t args[] = {make_uint(size), ptr, make_uint(pc)};
	return jit_insn_call_native(func, 0, (void *) load_memory, sig_3, args, 3, 0);
}

jit_value_t call_read_copreg(jit_function_t func, int cop, int reg) {
	jit_value_t args[] = {make_uint(cop), make_uint(reg)};
	return jit_insn_call_native(func, 0, (void *) read_copreg, sig_2, args, 2, 0);
}

jit_value_t call_read_copcreg(jit_function_t func, int cop, int reg) {
	jit_value_t args[] = {make_uint(cop), make_uint(reg)};
	return jit_insn_call_native(func, 0, (void *) read_copcreg, sig_2, args, 2, 0);
}

void call_write_copreg(jit_function_t func, int cop, int reg, jit_value_t val) {
	jit_value_t args[] = {make_uint(cop), make_uint(reg), val};
	jit_insn_call_native(func, 0, (void *) write_copreg, sig_3, args, 3, 0);
}

void call_write_copcreg(jit_function_t func, int cop, int reg, jit_value_t val) {
	jit_value_t args[] = {make_uint(cop), make_uint(reg), val};
	jit_insn_call_native(func, 0, (void *) write_copcreg, sig_3, args, 3, 0);
}

jit_value_t call_copfun(jit_function_t func, int cop, int cofun, uint32_t inst) {
	jit_value_t args[] = {make_uint(cop), make_uint(cofun), make_uint(inst)};
	return jit_insn_call_native(func, 0, (void *) copfun, sig_3, args, 3, 0);
}

void call_step(jit_function_t func, uint32_t arg) {
	jit_value_t args[] = {make_uint(arg)};
	jit_insn_call_native(func, 0, (void *) step, sig_1, args, 1, 0);
}

jit_value_t call_signext(jit_function_t func, int size, jit_value_t val) {
	jit_value_t args[] = {make_uint(size), val};
	return jit_insn_call_native(func, 0, (void *) signext, sig_2, args, 2, 0);
}

void call_syscall(jit_function_t func, uint32_t code, uint32_t pc, uint32_t inst) {
	jit_value_t args[] = {make_uint(code), make_uint(pc), make_uint(inst)};
	jit_insn_call_native(func, 0, (void *) ps_syscall, sig_3, args, 3, 0);
}

void call_break(jit_function_t func, uint32_t code, uint32_t pc, uint32_t inst) {
	jit_value_t args[] = {make_uint(code), make_uint(pc), make_uint(inst)};
	jit_insn_call_native(func, 0, (void *) break_, sig_3, args, 3, 0);
}

void call_branch(jit_function_t func, jit_value_t val) {
	jit_value_t args[] = {val};
	jit_insn_call_native(func, 0, (void *) branch, sig_1, args, 1, 0);
}

void call_branch_block(jit_function_t func, block_t *block) {
	jit_value_t args[] = {make_ptr(block)};
	jit_insn_call_native(func, 0, (void *) branch_block, sig_1_ptr, args, 1, 0);
}

void call_overflow(jit_function_t func, jit_value_t a, jit_value_t b, int dir, uint32_t pc, uint32_t inst) {
	jit_value_t args[] = {a, b, make_uint(dir), make_uint(pc), make_uint(inst)};
	jit_insn_call_native(func, 0, (void *) overflow, sig_5, args, 5, 0);
}

void call_alignment(jit_function_t func, jit_value_t addr, int size, int store, uint32_t pc) {
	jit_value_t args[] = {addr, make_uint(size), make_uint(store), make_uint(pc)};
	jit_insn_call_native(func, 0, (void *) alignment, sig_4, args, 4, 0);
}

void call_timestamp_inc(jit_function_t func, uint32_t amount) {
	jit_value_t args[] = {make_uint(amount)};
	jit_insn_call_native(func, 0, (void *) timestamp_inc, sig_1, args, 1, 0);
}

void call_muldiv_delay(jit_function_t func, jit_value_t a, jit_value_t b) {
	jit_value_t args[] = {a, b};
	jit_insn_call_native(func, 0, (void *) muldiv_delay, sig_2, args, 2, 0);
}

void call_absorb_muldiv_delay(jit_function_t func) {
	jit_insn_call_native(func, 0, (void *) absorb_muldiv_delay, sig_0, NULL, 0, 0);
}

void call_check_irq(jit_function_t func, uint32_t pc) {
	jit_value_t args[] = {make_uint(pc)};
	jit_insn_call_native(func, 0, (void *) check_irq, sig_1, args, 1, 0);
}

jit_context_t context;

jit_type_t block_sig;

void init_decompiler() {
	context = jit_context_create();
	jit_context_build_start(context);

	jit_type_t s5params[5];
	s5params[0] = jit_type_uint;
	s5params[1] = jit_type_uint;
	s5params[2] = jit_type_uint;
	s5params[3] = jit_type_uint;
	s5params[4] = jit_type_uint;
	sig_5 = jit_type_create_signature(jit_abi_cdecl, jit_type_uint, s5params, 5, 1);
	
	jit_type_t s4params[4];
	s4params[0] = jit_type_uint;
	s4params[1] = jit_type_uint;
	s4params[2] = jit_type_uint;
	s4params[3] = jit_type_uint;
	sig_4 = jit_type_create_signature(jit_abi_cdecl, jit_type_uint, s4params, 4, 1);
	
	jit_type_t s3params[3];
	s3params[0] = jit_type_uint;
	s3params[1] = jit_type_uint;
	s3params[2] = jit_type_uint;
	sig_3 = jit_type_create_signature(jit_abi_cdecl, jit_type_uint, s3params, 3, 1);
	
	jit_type_t sparams[2];
	sparams[0] = jit_type_uint;
	sparams[1] = jit_type_uint;
	sig_2 = jit_type_create_signature(jit_abi_cdecl, jit_type_uint, sparams, 2, 1);
	
	jit_type_t lparams[1];
	lparams[0] = jit_type_uint;
	sig_1 = jit_type_create_signature(jit_abi_cdecl, jit_type_uint, lparams, 1, 1);

	jit_type_t pparams[1];
	pparams[0] = jit_type_void_ptr;
	sig_1_ptr = jit_type_create_signature(jit_abi_cdecl, jit_type_void, pparams, 1, 1);

	sig_0 = jit_type_create_signature(jit_abi_cdecl, jit_type_void, NULL, 0, 1);

	jit_type_t params[7];
	params[0] = jit_type_create_pointer(jit_type_uint, 0); // State
	params[1] = jit_type_create_pointer(jit_type_ubyte, 0); // ReadAbsorb
	params[2] = jit_type_create_pointer(jit_type_ubyte, 0); // ReadAbsorbWhich
	params[3] = jit_type_create_pointer(jit_type_ubyte, 0); // ReadFudge
	params[4] = jit_type_create_pointer(jit_type_uint, 0); // LDWhich
	params[5] = jit_type_create_pointer(jit_type_uint, 0); // LDValue
	params[6] = jit_type_create_pointer(jit_type_uint, 0); // LDAbsorb
	block_sig = jit_type_create_signature(jit_abi_cdecl, jit_type_void, params, 7, 1);
}

jit_function_t create_function() {
	jit_function_t func = jit_function_create(context, block_sig);
	state = jit_value_get_param(func, 0);
	_ReadAbsorb = jit_value_get_param(func, 1);
	_ReadAbsorbWhich = jit_value_get_param(func, 2);
	_ReadFudge = jit_value_get_param(func, 3);
	LDWhich = jit_value_get_param(func, 4);
	LDValue = jit_value_get_param(func, 5);
	LDAbsorb = jit_value_get_param(func, 6);
	return func;
}

block_func_t compile_function(jit_function_t func) {
	//jit_dump_function(stdout, func, "block");
	jit_function_compile(func);
	jit_context_build_end(context);
	//jit_dump_function(stdout, func, "block");
	return (block_func_t) jit_function_to_closure(func);
}

#define INSNLOG(name) printf(#name "\n")
