import json, re, struct
import os.path
from tblgen import interpret, Dag, TableGenBits

def dag2expr(dag):
	def clean(value):
		if isinstance(value, tuple) and len(value) == 2 and value[0] == 'defref':
			return value[1]
		return value
	def sep((name, value)):
		if name is None:
			return clean(value)
		return name
	if isinstance(dag, Dag):
		return [dag2expr(sep(elem)) for elem in dag.elements]
	else:
		return dag

if not os.path.exists('insts.td.cache') or os.path.getmtime('insts.td') > os.path.getmtime('insts.td.cache'):
	print 'Rebuilding instruction cache'
	insts = interpret('insts.td').deriving('BaseInst')
	ops = []
	for name, (bases, data) in insts:
		ops.append((name, bases[1], data['Opcode'][1], data['Function'][1] if 'Function' in data else None, data['Disasm'][1], dag2expr(data['Eval'][1])))
	with file('insts.td.cache', 'w') as fp:
		json.dump(ops, fp)
else:
	ops = json.load(file('insts.td.cache'))

toplevel = {}

for name, type, op, funct, dasm, dag in ops:
	if funct is None:
		assert op not in toplevel
		toplevel[op] = name, type, dasm, dag
	else:
		if op not in toplevel:
			toplevel[op] = [type, {}]
		toplevel[op][1][funct] = name, type, dasm, dag

def generate(gfunc):
	switch = []
	for op, body in toplevel.items():
		if isinstance(body, list):
			type, body = body
			subswitch = []
			for funct, sub in body.items():
				subswitch.append(('case', funct, gfunc(sub)))
			if type == 'CFType':
				when = ('&', ('>>', 'inst', 21), 0x1F)
			elif type == 'RIType':
				when = ('&', ('>>', 'inst', 16), 0x1F)
			else:
				when = ('&', 'inst', 0x3F)
			switch.append(('case', op, ('switch', when, subswitch)))
		else:
			switch.append(('case', op, gfunc(body)))
	return ('switch', ('>>', 'inst', 26), switch)

def indent(str, single=True):
	if single and '\n' not in str:
		return ' %s ' % str
	else:
		return '\n%s\n' % '\n'.join('\t' + x for x in str.split('\n'))

def output(expr, top=True):
	if isinstance(expr, list):
		return '\n'.join(output(x, top=top) for x in expr)
	elif isinstance(expr, int) or isinstance(expr, long):
		return '0x%x' % expr
	elif isinstance(expr, str) or isinstance(expr, unicode):
		expr = expr.replace('$', '')
		return expr

	op = expr[0]
	if op == 'switch':
		return 'switch(%s) {%s}' % (output(expr[1]), indent(output(expr[2])))
	elif op == 'case':
		return 'case %s: {%s\tbreak;\n}' % (output(expr[1]), indent(output(expr[2]), single=False))
	elif op in ('+', '-', '*', '/', '%', '<<', '>>', '>>', '&', '|', '^', '==', '!=', '<', '<=', '>', '>='):
		return '(%s) %s (%s)' % (output(expr[1], top=False), op, output(expr[2], top=False))
	elif op == '!':
		return '!(%s)' % output(expr[1], top=False)
	elif op == '=':
		lval = output(expr[1], top=False)
		type = ''
		if lval != 'branched' and lval != 'no_delay':
			type = 'uint32_t '
		return '%s%s %s %s;' % (type, lval, op, output(expr[2], top=False))
	elif op == 'if':
		return 'if(%s) {%s} else {%s}' % (output(expr[1], top=False), indent(output(expr[2]), single=False), indent(output(expr[3]), single=False))
	elif op == 'when':
		return 'if(%s) {%s}' % (output(expr[1], top=False), indent(output(expr[2])))
	elif op == 'comment':
		return '/*%s*/' % indent(output(expr[1]))
	elif op == 'str':
		return `str(expr[1])`
	elif op == 'index':
		return '(%s)[%s]' % (output(expr[1], top=False), output(expr[2], top=False))
	elif op == 'emit':
		return '\n'.join(flatten(emitter(expr[1])))
	elif op in gops:
		return output(gops[op](*expr[1:]))
	elif op == 'zeroext':
		return output(expr[2])
	else:
		return '%s(%s)%s' % (op, ', '.join(output(x, top=False) for x in expr[1:]), ';' if top else '')

def flatten(x):
	if isinstance(x, list) or isinstance(x, tuple):
		return reduce(lambda a, b: a+b, map(flatten, x))
	else:
		return [x]

gops = {
	'add' : lambda a, b: ('+', a, b), 
	'sub' : lambda a, b: ('-', a, b), 
	'and' : lambda a, b: ('&', a, b), 
	'or' : lambda a, b: ('|', a, b), 
	'nor' : lambda a, b: ('~', ('|', a, b)), 
	'xor' : lambda a, b: ('^', a, b), 
	'mul' : lambda a, b: ('*', a, b),
	'div' : lambda a, b: ('/', a, b), 
	'mod' : lambda a, b: ('%', a, b), 
	'shl' : lambda a, b: ('<<', a, b), 
	'shra' : lambda a, b: ('>>', ('signed', a), ('signed', b)), 
	'shrl' : lambda a, b: ('>>', a, b), 

	'eq' : lambda a, b: ('==', a, b), 
	'ge' : lambda a, b: ('>=', a, b), 
	'gt' : lambda a, b: ('>', a, b), 
	'le' : lambda a, b: ('<=', a, b), 
	'lt' : lambda a, b: ('<', a, b), 
	'neq' : lambda a, b: ('!=', a, b), 
}

eops = {
	'add' : lambda a, b: ('jit_insn_add', a, b), 
	'sub' : lambda a, b: ('jit_insn_sub', a, b), 
	'and' : lambda a, b: ('jit_insn_and', a, b), 
	'or' : lambda a, b: ('jit_insn_or', a, b), 
	'nor' : lambda a, b: ('jit_insn_not', ('jit_insn_or', a, b)), 
	'xor' : lambda a, b: ('jit_insn_xor', a, b), 
	'mul' : lambda a, b: ('jit_insn_mul', a, b), # XXX: This needs to be a 64-bit mul!
	'div' : lambda a, b: ('jit_insn_div', a, b), 
	'mod' : lambda a, b: ('jit_insn_rem', a, b), 
	'shl' : lambda a, b: ('jit_insn_shl', a, b), 
	'shra' : lambda a, b: ('jit_insn_sshr', a, b), 
	'shrl' : lambda a, b: ('jit_insn_ushr', a, b), 

	'eq' : lambda a, b: ('jit_insn_eq', a, b), 
	'ge' : lambda a, b: ('jit_insn_ge', a, b), 
	'gt' : lambda a, b: ('jit_insn_gt', a, b), 
	'le' : lambda a, b: ('jit_insn_le', a, b), 
	'lt' : lambda a, b: ('jit_insn_lt', a, b), 
	'neq' : lambda a, b: ('jit_insn_ne', a, b), 
}

def cleansexp(sexp):
	if isinstance(sexp, list):
		return [cleansexp(x) for x in sexp if x != []]
	elif isinstance(sexp, tuple):
		return tuple([cleansexp(x) for x in sexp if x != []])
	else:
		return sexp

def find_deps(dag):
	if isinstance(dag, str) or isinstance(dag, unicode):
		return set([dag])
	elif not isinstance(dag, list):
		return set()

	return reduce(lambda a, b: a|b, map(find_deps, dag[1:])) if len(dag) != 1 else set()

def decoder(code, vars, type, dag):
	def decl(name, val):
		if name in deps:
			vars.append(name)
			code.append(('=', name, val))
	deps = find_deps(dag)
	if type == 'IType' or type == 'RIType':
		decl('$rs', ('&', ('>>', 'inst', 21), 0x1F))
		decl('$rt', ('&', ('>>', 'inst', 16), 0x1F))
		decl('$imm', ('&', 'inst', 0xFFFF))
	elif type == 'JType':
		decl('$imm', ('&', 'inst', 0x3FFFFFF))
	elif type == 'RType':
		decl('$rs', ('&', ('>>', 'inst', 21), 0x1F))
		decl('$rt', ('&', ('>>', 'inst', 16), 0x1F))
		decl('$rd', ('&', ('>>', 'inst', 11), 0x1F))
		decl('$shamt', ('&', ('>>', 'inst', 6), 0x1F))
	elif type == 'SType':
		decl('$code', ('&', ('>>', 'inst', 6), 0x0FFFFF))
	elif type == 'CFType':
		decl('$cop', ('&', ('>>', 'inst', 26), 3))
		decl('$rt', ('&', ('>>', 'inst', 16), 0x1F))
		decl('$rd', ('&', ('>>', 'inst', 11), 0x1F))
		decl('$cofun', ('&', 'inst', 0x01FFFFFF))
	else:
		print 'Unknown instruction type:', type
		assert False

debug = False
def dlog(dag, code, pos):
	if dag[0] == 'gpr':
		name = ('regname', dag[1])
	elif dag[0] == 'copreg':
		name = '+', ('+', ('+', ('str', 'cop'), dag[1]), ('str', ' reg ')), dag[2]
	elif dag[0] == 'copcreg':
		name = '+', ('+', ('+', ('str', 'cop'), dag[1]), ('str', ' control reg ')), dag[2]
	elif dag[0] in ('hi', 'lo', 'pc'):
		name = dag[0]
	elif dag[0] == 'store':
		name = '>>', dag[1], 0
	else:
		print 'Unknown dag to dlog:', dag
	
	return ('phex32', name, ('str', pos + ':'), code, ('str', 'uint:'), ('>>', code, 0))

temp_i = 0
def tempname():
	global temp_i
	temp_i += 1
	return 'temp_%i' % temp_i

def to_val(val):
	if val.startswith('jit_') or val.startswith('call_') or val.split('(')[0] in ('RGPR', 'WGPR', 'RPC', 'WPC', 'RHI', 'WHI', 'RLO', 'WLO'):
		return val
	return 'make_uint(%s)' % val

def emitter(sexp, storing=False):
	if isinstance(sexp, list):
		if len(sexp) == 1:
			sexp = sexp[0]
		else:
			return '/* Unhandled list */'

	if isinstance(sexp, str) or isinstance(sexp, unicode):
		return sexp.replace('$', '')
	elif isinstance(sexp, int):
		if sexp >= 0:
			return '0x%x' % sexp
		else:
			return '-0x%x' % -sexp
	op = sexp[0]
	if op == '=':
		lvalue = sexp[1]
		if isinstance(lvalue, list) and len(lvalue) == 1:
			lvalue = lvalue[0]
		if lvalue[0] == 'reg':
			return 'WGPR(%s, %s);' % (emitter(lvalue[1]), to_val(emitter(sexp[2])))
		elif lvalue[0] == 'pc':
			return 'WPC(%s);' % to_val(emitter(sexp[2]))
		elif lvalue[0] == 'hi':
			return 'WHI(%s)' % to_val(emitter(sexp[2]))
		elif lvalue[0] == 'lo':
			return 'WLO(%s)' % to_val(emitter(sexp[2]))
		elif lvalue[0] == 'copreg':
			return 'call_write_copreg(func, %s, %s, %s);' % (emitter(lvalue[1]), emitter(lvalue[2]), to_val(emitter(sexp[2])))
		elif lvalue[0] == 'copcreg':
			return 'call_write_copcreg(func, %s, %s, %s);' % (emitter(lvalue[1]), emitter(lvalue[2]), to_val(emitter(sexp[2])))
		else:
			print 'Unknown lvalue', lvalue
			raise False
	elif op == 'defer_set':
		return 'defer_set(func, %s, %s);' % (emitter(sexp[1][1]), to_val(emitter(sexp[2])))
	elif op == 'reg':
		return 'RGPR(%s)' % emitter(sexp[1])
	elif op == 'pc':
		return 'RPC()'
	elif op == 'hi':
		return 'RHI()'
	elif op == 'lo':
		return 'RLO()'
	elif op == 'copreg':
		return 'call_read_copreg(func, %s, %s)' % (emitter(sexp[1]), emitter(sexp[2]))
	elif op == 'copcreg':
		return 'call_read_copcreg(func, %s, %s)' % (emitter(sexp[1]), emitter(sexp[2]))
	elif op == 'branch':
		return 'call_branch(func, %s);' % (to_val(emitter(sexp[1])))
	elif op == 'syscall':
		return 'call_syscall(func, %s, %s, %s);' % (emitter(sexp[1]), emitter(sexp[2]), emitter(sexp[3]))
	elif op == 'break_':
		return 'call_break(func, %s);' % (emitter(sexp[1]))
	elif op == 'copfun':
		return 'call_copfun(func, %s, %s, %s);' % (emitter(sexp[1]), emitter(sexp[2]), emitter(sexp[3]))
	elif op == 'emit':
		return emitter(sexp[1], storing=storing)
	elif op == 'store':
		return 'call_store_memory(func, %i, %s, %s);' % (sexp[1], to_val(emitter(sexp[2])), to_val(emitter(sexp[3])))
	elif op == 'load':
		return 'call_load_memory(func, %i, %s)' % (sexp[1], to_val(emitter(sexp[2])))
	elif op == 'if':
		temp = tempname()
		end = tempname()
		return [
			'jit_label_t %s = jit_label_undefined, %s = jit_label_undefined;' % (temp, end), 
			'jit_insn_branch_if(func, %s, &%s);' % (to_val(emitter(sexp[1])), temp), 
			emitter(sexp[2]), 
			'jit_insn_branch(func, &%s);' % end, 
			'jit_insn_label(func, &%s);' % temp, 
			emitter(sexp[3]), 
			'jit_insn_label(func, &%s);' % end, 
		]
	elif op == 'when':
		temp = tempname()
		return [
			'jit_label_t %s = jit_label_undefined;' % temp, 
			'jit_insn_branch_if_not(func, %s, &%s);' % (to_val(emitter(sexp[1])), temp), 
			emitter(sexp[2]), 
			'jit_insn_label(func, &%s);' % temp
		]
	elif op == 'overflow':
		if sexp[1][0] == 'add':
			return 'call_overflow(func, %s, %s, 1);' % (to_val(emitter(sexp[1][1])), to_val(emitter(sexp[1][2])))
		else:
			return 'call_overflow(func, %s, %s, -1);' % (to_val(emitter(sexp[1][1])), to_val(emitter(sexp[1][2])))
	elif op == 'zeroext':
		return emitter(sexp[2], storing=storing)
	elif op == 'signext':
		return 'call_signext(func, %i, %s)' % (sexp[1], emitter(sexp[2], storing=storing))
	elif op in eops:
		return emitter(eops[op](*sexp[1:]), storing=storing)
	elif op.startswith('jit_'):
		return '%s(func, %s)' % (op, ', '.join([to_val(emitter(x, storing=storing)) for x in sexp[1:]]))
	else:
		print 'Unknown', sexp
	return ''

def findDepres(dag):
	dep, res = set(), set()
	if not isinstance(dag, list):
		return dep, res

	if dag[0] == 'set':
		if dag[1][0] == 'gpr':
			res.add(dag[1][1])
		sdep, sres = findDepres(dag[2])
		dep.update(sdep)
		res.update(sres)
	elif dag[0] == 'gpr':
		dep.add(dag[1])
	else:
		for sdep, sres in map(findDepres, dag):
			dep.update(sdep)
			res.update(sres)
	return dep, res

def genDecomp((name, type, dasm, dag)):
	code = [('comment', name), ('emit', ('=', ('pc', ), '$pc'))]
	vars = []
	decoder(code, vars, type, dag)
	has_branch = [False]
	no_delay = [False]

	dep, res = findDepres(dag)
	if len(dep) != 0 or len(res) != 0:
		depres = [('DEP', x) for x in dep] + [('RES', x) for x in res]
		code += depres

	code += [('do_lds', 'func')]
	#code += [('INSNLOG', name)]

	def subgen(dag):
		if isinstance(dag, str) or isinstance(dag, unicode):
			return dag
		elif isinstance(dag, int) or isinstance(dag, long):
			return dag
		elif not isinstance(dag, list):
			print 'Fail', dag
			assert False
		op = dag[0]
		if op in ('let', 'rlet'):
			if dag[1] not in vars:
				vars.append(dag[1])
			ret = [('=', dag[1], subgen(dag[2]))] + subgen(['block'] + dag[3:])
			if op == 'rlet':
				return [('emit', ret)]
			else:
				return ret
		elif op == 'set':
			left = dag[1]
			leftjs = subgen(left)
			ret = [('emit', ('=', leftjs, subgen(dag[2])))]
			return ret
		elif op == 'defer_set':
			left = dag[1]
			leftjs = subgen(left)
			ret = [('emit', ('defer_set', leftjs, subgen(dag[2])))]
			if left[0] == 'gpr':
				ret = [('when', ('neq', left[1], 0), ret)]
			return ret
		# XXX: Conditionals should detect if they can happen at decompile-time
		elif op == 'if':
			return [('emit', ('if', subgen(dag[1]), subgen(dag[2]), subgen(dag[3])))]
		elif op == 'when':
			return [('emit', ('when', subgen(dag[1]), subgen(dag[2])))]
		elif op in gops:
			return tuple(map(subgen, dag))
		elif op in ('signext', 'zeroext'):
			return (op, dag[1], subgen(dag[2]))
		elif op == 'pc':
			return ['$pc']
		elif op in ('hi', 'lo'):
			return [(op, )]
		elif op == 'pcd':
			return [('add', '$pc', 4)] # Return the delay slot position
		elif op == 'gpr':
			return ('reg', subgen(dag[1]))
		elif op == 'copreg':
			return ('copreg', subgen(dag[1]), subgen(dag[2]))
		elif op == 'copcreg':
			return ('copcreg', subgen(dag[1]), subgen(dag[2]))
		elif op == 'block':
			return list(map(subgen, dag[1:]))
		elif op == 'unsigned':
			return subgen(dag[1])
		elif op == 'signed':
			return subgen(dag[1])
		elif op == 'check_overflow':
			return [('emit', ('overflow', subgen(dag[1])))]
		elif op == 'raise':
			return [('emit', ('raise', dag[1]))]
		elif op == 'break':
			return [('emit', ('break_', dag[1]))]
		elif op == 'syscall':
			has_branch[0] = True
			no_delay[0] = True
			return [('emit', ('syscall', subgen(dag[1]), subgen(dag[2]), subgen(dag[3])))]
		elif op == 'branch':
			has_branch[0] = True
			return [('emit', ('branch', subgen(dag[1]), 'true'))]
		elif op == 'load':
			return [('load', dag[1], subgen(dag[2]))]
		elif op == 'store':
			return [('emit', ('store', dag[1], subgen(dag[2]), subgen(dag[3])))]
		elif op == 'copfun':
			return [('emit', ('copfun', subgen(dag[1]), subgen(dag[2]), subgen(dag[3])))]
		else:
			print 'Unknown op:', op
			return []

	code += cleansexp(subgen(dag))
	if has_branch[0]:
		code.append(('=', 'branched', 'true'))
	if no_delay[0]:
		code.append(('=', 'no_delay', 'true'))
	code.append(('return', 'true'))

	return code

def build():
	print 'Rebuilding from tables'
	with file('mednafen/psx/decomp.cpp', 'w') as fp:
		print >>fp, '/* Autogenerated from insts.td. DO NOT EDIT */'
		print >>fp, file('decompstub.cpp', 'r').read()
		print >>fp, 'bool decompile(jit_function_t func, uint32_t pc, uint32_t inst, bool &branched, bool &no_delay) {%s\treturn false;\n}' % indent(output(generate(genDecomp)))

if __name__=='__main__':
	build()
