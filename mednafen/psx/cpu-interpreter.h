#ifndef __MDFN_PSX_CPU_INTERPRETER_H
#define __MDFN_PSX_CPU_INTERPRETER_H

#include "decomp.h"

bool interpret(uint32_t *state, uint32_t pc, uint32_t inst);

class PS_CPU_Interpreter : public PS_CPU {
public:
   PS_CPU_Interpreter();

   int32_t RunReal(int32_t timestamp_in);
   void Interrupt(uint32_t addr);
   void CheckBreakpoints(void (*callback)(bool write, uint32_t address, unsigned int len), uint32_t instr);

   block_t *GetBlockReference(uint32_t pc);
   void StashBlock(uint32_t pc, block_t *block);
   void InvalidateBlocks(uint32_t addr);

   map<uint32_t, block_t *> BlockCache;
   list<block_t *> *BlockPages[0x100000]; // A list for every page
   block_t *LastBlock;
};

extern PS_CPU_Interpreter *icpu;

#endif
