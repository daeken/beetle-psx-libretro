#ifndef __MDFN_PSX_CPU_RECOMPILER_H
#define __MDFN_PSX_CPU_RECOMPILER_H

#include "decomp.h"

class PS_CPU_Recompiler : public PS_CPU {
public:
   PS_CPU_Recompiler();

   int32_t RunReal(int32_t timestamp_in);
   void CheckBreakpoints(void (*callback)(bool write, uint32_t address, unsigned int len), uint32_t instr);

   block_t *GetBlockReference(uint32_t pc);
   void StashBlock(uint32_t pc, block_t *block);
   void InvalidateBlocks(uint32_t addr);

   map<uint32_t, block_t *> BlockCache;
   list<block_t *> *BlockPages[0x100000]; // A list for every page
   block_t *LastBlock;
};

extern PS_CPU_Recompiler *rcpu;

#endif
