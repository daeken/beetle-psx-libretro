#include "psx.h"

bool failed, failedAny = false;
int testState = 0;

void testStart(const char *name) {
	printf("Test: %s\n", name);
	failed = false;
}

void testEnd() {
	if(failed) {
		printf("Test failed\n");
		failedAny = true;
	} else {
		printf("Test passed\n");
	}
}
#define testExpectEqual(lval, rval) if(lval != rval) { printf(#lval " == %i, expected " #rval "\n", lval); failed = true; }

uint32_t loadBlob(uint32_t addr, int len, uint32_t *blob) {
	for(int i = 0; i < len; ++i)
		cpu->PokeMem32(addr + i * 4, blob[i]);
	return addr;
}

uint32_t cpuTest() {
	uint32_t pc;
	switch(testState++) {
		case 0: {
			testStart("ADD 1");
			cpu->Power();
			cpu->GPR[1] = 10;
			cpu->GPR[2] = -15;
		
			uint32_t blob_1[] = {0x00201820, 0x00222020, 0x00412820, 0x00423020, 0x0bab6fb8, 0x00000000};
			pc = loadBlob(0x80000000, 6, blob_1);
			break;
		}
		
		case 1: {
			testExpectEqual(cpu->GPR[1], 10);
			testExpectEqual(cpu->GPR[2], -15);
			testExpectEqual(cpu->GPR[3], 10);
			testExpectEqual(cpu->GPR[4], -5);
			testExpectEqual(cpu->GPR[5], -5);
			testExpectEqual(cpu->GPR[6], -30);
			testEnd();
		
			testStart("Arithmetic/branching test");
			cpu->Power();
			cpu->GPR[2] = 57005;
			cpu->GPR[3] = 0;
			cpu->GPR[5] = 1;
		
			uint32_t blob_2[] = {0x00451023, 0x24630001, 0x1c40fffd, 0x00000000, 0x0bab6fb8, 0x00000000};
			pc = loadBlob(0x80000000, 6, blob_2);
			break;
		}
		
		case 2: {
			testExpectEqual(cpu->GPR[2], 0);
			testExpectEqual(cpu->GPR[3], 57005);
			testExpectEqual(cpu->GPR[5], 1);
			testEnd();
			break;
		}
	}

	if(testState == 3) {
		if(failedAny)
			exit(1);
		exit(0);
	}
	return pc;
}
