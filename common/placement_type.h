
#ifndef RADIUM_COMMON_PLACEMENT_TYPE_H
#define RADIUM_COMMON_PLACEMENT_TYPE_H

#include <stdint.h>


typedef uint32_t uint_32;
#define MAX_UINT32 65534  /* Sqr(max(uint_32))-1 (rounded down)*/


struct Placement{
	int line;
	uint_32 counter; // What the ...? This is extremely dangerous. It should not be unsigned
	uint_32 dividor; // Same here, although this one is probably not that much of a problem.
};
typedef struct Placement Place;

#endif
