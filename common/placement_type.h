
#ifndef RADIUM_COMMON_PLACEMENT_TYPE_H
#define RADIUM_COMMON_PLACEMENT_TYPE_H

#include <stdint.h>

typedef struct _func_t func_t;


typedef uint32_t uint_32;
#define MAX_UINT32 65534  /* Sqr(max(uint_32))-1 (rounded down)*/


struct Placement{
	int line;
	uint_32 counter; // What the ...? This is extremely dangerous. It should not be unsigned
	uint_32 dividor; // Same here, although this one is probably not that much of a problem.
};
typedef struct Placement Place;

#ifdef __cplusplus
extern "C" {
#endif

// Note: p_ToString(a) should be equal to p_toString(p_fromString(p_toString(a))), and so forth.
extern const char* p_ToString(const Place a);
extern Place p_FromString(const char* s);

#ifdef __cplusplus
}
#endif

#endif
