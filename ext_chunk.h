#ifndef __CHUNK
#define __CHUNK

#include <math.h>
#include "ext_shared.h"

/*
 * 		CHUNK CLASS
 */

// The core Tealeaf interface class.
typedef struct
{
    int rank;
    int x;
    int y;
    int page;
    int device_id;
    int innerX;
    int innerY;
} TeaLeafChunk;

#pragma omp declare target
// Globally stored list of chunks.
extern TeaLeafChunk _chunk;
#pragma omp end declare target

#endif
