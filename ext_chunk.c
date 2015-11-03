#include "ext_chunk.h"

/* 
 * 		CHUNK CLASS
 */

#pragma omp declare target
TeaLeafChunk _chunk;
#pragma omp end declare target

// Entry point for extension initialisation.
void ext_init_(
        int* xMax, 
        int* yMax, 
        int* rank,
        int* device_id)
{
    _chunk.x = *xMax+HALO_PAD*2;
    _chunk.y = *yMax+HALO_PAD*2;
    _chunk.innerX = *xMax;
    _chunk.innerY = *yMax;
    _chunk.rank = *rank;
    _chunk.page = _chunk.x*_chunk.y;
    _chunk.device_id = *device_id;

#pragma omp target update device(_chunk.device_id) to(_chunk)
}

void ext_finalise_()
{
    if(_chunk.rank == 0)
    {
        PRINT_PROFILING_RESULTS;
    }
}

