#include "ext_chunk.h"
#include <stdlib.h>

/*
 * 		UPDATE HALO KERNEL
 */	

// Updates the left face.
void update_left(
        const int depth, 
        double* buffer,
        int offload)
{
#pragma omp target device(_chunk.device_id) if(offload)
#pragma omp parallel for
    for(int jj = HALO_PAD; jj < _chunk.y-HALO_PAD; ++jj)
    {
        for(int kk = 0; kk < depth; ++kk)
        {
            int base = jj*_chunk.x;
            buffer[base+(HALO_PAD-kk-1)] = buffer[base+(HALO_PAD+kk)];			
        }
    }
}

// Updates the right face.
void update_right(
        const int depth,
        double* buffer,
        int offload)
{
#pragma omp target device(_chunk.device_id) if(offload)
#pragma omp parallel for
    for(int jj = HALO_PAD; jj < _chunk.y-HALO_PAD; ++jj)
    {
        for(int kk = 0; kk < depth; ++kk)
        {
            int base = jj*_chunk.x;
            buffer[base+(_chunk.x-HALO_PAD+kk)] 
                = buffer[base+(_chunk.x-HALO_PAD-1-kk)];
        }
    }
}

// Updates the top face.
void update_top(
        const int depth, 
        double* buffer,
        int offload)
{
#pragma omp target device(_chunk.device_id) if(offload)
#pragma omp parallel for
    for(int jj = 0; jj < depth; ++jj)
    {
        for(int kk = HALO_PAD; kk < _chunk.x-HALO_PAD; ++kk)
        {
            int base = kk;
            buffer[base+(_chunk.y-HALO_PAD+jj)*_chunk.x] 
                = buffer[base+(_chunk.y-HALO_PAD-1-jj)*_chunk.x];
        }
    }
}

// Updates the bottom face.
void update_bottom(
        const int depth, 
        double* buffer,
        int offload)
{
#pragma omp target device(_chunk.device_id) if(offload)
#pragma omp parallel for
    for(int jj = 0; jj < depth; ++jj)
    {
        for(int kk = HALO_PAD; kk < _chunk.x-HALO_PAD; ++kk)
        {
            int base = kk;
            buffer[base+(HALO_PAD-jj-1)*_chunk.x] 
                = buffer[base+(HALO_PAD+jj)*_chunk.x];
        }
    }
}

// Updates each face in turn.
void update_face(
        const int* chunkNeighbours,
        const int depth,
        double* buffer,
        int offload)
{
#define UPDATE_FACE(face, updateKernel) \
    if(chunkNeighbours[face-1] == EXTERNAL_FACE)\
    {\
        updateKernel(depth,buffer,offload);\
    }

    UPDATE_FACE(CHUNK_LEFT, update_left);
    UPDATE_FACE(CHUNK_RIGHT, update_right);
    UPDATE_FACE(CHUNK_TOP, update_top);
    UPDATE_FACE(CHUNK_BOTTOM, update_bottom);
}

// Entry point for updating halo.  
void ext_update_halo_kernel_(
        const int* chunk,
        double* density,
        double* energy0,
        double* energy,
        double* u,
        double* p,
        double* sd,
        const int* chunkNeighbours,
        const int* fields,
        const int* depth,
        int* offload)
{
#define LAUNCH_UPDATE(index, buffer, depth)\
    if(fields[index-1])\
    {\
        update_face(chunkNeighbours, depth, buffer, *offload);\
    }

    LAUNCH_UPDATE(FIELD_DENSITY, density, *depth);
    LAUNCH_UPDATE(FIELD_P, p, *depth);
    LAUNCH_UPDATE(FIELD_ENERGY0, energy0, *depth);
    LAUNCH_UPDATE(FIELD_ENERGY1, energy, *depth);
    LAUNCH_UPDATE(FIELD_U, u, *depth);
    LAUNCH_UPDATE(FIELD_SD, sd, *depth);
#undef LAUNCH_UPDATE
}
