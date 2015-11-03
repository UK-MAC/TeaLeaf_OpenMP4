#include "ext_chunk.h"
#include <stdlib.h>

#define CELL_DATA 1
#define VERTEX_DATA 2
#define X_FACE_DATA 3
#define Y_FACE_DATA 4
#define WARP_SIZE 32.0

// Packs left data into buffer.
void pack_left(
        const int xMax,
        const int yMax,
        double* field,
        double* buffer,
        const int depth,
        int offload)
{
#pragma omp target device(_chunk.device_id) if(offload) \
    map(from: buffer[:depth*_chunk.innerY])
#pragma omp parallel for
    for(int jj = HALO_PAD; jj < yMax-HALO_PAD; ++jj)
    {
        for(int kk = HALO_PAD; kk < HALO_PAD+depth; ++kk)
        {
            int bufIndex = (kk-HALO_PAD) +
                (jj-HALO_PAD)*depth;
            buffer[bufIndex] = field[jj*xMax+kk];
        }
    }
}

// Packs right data into buffer.
void pack_right(
        const int xMax,
        const int yMax,
        double* field,
        double* buffer,
        const int depth,
        int offload)
{
#pragma omp target device(_chunk.device_id) if(offload) \
    map(from: buffer[:depth*_chunk.innerY])
#pragma omp parallel for
    for(int jj = HALO_PAD; jj < yMax-HALO_PAD; ++jj)
    {
        for(int kk = xMax-HALO_PAD-depth; kk < xMax-HALO_PAD; ++kk)
        {
            int bufIndex = (kk - (xMax-HALO_PAD-depth)) +
                (jj-HALO_PAD)*depth;
            buffer[bufIndex] = field[jj*xMax+kk];
        }
    }
}

// Packs top data into buffer.
void pack_top(
        const int xMax,
        const int yMax,
        double* field,
        double* buffer,
        const int depth,
        int offload)
{
#pragma omp target device(_chunk.device_id) if(offload) \
    map(from: buffer[:_chunk.innerX*depth])
#pragma omp parallel for
    for(int jj = yMax-HALO_PAD-depth; jj < yMax-HALO_PAD; ++jj)
    {
        for(int kk = HALO_PAD; kk < xMax-HALO_PAD; ++kk)
        {
            int bufIndex = (kk-HALO_PAD) +
                (jj-(yMax-HALO_PAD-depth))*_chunk.innerX;
            buffer[bufIndex] = field[jj*xMax+kk];
        }
    }
}

// Packs bottom data into buffer.
void pack_bottom(
        const int xMax,
        const int yMax,
        double* field,
        double* buffer,
        const int depth,
        int offload)
{
#pragma omp target device(_chunk.device_id) if(offload) \
    map(from: buffer[:_chunk.innerX*depth])
#pragma omp parallel for
    for(int jj = HALO_PAD; jj < HALO_PAD+depth; ++jj)
    {
        for(int kk = HALO_PAD; kk < xMax-HALO_PAD; ++kk)
        {
            int bufIndex = (kk-HALO_PAD) +
                (jj-HALO_PAD)*_chunk.innerX;
            buffer[bufIndex] = field[jj*xMax+kk];
        }
    }
}

// Unpacks left data from buffer.
void unpack_left(
        const int xMax,
        const int yMax,
        double* field,
        double* buffer,
        const int depth,
        int offload)
{
#pragma omp target device(_chunk.device_id) if(offload) \
    map(to: buffer[:depth*_chunk.innerY])
#pragma omp parallel for
    for(int jj = HALO_PAD; jj < yMax-HALO_PAD; ++jj)
    {
        for(int kk = HALO_PAD-depth; kk < HALO_PAD; ++kk)
        {
            int bufIndex = (kk-(HALO_PAD-depth)) + 
                (jj-HALO_PAD)*depth;
            field[jj*xMax+kk] = buffer[bufIndex];
        }
    }
}

// Unpacks right data from buffer.
void unpack_right(
        const int xMax,
        const int yMax,
        double* field,
        double* buffer,
        const int depth,
        int offload)
{
#pragma omp target device(_chunk.device_id) if(offload) \
    map(to: buffer[:depth*_chunk.innerY])
#pragma omp parallel for
    for(int jj = HALO_PAD; jj < yMax-HALO_PAD; ++jj)
    {
        for(int kk = xMax-HALO_PAD; kk < xMax-HALO_PAD+depth; ++kk)
        {
            int bufIndex = (kk-(xMax-HALO_PAD)) + (jj-HALO_PAD)*depth;
            field[jj*xMax+kk] = buffer[bufIndex];
        }
    }
}

// Unpacks top data from buffer.
void unpack_top(
        const int xMax,
        const int yMax,
        double* field,
        double* buffer,
        const int depth,
        int offload)
{
#pragma omp target device(_chunk.device_id) if(offload) \
    map(to: buffer[:_chunk.innerX*depth])
#pragma omp parallel for
    for(int jj = yMax-HALO_PAD; jj < yMax-HALO_PAD+depth; ++jj)
    {
        for(int kk = HALO_PAD; kk < xMax-HALO_PAD; ++kk)
        {
            int bufIndex = (kk-HALO_PAD) + 
                (jj-(yMax-HALO_PAD))*_chunk.innerX;
            field[jj*xMax+kk] = buffer[bufIndex];
        }
    }
}

// Unpacks bottom data from buffer.
void unpack_bottom(
        const int xMax,
        const int yMax,
        double* field,
        double* buffer,
        const int depth,
        int offload)
{
#pragma omp target device(_chunk.device_id) if(offload) \
    map(to: buffer[:_chunk.innerX*depth])
#pragma omp parallel for
    for(int jj = HALO_PAD-depth; jj < HALO_PAD; ++jj)
    {
        for(int kk = HALO_PAD; kk < xMax-HALO_PAD; ++kk)
        {
            int bufIndex = (kk-HALO_PAD) + 
                (jj-(HALO_PAD-depth))*_chunk.innerX;
            field[jj*xMax+kk] = buffer[bufIndex];
        }
    }
}

typedef void (*PackKernel)(int,int,double*,double*,int,int);

// Either packs or unpacks data from/to buffers.
void pack_unpack_kernel(
        double* density,
        double* energy0,
        double* energy,
        double* u,
        double* p,
        double* sd,
        const int* fields,
        const int* offsets,
        const int depth,
        const int face,
        double* buffer,
        const int pack,
        int offload)
{
    int exchanges = 0;
    for(int ii = 0; ii != NUM_FIELDS; ++ii)
    {
        exchanges += fields[ii];
    }

    if(exchanges < 1) return;

    PackKernel kernel;

    switch(face)
    {
        case CHUNK_LEFT:
            kernel = pack ? pack_left : unpack_left;
            break;
        case CHUNK_RIGHT:
            kernel = pack ? pack_right : unpack_right;
            break;
        case CHUNK_TOP:
            kernel = pack ? pack_top : unpack_top;
            break;
        case CHUNK_BOTTOM:
            kernel = pack ? pack_bottom : unpack_bottom;
            break;
        default:
            panic(__LINE__, __FILE__, "Incorrect face provided: %d.\n", face);
    }

    for(int ii = 0; ii < NUM_FIELDS; ++ii)
    {
        if(fields[ii])
        {
            double* deviceField = NULL;
            switch(ii+1)
            {
                case FIELD_DENSITY:
                    deviceField = density;
                    break;
                case FIELD_ENERGY0:
                    deviceField = energy0;
                    break;
                case FIELD_ENERGY1:
                    deviceField = energy;
                    break;
                case FIELD_U:
                    deviceField = u;
                    break;
                case FIELD_P:
                    deviceField = p;
                    break;
                case FIELD_SD:
                    deviceField = sd;
                    break;
                default:
                    panic(__LINE__,__FILE__, "Incorrect field provided: %d.\n", ii+1);
            }

            kernel(_chunk.x, _chunk.y, deviceField, buffer+offsets[ii], depth, offload);
        }
    }
}

// Entry point for packing messages.
void ext_pack_message_(
        const int* chunk,
        double* density,
        double* energy0,
        double* energy,
        double* u,
        double* p,
        double* sd,
        const int* fields,
        const int* offsets,
        const int* depth,
        const int* face,
        double* buffer,
        int* offload)
{
    pack_unpack_kernel(
            density, energy0, energy, u, p, sd,
            fields, offsets, *depth, *face, buffer, 1, *offload);
}

// Entry point for unpacking messages.
void ext_unpack_message_(
        const int* chunk,
        double* density,
        double* energy0,
        double* energy,
        double* u,
        double* p,
        double* sd,
        const int* fields,
        const int* offsets,
        const int* depth,
        const int* face,
        double* buffer,
        int* offload)
{
    pack_unpack_kernel(
            density, energy0, energy, u, p, sd,
            fields, offsets, *depth, *face, buffer, 0, *offload);
}
