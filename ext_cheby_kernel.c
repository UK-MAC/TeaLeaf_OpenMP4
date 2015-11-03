#include "ext_chunk.h"

/*
 *		CHEBYSHEV SOLVER KERNEL
 */

// Calculates the new value for u.
void cheby_calc_u(
        double* u,
        double* p)
{
    START_PROFILING;
#pragma omp target device(_chunk.device_id)
#pragma omp parallel for
    for(int jj = HALO_PAD; jj < _chunk.y-HALO_PAD; ++jj)
    {
        for(int kk = HALO_PAD; kk < _chunk.x-HALO_PAD; ++kk)
        {	
            const int index = jj*_chunk.x+kk;
            u[index] += p[index];
        }
    }
    STOP_PROFILING(__func__);
}

// Entry point for Chebyshev initialisation.
void ext_cheby_solver_init_(
        const int* chunk,
        double* u,
        double* u0,
        double* p,
        double* r,
        double* mi,
        double* w,
        double* z,
        double* kx,
        double* ky,
        const double* theta,
        const int* preconditioner)
{
    START_PROFILING;
#pragma omp target device(_chunk.device_id)
#pragma omp parallel for
    for(int jj = HALO_PAD; jj < _chunk.y-HALO_PAD; ++jj)
    {
        for(int kk = HALO_PAD; kk < _chunk.x-HALO_PAD; ++kk)
        {
            const int index = jj*_chunk.x+kk;
            const double smvp = SMVP(u);
            w[index] = smvp;
            r[index] = u0[index]-w[index];
            p[index] = r[index]/ *theta;
        }
    }
    STOP_PROFILING(__func__);

    cheby_calc_u(u, p);
}

// Entry point for the Chebyshev iterations.
void ext_cheby_solver_iterate_(
        const int* chunk,
        double* u,
        double* u0,
        double* p,
        double* r,
        double* mi,
        double* w,
        double* z,
        double* kx,
        double* ky,
        double* alphas,
        double* betas,
        int* step,
        int* maxSteps)
{
    START_PROFILING;
    double alpha = alphas[*step-1];
    double beta = betas[*step-1];

#pragma omp target device(_chunk.device_id)
#pragma omp parallel for
    for(int jj = HALO_PAD; jj < _chunk.y-HALO_PAD; ++jj)
    {
        for(int kk = HALO_PAD; kk < _chunk.x-HALO_PAD; ++kk)
        {	
            const int index = jj*_chunk.x+kk;
            const double smvp = SMVP(u);
            w[index] = smvp;
            r[index] = u0[index]-w[index];
            p[index] = alpha*p[index] + beta*r[index];
        }
    }
    STOP_PROFILING(__func__);

    cheby_calc_u(u, p);
}
