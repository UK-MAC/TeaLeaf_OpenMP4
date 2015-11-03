MODULE tea_leaf_kernel_ppcg_module

    USE tea_leaf_kernel_module
    USE tea_leaf_kernel_cheby_module

    IMPLICIT NONE

CONTAINS

    SUBROUTINE tea_leaf_kernel_ppcg_init_sd(x_min,             &
            x_max,             &
            y_min,             &
            y_max,             &
            r,            &
            sd, &
            theta, &
            dev_id)

        IMPLICIT NONE

        INTEGER(KIND=4):: x_min,x_max,y_min,y_max
        REAL(KIND=8), DIMENSION(x_min-2:x_max+2,y_min-2:y_max+2) :: r, sd
        REAL(KIND=8) :: theta

        INTEGER :: j,k,l,dev_id

        !$OMP TARGET DEVICE(dev_id)
        !$OMP PARALLEL
        !$OMP DO
        DO k=y_min,y_max
            DO j=x_min,x_max
                sd(j, k) = r(j, k)/theta
            ENDDO
        ENDDO
        !$OMP END DO
        !$OMP END PARALLEL
        !$OMP END TARGET

    END SUBROUTINE tea_leaf_kernel_ppcg_init_sd

    SUBROUTINE tea_leaf_kernel_ppcg_inner(x_min,             &
            x_max,             &
            y_min,             &
            y_max,             &
            step, &
            alpha, &
            beta, &
            rx, ry, &
            u, &
            r, &
            Kx, &
            Ky, &
            sd, &
            dev_id)
        IMPLICIT NONE

        INTEGER(KIND=4):: x_min,x_max,y_min,y_max,dev_id
        REAL(KIND=8), DIMENSION(x_min-2:x_max+2,y_min-2:y_max+2) :: u, r, Kx, Ky, sd
        INTEGER(KIND=4) :: j,k,l, step
        REAL(KIND=8), DIMENSION(:) :: alpha, beta
        REAL(KIND=8) :: smvp, rx, ry

        !$OMP TARGET DEVICE(dev_id)
        !$OMP PARALLEL
        !$OMP DO
        DO k=y_min,y_max
            DO j=x_min,x_max
                smvp = (1.0_8                                      &
                    + rx*(Kx(j+1, k) + Kx(j, k)) &
                    + ry*(Ky(j, k+1) + Ky(j, k)))*sd(j, k)             &
                    - rx*(Kx(j+1, k)*sd(j+1, k) + Kx(j, k)*sd(j-1, k)) &
                    - ry*(Ky(j, k+1)*sd(j, k+1) + Ky(j, k)*sd(j, k-1))
                r(j, k) = r(j, k) - smvp
                u(j, k) = u(j, k) + sd(j, k)
            ENDDO
        ENDDO
        !$OMP END DO
        !$OMP DO
        DO k=y_min,y_max
            DO j=x_min,x_max
                sd(j, k) = alpha(step)*sd(j, k) + beta(step)*r(j, k)
            ENDDO
        ENDDO
        !$OMP END DO
        !$OMP END PARALLEL
        !$OMP END TARGET

    end SUBROUTINE

    SUBROUTINE tea_leaf_kernel_ppcg_init_p(x_min,             &
            x_max,             &
            y_min,             &
            y_max,             &
            p,                &
            r,            &
            rro, &
            dev_id)

        IMPLICIT NONE

        INTEGER(KIND=4):: x_min,x_max,y_min,y_max,dev_id
        REAL(KIND=8), DIMENSION(x_min-2:x_max+2,y_min-2:y_max+2) :: p, r

        INTEGER :: j,k,l
        REAL(KIND=8) ::  rro


        rro = 0.0_8

        !$OMP TARGET DEVICE(dev_id)
        !$OMP PARALLEL
        !$OMP DO REDUCTION(+:rro)
        DO k=y_min,y_max
            DO j=x_min,x_max
                p(j, k) = r(j, k)
                rro = rro + p(j, k)*r(j, k)
            ENDDO
        ENDDO
        !$OMP END DO
        !$OMP END PARALLEL
        !$OMP END TARGET

    END SUBROUTINE

    SUBROUTINE tea_calc_ls_coefs(ch_alphas, ch_betas, eigmin, eigmax, &
            theta, ppcg_inner_steps)

        INTEGER :: n, ppcg_inner_steps,dev_id
        REAL(KIND=8), DIMENSION(ppcg_inner_steps) :: ch_alphas, ch_betas
        REAL(KIND=8) :: eigmin, eigmax

        REAL(KIND=8) :: theta, delta, sigma, rho_old, rho_new, cur_alpha, cur_beta

        ! TODO
        CALL tea_calc_ch_coefs(ch_alphas, ch_betas, eigmin, eigmax, &
            theta, ppcg_inner_steps)

    END SUBROUTINE

END MODULE


