!Crown Copyright 2014 AWE.
!
! This file is part of TeaLeaf.
!
! TeaLeaf is free software: you can redistribute it and/or modify it under 
! the terms of the GNU General Public License as published by the 
! Free Software Foundation, either version 3 of the License, or (at your option) 
! any later version.
!
! TeaLeaf is distributed in the hope that it will be useful, but 
! WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or 
! FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more 
! details.
!
! You should have received a copy of the GNU General Public License along with 
! TeaLeaf. If not, see http://www.gnu.org/licenses/.

!>  @brief Fortran mpi buffer packing kernel
!>  @author Wayne Gaudin
!>  @details Packs/unpacks mpi send and receive buffers

MODULE pack_kernel_module

CONTAINS

SUBROUTINE tea_pack_message_left(x_min,x_max,y_min,y_max,field,                &
                                 left_snd_buffer,                              &
                                 CELL_DATA,VERTEX_DATA,X_FACE_DATA,Y_FACE_DATA,&
                                 depth,field_type,                             &
                                 buffer_offset,offload,dev_id)

  IMPLICIT NONE

  REAL(KIND=8) :: field(-1:,-1:) ! This seems to work for any type of mesh data
  REAL(KIND=8) :: left_snd_buffer(:)

  INTEGER      :: CELL_DATA,VERTEX_DATA,X_FACE_DATA,Y_FACE_DATA,offload,dev_id
  INTEGER      :: depth,field_type,x_min,x_max,y_min,y_max
  INTEGER      :: j,k,x_inc,y_inc,index,buffer_offset

  ! Pack 

  ! These array modifications still need to be added on, plus the donor data location changes as in update_halo
  IF(field_type.EQ.CELL_DATA) THEN
    x_inc=0
    y_inc=0
  ENDIF
  IF(field_type.EQ.VERTEX_DATA) THEN
    x_inc=1
    y_inc=1
  ENDIF
  IF(field_type.EQ.X_FACE_DATA) THEN
    x_inc=1
    y_inc=0
  ENDIF
  IF(field_type.EQ.Y_FACE_DATA) THEN
    x_inc=0
    y_inc=1
  ENDIF

!$OMP TARGET IF(offload.EQ.1) DEVICE(dev_id) &
!$OMP MAP(FROM:left_snd_buffer) MAP(TO:depth)
!$OMP PARALLEL DO PRIVATE(index)
  DO k=y_min-depth,y_max+y_inc+depth
    DO j=1,depth
      index=buffer_offset + j+(k+depth-1)*depth
      left_snd_buffer(index)=field(x_min+x_inc-1+j,k)
    ENDDO
  ENDDO
!$OMP END PARALLEL DO
!$OMP END TARGET

END SUBROUTINE tea_pack_message_left

SUBROUTINE tea_unpack_message_left(x_min,x_max,y_min,y_max,field,                &
                                   left_rcv_buffer,                              &
                                   CELL_DATA,VERTEX_DATA,X_FACE_DATA,Y_FACE_DATA,&
                                   depth,field_type,                             &
                                   buffer_offset,offload,dev_id)

  IMPLICIT NONE

  REAL(KIND=8) :: field(-1:,-1:) ! This seems to work for any type of mesh data
  REAL(KIND=8) :: left_rcv_buffer(:)

  INTEGER      :: CELL_DATA,VERTEX_DATA,X_FACE_DATA,Y_FACE_DATA
  INTEGER      :: depth,field_type,x_min,x_max,y_min,y_max
  INTEGER      :: j,k,x_inc,y_inc,index,buffer_offset,offload,dev_id

  ! Unpack 

  ! These array modifications still need to be added on, plus the donor data location changes as in update_halo
  IF(field_type.EQ.CELL_DATA) THEN
    x_inc=0
    y_inc=0
  ENDIF
  IF(field_type.EQ.VERTEX_DATA) THEN
    x_inc=1
    y_inc=1
  ENDIF
  IF(field_type.EQ.X_FACE_DATA) THEN
    x_inc=1
    y_inc=0
  ENDIF
  IF(field_type.EQ.Y_FACE_DATA) THEN
    x_inc=0
    y_inc=1
  ENDIF

!$OMP TARGET IF(offload.EQ.1) DEVICE(dev_id) &
!$OMP MAP(TO:left_rcv_buffer) MAP(TO:depth)
!$OMP PARALLEL DO PRIVATE(index)
  DO k=y_min-depth,y_max+y_inc+depth
    DO j=1,depth
      index= buffer_offset + j+(k+depth-1)*depth
      field(x_min-j,k)=left_rcv_buffer(index)
    ENDDO
  ENDDO
!$OMP END PARALLEL DO
!$OMP END TARGET

END SUBROUTINE tea_unpack_message_left

SUBROUTINE tea_pack_message_right(x_min,x_max,y_min,y_max,field,                &
                                  right_snd_buffer,                             &
                                  CELL_DATA,VERTEX_DATA,X_FACE_DATA,Y_FACE_DATA,&
                                  depth,field_type,                             &
                                  buffer_offset,offload,dev_id)

  IMPLICIT NONE

  REAL(KIND=8) :: field(-1:,-1:) ! This seems to work for any type of mesh data
  REAL(KIND=8) :: right_snd_buffer(:)

  INTEGER      :: CELL_DATA,VERTEX_DATA,X_FACE_DATA,Y_FACE_DATA
  INTEGER      :: depth,field_type,x_min,x_max,y_min,y_max
  INTEGER      :: j,k,x_inc,y_inc,index,buffer_offset,offload,dev_id

  ! Pack 

  ! These array modifications still need to be added on, plus the donor data location changes as in update_halo
  IF(field_type.EQ.CELL_DATA) THEN
    x_inc=0
    y_inc=0
  ENDIF
  IF(field_type.EQ.VERTEX_DATA) THEN
    x_inc=1
    y_inc=1
  ENDIF
  IF(field_type.EQ.X_FACE_DATA) THEN
    x_inc=1
    y_inc=0
  ENDIF
  IF(field_type.EQ.Y_FACE_DATA) THEN
    x_inc=0
    y_inc=1
  ENDIF

!$OMP TARGET IF(offload.EQ.1) DEVICE(dev_id) &
!$OMP MAP(FROM:right_snd_buffer) MAP(TO:depth)
!$OMP PARALLEL DO PRIVATE(index)
  DO k=y_min-depth,y_max+y_inc+depth
    DO j=1,depth
      index= buffer_offset + j+(k+depth-1)*depth
      right_snd_buffer(index)=field(x_max+1-j,k)
    ENDDO
  ENDDO
!$OMP END PARALLEL DO
!$OMP END TARGET

END SUBROUTINE tea_pack_message_right

SUBROUTINE tea_unpack_message_right(x_min,x_max,y_min,y_max,field,                &
                                    right_rcv_buffer,                             &
                                    CELL_DATA,VERTEX_DATA,X_FACE_DATA,Y_FACE_DATA,&
                                    depth,field_type,                             &
                                    buffer_offset,offload,dev_id)

  IMPLICIT NONE

  REAL(KIND=8) :: field(-1:,-1:) ! This seems to work for any type of mesh data
  REAL(KIND=8) :: right_rcv_buffer(:)

  INTEGER      :: CELL_DATA,VERTEX_DATA,X_FACE_DATA,Y_FACE_DATA
  INTEGER      :: depth,field_type,x_min,x_max,y_min,y_max
  INTEGER      :: j,k,x_inc,y_inc,index,buffer_offset,offload,dev_id

  ! Unpack 

  ! These array modifications still need to be added on, plus the donor data location changes as in update_halo
  IF(field_type.EQ.CELL_DATA) THEN
    x_inc=0
    y_inc=0
  ENDIF
  IF(field_type.EQ.VERTEX_DATA) THEN
    x_inc=1
    y_inc=1
  ENDIF
  IF(field_type.EQ.X_FACE_DATA) THEN
    x_inc=1
    y_inc=0
  ENDIF
  IF(field_type.EQ.Y_FACE_DATA) THEN
    x_inc=0
    y_inc=1
  ENDIF

!$OMP TARGET IF(offload.EQ.1) DEVICE(dev_id) &
!$OMP MAP(TO:right_rcv_buffer) MAP(TO:depth)
!$OMP PARALLEL DO PRIVATE(index)
  DO k=y_min-depth,y_max+y_inc+depth
    DO j=1,depth
      index= buffer_offset + j+(k+depth-1)*depth
      field(x_max+x_inc+j,k)=right_rcv_buffer(index)
    ENDDO
  ENDDO
!$OMP END PARALLEL DO
!$OMP END TARGET

END SUBROUTINE tea_unpack_message_right

SUBROUTINE tea_pack_message_top(x_min,x_max,y_min,y_max,field,                &
                                top_snd_buffer,                               &
                                CELL_DATA,VERTEX_DATA,X_FACE_DATA,Y_FACE_DATA,&
                                depth,field_type,                             &
                                buffer_offset,offload,dev_id)

  IMPLICIT NONE

  REAL(KIND=8) :: field(-1:,-1:) ! This seems to work for any type of mesh data
  REAL(KIND=8) :: top_snd_buffer(:)

  INTEGER      :: CELL_DATA,VERTEX_DATA,X_FACE_DATA,Y_FACE_DATA
  INTEGER      :: depth,field_type,x_min,x_max,y_min,y_max
  INTEGER      :: j,k,x_inc,y_inc,index,buffer_offset,offload,dev_id

  ! Pack 

  ! These array modifications still need to be added on, plus the donor data location changes as in update_halo
  IF(field_type.EQ.CELL_DATA) THEN
    x_inc=0
    y_inc=0
  ENDIF
  IF(field_type.EQ.VERTEX_DATA) THEN
    x_inc=1
    y_inc=1
  ENDIF
  IF(field_type.EQ.X_FACE_DATA) THEN
    x_inc=1
    y_inc=0
  ENDIF
  IF(field_type.EQ.Y_FACE_DATA) THEN
    x_inc=0
    y_inc=1
  ENDIF

  DO k=1,depth
!$OMP TARGET IF(offload.EQ.1) DEVICE(dev_id) &
!$OMP MAP(FROM:top_snd_buffer) MAP(TO:depth)
!$OMP PARALLEL DO PRIVATE(index)
    DO j=x_min-depth,x_max+x_inc+depth
      index= buffer_offset + j+depth+(k-1)*(x_max+x_inc+(2*depth))
      top_snd_buffer(index)=field(j,y_max+1-k)
    ENDDO
!$OMP END PARALLEL DO
!$OMP END TARGET
  ENDDO

END SUBROUTINE tea_pack_message_top

SUBROUTINE tea_unpack_message_top(x_min,x_max,y_min,y_max,field,                &
                                  top_rcv_buffer,                               &
                                  CELL_DATA,VERTEX_DATA,X_FACE_DATA,Y_FACE_DATA,&
                                  depth,field_type,                             &
                                  buffer_offset,offload,dev_id)

  IMPLICIT NONE

  REAL(KIND=8) :: field(-1:,-1:) ! This seems to work for any type of mesh data
  REAL(KIND=8) :: top_rcv_buffer(:)

  INTEGER      :: CELL_DATA,VERTEX_DATA,X_FACE_DATA,Y_FACE_DATA
  INTEGER      :: depth,field_type,x_min,x_max,y_min,y_max
  INTEGER      :: j,k,x_inc,y_inc,index,buffer_offset,offload,dev_id

  ! Unpack 

  ! These array modifications still need to be added on, plus the donor data location changes as in update_halo
  IF(field_type.EQ.CELL_DATA) THEN
    x_inc=0
    y_inc=0
  ENDIF
  IF(field_type.EQ.VERTEX_DATA) THEN
    x_inc=1
    y_inc=1
  ENDIF
  IF(field_type.EQ.X_FACE_DATA) THEN
    x_inc=1
    y_inc=0
  ENDIF
  IF(field_type.EQ.Y_FACE_DATA) THEN
    x_inc=0
    y_inc=1
  ENDIF

  DO k=1,depth
!$OMP TARGET IF(offload.EQ.1) DEVICE(dev_id) &
!$OMP MAP(TO:top_rcv_buffer) MAP(TO:depth)
!$OMP PARALLEL DO PRIVATE(index)
    DO j=x_min-depth,x_max+x_inc+depth
      index= buffer_offset + j + depth+(k-1)*(x_max+x_inc+(2*depth))
      field(j,y_max+y_inc+k)=top_rcv_buffer(index)
    ENDDO
!$OMP END PARALLEL DO
!$OMP END TARGET
  ENDDO

END SUBROUTINE tea_unpack_message_top

SUBROUTINE tea_pack_message_bottom(x_min,x_max,y_min,y_max,field,                &
                                   bottom_snd_buffer,                            &
                                   CELL_DATA,VERTEX_DATA,X_FACE_DATA,Y_FACE_DATA,&
                                   depth,field_type,                             &
                                   buffer_offset,offload,dev_id)

  IMPLICIT NONE

  REAL(KIND=8) :: field(-1:,-1:) ! This seems to work for any type of mesh data
  REAL(KIND=8) :: bottom_snd_buffer(:)

  INTEGER      :: CELL_DATA,VERTEX_DATA,X_FACE_DATA,Y_FACE_DATA
  INTEGER      :: depth,field_type,x_min,x_max,y_min,y_max
  INTEGER      :: j,k,x_inc,y_inc,index,buffer_offset,offload,dev_id

  ! Pack 

  ! These array modifications still need to be added on, plus the donor data location changes as in update_halo
  IF(field_type.EQ.CELL_DATA) THEN
    x_inc=0
    y_inc=0
  ENDIF
  IF(field_type.EQ.VERTEX_DATA) THEN
    x_inc=1
    y_inc=1
  ENDIF
  IF(field_type.EQ.X_FACE_DATA) THEN
    x_inc=1
    y_inc=0
  ENDIF
  IF(field_type.EQ.Y_FACE_DATA) THEN
    x_inc=0
    y_inc=1
  ENDIF

!$OMP TARGET IF(offload.EQ.1) DEVICE(dev_id) &
!$OMP MAP(FROM:bottom_snd_buffer) MAP(TO:depth)
!$OMP PARALLEL DO PRIVATE(index)
  DO k=1,depth
    DO j=x_min-depth,x_max+x_inc+depth
      index= buffer_offset + j+depth+(k-1)*(x_max+x_inc+(2*depth))
      bottom_snd_buffer(index)=field(j,y_min+y_inc-1+k)
    ENDDO
  ENDDO
!$OMP END PARALLEL DO
!$OMP END TARGET

END SUBROUTINE tea_pack_message_bottom

SUBROUTINE tea_unpack_message_bottom(x_min,x_max,y_min,y_max,field,                &
                                     bottom_rcv_buffer,                            &
                                     CELL_DATA,VERTEX_DATA,X_FACE_DATA,Y_FACE_DATA,&
                                     depth,field_type,                             &
                                     buffer_offset,offload,dev_id)

  IMPLICIT NONE

  REAL(KIND=8) :: field(-1:,-1:) ! This seems to work for any type of mesh data
  REAL(KIND=8) :: bottom_rcv_buffer(:)

  INTEGER      :: CELL_DATA,VERTEX_DATA,X_FACE_DATA,Y_FACE_DATA
  INTEGER      :: depth,field_type,x_min,x_max,y_min,y_max
  INTEGER      :: j,k,x_inc,y_inc,index,buffer_offset,offload,dev_id

  ! Unpack 

  ! These array modifications still need to be added on, plus the donor data location changes as in update_halo
  IF(field_type.EQ.CELL_DATA) THEN
    x_inc=0
    y_inc=0
  ENDIF
  IF(field_type.EQ.VERTEX_DATA) THEN
    x_inc=1
    y_inc=1
  ENDIF
  IF(field_type.EQ.X_FACE_DATA) THEN
    x_inc=1
    y_inc=0
  ENDIF
  IF(field_type.EQ.Y_FACE_DATA) THEN
    x_inc=0
    y_inc=1
  ENDIF

  DO k=1,depth
!$OMP TARGET IF(offload.EQ.1) DEVICE(dev_id) &
!$OMP MAP(TO:bottom_rcv_buffer) MAP(TO:depth)
!$OMP PARALLEL DO PRIVATE(index)
    DO j=x_min-depth,x_max+x_inc+depth
      index= buffer_offset + j+depth+(k-1)*(x_max+x_inc+(2*depth))
      field(j,y_min-k)=bottom_rcv_buffer(index)
    ENDDO
!$OMP END PARALLEL DO
!$OMP END TARGET
  ENDDO

END SUBROUTINE tea_unpack_message_bottom

END MODULE pack_kernel_module
