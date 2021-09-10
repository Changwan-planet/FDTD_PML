SUBROUTINE 3D_PML_E
IMPLICIT NONE

INCLUDE "FDTD_3D_var.h"
INCLUDE "PML_para.h"
INCLUDE "PML_var.h"

!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


!     ++++++++++++++++++++++++
!++++++YZ-plane : normal to X++++++
!     ++++++++++++++++++++++++
MT_MS_x =    -MS_PML2   !Minus Temporary (MT)
PT_MS_x =     MS_PML2   !Plus Temporary  (PT)
MT_MS_y =    -MS_PML2
PT_MS_y =MS_y+MS_PML2
MT_MS_z =    -MS_PML2
PT_MS_z =MS_z+MS_PML2

!     ++++++++++++++++++
!++++++YZ-plane : x < 0++++++
!     ++++++++++++++++++

DO ii = -MS_PML, -1
DO kk =  ii, MS_z-ii
DO jj =  ii, MS_y-ii

i = ii
j = jj
k = kk

CALL PML_FDTD_H(i,j,k, ii,jj,kk&
               &YZ_H_xy, YZ_H_xz, YZ_H_yz, YZ_H_yx, YZ_H_zx, YZ_H_zy&
               &MT_MS_X,PT_MS_x, MT_MS_y,PT_MS_y, MT_MS_z,PT_MS_z)

END DO
END DO
END DO 

!     ++++++++++++++++++
!++++++YZ-plane : x > MS_x++++++
!     ++++++++++++++++++

DO ii =  1,MS_PML
DO kk =  -ii, MS_z+ii
DO jj =  -ii, MS_y+ii

i = ii+MS_x
j = jj
k = kk

CALL PML_FDTD_H(i,j,k, ii,jj,kk&
               &YZ_H_xy, YZ_H_xz, YZ_H_yz, YZ_H_yx, YZ_H_zx, YZ_H_zy&
               &MT_MS_X,PT_MS_x, MT_MS_y,PT_MS_y, MT_MS_z,PT_MS_z)

END DO
END DO
END DO 



!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



!     ++++++++++++++++++++++++
!++++++ZX-plane : normal to Y++++++
!     ++++++++++++++++++++++++
MT_MS_x =    -MS_PML2   !Minus Temporary (MT)
PT_MS_x =MS_x+MS_PML2   !Plus Temporary  (PT)
MT_MS_y =    -MS_PML2
PT_MS_y =     MS_PML2
MT_MS_z =    -MS_PML2
PT_MS_z =MS_z+MS_PML2

!     ++++++++++++++++++
!++++++ZX-plane : y < 0++++++
!     ++++++++++++++++++
DO jj = -MS_PML, -1
DO kk =  jj, MS_z-jj
DO ii =  jj, MS_x-(jj+1)

i = ii
j = jj
k = kk

CALL PML_FDTD_H(i,j,k, ii,jj,kk&
               &ZX_H_xy, ZX_H_xz, ZX_H_yz, ZX_H_yx, ZX_H_zx, ZX_H_zy&
               &MT_MS_X,PT_MS_x, MT_MS_y,PT_MS_y, MT_MS_z,PT_MS_z)

END DO
END DO
END DO 

!     ++++++++++++++++++
!++++++ZX-plane : y > MS_y++++++
!     ++++++++++++++++++
DO jj =   1, MS_PML
DO kk =  -jj    , MS_z+jj
DO ii =  -(jj-1), MS_x-(jj-1)

i = ii
j = jj+MS_y
k = kk

CALL PML_FDTD_H(i,j,k, ii,jj,kk&
               &ZX_H_xy, ZX_H_xz, ZX_H_yz, ZX_H_yx, ZX_H_zx, ZX_H_zy&
               &MT_MS_X,PT_MS_x, MT_MS_y,PT_MS_y, MT_MS_z,PT_MS_z)

END DO
END DO
END DO 



!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


!     ++++++++++++++++++++++++
!++++++XY-plane : normal to Z++++++
!     ++++++++++++++++++++++++
MT_MS_x =    -MS_PML2   !Minus Temporary (MT)
PT_MS_x =MS_x+MS_PML2   !Plus Temporary  (PT)
MT_MS_y =    -MS_PML2
PT_MS_y =MS_y+MS_PML2
MT_MS_z =    -MS_PML2
PT_MS_z =     MS_PML2

!     ++++++++++++++++++
!++++++XY-plane : z < 0++++++
!     ++++++++++++++++++
DO kk = -MS_PML, -1
DO jj =  kk+1, MS_y-(kk+1)
DO ii =  kk+1, MS_x-(kk+1)

i = ii
j = jj
k = kk

CALL PML_FDTD_H(i,j,k, ii,jj,kk&
               &XY_H_xy, XY_H_xz, XY_H_yz, XY_H_yx, XY_H_zx, XY_H_zy&
               &MT_MS_X,PT_MS_x, MT_MS_y,PT_MS_y, MT_MS_z,PT_MS_z)

END DO
END DO
END DO 

!     ++++++++++++++++++
!++++++XY-plane : z > MS_z++++++
!     ++++++++++++++++++
DO jj =   1, MS_PML
DO kk =  -(kk-1), MS_y+kk-1
DO ii =  -(kk-1), MS_x-(kk-1)

i = ii
j = jj
k = kk+MS_z

CALL PML_FDTD_H(i,j,k, ii,jj,kk&
               &XY_H_xy, XY_H_xz, XY_H_yz, XY_H_yx, XY_H_zx, XY_H_zy&
               &MT_MS_X,PT_MS_x, MT_MS_y,PT_MS_y, MT_MS_z,PT_MS_z)

END DO
END DO
END DO 



END SUBROUTINE 3D_PML_H


SUBROUTINE PML_FDTD_H(i,j,k, ii,jj,kk&
                     &H_xy, H_xz, H_yz, H_yx, H_zx, H_zy&
                     &MT_MS_X,PT_MS_x, MT_MS_y,PT_MS_y, MT_MS_z,PT_MS_z) 
IMPLICIT REAL*8 (a-h,o-z)

INCLUDE "FDTD_3D_para.h"
INCLUDE "FDTD_3D_var.h"

REAL*8 ::&
&H_xy(MT_MS_x:PT_MS_x, MT_MS_y:PT_MS_y, MT_MS_z:PT_MS_z),
&H_yz(MT_MS_x:PT_MS_x, MT_MS_y:PT_MS_y, MT_MS_z:PT_MS_z),
&H_yz(MT_MS_x:PT_MS_x, MT_MS_y:PT_MS_y, MT_MS_z:PT_MS_z),
&H_yx(MT_MS_x:PT_MS_x, MT_MS_y:PT_MS_y, MT_MS_z:PT_MS_z),
&H_zx(MT_MS_x:PT_MS_x, MT_MS_y:PT_MS_y, MT_MS_z:PT_MS_z),
&H_zy(MT_MS_x:PT_MS_x, MT_MS_y:PT_MS_y, MT_MS_z:PT_MS_z)

H_xy(ii,jj,kk) = Da_y(i,j,k) * H_xy(ii,jj,kk)&+
                &Db_y(i,j,k) * ( -(E_z(i  ,j+1,k  ) - E_z(i  ,j  ,k  ))/MS_y)
H_xz(ii,jj,kk) = Da_y(i,j,k) * H_xz(ii,jj,kk)&+
                &Db_y(i,j,k) * (  (E_y(i  ,j+1,k  ) - E_y(i  ,j  ,k  ))/MS_z)
H_yz(ii,jj,kk) = Da_y(i,j,k) * H_yz(ii,jj,kk)&+
                &Db_y(i,j,k) * ( -(E_x(i  ,j  ,k+1) - E_x(i  ,j  ,k  ))/MS_z)
H_yx(ii,jj,kk) = Da_y(i,j,k) * H_yx(ii,jj,kk)&+
                &Db_y(i,j,k) * (  (E_z(i+1,j  ,k  ) - E_z(i   ,j  ,k  ))/MS_x)
H_zx(ii,jj,kk) = Da_y(i,j,k) * H_zx(ii,jj,kk)&+
                &Db_y(i,j,k) * ( -(E_y(i+1,j  ,k  ) - E_y(i  ,j  ,k  ))/MS_x)
H_zy(ii,jj,kk) = Da_y(i,j,k) * H_zy(ii,jj,kk)&+
                &Db_y(i,j,k) * (  (E_x(i  ,j+1,k  ) - E_x(i  ,j  ,k  ))/MS_y)
               
H_x(i,j,k) = H_xy(ii,jj,kk) + H_xz(ii,jj,kk) 
H_y(i,j,k) = H_yz(ii,jj,kk) + H_yx(ii,jj,kk)
H_z(i,j,k) = H_zy(ii,jj,kk) + H_zx(ii,jj,kk)

RETURN

END PML_FDTD_H 

