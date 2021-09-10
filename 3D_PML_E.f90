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

CALL PML_FDTD_E(i,j,k, ii,jj,kk&
               &YZ_E_xy, YZ_E_xz, YZ_E_yz, YZ_E_yx, YZ_E_zx, YZ_E_zy&
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

CALL PML_FDTD_E(i,j,k, ii,jj,kk&
               &YZ_E_xy, YZ_E_xz, YZ_E_yz, YZ_E_yx, YZ_E_zx, YZ_E_zy&
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

CALL PML_FDTD_E(i,j,k, ii,jj,kk&
               &ZX_E_xy, ZX_E_xz, ZX_E_yz, ZX_E_yx, ZX_E_zx, ZX_E_zy&
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

CALL PML_FDTD_E(i,j,k, ii,jj,kk&
               &ZX_E_xy, ZX_E_xz, ZX_E_yz, ZX_E_yx, ZX_E_zx, ZX_E_zy&
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

CALL PML_FDTD_E(i,j,k, ii,jj,kk&
               &XY_E_xy, XY_E_xz, XY_E_yz, XY_E_yx, XY_E_zx, XY_E_zy&
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

CALL PML_FDTD_E(i,j,k, ii,jj,kk&
               &XY_E_xy, XY_E_xz, XY_E_yz, XY_E_yx, XY_E_zx, XY_E_zy&
               &MT_MS_X,PT_MS_x, MT_MS_y,PT_MS_y, MT_MS_z,PT_MS_z)

END DO
END DO
END DO 



END SUBROUTINE 3D_PML_E


SUBROUTINE PML_FDTD_E(i,j,k, ii,jj,kk&
                     &E_xy, E_xz, E_yz, E_yx, E_zx, E_zy&
                     &MT_MS_X,PT_MS_x, MT_MS_y,PT_MS_y, MT_MS_z,PT_MS_z) 
IMPLICIT REAL*8 (a-h,o-z)

INCLUDE "FDTD_3D_para.h"
INCLUDE "FDTD_3D_var.h"

REAL*8 ::&
&E_xy(MT_MS_x:PT_MS_x, MT_MS_y:PT_MS_y, MT_MS_z:PT_MS_z),
&E_yz(MT_MS_x:PT_MS_x, MT_MS_y:PT_MS_y, MT_MS_z:PT_MS_z),
&E_yz(MT_MS_x:PT_MS_x, MT_MS_y:PT_MS_y, MT_MS_z:PT_MS_z),
&E_yx(MT_MS_x:PT_MS_x, MT_MS_y:PT_MS_y, MT_MS_z:PT_MS_z),
&E_zx(MT_MS_x:PT_MS_x, MT_MS_y:PT_MS_y, MT_MS_z:PT_MS_z),
&E_zy(MT_MS_x:PT_MS_x, MT_MS_y:PT_MS_y, MT_MS_z:PT_MS_z)

E_xy(ii,jj,kk) = Ca_y(i,j,k) * E_xy(ii,jj,kk)&+
                &Cb_y(i,j,k) * ( (H_z(i  ,j  ,k  ) - H_z(i  ,j-1,k  ))/MS_y)
E_xz(ii,jj,kk) = Ca_y(i,j,k) * E_xz(ii,jj,kk)&+
                &Cb_y(i,j,k) * (-(H_y(i  ,j  ,k  ) - H_y(i  ,j  ,k-1))/MS_z)
E_yz(ii,jj,kk) = Ca_y(i,j,k) * E_yz(ii,jj,kk)&+
                &Cb_y(i,j,k) * ( (H_x(i  ,j  ,k  ) - H_x(i  ,j  ,k-1))/MS_z)
E_yx(ii,jj,kk) = Ca_y(i,j,k) * E_yx(ii,jj,kk)&+
                &Cb_y(i,j,k) * (-(H_z(i  ,j  ,k  ) - H_z(i-1,j  ,k  ))/MS_x)
E_zx(ii,jj,kk) = Ca_y(i,j,k) * E_zx(ii,jj,kk)&+
                &Cb_y(i,j,k) * ( (H_y(i  ,j  ,k  ) - H_y(i-1,j  ,k  ))/MS_x)
E_zy(ii,jj,kk) = Ca_y(i,j,k) * E_zy(ii,jj,kk)&+
                &Cb_y(i,j,k) * (-(H_x(i  ,j  ,k  ) - H_x(i  ,j-1,k  ))/MS_y)
               
E_x(i,j,k) = E_xy(ii,jj,kk) + E_xz(ii,jj,kk) 
E_y(i,j,k) = E_yz(ii,jj,kk) + E_yx(ii,jj,kk)
E_z(i,j,k) = E_zy(ii,jj,kk) + E_zx(ii,jj,kk)

RETURN

END PML_FDTD 

