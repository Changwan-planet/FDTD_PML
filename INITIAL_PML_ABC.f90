SUBROUTINE 3D_PML_ABC
IMPLICIT REAL*8 (a-h,o-z)

INCLUDE "FDTD_3D_para.h"
INCLUDE "FDTD_3D_var.h"

!     ++++++++++++++++++++++
!++++++PML-ABC : free space++++++
!     ++++++++++++++++++++++


REAL*8 :: Permit_PML 
REAL*8 :: Permeat_PML 
REAL*8 :: Conduct_PML
REAL*8 :: Magloss_PML
REAL*8 :: IMP_PML
REAL*8 :: Conduct_max
REAL*8 :: Ca_PML, Cb_PML, Da_PML, Db_PML

pi = ACOS(-1.0d+0)
Permit_PML = 1/(36*pi)*10**(-9)
Permeat_PML = (4*pi)*10**(-7)
IMP_PML = SQRT(Permit_PML/Permeat_PML)

Conduct_max = 0.8*(4.0) / (IMP_PML*CS_z)

!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

DO N = 1, MS_PML

Conduct_PML = ((dble(N) / MS_PML)**3) * Conduct_max

Ca_PML = (1.0-((Conduct_PML*T_S)/(2.0*Permit_PML)) / &
        &(1.0+((Conduct_PML*T_S)/(2.0*Permit_PML))))

Cb_PML = (T_S/(Permit_PML*CS_z)) / &
        &(1.0+(Conduct_PML*T_S/(2.0*Permit_PML))) 

Conduct_PML = ((dble(N)-0.5 / MS_PML)**3) * Conduct_max

!     ++++++++++++++++++++++++++++++++++++++
!++++++KEY CONDITION FOR PML : REFLECTLESS ++++++
!     ++++++++++++++++++++++++++++++++++++++
Magloss_PML = Conduct_PML*(IMP_PML*IMP_PML)
!++++++++++++++++++++++++++++++++++++++++++++++++


Da_PML = (1.0-((Magloss_PML*T_S)/(2.0*Permeat_PML)) / &
        &(1.0+((Magloss_PML*T_S)/(2.0*Permeat_PML))))

Db_PML = (T_S/(Magloss_PML*CS_z)) / &
        &(1.0+(Magloss_PML*T_S/(2.0*Permeat_PML))) 


!     +++++++++++++++++++++++++++++
!++++++Layer normal to x-direction++++++
!     +++++++++++++++++++++++++++++

DO k = -MS_PML, MS_z+MS_PML
DO j = -MS_PML, MS_y+MS_PML

Ca_x (-N, j, k) = Ca_PML
Cb_x (-N, j, k) = Cb_PML
Da_x (-N, j, k) = Da_PML
Db_x (-N, j, k) = Db_PML

END DO 
END DO 


!     +++++++++++++++++++++++++++++
!++++++Layer normal to y-direction++++++
!     +++++++++++++++++++++++++++++

DO k = -MS_PML, MS_z+MS_PML
DO i = -MS_PML, MS_x+MS_PML

Ca_y (-i, N, k) = Ca_PML
Cb_y (-i, N, k) = Cb_PML
Da_y (-i, N, k) = Da_PML
Db_y (-i, N, k) = Db_PML

END DO 
END DO 



!     +++++++++++++++++++++++++++++
!++++++Layer normal to z-direction++++++
!     +++++++++++++++++++++++++++++

DO j = -MS_PML, MS_y+MS_PML
DO i = -MS_PML, MS_x+MS_PML

Ca_y (-i, j, N) = Ca_PML
Cb_y (-i, j, N) = Cb_PML
Da_y (-i, j, N) = Da_PML
Db_y (-i, j, N) = Db_PML

END DO 
END DO 

END DO

!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

DO N = 1, MS_PML

Conduct_PML = (((dble(N)-0.5) / MS_PML)**3) * Conduct_max

Ca_PML = (1.0-((Conduct_PML*T_S)/(2.0*Permit_PML)) / &
        &(1.0+((Conduct_PML*T_S)/(2.0*Permit_PML))))

Cb_PML = (T_S/(Permit_PML*CS_z)) / &
        &(1.0+(Conduct_PML*T_S/(2.0*Permit_PML))) 

Conduct_PML = ((dble(N) / MS_PML)**3) * Conduct_max

!     +++++++++++++++++++++++
!++++++KEY CONDITION FOR PML : REFLECTLESS ++++++
!     +++++++++++++++++++++++
Magloss_PML = Conduct_PML*(IMP_PML*IMP_PML)
!++++++++++++++++++++++++++++++++++++++++++++++++


Da_PML = (1.0-((Magloss_PML*T_S)/(2.0*Permeat_PML)) / &
        &(1.0+((Magloss_PML*T_S)/(2.0*Permeat_PML))))

Db_PML = (T_S/(Magloss_PML*CS_z)) / &
        &(1.0+(Magloss_PML*T_S/(2.0*Permeat_PML))) 


!     +++++++++++++++++++++++++++++
!++++++Layer normal to x-direction++++++
!     +++++++++++++++++++++++++++++

DO k = -MS_PML, MS_z+MS_PML
DO j = -MS_PML, MS_y+MS_PML

Ca_x (-MS_x+N, j, k) = Ca_PML
Cb_x (-MS_x+N, j, k) = Cb_PML
Da_x (-MS_x+N, j, k) = Da_PML
Db_x (-MS_x+N, j, k) = Db_PML

END DO 
END DO 


!     +++++++++++++++++++++++++++++
!++++++Layer normal to y-direction++++++
!     +++++++++++++++++++++++++++++

DO k = -MS_PML, MS_z+MS_PML
DO i = -MS_PML, MS_x+MS_PML

Ca_y (-i, MS_y+N, k) = Ca_PML
Cb_y (-i, MS_y+N, k) = Cb_PML
Da_y (-i, MS_y+N, k) = Da_PML
Db_y (-i, MS_y+N, k) = Db_PML

END DO 
END DO 



!     +++++++++++++++++++++++++++++
!++++++Layer normal to z-direction++++++
!     +++++++++++++++++++++++++++++

DO j = -MS_PML, MS_y+MS_PML
DO i = -MS_PML, MS_x+MS_PML

Ca_y (-i, j, MS_z+N) = Ca_PML
Cb_y (-i, j, MS_z+N) = Cb_PML
Da_y (-i, j, MS_z+N) = Da_PML
Db_y (-i, j, MS_z+N) = Db_PML

END DO 
END DO 

END DO

!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


DO k = -MS_PML2, MS_z+MS_PML2
DO j = -MS_PML2, MS_y+MS_PML2
DO i = -MS_PML2,      MS_PML2

YZ_E_xy(i, j, k) = 0.0d+0
YZ_E_xz(i, j, k) = 0.0d+0
YZ_E_yx(i, j, k) = 0.0d+0
YZ_E_yz(i, j, k) = 0.0d+0
YZ_E_zx(i, j, k) = 0.0d+0
YZ_E_zy(i, j, k) = 0.0d+0


YZ_H_xy(i, j, k) = 0.0d+0
YZ_H_xz(i, j, k) = 0.0d+0
YZ_H_yx(i, j, k) = 0.0d+0
YZ_H_yz(i, j, k) = 0.0d+0
YZ_H_zx(i, j, k) = 0.0d+0
YZ_H_zy(i, j, k) = 0.0d+0

END DO 
END DO
END DO


DO k = -MS_PML2, MS_z+MS_PML2
DO j = -MS_PML2,      MS_PML2
DO i = -MS_PML2, MS_x+MS_PML2

ZX_E_xy(i, j, k) = 0.0d+0
ZX_E_xz(i, j, k) = 0.0d+0
ZX_E_yx(i, j, k) = 0.0d+0
ZX_E_yz(i, j, k) = 0.0d+0
ZX_E_zx(i, j, k) = 0.0d+0
ZX_E_zy(i, j, k) = 0.0d+0


ZX_H_xy(i, j, k) = 0.0d+0
ZX_H_xz(i, j, k) = 0.0d+0
ZX_H_yx(i, j, k) = 0.0d+0
ZX_H_yz(i, j, k) = 0.0d+0
ZX_H_zx(i, j, k) = 0.0d+0
ZX_H_zy(i, j, k) = 0.0d+0

END DO 
END DO
END DO



DO k = -MS_PML2,      MS_PML2
DO j = -MS_PML2, MS_y+MS_PML2
DO i = -MS_PML2, MS_x+MS_PML2

XY_E_xy(i, j, k) = 0.0d+0
XY_E_xz(i, j, k) = 0.0d+0
XY_E_yx(i, j, k) = 0.0d+0
XY_E_yz(i, j, k) = 0.0d+0
XY_E_zx(i, j, k) = 0.0d+0
XY_E_zy(i, j, k) = 0.0d+0


XY_H_xy(i, j, k) = 0.0d+0
XY_H_xz(i, j, k) = 0.0d+0
XY_H_yx(i, j, k) = 0.0d+0
XY_H_yz(i, j, k) = 0.0d+0
XY_H_zx(i, j, k) = 0.0d+0
XY_H_zy(i, j, k) = 0.0d+0

END DO 
END DO
END DO






END SUBROUTINE 3D_PML_ABC
