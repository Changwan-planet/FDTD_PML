SUBROUTINE INITIAL_FDTD
IMPLICIT NONE

INCLUDE "FDTD_3D_var.h"
INCLUDE "FDTD_3D_para.h"

!INITIALIZATION OF THE PARAMETER

MS_x = 1.0d+2
MS_y = 1.0d+2
MS_z = 1.0d+2
MS_PML = 10
MS_PML2 = MS_PML+1

N = 3.0d+2
f = 5.0d+11

CS_x = 6.0d-5
CS_y = 6.0d-5
CS_z = 6.0d-5

T_S = 1.0d-14
pi = Acos(-1.0)



!INITIALIZATION OF THE SPACE
!     ++++++++++++
!++++++FREE SPCAE++++++
!     ++++++++++++
Permit_x = 8.854*(10.0)**(-12)
Permit_y = 8.854*(10.0)**(-12)
Permit_z = 8.854*(10.0)**(-12)

Permeat_x = (4.0 * pi) *(10.0) ** (-7)
Permeat_y = (4.0 * pi) *(10.0) ** (-7)
Permeat_z = (4.0 * pi) *(10.0) ** (-7)

Conduct_x = 0
Conduct_y = 0
Conduct_z = 0

Magloss_x = 0
Magloss_y = 0
Magloss_z = 0


!     +++++++++++++++
!++++++REALATIONSHIP++++++
!     +++++++++++++++

Ev_x = 1.0 / sqrt(Permit_x* Permeat_x)
Ev_y = 1.0 / sqrt(Permit_y* Permeat_y)
Ev_z = 1.0 / sqrt(Permit_z* Permeat_z)

Ca_x = (1.0-((Conduct_x*T_S)/(2.0*Permit_x))) / (1.0+((Conduct_x*T_S)/(2.0*Permit_x)))
Ca_y = (1.0-((Conduct_y*T_S)/(2.0*Permit_y))) / (1.0+((Conduct_y*T_S)/(2.0*Permit_y)))
Ca_z = (1.0-((Conduct_z*T_S)/(2.0*Permit_Z))) / (1.0+((Conduct_z*T_S)/(2.0*Permit_z)))

Cb_x = (T_S/(Permit_x*CS_x)) / (1.0+(Conduct_x*T_S/(2.0*Permit_x)))
Cb_y = (T_S/(Permit_y*CS_y)) / (1.0+(Conduct_y*T_S/(2.0*Permit_y)))
Cb_z = (T_S/(Permit_z*CS_z)) / (1.0+(Conduct_z*T_S/(2.0*Permit_z)))

Da_x = (1.0-((Magloss_x*T_S)/(2.0*Permeat_X))) / (1.0+((Magloss_x*T_S)/(2.0*Permeat_x)))
Da_y = (1.0-((Magloss_y*T_S)/(2.0*Permeat_y))) / (1.0+((Magloss_y*T_S)/(2.0*Permeat_y)))
Da_z = (1.0-((Magloss_Z*T_S)/(2.0*Permeat_z))) / (1.0+((Magloss_z*T_S)/(2.0*Permeat_z)))

Db_x = (T_S/(Permeat_x*CS_x)) / (1.0+(Magloss_x*T_S/(2.0*Permeat_x)))
Db_y = (T_S/(Permeat_y*CS_y)) / (1.0+(Magloss_y*T_S/(2.0*Permeat_y)))
Db_z = (T_S/(Permeat_z*CS_z)) / (1.0+(Magloss_z*T_S/(2.0*Permeat_z)))


free_Ca=Ca_x

!INITIALIZATION OF THE EM FILED
DO k = -MS_PML2, MS_z+MS_PML2
DO j = -MS_PML2, MS_y+MS_PML2
DO i = -MS_PML2, MS_x+MS_PML2

   E_x(i,j,k) = 0.0d+0
   E_y(i,j,k) = 0.0d+0
   E_z(i,j,k) = 0.0d+0

   H_x(i,j,k) = 0.0d+0
   H_y(i,j,k) = 0.0d+0
   H_z(i,j,k) = 0.0d+0

END DO
END DO
END DO


!INITIALIZATION OF THE SOURCES
DO k = 0 : MS_z
DO j = 0 : MS_y
DO i = 0 : MS_x

   J_x(i,j,k) = 0.0d+0
   J_y(i,j,k) = 0.0d+0
   J_z(i,j,k) = 0.0d+0

   M_x(i,j,k) = 0.0d+0
   M_y(i,j,k) = 0.0d+0
   M_z(i,j,k) = 0.0d+0

END DO
END DO
END DO





END SUBROUTINE INITIAL_FDTD


