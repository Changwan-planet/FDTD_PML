PROGRAM MAIN_FDTD
IMPLICIT NONE

INCLUDE "FDTD_3D_para.h"
INCLUDE "FDTD_3D_var.h"
INCLUDE "3D_OPEN_PATH.h"
INCLUDE "3D_PRINT.h"

!     ++++++
!++++++TIME++++++
!     ++++++

DO time = 0, N

!+++++CAUTION+++++
!     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ 
!++++++KEEP IN MIND THAT YOU SHOULD USE THE LEAPFROG ALGORITHM.++++++
!     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
! 
!    +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++   
!+++++IN THE DO-LOOP, LEFT SIZE IS N+1 AND RIGHT SIZE IS N IN THE TIME DOMAIN+++++
!    ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!

CALL FDTD_3D_E
CALL 3D_PML_E

INCLUDE "FDTD_3D_Source.h"

CALL FDTD_3D_H
CALL 3D_PML_H



END DO



END PROGRAM MAIN_FDTD
