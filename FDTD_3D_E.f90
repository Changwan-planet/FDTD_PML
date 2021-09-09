SUBROUTINE FDTD_3D_E
IMPLICIT REAL*8 (a-h,o-z)

INCLUDE "FDTD_para.h"
INCLUDE "OPEN_PATH.h"
!
!     +++++++++++++++
!++++++Elecric Field++++++
!     +++++++++++++++

DO k = 0, MS_z 
DO j = 0, MS_y 
DO i = 0, MS_x

      !n+1!  = !n!                                 !Time index 
E_x (i,j,k) = Ca_x(i,j,k) * E_x(i,j,k) + &
             &Cb_x(i,j,k) * ( H_z(i  ,j+1,k  ) - H_z(i  ,j-1,k  )+ &
                             &H_y(i  ,j  ,k-1) - H_y(i  ,j  ,k+1)- &
                             &J_x(i,j,k) * CS_x)

 
E_y (i,j,k) = Ca_y(i,j,k) * E_y (i,j,k) + &
             &Cb_y(i,j,k) * ( H_x(i  ,j  ,k+1) - H_x(i  ,j  ,k-1)+ &
                             &H_z(i-1,j  ,k  ) - H_z(i+1,j  ,k  )- &
                             &J_y(i,j,k) * CS_y)

  
E_z (i,j,k) = Ca_z(i,j,k) * E_z(i,j,k) + &
             &Cb_z(i,j,k) * ( H_y(i+1,j  ,k  ) - H_y(i-1,j  ,k  )+ &
                           &  H_x(i  ,j-1,k  ) - H_x(i  ,j+1,k  )- &
                           &J_z(i,j,k) * CS_y)                    


END DO
END DO
END DO   

!     +++++++++++++      
!++++++First ouput++++++
!     +++++++++++++

!       IF (time==1/(f*T_S)) THEN
!      IF (time==3*1.0/(f*T_S)) THEN
!      IF (time==1) THEN
         ! 1.0/ (f*T_S)= 200 time step = 1 Wavelenth 

       IF (time==300) THEN !Before hitting the boundary. Light can mover 3 meter during 10 T_S in free space.
!         j=50
!         k=50
             DO k = 0,MS_z+2
                DO j = 0,MS_y+2
                   DO i = 0,MS_x+2

                      WRITE(20,*) E_x(i,j,k), E_y(i,j,k), E_z(i,j,k)
                      !WRITE(20,*) Log(E_x(i,j,k))**2, Log(E_y(i,j,k))**2, Log(E_z(i,j,k))**2 !Divergence check          
                   END DO
                END DO
             END DO 

     END IF


!     ++++++++++++++      
!++++++Final output++++++
!     ++++++++++++++
!     IF (time==N) THEN
!      IF (time==2*1/(f*T_S)) THEN
!             i=0
!             j=0
!             k=0
!             DO i=0,M_S+2
!                DO j=0,tm+2
!                   DO k=0,tm+2

!                      WRITE(23,*) E_x(i,j,k), E_y(i,j,k), E_z(i,j,k)
                      !WRITE(23,*) Log(E_x(i,j,k))**2, Log(E_y(i,j,k))**2, Log(E_z(i,j,k))**2 !Divergence check
        
               !Write(20,100) E_z(i)
                !100 FORMAT(E15.7)
                !Write(20,*) Log(E_z(i)**2)  !Divergence Check
!                   END DO 
!                END DO
!             END DO   
!      END IF 

END SUBROUTINE
