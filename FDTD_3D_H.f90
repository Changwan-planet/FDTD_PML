SUBROUTINE FDTD_3D_H
IMPLICIT REAL*8 (a-h,-o-z)

INCLUDE "FDTD_para.h"
INCLUDE "OPEN_PATH.h"

!     ++++++++++++++++    
!++++++Magnetic Field++++++
!     ++++++++++++++++
DO k = 0, MS_z
DO j = 0, MS_y
DO i = 0, MS_x

    !n+2!  = !n+1!              !Time index
H_x(i,j,k) = Da_x(i,j,k) * H_x(i,j,k) + &
            &Db_X(i,j,k) * ( E_y(i  ,j  ,k+1) - E_y(i  ,j  ,k-1)+ &
                            &E_z(i  ,j-1,k  ) - E_z(i  ,j+1,k  )- &
                            &M_x(i,j,k) * CS_x)   


H_y(i,j,k) = Da_y(i,j,k) * H_y(i,j,k)+ &
            &Db_y(i,j,k) * ( E_z(i+1,j  ,k  ) - E_z(i-1,j,  k  )+ &
                            &E_x(i,  j,  k-1) - E_x(i,  j,  k+1)- &
                            &M_y(i,j,k) * CS_y)   


H_z(i,j,k) = Da_z(i,j,k) * H_z(i,j,k) + &
            &Db_z(i,j,k) * ( E_x(i  ,j+1,k  ) - E_x(i  ,j-1,k  )+ & 
                            &E_y(i-1,j  ,k  ) - E_y(i+1,j  ,k  )- &
                            &M_z(i,j,k) * CS_z)   
 

END DO   
END DO
END DO   

!     ++++++++++++++ 
!++++++First output++++++
!     ++++++++++++++      
!      IF(time==1.0/(f*T_S)) THEN
       IF(time==300) THEN
            DO k=0,MS_z+2
               DO j=0,MS_y+2
                 DO i=0,MS_x+2

                    WRITE(21,*) H_x(i,j,k), H_y(i,j,k), H_z(i,j,k)
            
                 END DO
               END DO
             END DO

      END IF

!     ++++++++++++++
!++++++Final output++++++
!     ++++++++++++++
!      IF(time==N) THEN
!            DO i=0,M_S+2
!                DO j=0,tm+2
!                   DO k=0,tm+2
!                      WRITE(21,*) H_x(i,j,k), H_y(i,j,k), H_z(i,j,k)
                      !Write(21,100) H_y(i)
                      !Write(21,*) Log(H_y(j)**2) !Divergence Chek
             
!                   END DO
!                END DO
!             END DO   
!      END IF 


END SUBROUTINE 
