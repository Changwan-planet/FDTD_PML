
!Source ( Boundary Condition )
!===========================================================================      
!       E_z(0,0,0) = 1-cos(2*pi*f*time)
!       Print*,"E_z(0,0,0)=", E_z(0,0,0)    
!===========================================================================   

!Long-time sources boudary condition(Cell_size=10000)
!===========================================================================
!     If (time<=(1.0d-4/T_D)) then
!          E_z(0) = 1-cos(2*pi*f*time*T_D)
!     Else
!          E_z(0) = 0
!     End if 
!===========================================================================


!One-time sources (Boundary condition)
!===========================================================================
!      IF (time<=1.0/(f*T_S)) THEN
       IF (time<=1*1.0/(f*T_S)) THEN
          
!         E_x(50,50,50) = 1
!         E_y(50,50,50) = 1-COS(2*pi*f*time*T_S)
!         H_y(50,50,50) = 1-COS(2*pi*f*time*T_S)
          E_x(50,50,50) = 1-COS(2*pi*f*time*T_S)
!         E_y(1,4,3) = 1-COS(2*pi*f*time*T_S)
!         J_z(10,10,10) = 1-COS(2*pi*f*time*T_S)

       ELSE 
  
!         E_y(50,50,50) = 0
!         H_y(50,50,50) = 0
          E_x(50,50,50) = 0
!         E_y(1,4,3) = 0
!         J_z(10,10,10) = 0
       
       END IF  
!===========================================================================


!Weigted Long-time Sources (Boundary sources)
!===========================================================================
!     IF (time<=pp1) THEN
!          E_z(1,3,4) = Weight_fun1(time)*(1-cos(2*pi*f*time*T_S-pi))
!       
!     ELSE IF ((pp1<time).AND.(time<pp2)) THEN
!          E_z(1,3,4) = 1-cos(2*pi*f*time*T_S)
!
!     ELSE IF ((pp2<=time).AND.(time<=pp3)) THEN
!          E_z(1,3,4) = Weight_fun2(time)*(1-cos(2*pi*f*time*T_S+pi))
!
!     ELSE
!          E_z(1,3,4) = 0
!     
!     END IF
!===========================================================================

!Hard source 1, 

!Hard source 2, a lowpass Gaussian pulse with finite dc content
!         IF(time
!     E_z(1,3,4) = E_0 * EXP(-(time-time_0)/time_decay)**2

!PRINT *, E_x(2,3,3)
!WRITE(22,*) E_x(2,3,3), E_y(1,4,3), E_z(1,3,4)
WRITE(22,*) E_x(50,50,50), E_y(50,50,50), E_z(50,50,50)
!WRITE(22,*) H_x(50,50,50), H_y(50,50,50), H_z(50,50,50)
!WRITE(22,*) E_x(2,3,3), E_y(0,4,3), E_z(1,3,4)
!WRITE(22,*) H_x(1,4,4), H_y(2,3,4), H_z(2,4,3)
 
