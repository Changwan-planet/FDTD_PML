PRINT*,"Permit(1,1,1)",Permit(1,1,1)
PRINT*,"Permeat(1,1,1)",Permeat(1,1,1)
PRINT*,"Conduct(1,1,1)",Conduct(1,1,1)
PRINT*,"Magloss(1,1,1)",Magloss(1,1,1)
PRINT*,"================================"
PRINT*,"C_a(0,0,0)",C_a(0,0,0)
PRINT*,"C_b(0,0,0)",C_b(0,0,0)
PRINT*,"D_a(1,1,1)",D_a(3,3,3)
PRINT*,"D_b(1,1,1)",D_b(4,4,4)
PRINT*,"================================"
PRINT*,"M_S (Matrix Size)=",M_S
PRINT*,"C_S (Cell Size)=",C_S
PRINT*,"E_v(1,1,1) (velocity) [m/s]=", E_v(1,1,1)

!Print*,"Wavelength=",E_v(1,1,1)*N*T_D/(C_S*k) 
!                         C(velocity of light) x N (number of time step) x T_D 
!         Wavelength = ----------------------------------------------------------
!                             C_S (Cell size) x k (number of wavelength)
!
!         d = C X N X T_D (Total wave from distance)
!         d/k (distance of a wavelength)
!         N x T_D    (travleing time)

PRINT*,"T_S (Time Difference) [s] ",T_S
PRINT*,"N (Number of time_step)",N
PRINT*,"Frequency [Hz]=", f
PRINT*,"Wavelength (Distance of one period)",E_v(1,1,1)/f
PRINT*,"The number of time step of one period",1/(f*T_S)
PRINT*,"Number of C_S of one period",(E_v(1,1,1)/f)/C_S
PRINT*,"time of one period", 1.0/f 
PRINT*,"Distance = M_S*C_S [m]", M_S*C_S
PRINT*,"Velocity of EM * T_S / C_S", E_v(1,1,1)*T_S/C_S
PRINT*,"Distance per T_S =", E_v(1,1,1) * T_S

