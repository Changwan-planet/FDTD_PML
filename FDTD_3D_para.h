
INTEGER :: a = 0   !!X coordinate
INTEGER :: i = 0
INTEGER :: j = 0   
INTEGER :: k = 0
INTEGER :: time = 0


!     +++++++++++++++++++++++++++
!++++++Distance=C_S*Maxtrix_SIZE++++++
!     +++++++++++++++++++++++++++
!Maxtrix_size
!This value cause the simulation program to be time-consuming. 
!If this is too small, it's unrealistic, but fast.
INTEGER, PARAMETER :: MS_x 
INTEGER, PARAMETER :: MS_y 
INTEGER, PARAMETER :: MS_z 
INTEGER, PARAMETER :: MS_PML 
INTEGER, PARAMETER :: MS_PML2 = MS_PML+1
   

!Number of time step
INTEGER, PARAMETER :: N      

!Frequency
REAL*8, PARAMETER :: f 

!Cell size [m]
!If this is too small, it's realistic but slow.
!If this is too big, it's fast but errorneous.
REAL*8, PARAMETER :: CS_x                             
REAL*8, PARAMETER :: CS_y                             
REAL*8, PARAMETER :: CS_z                             


!Time step [s]
REAL*8, PARAMETER :: T_S         
REAL*8, PARAMETER :: pi 

!As for x-directed, z-polarized TEM mode, M_source=0 for all time

!     ++++++++++++++++++++
!++++++Material Propertiy++++++
!     +++++++++++++++++++
Conductivity 
REAL*8(-MS_PML2:MS_x+MS_PML2, -MS_PML2:MS_y+MS_PML2, -MS_PML2:MS_z+MS_PML2), PARAMETER ::&
      & Conduct_x
REAL*8(-MS_PML2:MS_x+MS_PML2, -MS_PML2:MS_y+MS_PML2, -MS_PML2:MS_z+MS_PML2), PARAMETER ::&
      & Conduct_y
REAL*8(-MS_PML2:MS_x+MS_PML2, -MS_PML2:MS_y+MS_PML2, -MS_PML2:MS_z+MS_PML2), PARAMETER ::&
      & Conduct_z


!Magnetic loss
REAL*8(-MS_PML2:MS_x+MS_PML2, -MS_PML2:MS_y+MS_PML2, -MS_PML2:MS_z+MS_PML2), PARAMETER ::&
      & Magloss_x
REAL*8(-MS_PML2:MS_x+MS_PML2, -MS_PML2:MS_y+MS_PML2, -MS_PML2:MS_z+MS_PML2), PARAMETER ::&
      & Magloss_y
REAL*8(-MS_PML2:MS_x+MS_PML2, -MS_PML2:MS_y+MS_PML2, -MS_PML2:MS_z+MS_PML2), PARAMETER ::&
      & Magloss_z
 

!Permitivity  Free space=1/(36*pi)*10**(-9)  
REAL*8(-MS_PML2:MS_x+MS_PML2, -MS_PML2:MS_y+MS_PML2, -MS_PML2:MS_z+MS_PML2), PARAMETER ::&
      & Permit_x 
REAL*8(-MS_PML2:MS_x+MS_PML2, -MS_PML2:MS_y+MS_PML2, -MS_PML2:MS_z+MS_PML2), PARAMETER ::&
      & Permit_y 
REAL*8(-MS_PML2:MS_x+MS_PML2, -MS_PML2:MS_y+MS_PML2, -MS_PML2:MS_z+MS_PML2), PARAMETER ::&
      & Permit_z 


!Permeativity Free space=(4*pi)*10**(-7) 
REAL*8(-MS_PML2:MS_x+MS_PML2, -MS_PML2:MS_y+MS_PML2, -MS_PML2:MS_z+MS_PML2), PARAMETER ::&
      & Permeat_x 
REAL*8(-MS_PML2:MS_x+MS_PML2, -MS_PML2:MS_y+MS_PML2, -MS_PML2:MS_z+MS_PML2), PARAMETER ::&
      & Permeat_y 
REAL*8(-MS_PML2:MS_x+MS_PML2, -MS_PML2:MS_y+MS_PML2, -MS_PML2:MS_z+MS_PML2), PARAMETER ::&
      & Permeat_z 
 
