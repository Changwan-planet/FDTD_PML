     ++++++++++
!++++++Velocity++++++
!     ++++++++++
REAL*8 :: Ev_x (-MS_PML2:MS_x+MS_PML2, -MS_PML2:MS_y+MS_PML2, -MS_PML2:MS_z+MS_PML2)
REAL*8 :: Ev_y (-MS_PML2:MS_x+MS_PML2, -MS_PML2:MS_y+MS_PML2, -MS_PML2:MS_z+MS_PML2)  
REAL*8 :: Ev_z (-MS_PML2:MS_x+MS_PML2, -MS_PML2:MS_y+MS_PML2, -MS_PML2:MS_z+MS_PML2)  


!     +++++++++
!++++++E-FIELD++++++
!     +++++++++ 
REAL*8 :: E_X (-MS_PML2:MS_x+MS_PML2, -MS_PML2:MS_y+MS_PML2, -MS_PML2:MS_z+MS_PML2)  
REAL*8 :: E_y (-MS_PML2:MS_x+MS_PML2, -MS_PML2:MS_y+MS_PML2, -MS_PML2:MS_z+MS_PML2)  
REAL*8 :: E_z (-MS_PML2:MS_x+MS_PML2, -MS_PML2:MS_y+MS_PML2, -MS_PML2:MS_z+MS_PML2)  


!Independent source
REAL*8 :: J_x (-MS_PML2:MS_x+MS_PML2, -MS_PML2:MS_y+MS_PML2, -MS_PML2:MS_z+MS_PML2)  
REAL*8 :: J_y (-MS_PML2:MS_x+MS_PML2, -MS_PML2:MS_y+MS_PML2, -MS_PML2:MS_z+MS_PML2)  
REAL*8 :: J_z (-MS_PML2:MS_x+MS_PML2, -MS_PML2:MS_y+MS_PML2, -MS_PML2:MS_z+MS_PML2)  


!     +++++++++
!++++++H-FIELD++++++
!     +++++++++
REAL*8 :: H_x (-MS_PML2:MS_x+MS_PML2, -MS_PML2:MS_y+MS_PML2, -MS_PML2:MS_z+MS_PML2)  
REAL*8 :: H_y (-MS_PML2:MS_x+MS_PML2, -MS_PML2:MS_y+MS_PML2, -MS_PML2:MS_z+MS_PML2)  
REAL*8 :: H_z (-MS_PML2:MS_x+MS_PML2, -MS_PML2:MS_y+MS_PML2, -MS_PML2:MS_z+MS_PML2)  


!Indenpendent source
REAL*8 :: M_x (-MS_PML2:MS_x+MS_PML2, -MS_PML2:MS_y+MS_PML2, -MS_PML2:MS_z+MS_PML2)  
REAL*8 :: M_y (-MS_PML2:MS_x+MS_PML2, -MS_PML2:MS_y+MS_PML2, -MS_PML2:MS_z+MS_PML2)  
REAL*8 :: M_z (-MS_PML2:MS_x+MS_PML2, -MS_PML2:MS_y+MS_PML2, -MS_PML2:MS_z+MS_PML2)  


!     +++++++++++++++++++
!++++++E-Filed Component++++++
!     +++++++++++++++++++    
REAL*8 :: Ca_x (-MS_PML2:MS_x+MS_PML2, -MS_PML2:MS_y+MS_PML2, -MS_PML2:MS_z+MS_PML2) 
REAL*8 :: Ca_y (-MS_PML2:MS_x+MS_PML2, -MS_PML2:MS_y+MS_PML2, -MS_PML2:MS_z+MS_PML2) 
REAL*8 :: Ca_z (-MS_PML2:MS_x+MS_PML2, -MS_PML2:MS_y+MS_PML2, -MS_PML2:MS_z+MS_PML2) 


REAL*8 :: Cb_x (-MS_PML2:MS_x+MS_PML2, -MS_PML2:MS_y+MS_PML2, -MS_PML2:MS_z+MS_PML2)  
REAL*8 :: Cb_y (-MS_PML2:MS_x+MS_PML2, -MS_PML2:MS_y+MS_PML2, -MS_PML2:MS_z+MS_PML2)  
REAL*8 :: Cb_z (-MS_PML2:MS_x+MS_PML2, -MS_PML2:MS_y+MS_PML2, -MS_PML2:MS_z+MS_PML2)  


!     +++++++++++++++++++
!++++++H-Filed Component++++++
!     +++++++++++++++++++ 
REAL*8 :: Da_X (-MS_PML2:MS_x+MS_PML2, -MS_PML2:MS_y+MS_PML2, -MS_PML2:MS_z+MS_PML2)  
REAL*8 :: Da_y (-MS_PML2:MS_x+MS_PML2, -MS_PML2:MS_y+MS_PML2, -MS_PML2:MS_z+MS_PML2)  
REAL*8 :: Da_z (-MS_PML2:MS_x+MS_PML2, -MS_PML2:MS_y+MS_PML2, -MS_PML2:MS_z+MS_PML2)  


REAL*8 :: Db_x (-MS_PML2:MS_x+MS_PML2, -MS_PML2:MS_y+MS_PML2, -MS_PML2:MS_z+MS_PML2) 
REAL*8 :: Db_y (-MS_PML2:MS_x+MS_PML2, -MS_PML2:MS_y+MS_PML2, -MS_PML2:MS_z+MS_PML2)  
REAL*8 :: Db_z (-MS_PML2:MS_x+MS_PML2, -MS_PML2:MS_y+MS_PML2, -MS_PML2:MS_z+MS_PML2)  


!REAL*8  ::  pp1 = (1.0d-5)/T_S
!REAL*8  ::  pp2 = (1.0d-4-1.d-5)/T_S
!REAL*8  ::  pp3 = (1.0d-4)/T_S



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

