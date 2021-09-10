
DO i=0,9

PML_Conduct_x = (g^(1/C_S)^(C_ * PML_Conduct_0
PML_Conduct_y = (g^(1/C_S)^y * PML_Conduct_0
PML_Conduct_z = (g^(1/C_S)^z * PML_Conduct_0

PML_KAPPA_x = ((K_max)^(1/x) * g^(1/C_S))^x
PML_KAPPA_y = ((K_max)^(1/y) * g^(1/C_S))^y
PML_KAPPA_z = ((K_max)^(1/z) * g^(1/C_S))^z




!     ++++++++++++++++++++++++
!++++++Berenger'PML Parameter++++++
!     ++++++++++++++++++++++++
DO i=0,9

C1_x(i) = (2*PML_Permeat_0*PML_KAPPA_y(i) - PML_Conduct_y(i)*T_S) / &
             &(2*PML_Permeat_0*PML_KAPPA_y(i) + PML_Conduct_y(i)*T_S)
C2_x(i) = (2*PML_Permeat_0*T_S) / &
             &(2*PML_Permeat_0*PML_KAPPA_y(i) + PML_Conduct_y(i)*T_S)

C3_x(i) = (2*PML_Permeat_0*PML_KAPPA_z(i) - PML_Conduct_z(i)*T_S) / &
             &(2*PML_Permeat_0*PML_KAPPA_z(i) + PML_Conduct_z(i)*T_S)
C4_x(i) = 1 /(2*PML_Permeat_0*PML_KAPPA_z(i) + PML_Conduct_z(i)*T_S)
C5_x(i) = 2*PML_Permeat_0*PML_KAPPA_x(i) + PML_Conduct_x(i)*T_S
C6_x(i) = 2*PML_Permeat_0*PML_KAPPA_x(i) - PML_Conduct_x(i)*T_S

END DO

DO j=0,9

C1_y(j) = (2*PML_Permeat_0*PML_KAPPA_z(j) - PML_Conduct_z(j)*T_S) / &
             &(2*PML_Permeat_0*PML_KAPPA_z(j) + PML_Conduct_z(j)*T_S)
C2_y(j) = (2*PML_Permeat_0*T_S) / &
             &(2*PML_Permeat_0*PML_KAPPA_z(j) + PML_Conduct_z(j)*T_S)
C3_y(j) = (2*PML_Permeat_0*PML_KAPPA_x(j) - PML_Conduct_x(j)*T_S) / &
             &(2*PML_Permeat_0*PML_KAPPA_x(j) + PML_Conduct_x(j)*T_S)
C4_y(j) = 1 /(2*PML_Permeat_0*PML_KAPPA_x(j) + PML_Conduct_x(j)*T_S)
C5_y(j) = 2*PML_Permeat_0*PML_KAPPA_y(j) + PML_Conduct_y(j)*T_S
C6_y(j) = 2*PML_Permeat_0*PML_KAPPA_y(j) - PML_Conduct_y(j)*T_S

END DO

DO j=0,9

C1_z(j) = (2*PML_Permeat_0*PML_KAPPA_x(j) - PML_Conduct_x(j)*T_S) / &
             &(2*PML_Permeat_0*PML_KAPPA_x(j) + PML_Conduct_x(j)*T_S)
C2_z(j) = (2*PML_Permeat_0*T_S) / &
             &(2*PML_Permeat_0*PML_KAPPA_x(j) + PML_Conduct_x(j)*T_S)
C3_z(j) = (2*PML_Permeat_0*PML_KAPPA_y(j) - PML_Conduct_y(j)*T_S) / &
             &(2*PML_Permeat_0*PML_KAPPA_y(j) + PML_Conduct_y(j)*T_S)
C4_z(j) = 1 /(2*PML_Permeat_0*PML_KAPPA_y(j) + PML_Conduct_xyj)*T_S)
C5_z(j) = 2*PML_Permeat_0*PML_KAPPA_z(j) + PML_Conduct_z(j)*T_S
C6_z(j) = 2*PML_Permeat_0*PML_KAPPA_z(j) - PML_Conduct_z(j)*T_S

END DO

!     +++++++++
!++++++E-Field++++++
!     +++++++++
EFD_x(i+1,j,k) = C1_x(i) * EFD(i+1,j,k) &
             &+C2_x(i) * ( ( H_z(i,+1,j+1,k)-H_z(i+1,j-1,k)) / C_S - (H_y(i+1,j,k+1) - H_y(i+1,j,k-1) )/ C_S )
EFD_y(i,j,k) = C1_y(i) * EFD(i+1,j,k) &
             &+C2_y(i) * ( ( H_x(i,+1,j+1,k)-H_x(i+1,j-1,k)) / C_S - (H_z(i+1,j,k+1) - H_z(i+1,j,k-1) )/ C_S )
EFD_z(i,j,k) = C1_z(i) * EFD(i+1,j,k) &
             &+C2_z(i) * ( ( H_y(i,+1,j+1,k)-H_y(i+1,j-1,k)) / C_S - (H_x(i+1,j,k+1) - H_x(i+1,j,k-1) )/ C_S )


E_x(i+1,j,k) = C3_x(i)*E_x(i+1,j,k) &
             &+(C4_x(i)*(C5_x(i)*EFD_x(i+1,j,k) - C6_x(i)*EFD_x(i+1,j,k))
E_y(i+1,j,k) = C3_y(i)*E_x(i+1,j,k) &
             &+(C4_y(i)*(C5_y(i)*EFD_y(i+1,j,k) - C6_y(i)*EFD_y(i+1,j,k))
E_z(i+1,j,k) = C3_z(i)*E_x(i+1,j,k) &
             &+(C4_z(i)*(C5_z(i)*EFD_z(i+1,j,k) - C6_z(i)*EFD_z(i+1,j,k))


!     +++++++++
!++++++H-Field++++++
!     +++++++++
MFD_x(i+1,j,k) = C1_x(i) * MFD(i+1,j,k) &
             &+C2_x(i) * ( ( E_z(i,+1,j+1,k)-E_z(i+1,j-1,k)) / C_S - (E_y(i+1,j,k+1) - E_y(i+1,j,k-1) )/ C_S )
MFD_y(i,j,k) = C1_y(i) * MFD(i+1,j,k) &
             &+C2_y(i) * ( ( E_x(i,+1,j+1,k)-E_x(i+1,j-1,k)) / C_S - (E_z(i+1,j,k+1) - E_z(i+1,j,k-1) )/ C_S )
MFD_z(i,j,k) = C1_z(i) * MFD(i+1,j,k) &
             &+C2_z(i) * ( ( E_y(i,+1,j+1,k)-E_y(i+1,j-1,k)) / C_S - (E_x(i+1,j,k+1) - E_x(i+1,j,k-1) )/ C_S )


H_x(i+1,j,k) = C3_x(i)*H_x(i+1,j,k) &
             &+(C4_x(i)*(C5_x(i)*MFD_x(i+1,j,k) - C6_x(i)*MFD_x(i+1,j,k))
H_y(i+1,j,k) = C3_y(i)*H_x(i+1,j,k) &
             &+(C4_y(i)*(C5_y(i)*MFD_y(i+1,j,k) - C6_y(i)*MFD_y(i+1,j,k))
H_z(i+1,j,k) = C3_z(i)*H_x(i+1,j,k) &
             &+(C4_z(i)*(C5_z(i)*MFD_z(i+1,j,k) - C6_z(i)*MFD_z(i+1,j,k))



