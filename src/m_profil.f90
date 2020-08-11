!***********************************************************************
!    Module:  profil.f                                                  
!                                                                       
!    Copyright (C) 2000 Mark Drela                                      
!                                                                       
!    This program is free software; you can redistribute it and/or modif
!    it under the terms of the GNU General Public License as published b
!    the Free Software Foundation; either version 2 of the License, or  
!    (at your option) any later version.                                
!                                                                       
!    This program is distributed in the hope that it will be useful,    
!    but WITHOUT ANY WARRANTY; without even the implied warranty of     
!    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the      
!    GNU General Public License for more details.                       
!                                                                       
!    You should have received a copy of the GNU General Public License  
!    along with this program; if not, write to the Free Software        
!    Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.          
!***********************************************************************
module m_profil
contains
      !                                                                                    
      SUBROUTINE PRWALL(DSTAR,THETA,UO,RT,MS,CT, BB,                    &
      &                  DO, DO_DS, DO_TH, DO_UO, DO_RT, DO_MS,          &
      &                  UI, UI_DS, UI_TH, UI_UO, UI_RT, UI_MS,          &
      &                  HS, HS_DS, HS_TH, HS_UO, HS_RT, HS_MS,          &
      &                  CF, CF_DS, CF_TH, CF_UO, CF_RT, CF_MS,          &
      &                  CD, CD_DS, CD_TH, CD_UO, CD_RT, CD_MS, CD_CT )  
      !
      use m_xblsys, only: cft
      !
      IMPLICIT REAL*8 (A-H,M,O-Z) 
      !================================================================       
      !     Returns wall slip velocity and thickness of wall BL profile       
      !                                                                       
      !     Input:                                                            
      !        DSTAR  kinematic displacement thickness                        
      !        THETA  kinematic momentum thickness                            
      !        RT     momentum thickness based on ue and THETA                
      !        MS     Mach^2 based on ue                                      
      !                                                                       
      !        UO     uo/ue outer velocity;  assumed = 1  in this version     
      !                                                                       
      !     Output:                                                           
      !        BB     outer profile exponent                                  
      !        DO     thickness of profile deck                               
      !        UI     inner "slip" velocity                                   
      !        CF     wall skin friction                                      
      !================================================================       
      !                                                                   
            PARAMETER (N=65) 
            DIMENSION ETA(N), UIP(N), UIP_DP(N), G(N), G_BB(N) 
      !                                                                       
      !---- pi/2 ,  2/pi                                                      
            DATA HPI, TOPI / 1.570796327 , 0.6366197723 / 
      !                                                                       
            DATA T, SQT / 0.28 , 0.5291502622 / 
      !                                                                       
      !---- TCON = ( atan(T^1/2) / T^1/2  -  1/(1+T)) / (2T)  +  0.5/(1.0 + T)
      !                - atan(1/T^1/2) / 2T^1/2                               
            DATA TCON / -0.3864027035 / 
      !                                                                       
      !---- slip velocity coefficient                                         
            DATA AK / 0.09 / 
      !                                                                       
      !---- log-law constants                                                 
            DATA VKAP, VB / 0.40 , 5.0 / 
      !                                                                       
            HK = DSTAR/THETA 
      !                                                                       
            UO = 1.0 
            BB = 1.0 
      !                                                                       
      !---- initialize variables                                              
            CALL CFT(HK,RT,MS,CF,CF_HK,CF_RT,CF_MS) 
            SGN = SIGN( 1.0 , CF ) 
            UT = SGN * SQRT(0.5*ABS(CF)) 
      !                                                                       
            UI = MIN( UT/AK * HPI , 0.90 ) 
            DO = HK*THETA / (1.0 - 0.5*(UO+UI)) 
      !                                                                       
            EBK = EXP(-VB*VKAP) 
      !                                                                       
            DO 1000 ITER=1, 40 
      !                                                                       
            SGN = SIGN( 1.0 , UT ) 
      !                                                                       
      !------ set d+ = DP(UT DO ; RT TH)                                      
            DP    = SGN * UT*RT*DO/THETA 
            DP_DO = SGN * UT*RT   /THETA 
            DP_UT = SGN *    RT*DO/THETA 
      !                                                                       
            DP_TH = -DP/THETA 
            DP_RT = SGN * UT   *DO/THETA 
            DP_MS = 0.0 
      !                                                                       
      !------ estimate inner profile edge velocity Ui+ using log-law          
            UPE = LOG(DP)/VKAP + VB 
      !                                                                       
      !------ converge exact Ui+ using Spalding formula                       
            DO 10 ITUP=1, 5 
            UK = UPE*VKAP 
            ARG = UK - VB*VKAP 
            EXU = EXP(ARG) 
            REZ  = UPE +  EXU - EBK*(1.0 + UK + UK*UK/2.0 + UK*UK*UK/6.0) &
      &         - DP                                                     
            DP_U = 1.0 + (EXU - EBK*(1.0 + UK + UK*UK/2.0))*VKAP 
      !                                                                       
            IF(ABS(REZ/DP) .LT. 1.0E-5) GO TO 11 
      !                                                                       
            DUPE = -REZ/DP_U 
            UPE = UPE + DUPE 
      10   CONTINUE 
            WRITE(*,*) 'PRWALL: Ue+ convergence failed,  Res =', REZ/DP 
      11   CONTINUE 
      !                                                                       
            UPE_DP = 1.0/DP_U 
      !                                                                       
      !            2      2        3      3                                   
      !------ set d y+/du+   and  d y+/du+   at BL edge                       
            DP_UU  = (EXU - EBK*(1.0 + UK))*VKAP**2 
            DP_UUU = (EXU - EBK           )*VKAP**3 
      !                                                                       
      !------ set  du+/dy+  at BL edge                                        
            UPD    = 1.0/DP_U 
            UPD_DP = (-1.0/DP_U**3) * DP_UU 
      !                                                                       
      !             2      2                                                  
      !------ set  d u+/dy+   at BL edge                                      
      !CC     UPD_DP = (-1.0/DP_U**3) * DP_UU                                 
            UPDD    = UPD_DP 
            UPDD_DP = (-1.0/DP_U**4) * DP_UUU                               &
      &          + ( 3.0/DP_U**5) * DP_UU**2                             
      !                                                                       
      !------ set coefficients for Spalding profile correction polynomial     
            DC2    = 0.5*DP*DP*UPDD    - DP*UPD 
            DC2_DP =        DP*UPDD    -    UPD                             &
      &         + 0.5*DP*DP*UPDD_DP - DP*UPD_DP                          
      !                                                                       
            DC3    = -(  DP*DP*UPDD    - DP*UPD   ) / 3.0 
            DC3_DP = -(2.0 *DP*UPDD    -    UPD   ) / 3.0                   &
      &           -(  DP*DP*UPDD_DP - DP*UPD_DP) / 3.0                   
      !                                                                       
      !------ set outer profile amplitude DUO                                 
            DUO    = UO - UT*(UPE    + DC2    + DC3   ) 
            DUO_DP =    - UT*(UPE_DP + DC2_DP + DC3_DP) 
      !                                                                       
            DUO_UT =    -    (UPE    + DC2    + DC3   )                     &
      &         + DUO_DP*DP_UT                                           
            DUO_DO = DUO_DP*DP_DO 
      !                                                                       
            DUO_TH = DUO_DP*DP_TH 
            DUO_RT = DUO_DP*DP_RT 
            DUO_MS = DUO_DP*DP_MS 
      !                                                                       
      !        write(*,*) 'dUo', duo, duo_dp, duo_ut, duo_do                  
      !        read(*,*) ddo, dut                                             
      !        if(ddo.ne.0.0 .or. dut.ne.0.0) then                            
      !          do = do+ddo                                                  
      !          ut = ut+dut                                                  
      !          write(*,*) 'new', duo + duo_do*ddo + duo_ut*dut              
      !          go to 666                                                    
      !        endif                                                          
      !                                                                       
      !------ set wake profile coefficients                                   
            BB1    =  3.0*(BB    +2.0)*(BB+3.0)/(BB+7.0) 
            BB1_BB =  3.0*(BB*2.0+5.0         )/(BB+7.0) - BB1/(BB+7.0) 
            BB2    = -5.0*(BB    +1.0)*(BB+3.0)/(BB+7.0) 
            BB2_BB = -5.0*(BB*2.0+4.0         )/(BB+7.0) - BB2/(BB+7.0) 
            BB3    =  2.0*(BB    +1.0)*(BB+2.0)/(BB+7.0) 
            BB3_BB =  2.0*(BB*2.0+3.0         )/(BB+7.0) - BB3/(BB+7.0) 
      !                                                                       
      !------ fill eta coordinate and inner profile arrays                    
      !CC     EXUPE = EXP(UPE*VKAP - VB*VKAP)                                 
            EXUPE = EXU 
      !                                                                       
            DEXU = (EXUPE - EBK)/FLOAT(N-1) 
      !                                                                       
            I = 1 
            ETA(I) = 0.0 
            UIP(I)    = 0.0 
            UIP_DP(I) = 0.0 
            G(I)    = 0.0 
            G_BB(I) = 0.0 
      !                                                                       
            DO 20 I=2, N 
      !cc       EXU = EBK + DEXU*FLOAT(I-1)                                   
            EXU = EBK + (DEXU - 0.75*DEXU*FLOAT(N-I)/FLOAT(N-1))          &
      &                *FLOAT(I-1)                                       
      !                                                                       
      !CC       UK = UP*VKAP                                                  
            UK = LOG(EXU) + VB*VKAP 
      !                                                                       
            UP = UK/VKAP 
      !                                                                       
      !-------- set "inverse" Spalding profile  y+(u+)  and derivatives       
            YP    =  UP +  EXU - EBK*(1.0 + UK + UK*UK/2.0 + UK*UK*UK/6.0) 
            YP_U  = 1.0 + (EXU - EBK*(1.0 + UK + UK*UK/2.0))*VKAP 
            YP_UU =       (EXU - EBK*(1.0 + UK            ))*VKAP**2 
      !                                                                       
            ET = YP/DP 
      !                                                                       
      !-------- set final inner profile (fudged Spalding)                     
            UIP(I)     = UP       +     DC2   *ET**2 +     DC3   *ET**3 
            UIP_DP(I)  =                DC2_DP*ET**2 +     DC3_DP*ET**3 
      !                                                                       
      !cc          UIPD(I)    = 1.0/YP_U + 2.0*DC2   *ET    + 3.0*DC3   *ET**2
      !cc          UIPD_DP(I) = (-1.0/YP_U**3)*YPUU                           
      !cc     &                          + 2.0*DC2_DP*ET    + 3.0*DC3_DP*ET**2
      !                                                                       
      !-------- set outer profile                                             
            ETB = ET**BB 
            ALE = LOG(ET) 
      !                                                                       
      !cc          G(I)    =  2.0*ETB -     ETB**2                            
      !cc          G_BB(I) = (2.0*ETB - 2.0*ETB**2)*ALE                       
      !                                                                       
            G(I)    = (BB1   *ET + BB2   *ET**2 + BB3   *ET**3)*ETB 
            G_BB(I) = (BB1_BB*ET + BB2_BB*ET**2 + BB3_BB*ET**3)*ETB       &
      &            + G(I)*ALE                                            
      !                                                                       
            ETA(I) = ET 
      !                                                                       
      20   CONTINUE 
      !                                                                       
      !                                                                       
            DSN    = 0.0 
            DSN_DO = 0.0 
            DSN_UT = 0.0 
            DSN_BB = 0.0 
      !                                                                       
            DSN_TH = 0.0 
            DSN_RT = 0.0 
            DSN_MS = 0.0 
      !                                                                       
      !                                                                       
            THN    = 0.0 
            THN_DO = 0.0 
            THN_UT = 0.0 
            THN_BB = 0.0 
      !                                                                       
            THN_TH = 0.0 
            THN_RT = 0.0 
            THN_MS = 0.0 
      !                                                                       
      !        TSN    = 0.0                                                   
      !        TSN_DO = 0.0                                                   
      !        TSN_UT = 0.0                                                   
      !        TSN_BB = 0.0                                                   
      !                                                                       
      !        TSN_TH = 0.0                                                   
      !        TSN_RT = 0.0                                                   
      !        TSN_MS = 0.0                                                   
      !                                                                       
      !------ perform integration                                             
            DO 100 I=1, N-1 
            DETA = ETA(I+1) - ETA(I) 
            GA    = 0.5*(G(I+1)    + G(I)   ) 
            GA_BB = 0.5*(G_BB(I+1) + G_BB(I)) 
      !                                                                       
            UIPA    = 0.5*(UIP(I+1)    + UIP(I)   ) 
            UIPA_DP = 0.5*(UIP_DP(I+1) + UIP_DP(I)) 
      !                                                                       
            U    = UT*UIPA    + DUO   *GA 
            U_DP = UT*UIPA_DP 
      !                                                                       
            U_DO =              DUO_DO*GA + U_DP*DP_DO 
            U_UT =    UIPA    + DUO_UT*GA + U_DP*DP_UT 
            U_BB =              DUO   *GA_BB 
      !                                                                       
            U_TH =              DUO_TH*GA + U_DP*DP_TH 
            U_RT =              DUO_RT*GA + U_DP*DP_RT 
            U_MS =              DUO_MS*GA + U_DP*DP_MS 
      !                                                                       
      !                                                                       
            DSN    = DSN    + (1.0 - U   )*DETA 
            DSN_DO = DSN_DO -        U_DO *DETA 
            DSN_UT = DSN_UT -        U_UT *DETA 
            DSN_BB = DSN_BB -        U_BB *DETA 
      !                                                                       
            DSN_TH = DSN_TH -        U_TH *DETA 
            DSN_RT = DSN_RT -        U_RT *DETA 
            DSN_MS = DSN_MS -        U_MS *DETA 
      !                                                                       
      !                                                                       
            THN    = THN    + (U   -   U*U)     *DETA 
            THN_DO = THN_DO + (1.0 - 2.0*U)*U_DO*DETA 
            THN_UT = THN_UT + (1.0 - 2.0*U)*U_UT*DETA 
            THN_BB = THN_BB + (1.0 - 2.0*U)*U_BB*DETA 
      !                                                                       
            THN_TH = THN_TH + (1.0 - 2.0*U)*U_TH*DETA 
            THN_RT = THN_RT + (1.0 - 2.0*U)*U_RT*DETA 
            THN_MS = THN_MS + (1.0 - 2.0*U)*U_MS*DETA 
      !                                                                       
      !          TSN    = TSN    + (U   -   U*U*U)     *DETA                  
      !          TSN_DO = TSN_DO + (1.0 - 3.0*U*U)*U_DO*DETA                  
      !          TSN_UT = TSN_UT + (1.0 - 3.0*U*U)*U_UT*DETA                  
      !          TSN_BB = TSN_BB + (1.0 - 3.0*U*U)*U_BB*DETA                  
      !                                                                       
      !          TSN_TH = TSN_TH + (1.0 - 3.0*U*U)*U_TH*DETA                  
      !          TSN_RT = TSN_RT + (1.0 - 3.0*U*U)*U_RT*DETA                  
      !          TSN_MS = TSN_MS + (1.0 - 3.0*U*U)*U_MS*DETA                  
      !                                                                       
      100   CONTINUE 
      !                                                                       
      !------ set up 2x2 system for DO UT                                     
            REZ1 = DO*DSN    - THETA*HK 
            A11  = DO*DSN_DO + DSN 
            A12  = DO*DSN_UT 
      !c        A12  = DO*DSN_BB                                              
      !                                                                       
            REZ2 = DO*THN    - THETA 
            A21  = DO*THN_DO + THN 
            A22  = DO*THN_UT 
      !c        A22  = DO*THN_BB                                              
      !                                                                       
            IF(ABS(REZ1/THETA) .LT. 2.0E-5 .AND.                            &
      &     ABS(REZ2/THETA) .LT. 2.0E-5      ) GO TO 1010                
      !        IF(ABS(REZ1/THETA) .LT. 1.0E-3 .AND.                           
      !     &     ABS(REZ2/THETA) .LT. 1.0E-3      ) GO TO 1010               
      !                                                                       
            DET = A11*A22 - A12*A21 
            B11 =  A22/DET 
            B12 = -A12/DET 
            B21 = -A21/DET 
            B22 =  A11/DET 
      !                                                                       
            DDO = -(B11*REZ1 + B12*REZ2) 
            DUT = -(B21*REZ1 + B22*REZ2) 
      !c        DBB = -(B21*REZ1 + B22*REZ2)                                  
      !                                                                       
            DMAX = MAX( ABS(DDO/DO) , ABS(DUT/0.05) ) 
      !c        DMAX = MAX( ABS(DDO/DO) , ABS(DBB/BB  ) )                     
            RLX = 1.0 
            IF(DMAX.GT.0.5) RLX = 0.5/DMAX 
      !                                                                       
            DO = DO + RLX*DDO 
            UT = UT + RLX*DUT 
      !c        BB = BB + RLX*DBB                                             
      !                                                                       
      !        write(*,4400) iter, do, ut, rez1, rez2, rlx                    
      !c        write(*,*) iter, do, bb, rez1, rez2                           
      !4400   format(1x,i5,f10.4,f11.6,2e12.4,f8.4) 
      !                                                                       
      1000 END DO 
      !                                                                       
            WRITE(*,*) 'PRWALL: Convergence failed. Res =', REZ1, REZ2 
      !                                                                       
      1010 CONTINUE 
      !                                                                       
      !                                                                       
      !                                                                       
      !CC   REZ1  = DO*DSN    - THETA*HK                                      
            Z1_HK =           - THETA 
            Z1_TH = DO*DSN_TH -       HK 
            Z1_RT = DO*DSN_RT 
            Z1_MS = DO*DSN_MS 
      !                                                                       
      !CC   REZ2  = DO*THN    - THETA                                         
            Z2_HK = 0.0 
            Z2_TH = DO*THN_TH - 1.0 
            Z2_RT = DO*THN_RT 
            Z2_MS = DO*THN_MS 
      !                                                                       
            DO_HK = -(B11*Z1_HK + B12*Z2_HK) 
            DO_TH = -(B11*Z1_TH + B12*Z2_TH) 
            DO_RT = -(B11*Z1_RT + B12*Z2_RT) 
            DO_MS = -(B11*Z1_MS + B12*Z2_MS) 
      !                                                                       
            UT_HK = 0.0 
            UT_TH = 0.0 
            UT_RT = 0.0 
            UT_MS = 0.0 
      !                                                                       
            BB_HK = 0.0 
            BB_TH = 0.0 
            BB_RT = 0.0 
            BB_MS = 0.0 
      !                                                                       
            UT_HK = -(B21*Z1_HK + B22*Z2_HK) 
            UT_TH = -(B21*Z1_TH + B22*Z2_TH) 
            UT_RT = -(B21*Z1_RT + B22*Z2_RT) 
            UT_MS = -(B21*Z1_MS + B22*Z2_MS) 
      !                                                                       
      !c      BB_HK = -(B21*Z1_HK + B22*Z2_HK)                                
      !c      BB_TH = -(B21*Z1_TH + B22*Z2_TH)                                
      !c      BB_RT = -(B21*Z1_RT + B22*Z2_RT)                                
      !c      BB_MS = -(B21*Z1_MS + B22*Z2_MS)                                
      !                                                                       
      !                                                                       
      !---- set and linearize Cf                                              
            CF    = SGN*2.0*UT**2 
            CF_UT = SGN*4.0*UT 
            CF_DO = 0.0 
      !                                                                       
            CF_HK = CF_UT*UT_HK + CF_DO*DO_HK 
            CF_TH = CF_UT*UT_TH + CF_DO*DO_TH 
            CF_RT = CF_UT*UT_RT + CF_DO*DO_RT 
            CF_MS = CF_UT*UT_MS + CF_DO*DO_MS 
      !                                                                       
      !                                                                       
      !---- set and linearize "slip" velocity UI = UI( DUO(DO UT TH RT MS) )  
            UI    = UO - DUO 
            UI_UT =    - DUO_UT 
            UI_DO =    - DUO_DO 
      !                                                                       
            UI_HK = UI_UT*UT_HK + UI_DO*DO_HK 
            UI_TH = UI_UT*UT_TH + UI_DO*DO_TH - DUO_TH 
            UI_RT = UI_UT*UT_RT + UI_DO*DO_RT - DUO_RT 
            UI_MS = UI_UT*UT_MS + UI_DO*DO_MS - DUO_MS 
      !                                                                       
      !                                                                        
      RETURN 
            ! PRWALL                                                      
      END subroutine PRWALL                                           
                                                                              
                                                                              
                                                                              
      SUBROUTINE UWALL(TH,UO,DO,UI,RT,CF,BB, Y,U,N) 
      !------------------------------------------                             
      !     Returns wall BL profile U(Y).                                     
      !                                                                       
      !     Input:                                                            
      !        TH    kinematic momentum thickness                             
      !        UO    uo/ue outer velocity  (= 1 for normal BL)                
      !        DO    BL thickness                                             
      !        UI    inner "slip" velocity                                    
      !        RT    momentum thickness based on ue and THETA                 
      !        CF    wall skin friction                                       
      !        BB    outer profile exponent                                   
      !        N     number of profile array points                           
      !                                                                       
      !     Output:                                                           
      !        Y(i)  normal coordinate array                                  
      !        U(i)  u/ue velocity profile array                              
      !-------------------------------------------                            
      !                                                                       
            IMPLICIT REAL*8 (A-H,M,O-Z) 
            DIMENSION Y(N), U(N) 
            DATA HPI / 1.570796327 / 
            DATA AK / 0.09 / 
      !                                                                       
      !---- log-law constants                                                 
            DATA VKAP, VB / 0.40 , 5.0 / 
      !                                                                       
            EBK = EXP(-VB*VKAP) 
      !                                                                       
            SGN = SIGN( 1.0 , CF ) 
            UT = SGN * SQRT(0.5*ABS(CF)) 
      !                                                                       
      !                                                                       
      !---- set d+ = DP(UT DO ; RT TH)                                        
            DP    = SGN * UT*RT*DO/TH 
      !                                                                       
      !---- estimate inner profile edge velocity Ui+ using log-law            
            UPE = LOG(DP)/VKAP + VB 
      !                                                                       
      !---- converge exact Ui+ using Spalding formula                         
            DO 10 ITUP=1, 5 
            UK = UPE*VKAP 
            ARG = UK - VB*VKAP 
            EXU = EXP(ARG) 
            REZ  = UPE +  EXU - EBK*(1.0 + UK + UK*UK/2.0 + UK*UK*UK/6.0)   &
      &       - DP                                                       
            DP_U = 1.0 + (EXU - EBK*(1.0 + UK + UK*UK/2.0))*VKAP 
      !                                                                       
            IF(ABS(REZ/DP) .LT. 1.0E-5) GO TO 11 
      !                                                                       
            DUPE = -REZ/DP_U 
            UPE = UPE + DUPE 
      10 END DO 
            WRITE(*,*) 'UWALL: Ue+ convergence failed,  Res =', REZ/DP 
      11 CONTINUE 
      !                                                                       
      !          2      2        3      3                                     
      !---- set d y+/du+   and  d y+/du+   at BL edge                         
            DP_UU  = (EXU - EBK*(1.0 + UK))*VKAP**2 
            DP_UUU = (EXU - EBK           )*VKAP**3 
      !                                                                       
      !                         2      2                                      
      !---- set  du+/dy+  and  d u+/dy+   at BL edge                          
            UPD  = 1.0/DP_U 
            UPDD = (-1.0/DP_U**3) * DP_UU 
      !                                                                       
      !---- set coefficients for Spalding profile correction polynomial       
            DC2  = 0.5*DP*DP*UPDD    - DP*UPD 
            DC3  = -(  DP*DP*UPDD    - DP*UPD   ) / 3.0 
      !                                                                       
      !---- set outer profile amplitude DUO                                   
            DUO    = UO - UT*(UPE + DC2 + DC3) 
      !                                                                       
      !                                                                       
            BB1    =  3.0*(BB    +2.0)*(BB+3.0)/(BB+7.0) 
            BB2    = -5.0*(BB    +1.0)*(BB+3.0)/(BB+7.0) 
            BB3    =  2.0*(BB    +1.0)*(BB+2.0)/(BB+7.0) 
      !                                                                       
      !      NE = (N*9)/10                                                    
            NE = N 
      !                                                                       
      !---- fill Y coordinate and U profile arrays                            
      !CC   EXUPE = EXP(UPE*VKAP - VB*VKAP)                                   
            EXUPE = EXU 
      !                                                                       
            DEXU = (EXUPE - EBK)/FLOAT(NE-1) 
      !                                                                       
            I = 1 
            Y(I) = 0.0 
            U(I) = 0.0 
            DO 20 I=2, NE 
      !cc     EXU = EBK +  DEXU*FLOAT(I-1)                                    
            EXU = EBK + (DEXU - 0.75*DEXU*FLOAT(NE-I)/FLOAT(NE-1))          &
      &              *FLOAT(I-1)                                         
      !                                                                       
      !CC     UK = UP*VKAP                                                    
            UK = LOG(EXU) + VB*VKAP 
      !                                                                       
            UP = UK/VKAP 
      !                                                                       
      !------ set "inverse" Spalding profile  y+(u+)                          
            YP    =  UP +  EXU - EBK*(1.0 + UK + UK*UK/2.0 + UK*UK*UK/6.0) 
            YP_UP = 1.0 + (EXU - EBK*(1.0 + UK + UK*UK/2.0))*VKAP 
      !                                                                       
            ET = YP/DP 
      !                                                                       
      !------ set final inner profile (fudged Spalding)                       
            UIP = UP + DC2*ET**2 + DC3*ET**3 
      !                                                                       
      !------ set outer profile                                               
            SQE = SQRT(ET) 
            ETB = ET**BB 
      !                                                                       
      !cc        G = 2.0*ETB - ETB**2                                         
      !                                                                       
            G = (BB1   *ET + BB2   *ET**2 + BB3   *ET**3)*ETB 
      !                                                                       
            Y(I) = ET*DO 
            U(I) = UT*UIP  +  DUO*G 
      !                                                                       
      20 END DO 
      !                                                                       
      !      DETA = 0.1 / FLOAT(N - NE - 1)                                   
      !      DO 300 I=NE+1, N                                                 
      !        ETA = 1.0 + DETA*FLOAT(I-NE)                                   
      !        Y(I) = DO*ETA                                                  
      !        U(I) = 1.0                                                     
      ! 300  CONTINUE                                                         
      !                                                                       
      RETURN 
            ! UWALL                                                       
      END SUBROUTINE UWALL                                           
                                                                              
                                                                              
                                                                              
      SUBROUTINE FS(INORM,ISPEC,BSPEC,HSPEC,N,ETAE,GEO,ETA,F,U,S,DELTA) 
            IMPLICIT REAL*8 (A-H,M,O-Z) 
            DIMENSION ETA(N), F(N), U(N), S(N) 
      !-----------------------------------------------------                  
      !     Routine for solving the Falkner-Skan equation.                    
      !                                                                       
      !     Input:                                                            
      !     ------                                                            
      !      INORM   1: eta = y / sqrt(vx/Ue)  "standard" Falkner-Skan coordin
      !              2: eta = y / sqrt(2vx/(m+1)Ue)  Hartree's coordinate     
      !              3: eta = y / Theta  momentum thickness normalized coordin
      !      ISPEC   1: BU  = x/Ue dUe/dx ( = "m")  specified                 
      !              2: H12 = Dstar/Theta  specified                          
      !      BSPEC   specified pressure gradient parameter  (if ISPEC = 1)    
      !      HSPEC   specified shape parameter of U profile (if ISPEC = 2)    
      !      N       total number of points in profiles                       
      !      ETAE    edge value of normal coordinate                          
      !      GEO     exponential stretching factor for ETA:                   
      !                                                                       
      !     Output:                                                           
      !     -------                                                           
      !      BSPEC   calculated pressure gradient parameter  (if ISPEC = 2)   
      !      HSPEC   calculated shape parameter of U profile (if ISPEC = 1)   
      !      ETA     normal BL coordinate                                     
      !      F,U,S   Falkner Skan profiles                                    
      !      DELTA   normal coordinate scale  y = eta * Delta                 
      !-----------------------------------------------------                  
      !                                                                       
            PARAMETER (NMAX=257,NRMAX=3) 
            DIMENSION A(3,3,NMAX),B(3,3,NMAX),C(3,3,NMAX),                    &
      &          R(3,NRMAX,NMAX)                                         
      !                                                                       
      !---- set number of righthand sides.                                    
            DATA NRHS / 3 / 
      !                                                                       
            ITMAX = 40 
      !                                                                       
            IF(N.GT.NMAX) STOP 'FS: Array overflow.' 
      !                                                                       
            PI = 4.0*ATAN(1.0) 
      !                                                                       
      !CC      if(u(n) .ne. 0.0) go to 9991                                   
                                                                              
      !                                                                       
      !---- initialize H or BetaU with empirical curve fits                   
            IF(ISPEC.EQ.1) THEN 
            H = 2.6 
            BU = BSPEC 
            ELSE 
            H = HSPEC 
            IF(H .LE. 2.17) THEN 
            WRITE(*,*) 'FS: Specified H too low' 
            H = 2.17 
            ENDIF 
            BU = (0.058*(H-4.0)**2/(H-1.0) - 0.068) / (6.54*H - 14.07) * H**2 
            IF(H .GT. 4.0) BU = MIN( BU , 0.0 ) 
            ENDIF 
      !                                                                       
      !---- initialize TN = Delta^2 Ue / vx                                   
            IF(INORM.EQ.3) THEN 
            TN = (6.54*H - 14.07) / H**2 
            ELSE 
            TN = 1.0 
            ENDIF 
      !                                                                       
      !---- set eta array                                                     
            DETA = 1.0 
            ETA(1) = 0.0 
            DO 5 I=2, N 
            ETA(I) = ETA(I-1) + DETA 
            DETA = GEO*DETA 
      5 END DO 
      !                                                                       
            DO 6 I=1, N 
            ETA(I) = ETA(I) * ETAE/ETA(N) 
      6 END DO 
      !                                                                       
      !                                                                       
      !---- initial guess for profiles using a sine loop for U for half near w
            IF(H .LE. 3.0) THEN 
      !                                                                       
            IF(INORM.EQ.3) THEN 
            ETJOIN = 7.3 
            ELSE 
            ETJOIN = 5.0 
            ENDIF 
      !                                                                       
            EFAC = 0.5*PI/ETJOIN 
            DO 10 I=1, N 
            U(I) = SIN(EFAC*ETA(I)) 
            F(I) = 1.0/EFAC * (1.0 - COS(EFAC*ETA(I))) 
            S(I) = EFAC*COS(EFAC*ETA(I)) 
            IF(ETA(I) .GT. ETJOIN) GO TO 11 
      10  CONTINUE 
      11  CONTINUE 
            IJOIN = I 
      !                                                                       
      !----- constant U for outer half                                        
            DO 12 I=IJOIN+1, N 
            U(I) = 1.0 
            F(I) = F(IJOIN) + ETA(I) - ETA(IJOIN) 
            S(I) = 0. 
      12  CONTINUE 
      !                                                                       
            ELSE 
      !                                                                       
            IF(INORM.EQ.3) THEN 
            ETJOIN1 = 0.0 
            ETJOIN2 = 8.0 
            IF(H .GT. 4.0) THEN 
            ETJOIN1 = H - 4.0 
            ETJOIN2 = ETJOIN1 + 8.0 
            ENDIF 
            ELSE 
            ETJOIN1 = 0.0 
            ETJOIN2 = 8.0 
            ENDIF 
      !                                                                       
            DO 13 I=1, N 
            U(I) = 0.0 
            S(I) = 0.0 
            F(I) = 0.0 
            IF(ETA(I) .GE. ETJOIN1) GO TO 14 
      13  CONTINUE 
      14  CONTINUE 
            IJOIN = I 
      !                                                                       
            EFAC = 0.5*PI/(ETJOIN2-ETJOIN1) 
            DO 15 I=IJOIN+1, N 
            EBAR = ETA(I) - ETJOIN1 
            U(I) = 0.5 - 0.5*COS(2.0*EFAC*EBAR) 
            F(I) = 0.5*EBAR - 0.25/EFAC * SIN(2.0*EFAC*EBAR) 
            S(I) = EFAC*SIN(2.0*EFAC*EBAR) 
            IF(ETA(I) .GE. ETJOIN2) GO TO 16 
      15  CONTINUE 
      16  CONTINUE 
            IJOIN = I 
      !                                                                       
      !----- constant U for outer half                                        
            DO 17 I=IJOIN+1, N 
            U(I) = 1.0 
            F(I) = F(IJOIN) + ETA(I) - ETA(IJOIN) 
            S(I) = 0. 
      17  CONTINUE 
      !                                                                       
            ENDIF 
      !                                                                       
                                                                              
      9991 continue 
      !                                                                       
            RMS = 1.0 
      !                                                                       
      !---- Newton iteration loop                                             
            DO 100 ITER=1, ITMAX 
      !                                                                       
      !------ zero out A,B,C blocks and righthand sides R                     
            DO 20 I=1, N 
            DO 201 II=1,3 
                  DO 2001 III=1,3 
                  A(II,III,I) = 0. 
                  B(II,III,I) = 0. 
                  C(II,III,I) = 0. 
      2001       CONTINUE 
                  R(II,1,I) = 0. 
                  R(II,2,I) = 0. 
                  R(II,3,I) = 0. 
      201     CONTINUE 
      20   CONTINUE 
      !                                                                       
      !------ calculate Theta in computational space                          
            THI = 0. 
            DO I=1,N-1 
            US  = U(I) + U(I+1) 
            DETA = ETA(I+1) - ETA(I) 
            THI = THI + (1.0 - 0.5*US)*0.5*US*DETA 
            ENDDO 
            IF(INORM.EQ.3) THEN 
      !         TN = TN/THI**2                                                
            ENDIF 
      !                                                                       
      !...................................................                    
      !                                                                       
            A(1,1,1) = 1.0 
            A(2,2,1) = 1.0 
            A(3,2,N) = 1.0 
            R(1,1,1) = F(1) 
            R(2,1,1) = U(1) 
            R(3,1,N) = U(N) - 1.0 
      !                                                                       
            IF(INORM.EQ.2) THEN 
            BETU    = 2.0*BU/(BU+1.0) 
            BETU_BU = (2.0 - BETU/(BU+1.0))/(BU+1.0) 
            BETN    = 1.0 
            BETN_BU = 0.0 
            ELSE 
            BETU    = BU 
            BETU_BU = 1.0 
            BETN    = 0.5*(1.0 + BU) 
            BETN_BU = 0.5 
            ENDIF 
      !                                                                       
            DO 30 I=1,N-1 
      !                                                                       
            DETA = ETA(I+1) - ETA(I) 
            R(1,1,I+1) = F(I+1) - F(I) - 0.5*DETA*(U(I+1)+U(I)) 
            R(2,1,I+1) = U(I+1) - U(I) - 0.5*DETA*(S(I+1)+S(I)) 
            R(3,1,I)   = S(I+1) - S(I)                                    &
      &      + TN * (  BETN*DETA*0.5*(F(I+1)*S(I+1) + F(I)*S(I))         &
      &              + BETU*DETA*(1.0 - 0.5*(U(I+1)**2 + U(I)**2)) )     
      !                                                                       
            A(3,1,I) =  TN * BETN*0.5*DETA*S(I) 
            C(3,1,I) =  TN * BETN*0.5*DETA*S(I+1) 
            A(3,2,I) = -TN * BETU    *DETA*U(I) 
            C(3,2,I) = -TN * BETU    *DETA*U(I+1) 
            A(3,3,I) =  TN * BETN*0.5*DETA*F(I)   - 1.0 
            C(3,3,I) =  TN * BETN*0.5*DETA*F(I+1) + 1.0 
      !                                                                       
            B(1,1,I+1) = -1.0 
            A(1,1,I+1) =  1.0 
            B(1,2,I+1) = -0.5*DETA 
            A(1,2,I+1) = -0.5*DETA 
      !                                                                       
            B(2,2,I+1) = -1.0 
            A(2,2,I+1) =  1.0 
            B(2,3,I+1) = -0.5*DETA 
            A(2,3,I+1) = -0.5*DETA 
      !                                                                       
            R(3,2,I) = TN                                                 &
      &          * ( BETN_BU*DETA*0.5*(F(I+1)*S(I+1) + F(I)*S(I))        &
      &            + BETU_BU*DETA*(1.0 - 0.5*(U(I+1)**2 + U(I)**2)))     
            R(3,3,I) = ( BETN*DETA*0.5*(F(I+1)*S(I+1) + F(I)*S(I))        &
      &               + BETU*DETA*(1.0 - 0.5*(U(I+1)**2 + U(I)**2)) )    
      !                                                                       
      30   CONTINUE 
      !...........................................................            
      !                                                                       
      !                                                                       
      !---- solve Newton system for the three solution vectors                
            CALL B3SOLV(A,B,C,R,N,NRHS,NRMAX) 
      !                                                                       
      !                                                                       
      !---- calculate and linearize Dstar, Theta, in computational space      
            DSI = 0. 
            DSI1 = 0. 
            DSI2 = 0. 
            DSI3 = 0. 
      !                                                                       
            THI = 0. 
            THI1 = 0. 
            THI2 = 0. 
            THI3 = 0. 
      !                                                                       
            DO 40 I=1,N-1 
            US  = U(I) + U(I+1) 
            DETA = ETA(I+1) - ETA(I) 
      !                                                                       
            DSI = DSI + (1.0 - 0.5*US)*DETA 
            DSI_US = -0.5*DETA 
      !                                                                       
            THI = THI + (1.0 - 0.5*US)*0.5*US*DETA 
            THI_US = (0.5 - 0.5*US)*DETA 
      !                                                                       
            DSI1 = DSI1 + DSI_US*(R(2,1,I) + R(2,1,I+1)) 
            DSI2 = DSI2 + DSI_US*(R(2,2,I) + R(2,2,I+1)) 
            DSI3 = DSI3 + DSI_US*(R(2,3,I) + R(2,3,I+1)) 
      !                                                                       
            THI1 = THI1 + THI_US*(R(2,1,I) + R(2,1,I+1)) 
            THI2 = THI2 + THI_US*(R(2,2,I) + R(2,2,I+1)) 
            THI3 = THI3 + THI_US*(R(2,3,I) + R(2,3,I+1)) 
      40 END DO 
      !                                                                       
      !                                                                       
            IF(ISPEC.EQ.1) THEN 
      !                                                                       
      !----- set and linearize  Bu = Bspec  residual                          
            R1 = BSPEC - BU 
            Q11 = 1.0 
            Q12 = 0.0 
      !                                                                       
            ELSE 
      !                                                                       
      !----- set and linearize  H = Hspec  residual                           
            R1  =  DSI  - HSPEC*THI                                          &
      &       -DSI1 + HSPEC*THI1                                         
            Q11 = -DSI2 + HSPEC*THI2 
            Q12 = -DSI3 + HSPEC*THI3 
      !                                                                       
            ENDIF 
      !                                                                       
      !                                                                       
            IF(INORM.EQ.3) THEN 
      !                                                                       
      !----- set and linearize  normalized Theta = 1  residual                
            R2  =  THI  - 1.0                                                &
      &       -THI1                                                      
            Q21 = -THI2 
            Q22 = -THI3 
      !                                                                       
      !      R2  =  TN - 0.505                                                
      !      IF(ITER.LT.13) THEN                                              
      !      R2  =  0.                                                        
      !      Q21 = 0.0                                                        
      !      Q22 = 1.0                                                        
      !      ENDIF                                                            
                                                                              
            ELSE 
      !                                                                       
      !----- set eta scaling coefficient to unity                             
            R2  =  TN - 1.0 
            Q21 = 0.0 
            Q22 = 1.0 
      !                                                                       
            ENDIF 
      !                                                                       
      !                                                                       
            DET =   Q11*Q22 - Q12*Q21 
            DBU = -(R1 *Q22 - Q12*R2 ) / DET 
            DTN = -(Q11*R2  - R1 *Q21) / DET 
      !                                                                       
      !                                                                       
      !---- calculate changes in F,U,S, and the max and rms change            
            RMAX = 0. 
            RMS = 0. 
            DO 50 I=1,N 
            DF = -R(1,1,I) - DBU*R(1,2,I) - DTN*R(1,3,I) 
            DU = -R(2,1,I) - DBU*R(2,2,I) - DTN*R(2,3,I) 
            DS = -R(3,1,I) - DBU*R(3,2,I) - DTN*R(3,3,I) 
      !                                                                       
            RMAX = MAX(RMAX,ABS(DF),ABS(DU),ABS(DS)) 
            RMS = DF**2 + DU**2 + DS**2  +  RMS 
      50 END DO 
            RMS = SQRT(RMS/(3.0*FLOAT(N) + 3.0)) 
      !                                                                       
            RMAX = MAX(RMAX,ABS(DBU/1.0),ABS(DTN/TN)) 
      !                                                                       
      !---- set underrelaxation factor if necessary by limiting max change to 
            RLX = 1.0 
            IF(RMAX.GT.0.5) RLX = 0.5/RMAX 
      !                                                                       
      !---- update F,U,S                                                      
            DO 60 I=1,N 
            DF = -R(1,1,I) - DBU*R(1,2,I) - DTN*R(1,3,I) 
            DU = -R(2,1,I) - DBU*R(2,2,I) - DTN*R(2,3,I) 
            DS = -R(3,1,I) - DBU*R(3,2,I) - DTN*R(3,3,I) 
      !                                                                       
            F(I) = F(I) + RLX*DF 
            U(I) = U(I) + RLX*DU 
            S(I) = S(I) + RLX*DS 
      60 END DO 
      !                                                                       
      !---- update BetaU and Theta                                            
            BU = BU + RLX*DBU 
            TN = TN + RLX*DTN 
      !                                                                       
      !     write(*,'(1x,i5,e13.5,3f10.5,2x,f8.4)') iter, rms, bu,thi,tn, rlx 
                                                                              
      !---- check for convergence                                             
            IF(ITER.GT.3 .AND. RMS .LT. 1.0E-6) GO TO 105 
      !                                                                       
      100 END DO 
            WRITE(*,*) 'FS: Convergence failed' 
      !                                                                       
      105 CONTINUE 
      !                                                                       
            HSPEC = DSI/THI 
            BSPEC = BU 
      !                                                                       
            DELTA = SQRT(TN) 
      !                                                                       
            RETURN 
      !                                                                       
      !     The                                                               
      END subroutine FS                                          
                                                                              
                                                                              
      SUBROUTINE B3SOLV(A,B,C,R,N,NRHS,NRMAX) 
            IMPLICIT REAL*8(A-H,M,O-Z) 
            DIMENSION A(3,3,N), B(3,3,N), C(3,3,N), R(3,NRMAX,N) 
      !     ******************************************************************
      !      This routine solves a 3x3 block-tridiagonal system with an arbitr
      !      number of righthand sides by a standard block elimination scheme.
      !      The solutions are returned in the Rj vectors.                    
      !                                                                       
      !      |A C      ||d|   |R..|                                           
      !      |B A C    ||d|   |R..|                                           
      !      |  B . .  ||.| = |R..|                                           
      !      |    . . C||.|   |R..|                                           
      !      |      B A||d|   |R..|                                           
      !                                                  Mark Drela   10 March
      !     ******************************************************************
      !                                                                       
      !CC** Forward sweep: Elimination of lower block diagonal (B's).         
            DO 1 I=1, N 
      !                                                                       
            IM = I-1 
      !                                                                       
      !------ don't eliminate B1 block because it doesn't exist               
            IF(I.EQ.1) GO TO 12 
      !                                                                       
      !------ eliminate Bi block, thus modifying Ai and Ci blocks             
            DO 11 K=1, 3 
            DO 111 L=1, 3 
                  A(K,L,I) = A(K,L,I)                                         &
      & - (B(K,1,I)*C(1,L,IM) + B(K,2,I)*C(2,L,IM) + B(K,3,I)*C(3,L,IM)) 
      111     CONTINUE 
            DO 112 L=1, NRHS 
                  R(K,L,I) = R(K,L,I)                                         &
      & - (B(K,1,I)*R(1,L,IM) + B(K,2,I)*R(2,L,IM) + B(K,3,I)*R(3,L,IM)) 
      112     CONTINUE 
      11   CONTINUE 
      !                                                                       
      !                                                              -1       
      !CC---- multiply Ci block and righthand side Ri vectors by (Ai)         
      !       using Gaussian elimination.                                     
      !                                                                       
      12   DO 13 KPIV=1, 2 
            KP1 = KPIV+1 
      !                                                                       
      !-------- find max pivot index KX                                       
            KX = KPIV 
            DO 131 K=KP1, 3 
                  IF(ABS(A(K,KPIV,I)) .LE. ABS(A(KX,KPIV,I))) THEN 
                  GO TO 131 
                  ELSE 
                  GO TO 1311 
                  ENDIF 
      1311       KX = K 
      131     CONTINUE 
      !                                                                       
            IF(A(KX,KPIV,I).EQ.0.0) THEN 
            WRITE(*,*) 'Singular A block, i = ',I 
            STOP 
            ENDIF 
      !                                                                       
            PIVOT = 1.0/A(KX,KPIV,I) 
      !                                                                       
      !-------- switch pivots                                                 
            A(KX,KPIV,I) = A(KPIV,KPIV,I) 
      !                                                                       
      !-------- switch rows & normalize pivot row                             
            DO 132 L=KP1, 3 
                  TEMP = A(KX,L,I)*PIVOT 
                  A(KX,L,I) = A(KPIV,L,I) 
                  A(KPIV,L,I) = TEMP 
      132     CONTINUE 
      !                                                                       
            DO 133 L=1, 3 
                  TEMP = C(KX,L,I)*PIVOT 
                  C(KX,L,I) = C(KPIV,L,I) 
                  C(KPIV,L,I) = TEMP 
      133     CONTINUE 
      !                                                                       
            DO 134 L=1, NRHS 
                  TEMP = R(KX,L,I)*PIVOT 
                  R(KX,L,I) = R(KPIV,L,I) 
                  R(KPIV,L,I) = TEMP 
      134     CONTINUE 
      !B                                                                      
      !-------- forward eliminate everything                                  
            DO 135 K=KP1, 3 
                  DO 1351 L=KP1, 3 
                  A(K,L,I) = A(K,L,I) - A(K,KPIV,I)*A(KPIV,L,I) 
      1351       CONTINUE 
                  C(K,1,I) = C(K,1,I) - A(K,KPIV,I)*C(KPIV,1,I) 
                  C(K,2,I) = C(K,2,I) - A(K,KPIV,I)*C(KPIV,2,I) 
                  C(K,3,I) = C(K,3,I) - A(K,KPIV,I)*C(KPIV,3,I) 
                  DO 1352 L=1, NRHS 
                  R(K,L,I) = R(K,L,I) - A(K,KPIV,I)*R(KPIV,L,I) 
      1352       CONTINUE 
      135     CONTINUE 
      !                                                                       
      13   CONTINUE 
      !                                                                       
      !------ solve for last row                                              
            IF(A(3,3,I).EQ.0.0) THEN 
            WRITE(*,*) 'Singular A block, i = ',I 
            STOP 
            ENDIF 
            PIVOT = 1.0/A(3,3,I) 
            C(3,1,I) = C(3,1,I)*PIVOT 
            C(3,2,I) = C(3,2,I)*PIVOT 
            C(3,3,I) = C(3,3,I)*PIVOT 
            DO 14 L=1, NRHS 
            R(3,L,I) = R(3,L,I)*PIVOT 
      14   CONTINUE 
      !                                                                       
      !------ back substitute everything                                      
            DO 15 KPIV=2, 1, -1 
            KP1 = KPIV+1 
            DO 151 K=KP1, 3 
                  C(KPIV,1,I) = C(KPIV,1,I) - A(KPIV,K,I)*C(K,1,I) 
                  C(KPIV,2,I) = C(KPIV,2,I) - A(KPIV,K,I)*C(K,2,I) 
                  C(KPIV,3,I) = C(KPIV,3,I) - A(KPIV,K,I)*C(K,3,I) 
                  DO 1511 L=1, NRHS 
                  R(KPIV,L,I) = R(KPIV,L,I) - A(KPIV,K,I)*R(K,L,I) 
      1511       CONTINUE 
      151     CONTINUE 
      15   CONTINUE 
      1 END DO 
      !                                                                       
      !CC** Backward sweep: Back substitution using upper block diagonal (Ci's
            DO 2 I=N-1, 1, -1 
            IP = I+1 
            DO 21 L=1, NRHS 
            DO 211 K=1, 3 
                  R(K,L,I) = R(K,L,I)                                         &
      & - (R(1,L,IP)*C(K,1,I) + R(2,L,IP)*C(K,2,I) + R(3,L,IP)*C(K,3,I)) 
      211     CONTINUE 
      21   CONTINUE 
      2 END DO 
      !                                                                       
            RETURN 
            ! B3SOLV                                                      
      END subroutine B3SOLV                                           
end module m_profil