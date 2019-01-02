!***********************************************************************
!    Module:  xpanel.f
! 
!    Copyright (C) 2000 Mark Drela 
! 
!    This program is free software; you can redistribute it and/or modify
!    it under the terms of the GNU General Public License as published by
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


SUBROUTINE APCALC
    INCLUDE 'XFOIL.INC'
    !
    !---- set angles of airfoil panels
    DO I = 1, N - 1
        SX = X(I + 1) - X(I)
        SY = Y(I + 1) - Y(I)
        IF(SX == 0.0 .AND. SY == 0.0) THEN
            APANEL(I) = ATAN2(-NY(I), -NX(I))
        ELSE
            APANEL(I) = ATAN2(SX, -SY)
        ENDIF
    end do
    !
    !---- TE panel
    I = N
    IP = 1
    IF(SHARP) THEN
        APANEL(I) = PI
    ELSE
        SX = X(IP) - X(I)
        SY = Y(IP) - Y(I)
        APANEL(I) = ATAN2(-SX, SY) + PI
    ENDIF
    !
    RETURN
END


SUBROUTINE NCALC(X, Y, S, N, XN, YN)
    !---------------------------------------
    !     Calculates normal unit vector
    !     components at airfoil panel nodes
    !---------------------------------------
    DIMENSION X(N), Y(N), S(N), XN(N), YN(N)
    !
    IF(N <= 1) RETURN
    !
    CALL SEGSPL(X, XN, S, N)
    CALL SEGSPL(Y, YN, S, N)
    DO I = 1, N
        SX = YN(I)
        SY = -XN(I)
        SMOD = SQRT(SX * SX + SY * SY)
        IF(SMOD == 0.0) THEN
            XN(I) = -1.0
            YN(I) = 0.0
        ELSE
            XN(I) = SX / SMOD
            YN(I) = SY / SMOD
        ENDIF
    end do
    !
    !---- average normal vectors at corner points
    DO I = 1, N - 1
        IF(S(I) == S(I + 1)) THEN
            SX = 0.5 * (XN(I) + XN(I + 1))
            SY = 0.5 * (YN(I) + YN(I + 1))
            SMOD = SQRT(SX * SX + SY * SY)
            IF(SMOD == 0.0) THEN
                XN(I) = -1.0
                YN(I) = 0.0
                XN(I + 1) = -1.0
                YN(I + 1) = 0.0
            ELSE
                XN(I) = SX / SMOD
                YN(I) = SY / SMOD
                XN(I + 1) = SX / SMOD
                YN(I + 1) = SY / SMOD
            ENDIF
        ENDIF
    end do
    !
    RETURN
END


SUBROUTINE PSILIN(I, XI, YI, NXI, NYI, PSI, PSI_NI, GEOLIN, SIGLIN)
    !-----------------------------------------------------------------------
    !     Calculates current streamfunction Psi at panel node or wake node
    !     I due to freestream and all bound vorticity Gam on the airfoil. 
    !     Sensitivities of Psi with respect to alpha (Z_ALFA) and inverse
    !     Qspec DOFs (Z_QDOF0,Z_QDOF1) which influence Gam in inverse cases.
    !     Also calculates the sensitivity vector dPsi/dGam (DZDG).
    !
    !     If SIGLIN=True, then Psi includes the effects of the viscous
    !     source distribution Sig and the sensitivity vector dPsi/dSig
    !     (DZDM) is calculated.
    !
    !     If GEOLIN=True, then the geometric sensitivity vector dPsi/dn
    !     is calculated, where n is the normal motion of the jth node.
    !
    !          Airfoil:  1   < I < N
    !          Wake:     N+1 < I < N+NW
    !-----------------------------------------------------------------------
    INCLUDE 'XFOIL.INC'
    REAL NXO, NYO, NXP, NYP, NXI, NYI
    LOGICAL GEOLIN, SIGLIN
    !
    !---- distance tolerance for determining if two points are the same
    SEPS = (S(N) - S(1)) * 1.0E-5
    !
    IO = I
    !
    COSA = COS(ALFA)
    SINA = SIN(ALFA)
    !
    DO JO = 1, N
        DZDG(JO) = 0.0
        DZDN(JO) = 0.0
        DQDG(JO) = 0.0
    end do
    !
    DO JO = 1, N
        DZDM(JO) = 0.0
        DQDM(JO) = 0.0
    end do
    !
    Z_QINF = 0.
    Z_ALFA = 0.
    Z_QDOF0 = 0.
    Z_QDOF1 = 0.
    Z_QDOF2 = 0.
    Z_QDOF3 = 0.
    !
    PSI = 0.
    PSI_NI = 0.
    !
    QTAN1 = 0.
    QTAN2 = 0.
    QTANM = 0.
    !
    IF(SHARP) THEN
        SCS = 1.0
        SDS = 0.0
    ELSE
        SCS = ANTE / DSTE
        SDS = ASTE / DSTE
    ENDIF
    !
    DO JO = 1, N
        JP = JO + 1
        !
        JM = JO - 1
        JQ = JP + 1
        !
        IF(JO == 1) THEN
            JM = JO
        ELSE IF(JO == N - 1) THEN
            JQ = JP
        ELSE IF(JO == N) THEN
            JP = 1
            IF((X(JO) - X(JP))**2 + (Y(JO) - Y(JP))**2 < SEPS**2) GO TO 12
        ENDIF
        !
        DSO = SQRT((X(JO) - X(JP))**2 + (Y(JO) - Y(JP))**2)
        !
        !------ skip null panel
        IF(DSO == 0.0) GO TO 10
        !
        DSIO = 1.0 / DSO
        !
        APAN = APANEL(JO)
        !
        RX1 = XI - X(JO)
        RY1 = YI - Y(JO)
        RX2 = XI - X(JP)
        RY2 = YI - Y(JP)
        !
        SX = (X(JP) - X(JO)) * DSIO
        SY = (Y(JP) - Y(JO)) * DSIO
        !
        X1 = SX * RX1 + SY * RY1
        X2 = SX * RX2 + SY * RY2
        YY = SX * RY1 - SY * RX1
        !
        RS1 = RX1 * RX1 + RY1 * RY1
        RS2 = RX2 * RX2 + RY2 * RY2
        !
        !------ set reflection flag SGN to avoid branch problems with arctan
        IF(IO >= 1 .AND. IO <= N) THEN
            !------- no problem on airfoil surface
            SGN = 1.0
        ELSE
            !------- make sure arctan falls between  -/+  Pi/2
            SGN = SIGN(1.0, YY)
        ENDIF
        !
        !------ set log(r^2) and arctan(x/y), correcting for reflection if any
        IF(IO /= JO .AND. RS1 > 0.0) THEN
            G1 = LOG(RS1)
            T1 = ATAN2(SGN * X1, SGN * YY) + (0.5 - 0.5 * SGN) * PI
        ELSE
            G1 = 0.0
            T1 = 0.0
        ENDIF
        !
        IF(IO /= JP .AND. RS2 > 0.0) THEN
            G2 = LOG(RS2)
            T2 = ATAN2(SGN * X2, SGN * YY) + (0.5 - 0.5 * SGN) * PI
        ELSE
            G2 = 0.0
            T2 = 0.0
        ENDIF
        !
        X1I = SX * NXI + SY * NYI
        X2I = SX * NXI + SY * NYI
        YYI = SX * NYI - SY * NXI
        !
        IF(GEOLIN) THEN
            NXO = NX(JO)
            NYO = NY(JO)
            NXP = NX(JP)
            NYP = NY(JP)
            !
            X1O = -((RX1 - X1 * SX) * NXO + (RY1 - X1 * SY) * NYO) * DSIO - (SX * NXO + SY * NYO)
            X1P = ((RX1 - X1 * SX) * NXP + (RY1 - X1 * SY) * NYP) * DSIO
            X2O = -((RX2 - X2 * SX) * NXO + (RY2 - X2 * SY) * NYO) * DSIO
            X2P = ((RX2 - X2 * SX) * NXP + (RY2 - X2 * SY) * NYP) * DSIO - (SX * NXP + SY * NYP)
            YYO = ((RX1 + X1 * SY) * NYO - (RY1 - X1 * SX) * NXO) * DSIO - (SX * NYO - SY * NXO)
            YYP = -((RX1 - X1 * SY) * NYP - (RY1 + X1 * SX) * NXP) * DSIO
        ENDIF
        !
        IF(JO == N) GO TO 11
        !
        IF(SIGLIN) THEN
            !
            !------- set up midpoint quantities
            X0 = 0.5 * (X1 + X2)
            RS0 = X0 * X0 + YY * YY
            G0 = LOG(RS0)
            T0 = ATAN2(SGN * X0, SGN * YY) + (0.5 - 0.5 * SGN) * PI
            !
            !------- calculate source contribution to Psi  for  1-0  half-panel
            DXINV = 1.0 / (X1 - X0)
            PSUM = X0 * (T0 - APAN) - X1 * (T1 - APAN) + 0.5 * YY * (G1 - G0)
            PDIF = ((X1 + X0) * PSUM + RS1 * (T1 - APAN) - RS0 * (T0 - APAN)&
                    + (X0 - X1) * YY) * DXINV
            !
            PSX1 = -(T1 - APAN)
            PSX0 = T0 - APAN
            PSYY = 0.5 * (G1 - G0)
            !
            PDX1 = ((X1 + X0) * PSX1 + PSUM + 2.0 * X1 * (T1 - APAN) - PDIF) * DXINV
            PDX0 = ((X1 + X0) * PSX0 + PSUM - 2.0 * X0 * (T0 - APAN) + PDIF) * DXINV
            PDYY = ((X1 + X0) * PSYY + 2.0 * (X0 - X1 + YY * (T1 - T0))) * DXINV
            !
            DSM = SQRT((X(JP) - X(JM))**2 + (Y(JP) - Y(JM))**2)
            DSIM = 1.0 / DSM
            !
            !CC      SIG0 = (SIG(JP) - SIG(JO))*DSIO
            !CC      SIG1 = (SIG(JP) - SIG(JM))*DSIM
            !CC      SSUM = SIG0 + SIG1
            !CC      SDIF = SIG0 - SIG1
            !
            SSUM = (SIG(JP) - SIG(JO)) * DSIO + (SIG(JP) - SIG(JM)) * DSIM
            SDIF = (SIG(JP) - SIG(JO)) * DSIO - (SIG(JP) - SIG(JM)) * DSIM
            !
            PSI = PSI + QOPI * (PSUM * SSUM + PDIF * SDIF)
            !
            !------- dPsi/dm
            DZDM(JM) = DZDM(JM) + QOPI * (-PSUM * DSIM + PDIF * DSIM)
            DZDM(JO) = DZDM(JO) + QOPI * (-PSUM * DSIO - PDIF * DSIO)
            DZDM(JP) = DZDM(JP) + QOPI * (PSUM * (DSIO + DSIM)&
                    + PDIF * (DSIO - DSIM))
            !
            !------- dPsi/dni
            PSNI = PSX1 * X1I + PSX0 * (X1I + X2I) * 0.5 + PSYY * YYI
            PDNI = PDX1 * X1I + PDX0 * (X1I + X2I) * 0.5 + PDYY * YYI
            PSI_NI = PSI_NI + QOPI * (PSNI * SSUM + PDNI * SDIF)
            !
            QTANM = QTANM + QOPI * (PSNI * SSUM + PDNI * SDIF)
            !
            DQDM(JM) = DQDM(JM) + QOPI * (-PSNI * DSIM + PDNI * DSIM)
            DQDM(JO) = DQDM(JO) + QOPI * (-PSNI * DSIO - PDNI * DSIO)
            DQDM(JP) = DQDM(JP) + QOPI * (PSNI * (DSIO + DSIM)&
                    + PDNI * (DSIO - DSIM))
            !
            !
            !------- calculate source contribution to Psi  for  0-2  half-panel
            DXINV = 1.0 / (X0 - X2)
            PSUM = X2 * (T2 - APAN) - X0 * (T0 - APAN) + 0.5 * YY * (G0 - G2)
            PDIF = ((X0 + X2) * PSUM + RS0 * (T0 - APAN) - RS2 * (T2 - APAN)&
                    + (X2 - X0) * YY) * DXINV
            !
            PSX0 = -(T0 - APAN)
            PSX2 = T2 - APAN
            PSYY = 0.5 * (G0 - G2)
            !
            PDX0 = ((X0 + X2) * PSX0 + PSUM + 2.0 * X0 * (T0 - APAN) - PDIF) * DXINV
            PDX2 = ((X0 + X2) * PSX2 + PSUM - 2.0 * X2 * (T2 - APAN) + PDIF) * DXINV
            PDYY = ((X0 + X2) * PSYY + 2.0 * (X2 - X0 + YY * (T0 - T2))) * DXINV
            !
            DSP = SQRT((X(JQ) - X(JO))**2 + (Y(JQ) - Y(JO))**2)
            DSIP = 1.0 / DSP
            !
            !CC         SIG2 = (SIG(JQ) - SIG(JO))*DSIP
            !CC         SIG0 = (SIG(JP) - SIG(JO))*DSIO
            !CC         SSUM = SIG2 + SIG0
            !CC         SDIF = SIG2 - SIG0
            !
            SSUM = (SIG(JQ) - SIG(JO)) * DSIP + (SIG(JP) - SIG(JO)) * DSIO
            SDIF = (SIG(JQ) - SIG(JO)) * DSIP - (SIG(JP) - SIG(JO)) * DSIO
            !
            PSI = PSI + QOPI * (PSUM * SSUM + PDIF * SDIF)
            !
            !------- dPsi/dm
            DZDM(JO) = DZDM(JO) + QOPI * (-PSUM * (DSIP + DSIO)&
                    - PDIF * (DSIP - DSIO))
            DZDM(JP) = DZDM(JP) + QOPI * (PSUM * DSIO - PDIF * DSIO)
            DZDM(JQ) = DZDM(JQ) + QOPI * (PSUM * DSIP + PDIF * DSIP)
            !
            !------- dPsi/dni
            PSNI = PSX0 * (X1I + X2I) * 0.5 + PSX2 * X2I + PSYY * YYI
            PDNI = PDX0 * (X1I + X2I) * 0.5 + PDX2 * X2I + PDYY * YYI
            PSI_NI = PSI_NI + QOPI * (PSNI * SSUM + PDNI * SDIF)
            !
            QTANM = QTANM + QOPI * (PSNI * SSUM + PDNI * SDIF)
            !
            DQDM(JO) = DQDM(JO) + QOPI * (-PSNI * (DSIP + DSIO)&
                    - PDNI * (DSIP - DSIO))
            DQDM(JP) = DQDM(JP) + QOPI * (PSNI * DSIO - PDNI * DSIO)
            DQDM(JQ) = DQDM(JQ) + QOPI * (PSNI * DSIP + PDNI * DSIP)
            !
        ENDIF
        !
        !------ calculate vortex panel contribution to Psi
        DXINV = 1.0 / (X1 - X2)
        PSIS = 0.5 * X1 * G1 - 0.5 * X2 * G2 + X2 - X1 + YY * (T1 - T2)
        PSID = ((X1 + X2) * PSIS + 0.5 * (RS2 * G2 - RS1 * G1 + X1 * X1 - X2 * X2)) * DXINV
        !
        PSX1 = 0.5 * G1
        PSX2 = -.5 * G2
        PSYY = T1 - T2
        !
        PDX1 = ((X1 + X2) * PSX1 + PSIS - X1 * G1 - PSID) * DXINV
        PDX2 = ((X1 + X2) * PSX2 + PSIS + X2 * G2 + PSID) * DXINV
        PDYY = ((X1 + X2) * PSYY - YY * (G1 - G2)) * DXINV
        !
        GSUM1 = GAMU(JP, 1) + GAMU(JO, 1)
        GSUM2 = GAMU(JP, 2) + GAMU(JO, 2)
        GDIF1 = GAMU(JP, 1) - GAMU(JO, 1)
        GDIF2 = GAMU(JP, 2) - GAMU(JO, 2)
        !
        GSUM = GAM(JP) + GAM(JO)
        GDIF = GAM(JP) - GAM(JO)
        !
        PSI = PSI + QOPI * (PSIS * GSUM + PSID * GDIF)
        !
        !------ dPsi/dGam
        DZDG(JO) = DZDG(JO) + QOPI * (PSIS - PSID)
        DZDG(JP) = DZDG(JP) + QOPI * (PSIS + PSID)
        !
        !------ dPsi/dni
        PSNI = PSX1 * X1I + PSX2 * X2I + PSYY * YYI
        PDNI = PDX1 * X1I + PDX2 * X2I + PDYY * YYI
        PSI_NI = PSI_NI + QOPI * (GSUM * PSNI + GDIF * PDNI)
        !
        QTAN1 = QTAN1 + QOPI * (GSUM1 * PSNI + GDIF1 * PDNI)
        QTAN2 = QTAN2 + QOPI * (GSUM2 * PSNI + GDIF2 * PDNI)
        !
        DQDG(JO) = DQDG(JO) + QOPI * (PSNI - PDNI)
        DQDG(JP) = DQDG(JP) + QOPI * (PSNI + PDNI)
        !
        IF(GEOLIN) THEN
            !
            !------- dPsi/dn
            DZDN(JO) = DZDN(JO) + QOPI * GSUM * (PSX1 * X1O + PSX2 * X2O + PSYY * YYO)&
                    + QOPI * GDIF * (PDX1 * X1O + PDX2 * X2O + PDYY * YYO)
            DZDN(JP) = DZDN(JP) + QOPI * GSUM * (PSX1 * X1P + PSX2 * X2P + PSYY * YYP)&
                    + QOPI * GDIF * (PDX1 * X1P + PDX2 * X2P + PDYY * YYP)
            !------- dPsi/dP
            Z_QDOF0 = Z_QDOF0&
                    + QOPI * ((PSIS - PSID) * QF0(JO) + (PSIS + PSID) * QF0(JP))
            Z_QDOF1 = Z_QDOF1&
                    + QOPI * ((PSIS - PSID) * QF1(JO) + (PSIS + PSID) * QF1(JP))
            Z_QDOF2 = Z_QDOF2&
                    + QOPI * ((PSIS - PSID) * QF2(JO) + (PSIS + PSID) * QF2(JP))
            Z_QDOF3 = Z_QDOF3&
                    + QOPI * ((PSIS - PSID) * QF3(JO) + (PSIS + PSID) * QF3(JP))
        ENDIF
        !
        !
    10 CONTINUE
    end do
    !
    11 CONTINUE
    PSIG = 0.5 * YY * (G1 - G2) + X2 * (T2 - APAN) - X1 * (T1 - APAN)
    PGAM = 0.5 * X1 * G1 - 0.5 * X2 * G2 + X2 - X1 + YY * (T1 - T2)
    !
    PSIGX1 = -(T1 - APAN)
    PSIGX2 = T2 - APAN
    PSIGYY = 0.5 * (G1 - G2)
    PGAMX1 = 0.5 * G1
    PGAMX2 = -.5 * G2
    PGAMYY = T1 - T2
    !
    PSIGNI = PSIGX1 * X1I + PSIGX2 * X2I + PSIGYY * YYI
    PGAMNI = PGAMX1 * X1I + PGAMX2 * X2I + PGAMYY * YYI
    !
    !---- TE panel source and vortex strengths
    SIGTE1 = 0.5 * SCS * (GAMU(JP, 1) - GAMU(JO, 1))
    SIGTE2 = 0.5 * SCS * (GAMU(JP, 2) - GAMU(JO, 2))
    GAMTE1 = -.5 * SDS * (GAMU(JP, 1) - GAMU(JO, 1))
    GAMTE2 = -.5 * SDS * (GAMU(JP, 2) - GAMU(JO, 2))
    !
    SIGTE = 0.5 * SCS * (GAM(JP) - GAM(JO))
    GAMTE = -.5 * SDS * (GAM(JP) - GAM(JO))
    !
    !---- TE panel contribution to Psi
    PSI = PSI + HOPI * (PSIG * SIGTE + PGAM * GAMTE)
    !
    !---- dPsi/dGam
    DZDG(JO) = DZDG(JO) - HOPI * PSIG * SCS * 0.5
    DZDG(JP) = DZDG(JP) + HOPI * PSIG * SCS * 0.5
    !
    DZDG(JO) = DZDG(JO) + HOPI * PGAM * SDS * 0.5
    DZDG(JP) = DZDG(JP) - HOPI * PGAM * SDS * 0.5
    !
    !---- dPsi/dni
    PSI_NI = PSI_NI + HOPI * (PSIGNI * SIGTE + PGAMNI * GAMTE)
    !
    QTAN1 = QTAN1 + HOPI * (PSIGNI * SIGTE1 + PGAMNI * GAMTE1)
    QTAN2 = QTAN2 + HOPI * (PSIGNI * SIGTE2 + PGAMNI * GAMTE2)
    !
    DQDG(JO) = DQDG(JO) - HOPI * (PSIGNI * 0.5 * SCS - PGAMNI * 0.5 * SDS)
    DQDG(JP) = DQDG(JP) + HOPI * (PSIGNI * 0.5 * SCS - PGAMNI * 0.5 * SDS)
    !
    IF(GEOLIN) THEN
        !
        !----- dPsi/dn
        DZDN(JO) = DZDN(JO)&
                + HOPI * (PSIGX1 * X1O + PSIGX2 * X2O + PSIGYY * YYO) * SIGTE&
                + HOPI * (PGAMX1 * X1O + PGAMX2 * X2O + PGAMYY * YYO) * GAMTE
        DZDN(JP) = DZDN(JP)&
                + HOPI * (PSIGX1 * X1P + PSIGX2 * X2P + PSIGYY * YYP) * SIGTE&
                + HOPI * (PGAMX1 * X1P + PGAMX2 * X2P + PGAMYY * YYP) * GAMTE
        !
        !----- dPsi/dP
        Z_QDOF0 = Z_QDOF0 + HOPI * PSIG * 0.5 * (QF0(JP) - QF0(JO)) * SCS&
                - HOPI * PGAM * 0.5 * (QF0(JP) - QF0(JO)) * SDS
        Z_QDOF1 = Z_QDOF1 + HOPI * PSIG * 0.5 * (QF1(JP) - QF1(JO)) * SCS&
                - HOPI * PGAM * 0.5 * (QF1(JP) - QF1(JO)) * SDS
        Z_QDOF2 = Z_QDOF2 + HOPI * PSIG * 0.5 * (QF2(JP) - QF2(JO)) * SCS&
                - HOPI * PGAM * 0.5 * (QF2(JP) - QF2(JO)) * SDS
        Z_QDOF3 = Z_QDOF3 + HOPI * PSIG * 0.5 * (QF3(JP) - QF3(JO)) * SCS&
                - HOPI * PGAM * 0.5 * (QF3(JP) - QF3(JO)) * SDS
        !
    ENDIF
    !
    12 CONTINUE
    !
    !**** Freestream terms
    PSI = PSI + QINF * (COSA * YI - SINA * XI)
    !
    !---- dPsi/dn
    PSI_NI = PSI_NI + QINF * (COSA * NYI - SINA * NXI)
    !
    QTAN1 = QTAN1 + QINF * NYI
    QTAN2 = QTAN2 - QINF * NXI
    !
    !---- dPsi/dQinf
    Z_QINF = Z_QINF + (COSA * YI - SINA * XI)
    !
    !---- dPsi/dalfa
    Z_ALFA = Z_ALFA - QINF * (SINA * YI + COSA * XI)
    !
    IF(.NOT.LIMAGE) RETURN
    !
    !
    !
    DO JO = 1, N
        JP = JO + 1
        !
        JM = JO - 1
        JQ = JP + 1
        !
        IF(JO == 1) THEN
            JM = JO
        ELSE IF(JO == N - 1) THEN
            JQ = JP
        ELSE IF(JO == N) THEN
            JP = 1
            IF((X(JO) - X(JP))**2 + (Y(JO) - Y(JP))**2 < SEPS**2) GO TO 22
        ENDIF
        !
        DSO = SQRT((X(JO) - X(JP))**2 + (Y(JO) - Y(JP))**2)
        !
        !------ skip null panel
        IF(DSO == 0.0) GO TO 20
        !
        DSIO = 1.0 / DSO
        !
        !cc     APAN = APANEL(JO)
        APAN = PI - APANEL(JO) + 2.0 * ALFA
        !
        XJO = X(JO) + 2.0 * (YIMAGE + Y(JO)) * SINA
        YJO = Y(JO) - 2.0 * (YIMAGE + Y(JO)) * COSA
        XJP = X(JP) + 2.0 * (YIMAGE + Y(JP)) * SINA
        YJP = Y(JP) - 2.0 * (YIMAGE + Y(JP)) * COSA
        !
        RX1 = XI - XJO
        RY1 = YI - YJO
        RX2 = XI - XJP
        RY2 = YI - YJP
        !
        SX = (XJP - XJO) * DSIO
        SY = (YJP - YJO) * DSIO
        !
        X1 = SX * RX1 + SY * RY1
        X2 = SX * RX2 + SY * RY2
        YY = SX * RY1 - SY * RX1
        !
        RS1 = RX1 * RX1 + RY1 * RY1
        RS2 = RX2 * RX2 + RY2 * RY2
        !
        !------ set reflection flag SGN to avoid branch problems with arctan
        IF(IO >= 1 .AND. IO <= N) THEN
            !------- no problem on airfoil surface
            SGN = 1.0
        ELSE
            !------- make sure arctan falls between  -/+  Pi/2
            SGN = SIGN(1.0, YY)
        ENDIF
        !
        !------ set log(r^2) and arctan(x/y), correcting for reflection if any
        G1 = LOG(RS1)
        T1 = ATAN2(SGN * X1, SGN * YY) + (0.5 - 0.5 * SGN) * PI
        !
        G2 = LOG(RS2)
        T2 = ATAN2(SGN * X2, SGN * YY) + (0.5 - 0.5 * SGN) * PI
        !
        X1I = SX * NXI + SY * NYI
        X2I = SX * NXI + SY * NYI
        YYI = SX * NYI - SY * NXI
        !
        IF(GEOLIN) THEN
            NXO = NX(JO)
            NYO = NY(JO)
            NXP = NX(JP)
            NYP = NY(JP)
            !
            X1O = -((RX1 - X1 * SX) * NXO + (RY1 - X1 * SY) * NYO) * DSIO - (SX * NXO + SY * NYO)
            X1P = ((RX1 - X1 * SX) * NXP + (RY1 - X1 * SY) * NYP) * DSIO
            X2O = -((RX2 - X2 * SX) * NXO + (RY2 - X2 * SY) * NYO) * DSIO
            X2P = ((RX2 - X2 * SX) * NXP + (RY2 - X2 * SY) * NYP) * DSIO - (SX * NXP + SY * NYP)
            YYO = ((RX1 + X1 * SY) * NYO - (RY1 - X1 * SX) * NXO) * DSIO - (SX * NYO - SY * NXO)
            YYP = -((RX1 - X1 * SY) * NYP - (RY1 + X1 * SX) * NXP) * DSIO
        ENDIF
        !
        IF(JO == N) GO TO 21
        !
        IF(SIGLIN) THEN
            !
            !------- set up midpoint quantities
            X0 = 0.5 * (X1 + X2)
            RS0 = X0 * X0 + YY * YY
            G0 = LOG(RS0)
            T0 = ATAN2(SGN * X0, SGN * YY) + (0.5 - 0.5 * SGN) * PI
            !
            !------- calculate source contribution to Psi  for  1-0  half-panel
            DXINV = 1.0 / (X1 - X0)
            PSUM = X0 * (T0 - APAN) - X1 * (T1 - APAN) + 0.5 * YY * (G1 - G0)
            PDIF = ((X1 + X0) * PSUM + RS1 * (T1 - APAN) - RS0 * (T0 - APAN)&
                    + (X0 - X1) * YY) * DXINV
            !
            PSX1 = -(T1 - APAN)
            PSX0 = T0 - APAN
            PSYY = 0.5 * (G1 - G0)
            !
            PDX1 = ((X1 + X0) * PSX1 + PSUM + 2.0 * X1 * (T1 - APAN) - PDIF) * DXINV
            PDX0 = ((X1 + X0) * PSX0 + PSUM - 2.0 * X0 * (T0 - APAN) + PDIF) * DXINV
            PDYY = ((X1 + X0) * PSYY + 2.0 * (X0 - X1 + YY * (T1 - T0))) * DXINV
            !
            DSM = SQRT((X(JP) - X(JM))**2 + (Y(JP) - Y(JM))**2)
            DSIM = 1.0 / DSM
            !
            !CC      SIG0 = (SIG(JP) - SIG(JO))*DSIO
            !CC      SIG1 = (SIG(JP) - SIG(JM))*DSIM
            !CC      SSUM = SIG0 + SIG1
            !CC      SDIF = SIG0 - SIG1
            !
            SSUM = (SIG(JP) - SIG(JO)) * DSIO + (SIG(JP) - SIG(JM)) * DSIM
            SDIF = (SIG(JP) - SIG(JO)) * DSIO - (SIG(JP) - SIG(JM)) * DSIM
            !
            PSI = PSI + QOPI * (PSUM * SSUM + PDIF * SDIF)
            !
            !------- dPsi/dm
            DZDM(JM) = DZDM(JM) + QOPI * (-PSUM * DSIM + PDIF * DSIM)
            DZDM(JO) = DZDM(JO) + QOPI * (-PSUM * DSIO - PDIF * DSIO)
            DZDM(JP) = DZDM(JP) + QOPI * (PSUM * (DSIO + DSIM)&
                    + PDIF * (DSIO - DSIM))
            !
            !------- dPsi/dni
            PSNI = PSX1 * X1I + PSX0 * (X1I + X2I) * 0.5 + PSYY * YYI
            PDNI = PDX1 * X1I + PDX0 * (X1I + X2I) * 0.5 + PDYY * YYI
            PSI_NI = PSI_NI + QOPI * (PSNI * SSUM + PDNI * SDIF)
            !
            QTANM = QTANM + QOPI * (PSNI * SSUM + PDNI * SDIF)
            !
            DQDM(JM) = DQDM(JM) + QOPI * (-PSNI * DSIM + PDNI * DSIM)
            DQDM(JO) = DQDM(JO) + QOPI * (-PSNI * DSIO - PDNI * DSIO)
            DQDM(JP) = DQDM(JP) + QOPI * (PSNI * (DSIO + DSIM)&
                    + PDNI * (DSIO - DSIM))
            !
            !
            !------- calculate source contribution to Psi  for  0-2  half-panel
            DXINV = 1.0 / (X0 - X2)
            PSUM = X2 * (T2 - APAN) - X0 * (T0 - APAN) + 0.5 * YY * (G0 - G2)
            PDIF = ((X0 + X2) * PSUM + RS0 * (T0 - APAN) - RS2 * (T2 - APAN)&
                    + (X2 - X0) * YY) * DXINV
            !
            PSX0 = -(T0 - APAN)
            PSX2 = T2 - APAN
            PSYY = 0.5 * (G0 - G2)
            !
            PDX0 = ((X0 + X2) * PSX0 + PSUM + 2.0 * X0 * (T0 - APAN) - PDIF) * DXINV
            PDX2 = ((X0 + X2) * PSX2 + PSUM - 2.0 * X2 * (T2 - APAN) + PDIF) * DXINV
            PDYY = ((X0 + X2) * PSYY + 2.0 * (X2 - X0 + YY * (T0 - T2))) * DXINV
            !
            DSP = SQRT((X(JQ) - X(JO))**2 + (Y(JQ) - Y(JO))**2)
            DSIP = 1.0 / DSP
            !
            !CC         SIG2 = (SIG(JQ) - SIG(JO))*DSIP
            !CC         SIG0 = (SIG(JP) - SIG(JO))*DSIO
            !CC         SSUM = SIG2 + SIG0
            !CC         SDIF = SIG2 - SIG0
            !
            SSUM = (SIG(JQ) - SIG(JO)) * DSIP + (SIG(JP) - SIG(JO)) * DSIO
            SDIF = (SIG(JQ) - SIG(JO)) * DSIP - (SIG(JP) - SIG(JO)) * DSIO
            !
            PSI = PSI + QOPI * (PSUM * SSUM + PDIF * SDIF)
            !
            !------- dPsi/dm
            DZDM(JO) = DZDM(JO) + QOPI * (-PSUM * (DSIP + DSIO)&
                    - PDIF * (DSIP - DSIO))
            DZDM(JP) = DZDM(JP) + QOPI * (PSUM * DSIO - PDIF * DSIO)
            DZDM(JQ) = DZDM(JQ) + QOPI * (PSUM * DSIP + PDIF * DSIP)
            !
            !------- dPsi/dni
            PSNI = PSX0 * (X1I + X2I) * 0.5 + PSX2 * X2I + PSYY * YYI
            PDNI = PDX0 * (X1I + X2I) * 0.5 + PDX2 * X2I + PDYY * YYI
            PSI_NI = PSI_NI + QOPI * (PSNI * SSUM + PDNI * SDIF)
            !
            QTANM = QTANM + QOPI * (PSNI * SSUM + PDNI * SDIF)
            !
            DQDM(JO) = DQDM(JO) + QOPI * (-PSNI * (DSIP + DSIO)&
                    - PDNI * (DSIP - DSIO))
            DQDM(JP) = DQDM(JP) + QOPI * (PSNI * DSIO - PDNI * DSIO)
            DQDM(JQ) = DQDM(JQ) + QOPI * (PSNI * DSIP + PDNI * DSIP)
            !
        ENDIF
        !
        !------ calculate vortex panel contribution to Psi
        DXINV = 1.0 / (X1 - X2)
        PSIS = 0.5 * X1 * G1 - 0.5 * X2 * G2 + X2 - X1 + YY * (T1 - T2)
        PSID = ((X1 + X2) * PSIS + 0.5 * (RS2 * G2 - RS1 * G1 + X1 * X1 - X2 * X2)) * DXINV
        !
        PSX1 = 0.5 * G1
        PSX2 = -.5 * G2
        PSYY = T1 - T2
        !
        PDX1 = ((X1 + X2) * PSX1 + PSIS - X1 * G1 - PSID) * DXINV
        PDX2 = ((X1 + X2) * PSX2 + PSIS + X2 * G2 + PSID) * DXINV
        PDYY = ((X1 + X2) * PSYY - YY * (G1 - G2)) * DXINV
        !
        GSUM1 = GAMU(JP, 1) + GAMU(JO, 1)
        GSUM2 = GAMU(JP, 2) + GAMU(JO, 2)
        GDIF1 = GAMU(JP, 1) - GAMU(JO, 1)
        GDIF2 = GAMU(JP, 2) - GAMU(JO, 2)
        !
        GSUM = GAM(JP) + GAM(JO)
        GDIF = GAM(JP) - GAM(JO)
        !
        PSI = PSI - QOPI * (PSIS * GSUM + PSID * GDIF)
        !
        !------ dPsi/dGam
        DZDG(JO) = DZDG(JO) - QOPI * (PSIS - PSID)
        DZDG(JP) = DZDG(JP) - QOPI * (PSIS + PSID)
        !
        !------ dPsi/dni
        PSNI = PSX1 * X1I + PSX2 * X2I + PSYY * YYI
        PDNI = PDX1 * X1I + PDX2 * X2I + PDYY * YYI
        PSI_NI = PSI_NI - QOPI * (GSUM * PSNI + GDIF * PDNI)
        !
        QTAN1 = QTAN1 - QOPI * (GSUM1 * PSNI + GDIF1 * PDNI)
        QTAN2 = QTAN2 - QOPI * (GSUM2 * PSNI + GDIF2 * PDNI)
        !
        DQDG(JO) = DQDG(JO) - QOPI * (PSNI - PDNI)
        DQDG(JP) = DQDG(JP) - QOPI * (PSNI + PDNI)
        !
        IF(GEOLIN) THEN
            !
            !------- dPsi/dn
            DZDN(JO) = DZDN(JO) - QOPI * GSUM * (PSX1 * X1O + PSX2 * X2O + PSYY * YYO)&
                    - QOPI * GDIF * (PDX1 * X1O + PDX2 * X2O + PDYY * YYO)
            DZDN(JP) = DZDN(JP) - QOPI * GSUM * (PSX1 * X1P + PSX2 * X2P + PSYY * YYP)&
                    - QOPI * GDIF * (PDX1 * X1P + PDX2 * X2P + PDYY * YYP)
            !------- dPsi/dP
            Z_QDOF0 = Z_QDOF0&
                    - QOPI * ((PSIS - PSID) * QF0(JO) + (PSIS + PSID) * QF0(JP))
            Z_QDOF1 = Z_QDOF1&
                    - QOPI * ((PSIS - PSID) * QF1(JO) + (PSIS + PSID) * QF1(JP))
            Z_QDOF2 = Z_QDOF2&
                    - QOPI * ((PSIS - PSID) * QF2(JO) + (PSIS + PSID) * QF2(JP))
            Z_QDOF3 = Z_QDOF3&
                    - QOPI * ((PSIS - PSID) * QF3(JO) + (PSIS + PSID) * QF3(JP))
        ENDIF
        !
        !
    20 CONTINUE
    end do
    !
    21 CONTINUE
    PSIG = 0.5 * YY * (G1 - G2) + X2 * (T2 - APAN) - X1 * (T1 - APAN)
    PGAM = 0.5 * X1 * G1 - 0.5 * X2 * G2 + X2 - X1 + YY * (T1 - T2)
    !
    PSIGX1 = -(T1 - APAN)
    PSIGX2 = T2 - APAN
    PSIGYY = 0.5 * (G1 - G2)
    PGAMX1 = 0.5 * G1
    PGAMX2 = -.5 * G2
    PGAMYY = T1 - T2
    !
    PSIGNI = PSIGX1 * X1I + PSIGX2 * X2I + PSIGYY * YYI
    PGAMNI = PGAMX1 * X1I + PGAMX2 * X2I + PGAMYY * YYI
    !
    !---- TE panel source and vortex strengths
    SIGTE1 = 0.5 * SCS * (GAMU(JP, 1) - GAMU(JO, 1))
    SIGTE2 = 0.5 * SCS * (GAMU(JP, 2) - GAMU(JO, 2))
    GAMTE1 = -.5 * SDS * (GAMU(JP, 1) - GAMU(JO, 1))
    GAMTE2 = -.5 * SDS * (GAMU(JP, 2) - GAMU(JO, 2))
    !
    SIGTE = 0.5 * SCS * (GAM(JP) - GAM(JO))
    GAMTE = -.5 * SDS * (GAM(JP) - GAM(JO))
    !
    !---- TE panel contribution to Psi
    PSI = PSI + HOPI * (PSIG * SIGTE - PGAM * GAMTE)
    !
    !---- dPsi/dGam
    DZDG(JO) = DZDG(JO) - HOPI * PSIG * SCS * 0.5
    DZDG(JP) = DZDG(JP) + HOPI * PSIG * SCS * 0.5
    !
    DZDG(JO) = DZDG(JO) - HOPI * PGAM * SDS * 0.5
    DZDG(JP) = DZDG(JP) + HOPI * PGAM * SDS * 0.5
    !
    !---- dPsi/dni
    PSI_NI = PSI_NI + HOPI * (PSIGNI * SIGTE - PGAMNI * GAMTE)
    !
    QTAN1 = QTAN1 + HOPI * (PSIGNI * SIGTE1 - PGAMNI * GAMTE1)
    QTAN2 = QTAN2 + HOPI * (PSIGNI * SIGTE2 - PGAMNI * GAMTE2)
    !
    DQDG(JO) = DQDG(JO) - HOPI * (PSIGNI * 0.5 * SCS + PGAMNI * 0.5 * SDS)
    DQDG(JP) = DQDG(JP) + HOPI * (PSIGNI * 0.5 * SCS + PGAMNI * 0.5 * SDS)
    !
    IF(GEOLIN) THEN
        !
        !----- dPsi/dn
        DZDN(JO) = DZDN(JO)&
                + HOPI * (PSIGX1 * X1O + PSIGX2 * X2O + PSIGYY * YYO) * SIGTE&
                - HOPI * (PGAMX1 * X1O + PGAMX2 * X2O + PGAMYY * YYO) * GAMTE
        DZDN(JP) = DZDN(JP)&
                + HOPI * (PSIGX1 * X1P + PSIGX2 * X2P + PSIGYY * YYP) * SIGTE&
                - HOPI * (PGAMX1 * X1P + PGAMX2 * X2P + PGAMYY * YYP) * GAMTE
        !
        !----- dPsi/dP
        Z_QDOF0 = Z_QDOF0 + HOPI * PSIG * 0.5 * (QF0(JP) - QF0(JO)) * SCS&
                + HOPI * PGAM * 0.5 * (QF0(JP) - QF0(JO)) * SDS
        Z_QDOF1 = Z_QDOF1 + HOPI * PSIG * 0.5 * (QF1(JP) - QF1(JO)) * SCS&
                + HOPI * PGAM * 0.5 * (QF1(JP) - QF1(JO)) * SDS
        Z_QDOF2 = Z_QDOF2 + HOPI * PSIG * 0.5 * (QF2(JP) - QF2(JO)) * SCS&
                + HOPI * PGAM * 0.5 * (QF2(JP) - QF2(JO)) * SDS
        Z_QDOF3 = Z_QDOF3 + HOPI * PSIG * 0.5 * (QF3(JP) - QF3(JO)) * SCS&
                + HOPI * PGAM * 0.5 * (QF3(JP) - QF3(JO)) * SDS
        !
    ENDIF
    !
    22 CONTINUE
    !
    RETURN
END


SUBROUTINE PSWLIN(I, XI, YI, NXI, NYI, PSI, PSI_NI)
    !--------------------------------------------------------------------
    !     Calculates current streamfunction Psi and tangential velocity
    !     Qtan at panel node or wake node I due to freestream and wake
    !     sources Sig.  Also calculates sensitivity vectors dPsi/dSig
    !     (DZDM) and dQtan/dSig (DQDM).
    !
    !          Airfoil:  1   < I < N
    !          Wake:     N+1 < I < N+NW
    !--------------------------------------------------------------------
    INCLUDE 'XFOIL.INC'
    REAL NXI, NYI
    !
    IO = I
    !
    COSA = COS(ALFA)
    SINA = SIN(ALFA)
    !
    DO JO = N + 1, N + NW
        DZDM(JO) = 0.0
        DQDM(JO) = 0.0
    end do
    !
    PSI = 0.
    PSI_NI = 0.
    !
    DO JO = N + 1, N + NW - 1
        !
        JP = JO + 1
        !
        JM = JO - 1
        JQ = JP + 1
        IF(JO == N + 1) THEN
            JM = JO
        ELSE IF(JO == N + NW - 1) THEN
            JQ = JP
        ENDIF
        !
        DSO = SQRT((X(JO) - X(JP))**2 + (Y(JO) - Y(JP))**2)
        DSIO = 1.0 / DSO
        !
        APAN = APANEL(JO)
        !
        RX1 = XI - X(JO)
        RY1 = YI - Y(JO)
        RX2 = XI - X(JP)
        RY2 = YI - Y(JP)
        !
        SX = (X(JP) - X(JO)) * DSIO
        SY = (Y(JP) - Y(JO)) * DSIO
        !
        X1 = SX * RX1 + SY * RY1
        X2 = SX * RX2 + SY * RY2
        YY = SX * RY1 - SY * RX1
        !
        RS1 = RX1 * RX1 + RY1 * RY1
        RS2 = RX2 * RX2 + RY2 * RY2
        !
        IF(IO >= N + 1 .AND. IO <= N + NW) THEN
            SGN = 1.0
        ELSE
            SGN = SIGN(1.0, YY)
        ENDIF
        !
        IF(IO /= JO .AND. RS1 > 0.0) THEN
            G1 = LOG(RS1)
            T1 = ATAN2(SGN * X1, SGN * YY) - (0.5 - 0.5 * SGN) * PI
        ELSE
            G1 = 0.0
            T1 = 0.0
        ENDIF
        !
        IF(IO /= JP .AND. RS2 > 0.0) THEN
            G2 = LOG(RS2)
            T2 = ATAN2(SGN * X2, SGN * YY) - (0.5 - 0.5 * SGN) * PI
        ELSE
            G2 = 0.0
            T2 = 0.0
        ENDIF
        !
        X1I = SX * NXI + SY * NYI
        X2I = SX * NXI + SY * NYI
        YYI = SX * NYI - SY * NXI
        !
        !------- set up midpoint quantities
        X0 = 0.5 * (X1 + X2)
        RS0 = X0 * X0 + YY * YY
        G0 = LOG(RS0)
        T0 = ATAN2(SGN * X0, SGN * YY) - (0.5 - 0.5 * SGN) * PI
        !
        !------- calculate source contribution to Psi  for  1-0  half-panel
        DXINV = 1.0 / (X1 - X0)
        PSUM = X0 * (T0 - APAN) - X1 * (T1 - APAN) + 0.5 * YY * (G1 - G0)
        PDIF = ((X1 + X0) * PSUM + RS1 * (T1 - APAN) - RS0 * (T0 - APAN)&
                + (X0 - X1) * YY) * DXINV
        !
        PSX1 = -(T1 - APAN)
        PSX0 = T0 - APAN
        PSYY = 0.5 * (G1 - G0)
        !
        PDX1 = ((X1 + X0) * PSX1 + PSUM + 2.0 * X1 * (T1 - APAN) - PDIF) * DXINV
        PDX0 = ((X1 + X0) * PSX0 + PSUM - 2.0 * X0 * (T0 - APAN) + PDIF) * DXINV
        PDYY = ((X1 + X0) * PSYY + 2.0 * (X0 - X1 + YY * (T1 - T0))) * DXINV
        !
        DSM = SQRT((X(JP) - X(JM))**2 + (Y(JP) - Y(JM))**2)
        DSIM = 1.0 / DSM
        !
        !CC         SIG0 = (SIG(JP) - SIG(JO))*DSIO
        !CC         SIG1 = (SIG(JP) - SIG(JM))*DSIM
        !CC         SSUM = SIG0 + SIG1
        !CC         SDIF = SIG0 - SIG1
        !
        SSUM = (SIG(JP) - SIG(JO)) * DSIO + (SIG(JP) - SIG(JM)) * DSIM
        SDIF = (SIG(JP) - SIG(JO)) * DSIO - (SIG(JP) - SIG(JM)) * DSIM
        !
        PSI = PSI + QOPI * (PSUM * SSUM + PDIF * SDIF)
        !
        !------- dPsi/dm
        DZDM(JM) = DZDM(JM) + QOPI * (-PSUM * DSIM + PDIF * DSIM)
        DZDM(JO) = DZDM(JO) + QOPI * (-PSUM * DSIO - PDIF * DSIO)
        DZDM(JP) = DZDM(JP) + QOPI * (PSUM * (DSIO + DSIM)&
                + PDIF * (DSIO - DSIM))
        !
        !------- dPsi/dni
        PSNI = PSX1 * X1I + PSX0 * (X1I + X2I) * 0.5 + PSYY * YYI
        PDNI = PDX1 * X1I + PDX0 * (X1I + X2I) * 0.5 + PDYY * YYI
        PSI_NI = PSI_NI + QOPI * (PSNI * SSUM + PDNI * SDIF)
        !
        DQDM(JM) = DQDM(JM) + QOPI * (-PSNI * DSIM + PDNI * DSIM)
        DQDM(JO) = DQDM(JO) + QOPI * (-PSNI * DSIO - PDNI * DSIO)
        DQDM(JP) = DQDM(JP) + QOPI * (PSNI * (DSIO + DSIM)&
                + PDNI * (DSIO - DSIM))
        !
        !
        !------- calculate source contribution to Psi  for  0-2  half-panel
        DXINV = 1.0 / (X0 - X2)
        PSUM = X2 * (T2 - APAN) - X0 * (T0 - APAN) + 0.5 * YY * (G0 - G2)
        PDIF = ((X0 + X2) * PSUM + RS0 * (T0 - APAN) - RS2 * (T2 - APAN)&
                + (X2 - X0) * YY) * DXINV
        !
        PSX0 = -(T0 - APAN)
        PSX2 = T2 - APAN
        PSYY = 0.5 * (G0 - G2)
        !
        PDX0 = ((X0 + X2) * PSX0 + PSUM + 2.0 * X0 * (T0 - APAN) - PDIF) * DXINV
        PDX2 = ((X0 + X2) * PSX2 + PSUM - 2.0 * X2 * (T2 - APAN) + PDIF) * DXINV
        PDYY = ((X0 + X2) * PSYY + 2.0 * (X2 - X0 + YY * (T0 - T2))) * DXINV
        !
        DSP = SQRT((X(JQ) - X(JO))**2 + (Y(JQ) - Y(JO))**2)
        DSIP = 1.0 / DSP
        !
        !CC         SIG2 = (SIG(JQ) - SIG(JO))*DSIP
        !CC         SIG0 = (SIG(JP) - SIG(JO))*DSIO
        !CC         SSUM = SIG2 + SIG0
        !CC         SDIF = SIG2 - SIG0
        !
        SSUM = (SIG(JQ) - SIG(JO)) * DSIP + (SIG(JP) - SIG(JO)) * DSIO
        SDIF = (SIG(JQ) - SIG(JO)) * DSIP - (SIG(JP) - SIG(JO)) * DSIO
        !
        PSI = PSI + QOPI * (PSUM * SSUM + PDIF * SDIF)
        !
        !------- dPsi/dm
        DZDM(JO) = DZDM(JO) + QOPI * (-PSUM * (DSIP + DSIO)&
                - PDIF * (DSIP - DSIO))
        DZDM(JP) = DZDM(JP) + QOPI * (PSUM * DSIO - PDIF * DSIO)
        DZDM(JQ) = DZDM(JQ) + QOPI * (PSUM * DSIP + PDIF * DSIP)
        !
        !------- dPsi/dni
        PSNI = PSX0 * (X1I + X2I) * 0.5 + PSX2 * X2I + PSYY * YYI
        PDNI = PDX0 * (X1I + X2I) * 0.5 + PDX2 * X2I + PDYY * YYI
        PSI_NI = PSI_NI + QOPI * (PSNI * SSUM + PDNI * SDIF)
        !
        DQDM(JO) = DQDM(JO) + QOPI * (-PSNI * (DSIP + DSIO)&
                - PDNI * (DSIP - DSIO))
        DQDM(JP) = DQDM(JP) + QOPI * (PSNI * DSIO - PDNI * DSIO)
        DQDM(JQ) = DQDM(JQ) + QOPI * (PSNI * DSIP + PDNI * DSIP)
        !
    end do
    !
    RETURN
END


SUBROUTINE GGCALC
    !--------------------------------------------------------------
    !     Calculates two surface vorticity (gamma) distributions
    !     for alpha = 0, 90  degrees.  These are superimposed
    !     in SPECAL or SPECCL for specified alpha or CL.
    !--------------------------------------------------------------
    INCLUDE 'XFOIL.INC'
    !
    !---- distance of internal control point ahead of sharp TE
    !-    (fraction of smaller panel length adjacent to TE)
    BWT = 0.1
    !
    WRITE(*, *) 'Calculating unit vorticity distributions ...'
    !
    DO I = 1, N
        GAM(I) = 0.
        GAMU(I, 1) = 0.
        GAMU(I, 2) = 0.
    end do
    PSIO = 0.
    !
    !---- Set up matrix system for  Psi = Psio  on airfoil surface.
    !-    The unknowns are (dGamma)i and dPsio.
    DO I = 1, N
        !
        !------ calculate Psi and dPsi/dGamma array for current node
        CALL PSILIN(I, X(I), Y(I), NX(I), NY(I), PSI, PSI_N, .FALSE., .TRUE.)
        !
        PSIINF = QINF * (COS(ALFA) * Y(I) - SIN(ALFA) * X(I))
        !
        !------ RES1 = PSI( 0) - PSIO
        !------ RES2 = PSI(90) - PSIO
        RES1 = QINF * Y(I)
        RES2 = -QINF * X(I)
        !
        !------ dRes/dGamma
        DO J = 1, N
            AIJ(I, J) = DZDG(J)
        end do
        !
        DO J = 1, N
            BIJ(I, J) = -DZDM(J)
        end do
        !
        !------ dRes/dPsio
        AIJ(I, N + 1) = -1.0
        !
        GAMU(I, 1) = -RES1
        GAMU(I, 2) = -RES2
        !
    end do
    !
    !---- set Kutta condition
    !-    RES = GAM(1) + GAM(N)
    RES = 0.
    !
    DO J = 1, N + 1
        AIJ(N + 1, J) = 0.0
    end do
    !
    AIJ(N + 1, 1) = 1.0
    AIJ(N + 1, N) = 1.0
    !
    GAMU(N + 1, 1) = -RES
    GAMU(N + 1, 2) = -RES
    !
    !---- set up Kutta condition (no direct source influence)
    DO J = 1, N
        BIJ(N + 1, J) = 0.
    end do
    !
    IF(SHARP) THEN
        !----- set zero internal velocity in TE corner 
        !
        !----- set TE bisector angle
        AG1 = ATAN2(-YP(1), -XP(1))
        AG2 = ATANC(YP(N), XP(N), AG1)
        ABIS = 0.5 * (AG1 + AG2)
        CBIS = COS(ABIS)
        SBIS = SIN(ABIS)
        !
        !----- minimum panel length adjacent to TE
        DS1 = SQRT((X(1) - X(2))**2 + (Y(1) - Y(2))**2)
        DS2 = SQRT((X(N) - X(N - 1))**2 + (Y(N) - Y(N - 1))**2)
        DSMIN = MIN(DS1, DS2)
        !
        !----- control point on bisector just ahead of TE point
        XBIS = XTE - BWT * DSMIN * CBIS
        YBIS = YTE - BWT * DSMIN * SBIS
        !cc       write(*,*) xbis, ybis
        !
        !----- set velocity component along bisector line
        CALL PSILIN(0, XBIS, YBIS, -SBIS, CBIS, PSI, QBIS, .FALSE., .TRUE.)
        !
        !CC--- RES = DQDGj*Gammaj + DQDMj*Massj + QINF*(COSA*CBIS + SINA*SBIS)
        RES = QBIS
        !
        !----- dRes/dGamma
        DO J = 1, N
            AIJ(N, J) = DQDG(J)
        ENDDO
        !
        !----- -dRes/dMass
        DO J = 1, N
            BIJ(N, J) = -DQDM(J)
        ENDDO
        !
        !----- dRes/dPsio
        AIJ(N, N + 1) = 0.
        !
        !----- -dRes/dUinf
        GAMU(N, 1) = -CBIS
        !
        !----- -dRes/dVinf
        GAMU(N, 2) = -SBIS
        !
    ENDIF
    !
    !---- LU-factor coefficient matrix AIJ
    CALL LUDCMP(IQX, N + 1, AIJ, AIJPIV)
    LQAIJ = .TRUE.
    !
    !---- solve system for the two vorticity distributions
    CALL BAKSUB(IQX, N + 1, AIJ, AIJPIV, GAMU(1, 1))
    CALL BAKSUB(IQX, N + 1, AIJ, AIJPIV, GAMU(1, 2))
    !
    !---- set inviscid alpha=0,90 surface speeds for this geometry
    DO I = 1, N
        QINVU(I, 1) = GAMU(I, 1)
        QINVU(I, 2) = GAMU(I, 2)
    end do
    !
    LGAMU = .TRUE.
    !
    RETURN
END


SUBROUTINE QWCALC
    !---------------------------------------------------------------
    !     Sets inviscid tangential velocity for alpha = 0, 90
    !     on wake due to freestream and airfoil surface vorticity.
    !---------------------------------------------------------------
    INCLUDE 'XFOIL.INC'
    !
    !---- first wake point (same as TE)
    QINVU(N + 1, 1) = QINVU(N, 1)
    QINVU(N + 1, 2) = QINVU(N, 2)
    !
    !---- rest of wake
    DO I = N + 2, N + NW
        CALL PSILIN(I, X(I), Y(I), NX(I), NY(I), PSI, PSI_NI, .FALSE., .FALSE.)
        QINVU(I, 1) = QTAN1
        QINVU(I, 2) = QTAN2
    end do
    !
    RETURN
END


SUBROUTINE QDCALC
    !-----------------------------------------------------
    !     Calculates source panel influence coefficient
    !     matrix for current airfoil and wake geometry.
    !-----------------------------------------------------
    INCLUDE 'XFOIL.INC'
    !
    WRITE(*, *) 'Calculating source influence matrix ...'
    !
    IF(.NOT.LADIJ) THEN
        !
        !----- calculate source influence matrix for airfoil surface if it doesn't exist
        DO J = 1, N
            !
            !------- multiply each dPsi/Sig vector by inverse of factored dPsi/dGam matrix
            CALL BAKSUB(IQX, N + 1, AIJ, AIJPIV, BIJ(1, J))
            !
            !------- store resulting dGam/dSig = dQtan/dSig vector
            DO I = 1, N
                DIJ(I, J) = BIJ(I, J)
            end do
            !
        end do
        LADIJ = .TRUE.
        !
    ENDIF
    !
    !---- set up coefficient matrix of dPsi/dm on airfoil surface
    DO I = 1, N
        CALL PSWLIN(I, X(I), Y(I), NX(I), NY(I), PSI, PSI_N)
        DO J = N + 1, N + NW
            BIJ(I, J) = -DZDM(J)
        end do
    end do
    !
    !---- set up Kutta condition (no direct source influence)
    DO J = N + 1, N + NW
        BIJ(N + 1, J) = 0.
    end do
    !
    !---- sharp TE gamma extrapolation also has no source influence
    IF(SHARP) THEN
        DO J = N + 1, N + NW
            BIJ(N, J) = 0.
        end do
    ENDIF
    !
    !---- multiply by inverse of factored dPsi/dGam matrix
    DO J = N + 1, N + NW
        CALL BAKSUB(IQX, N + 1, AIJ, AIJPIV, BIJ(1, J))
    end do
    !
    !---- set the source influence matrix for the wake sources
    DO I = 1, N
        DO J = N + 1, N + NW
            DIJ(I, J) = BIJ(I, J)
        end do
    end do
    !
    !**** Now we need to calculate the influence of sources on the wake velocities
    !
    !---- calculcate dQtan/dGam and dQtan/dSig at the wake points
    DO I = N + 1, N + NW
        !
        IW = I - N
        !
        !------ airfoil contribution at wake panel node
        CALL PSILIN(I, X(I), Y(I), NX(I), NY(I), PSI, PSI_N, .FALSE., .TRUE.)
        !
        DO J = 1, N
            CIJ(IW, J) = DQDG(J)
        end do
        !  
        DO J = 1, N
            DIJ(I, J) = DQDM(J)
        end do
        !
        !------ wake contribution
        CALL PSWLIN(I, X(I), Y(I), NX(I), NY(I), PSI, PSI_N)
        !
        DO J = N + 1, N + NW
            DIJ(I, J) = DQDM(J)
        end do
        !
    end do
    !
    !---- add on effect of all sources on airfoil vorticity which effects wake Qtan
    DO I = N + 1, N + NW
        IW = I - N
        !
        !------ airfoil surface source contribution first
        DO J = 1, N
            SUM = 0.
            DO K = 1, N
                SUM = SUM + CIJ(IW, K) * DIJ(K, J)
            end do
            DIJ(I, J) = DIJ(I, J) + SUM
        end do
        !
        !------ wake source contribution next
        DO J = N + 1, N + NW
            SUM = 0.
            DO K = 1, N
                SUM = SUM + CIJ(IW, K) * BIJ(K, J)
            end do
            DIJ(I, J) = DIJ(I, J) + SUM
        end do
        !
    end do
    !
    !---- make sure first wake point has same velocity as trailing edge
    DO J = 1, N + NW
        DIJ(N + 1, J) = DIJ(N, J)
    end do
    !
    LWDIJ = .TRUE.
    !
    RETURN
END


SUBROUTINE XYWAKE
    !-----------------------------------------------------
    !     Sets wake coordinate array for current surface 
    !     vorticity and/or mass source distributions.
    !-----------------------------------------------------
    INCLUDE 'XFOIL.INC'
    !
    WRITE(*, *) 'Calculating wake trajectory ...'
    !
    !---- number of wake points
    NW = N / 12 + 10 * INT(WAKLEN)
    IF(NW > IWX) THEN
        WRITE(*, *)&
                'Array size (IWX) too small.  Last wake point index reduced.'
        NW = IWX
    ENDIF
    !
    DS1 = 0.5 * (S(2) - S(1) + S(N) - S(N - 1))
    CALL SETEXP(SNEW(N + 1), DS1, WAKLEN * CHORD, NW)
    !

    !      write(*,*) waklen, chord, waklen*chord
    !      write(*,*) ds1
    !      do i = n+1, n+nw
    !        write(*,*) i-n, snew(i)
    !      enddo

    XTE = 0.5 * (X(1) + X(N))
    YTE = 0.5 * (Y(1) + Y(N))
    !
    !---- set first wake point a tiny distance behind TE
    I = N + 1
    SX = 0.5 * (YP(N) - YP(1))
    SY = 0.5 * (XP(1) - XP(N))
    SMOD = SQRT(SX**2 + SY**2)
    NX(I) = SX / SMOD
    NY(I) = SY / SMOD
    X(I) = XTE - 0.0001 * NY(I)
    Y(I) = YTE + 0.0001 * NX(I)
    S(I) = S(N)
    !
    !---- calculate streamfunction gradient components at first point
    CALL PSILIN(I, X(I), Y(I), 1.0, 0.0, PSI, PSI_X, .FALSE., .FALSE.)
    CALL PSILIN(I, X(I), Y(I), 0.0, 1.0, PSI, PSI_Y, .FALSE., .FALSE.)
    !
    !---- set unit vector normal to wake at first point
    NX(I + 1) = -PSI_X / SQRT(PSI_X**2 + PSI_Y**2)
    NY(I + 1) = -PSI_Y / SQRT(PSI_X**2 + PSI_Y**2)
    !
    !---- set angle of wake panel normal
    APANEL(I) = ATAN2(PSI_Y, PSI_X)
    !
    !---- set rest of wake points
    DO I = N + 2, N + NW
        DS = SNEW(I) - SNEW(I - 1)
        !
        !------ set new point DS downstream of last point
        X(I) = X(I - 1) - DS * NY(I)
        Y(I) = Y(I - 1) + DS * NX(I)
        S(I) = S(I - 1) + DS
        !
        IF(I == N + NW) GO TO 10
        !
        !------- calculate normal vector for next point
        CALL PSILIN(I, X(I), Y(I), 1.0, 0.0, PSI, PSI_X, .FALSE., .FALSE.)
        CALL PSILIN(I, X(I), Y(I), 0.0, 1.0, PSI, PSI_Y, .FALSE., .FALSE.)
        !
        NX(I + 1) = -PSI_X / SQRT(PSI_X**2 + PSI_Y**2)
        NY(I + 1) = -PSI_Y / SQRT(PSI_X**2 + PSI_Y**2)
        !
        !------- set angle of wake panel normal
        APANEL(I) = ATAN2(PSI_Y, PSI_X)
        !
    10 CONTINUE
    end do
    !
    !---- set wake presence flag and corresponding alpha
    LWAKE = .TRUE.
    AWAKE = ALFA
    !
    !---- old source influence matrix is invalid for the new wake geometry
    LWDIJ = .FALSE.
    !
    RETURN
END


SUBROUTINE STFIND
    !-----------------------------------------
    !     Locates stagnation point arc length 
    !     location SST and panel index IST.
    !-----------------------------------------
    INCLUDE 'XFOIL.INC'
    !
    DO I = 1, N - 1
        IF(GAM(I) >= 0.0 .AND. GAM(I + 1) < 0.0) GO TO 11
    end do
    !
    WRITE(*, *) 'STFIND: Stagnation point not found. Continuing ...'
    I = N / 2
    !
    11 CONTINUE
    !
    IST = I
    DGAM = GAM(I + 1) - GAM(I)
    DS = S(I + 1) - S(I)
    !
    !---- evaluate so as to minimize roundoff for very small GAM(I) or GAM(I+1)
    IF(GAM(I) < -GAM(I + 1)) THEN
        SST = S(I) - DS * (GAM(I) / DGAM)
    ELSE
        SST = S(I + 1) - DS * (GAM(I + 1) / DGAM)
    ENDIF
    !
    !---- tweak stagnation point if it falls right on a node (very unlikely)
    IF(SST <= S(I)) SST = S(I) + 1.0E-7
    IF(SST >= S(I + 1)) SST = S(I + 1) - 1.0E-7
    !
    SST_GO = (SST - S(I + 1)) / DGAM
    SST_GP = (S(I) - SST) / DGAM
    !
    RETURN
END


SUBROUTINE IBLPAN
    !-------------------------------------------------------------
    !     Sets  BL location -> panel location  pointer array IPAN
    !-------------------------------------------------------------
    INCLUDE 'XFOIL.INC'
    !
    !---- top surface first
    IS = 1
    !
    IBL = 1
    DO I = IST, 1, -1
        IBL = IBL + 1
        IPAN(IBL, IS) = I
        VTI(IBL, IS) = 1.0
    end do
    !
    IBLTE(IS) = IBL
    NBL(IS) = IBL
    !
    !---- bottom surface next
    IS = 2
    !
    IBL = 1
    DO I = IST + 1, N
        IBL = IBL + 1
        IPAN(IBL, IS) = I
        VTI(IBL, IS) = -1.0
    end do
    !
    !---- wake
    IBLTE(IS) = IBL
    !
    DO IW = 1, NW
        I = N + IW
        IBL = IBLTE(IS) + IW
        IPAN(IBL, IS) = I
        VTI(IBL, IS) = -1.0
    end do
    !
    NBL(IS) = IBLTE(IS) + NW
    !
    !---- upper wake pointers (for plotting only)
    DO IW = 1, NW
        IPAN(IBLTE(1) + IW, 1) = IPAN(IBLTE(2) + IW, 2)
        VTI(IBLTE(1) + IW, 1) = 1.0
    end do
    !
    !
    IBLMAX = MAX(IBLTE(1), IBLTE(2)) + NW
    IF(IBLMAX > IVX) THEN
        WRITE(*, *) ' ***  BL array overflow.'
        WRITE(*, *) ' ***  Increase IVX to at least', IBLMAX
        STOP
    ENDIF
    !
    LIPAN = .TRUE.
    RETURN
END


SUBROUTINE XICALC
    !-------------------------------------------------------------
    !     Sets BL arc length array on each airfoil side and wake
    !-------------------------------------------------------------
    INCLUDE 'XFOIL.INC'
    DATA XFEPS / 1.0E-7 /
    !
    !---- minimum xi node arc length near stagnation point
    XEPS = XFEPS * (S(N) - S(1))
    !
    IS = 1
    !
    XSSI(1, IS) = 0.
    !
    DO IBL = 2, IBLTE(IS)
        I = IPAN(IBL, IS)
        XSSI(IBL, IS) = MAX(SST - S(I), XEPS)
    end do
    !
    !
    IS = 2
    !
    XSSI(1, IS) = 0.
    !
    DO IBL = 2, IBLTE(IS)
        I = IPAN(IBL, IS)
        XSSI(IBL, IS) = MAX(S(I) - SST, XEPS)
    end do
    !
    !
    IS1 = 1
    IS2 = 2
    !
    IBL1 = IBLTE(IS1) + 1
    XSSI(IBL1, IS1) = XSSI(IBL1 - 1, IS1)
    !
    IBL2 = IBLTE(IS2) + 1
    XSSI(IBL2, IS2) = XSSI(IBL2 - 1, IS2)
    !
    DO IBL = IBLTE(IS) + 2, NBL(IS)
        I = IPAN(IBL, IS)
        DXSSI = SQRT((X(I) - X(I - 1))**2 + (Y(I) - Y(I - 1))**2)
        !
        IBL1 = IBLTE(IS1) + IBL - IBLTE(IS)
        IBL2 = IBLTE(IS2) + IBL - IBLTE(IS)
        XSSI(IBL1, IS1) = XSSI(IBL1 - 1, IS1) + DXSSI
        XSSI(IBL2, IS2) = XSSI(IBL2 - 1, IS2) + DXSSI
    end do
    !
    !---- trailing edge flap length to TE gap ratio
    TELRAT = 2.50
    !
    !---- set up parameters for TE flap cubics
    !
    !cc   DWDXTE = YP(1)/XP(1) + YP(N)/XP(N)    !!! BUG  2/2/95
    !
    CROSP = (XP(1) * YP(N) - YP(1) * XP(N))&
            / SQRT((XP(1)**2 + YP(1)**2)&
                    * (XP(N)**2 + YP(N)**2))
    DWDXTE = CROSP / SQRT(1.0 - CROSP**2)
    !
    !---- limit cubic to avoid absurd TE gap widths
    DWDXTE = MAX(DWDXTE, -3.0 / TELRAT)
    DWDXTE = MIN(DWDXTE, 3.0 / TELRAT)
    !
    AA = 3.0 + TELRAT * DWDXTE
    BB = -2.0 - TELRAT * DWDXTE
    !
    IF(SHARP) THEN
        DO IW = 1, NW
            WGAP(IW) = 0.
        end do
    ELSE
        !----- set TE flap (wake gap) array
        IS = 2
        DO IW = 1, NW
            IBL = IBLTE(IS) + IW
            ZN = 1.0 - (XSSI(IBL, IS) - XSSI(IBLTE(IS), IS)) / (TELRAT * ANTE)
            WGAP(IW) = 0.
            IF(ZN >= 0.0) WGAP(IW) = ANTE * (AA + BB * ZN) * ZN**2
        end do
    ENDIF
    !
    RETURN
END


SUBROUTINE UICALC
    !--------------------------------------------------------------
    !     Sets inviscid Ue from panel inviscid tangential velocity
    !--------------------------------------------------------------
    INCLUDE 'XFOIL.INC'
    !
    DO IS = 1, 2
        UINV  (1, IS) = 0.
        UINV_A(1, IS) = 0.
        DO IBL = 2, NBL(IS)
            I = IPAN(IBL, IS)
            UINV  (IBL, IS) = VTI(IBL, IS) * QINV  (I)
            UINV_A(IBL, IS) = VTI(IBL, IS) * QINV_A(I)
        end do
    end do
    !
    RETURN
END


SUBROUTINE UECALC
    !--------------------------------------------------------------
    !     Sets viscous Ue from panel viscous tangential velocity
    !--------------------------------------------------------------
    INCLUDE 'XFOIL.INC'
    !
    DO IS = 1, 2
        UEDG(1, IS) = 0.
        DO IBL = 2, NBL(IS)
            I = IPAN(IBL, IS)
            UEDG(IBL, IS) = VTI(IBL, IS) * QVIS(I)
        end do
    end do
    !
    RETURN
END


SUBROUTINE QVFUE
    !--------------------------------------------------------------
    !     Sets panel viscous tangential velocity from viscous Ue
    !--------------------------------------------------------------
    INCLUDE 'XFOIL.INC'
    !
    DO IS = 1, 2
        DO IBL = 2, NBL(IS)
            I = IPAN(IBL, IS)
            QVIS(I) = VTI(IBL, IS) * UEDG(IBL, IS)
        end do
    end do
    !
    RETURN
END


SUBROUTINE QISET
    !-------------------------------------------------------
    !     Sets inviscid panel tangential velocity for
    !     current alpha.
    !-------------------------------------------------------
    INCLUDE 'XFOIL.INC'
    !
    COSA = COS(ALFA)
    SINA = SIN(ALFA)
    !
    DO I = 1, N + NW
        QINV  (I) = COSA * QINVU(I, 1) + SINA * QINVU(I, 2)
        QINV_A(I) = -SINA * QINVU(I, 1) + COSA * QINVU(I, 2)
    end do
    !
    RETURN
END


SUBROUTINE GAMQV
    INCLUDE 'XFOIL.INC'
    !
    DO I = 1, N
        GAM(I) = QVIS(I)
        GAM_A(I) = QINV_A(I)
    end do
    !
    RETURN
END


SUBROUTINE STMOVE
    !---------------------------------------------------
    !     Moves stagnation point location to new panel.
    !---------------------------------------------------
    INCLUDE 'XFOIL.INC'
    !
    !---- locate new stagnation point arc length SST from GAM distribution
    ISTOLD = IST
    CALL STFIND
    !
    IF(ISTOLD == IST) THEN
        !
        !----- recalculate new arc length array
        CALL XICALC
        !
    ELSE
        !
        !CC       WRITE(*,*) 'STMOVE: Resetting stagnation point'
        !
        !----- set new BL position -> panel position  pointers
        CALL IBLPAN
        !
        !----- set new inviscid BL edge velocity UINV from QINV
        CALL UICALC
        !
        !----- recalculate new arc length array
        CALL XICALC
        !
        !----- set  BL position -> system line  pointers
        CALL IBLSYS
        !
        IF(IST > ISTOLD) THEN
            !------ increase in number of points on top side (IS=1)
            IDIF = IST - ISTOLD
            !
            ITRAN(1) = ITRAN(1) + IDIF
            ITRAN(2) = ITRAN(2) - IDIF
            !
            !------ move top side BL variables downstream
            DO IBL = NBL(1), IDIF + 2, -1
                CTAU(IBL, 1) = CTAU(IBL - IDIF, 1)
                THET(IBL, 1) = THET(IBL - IDIF, 1)
                DSTR(IBL, 1) = DSTR(IBL - IDIF, 1)
                UEDG(IBL, 1) = UEDG(IBL - IDIF, 1)
            end do
            !
            !------ set BL variables between old and new stagnation point
            DUDX = UEDG(IDIF + 2, 1) / XSSI(IDIF + 2, 1)
            DO IBL = IDIF + 1, 2, -1
                CTAU(IBL, 1) = CTAU(IDIF + 2, 1)
                THET(IBL, 1) = THET(IDIF + 2, 1)
                DSTR(IBL, 1) = DSTR(IDIF + 2, 1)
                UEDG(IBL, 1) = DUDX * XSSI(IBL, 1)
            end do
            !
            !------ move bottom side BL variables upstream
            DO IBL = 2, NBL(2)
                CTAU(IBL, 2) = CTAU(IBL + IDIF, 2)
                THET(IBL, 2) = THET(IBL + IDIF, 2)
                DSTR(IBL, 2) = DSTR(IBL + IDIF, 2)
                UEDG(IBL, 2) = UEDG(IBL + IDIF, 2)
            end do
            !
        ELSE
            !------ increase in number of points on bottom side (IS=2)
            IDIF = ISTOLD - IST
            !
            ITRAN(1) = ITRAN(1) - IDIF
            ITRAN(2) = ITRAN(2) + IDIF
            !
            !------ move bottom side BL variables downstream
            DO IBL = NBL(2), IDIF + 2, -1
                CTAU(IBL, 2) = CTAU(IBL - IDIF, 2)
                THET(IBL, 2) = THET(IBL - IDIF, 2)
                DSTR(IBL, 2) = DSTR(IBL - IDIF, 2)
                UEDG(IBL, 2) = UEDG(IBL - IDIF, 2)
            end do
            !
            !------ set BL variables between old and new stagnation point
            DUDX = UEDG(IDIF + 2, 2) / XSSI(IDIF + 2, 2)


            !        write(*,*) 'idif Ue xi dudx', 
            !     &    idif, UEDG(idif+2,2), xssi(idif+2,2), dudx

            DO IBL = IDIF + 1, 2, -1
                CTAU(IBL, 2) = CTAU(IDIF + 2, 2)
                THET(IBL, 2) = THET(IDIF + 2, 2)
                DSTR(IBL, 2) = DSTR(IDIF + 2, 2)
                UEDG(IBL, 2) = DUDX * XSSI(IBL, 2)
            end do

            !        write(*,*) 'Uenew xinew', idif+1, uedg(idif+1,2), xssi(idif+1,2)

            !
            !------ move top side BL variables upstream
            DO IBL = 2, NBL(1)
                CTAU(IBL, 1) = CTAU(IBL + IDIF, 1)
                THET(IBL, 1) = THET(IBL + IDIF, 1)
                DSTR(IBL, 1) = DSTR(IBL + IDIF, 1)
                UEDG(IBL, 1) = UEDG(IBL + IDIF, 1)
            end do
        ENDIF
        !
        !----- tweak Ue so it's not zero, in case stag. point is right on node
        UEPS = 1.0E-7
        DO IS = 1, 2
            DO IBL = 2, NBL(IS)
                I = IPAN(IBL, IS)
                IF(UEDG(IBL, IS) <= UEPS) THEN
                    UEDG(IBL, IS) = UEPS
                    QVIS(I) = VTI(IBL, IS) * UEPS
                    GAM(I) = VTI(IBL, IS) * UEPS
                ENDIF
            ENDDO
        ENDDO
        !
    ENDIF
    !
    !---- set new mass array since Ue has been tweaked
    DO IS = 1, 2
        DO IBL = 2, NBL(IS)
            MASS(IBL, IS) = DSTR(IBL, IS) * UEDG(IBL, IS)
        end do
    end do
    !
    RETURN
END


SUBROUTINE UESET
    !---------------------------------------------------------
    !     Sets Ue from inviscid Ue plus all source influence
    !---------------------------------------------------------
    INCLUDE 'XFOIL.INC'
    !
    DO IS = 1, 2
        DO IBL = 2, NBL(IS)
            I = IPAN(IBL, IS)
            !
            DUI = 0.
            DO JS = 1, 2
                DO JBL = 2, NBL(JS)
                    J = IPAN(JBL, JS)
                    UE_M = -VTI(IBL, IS) * VTI(JBL, JS) * DIJ(I, J)
                    DUI = DUI + UE_M * MASS(JBL, JS)
                end do
            end do
            !
            UEDG(IBL, IS) = UINV(IBL, IS) + DUI
            !
        end do
    end do
    !
    RETURN
END


SUBROUTINE DSSET
    INCLUDE 'XFOIL.INC'
    !
    DO IS = 1, 2
        DO IBL = 2, NBL(IS)
            DSTR(IBL, IS) = MASS(IBL, IS) / UEDG(IBL, IS)
        end do
    end do
    !
    RETURN
END