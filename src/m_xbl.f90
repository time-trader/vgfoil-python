!***********************************************************************
!   Copyright (c) 2019 D. de Vries
!   Original Copyright (c) 2000 Mark Drela
!
!   This file is part of XFoil.
!
!   XFoil is free software: you can redistribute it and/or modify
!   it under the terms of the GNU General Public License as published by
!   the Free Software Foundation, either version 3 of the License, or
!   (at your option) any later version.
!
!   XFoil is distributed in the hope that it will be useful,
!   but WITHOUT ANY WARRANTY; without even the implied warranty of
!   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!   GNU General Public License for more details.
!
!   You should have received a copy of the GNU General Public License
!   along with XFoil.  If not, see <https://www.gnu.org/licenses/>.
!***********************************************************************
!   DISCLAIMER 2018.10.24   
!    Author: Elia Daniele - Fraunhofer IWES,
!    Added subroutine:
!       statusvg
!    See further modification by the name DanEli
!  DISCLAIMER 2020:
!    Global Convergent Method, has not been moved to this library.

!*==PREPTRS.f90  processed by SPAG 7.21DC at 11:25 on 11 Jan 2019
! GETARG0
module m_xbl
contains
    subroutine preptrs
        use i_xbl
        implicit none
        !
        !*** Start of declarations rewritten by SPAG
        !
        !*** End of declarations rewritten by SPAG
        !
        !
        !*** Start of declarations rewritten by SPAG
        !
        !*** End of declarations rewritten by SPAG
        !
        X1 => COM1(1)
        U1 => COM1(2)
        T1 => COM1(3)
        D1 => COM1(4)
        S1 => COM1(5)
        AMPl1 => COM1(6)
        U1_uei => COM1(7)
        U1_ms => COM1(8)
        DW1 => COM1(9)
        H1 => COM1(10)
        H1_t1 => COM1(11)
        H1_d1 => COM1(12)
        M1 => COM1(13)
        M1_u1 => COM1(14)
        M1_ms => COM1(15)
        R1 => COM1(16)
        R1_u1 => COM1(17)
        R1_ms => COM1(18)
        V1 => COM1(19)
        V1_u1 => COM1(20)
        V1_ms => COM1(21)
        V1_re => COM1(22)
        HK1 => COM1(23)
        HK1_u1 => COM1(24)
        HK1_t1 => COM1(25)
        HK1_d1 => COM1(26)
        HK1_ms => COM1(27)
        HS1 => COM1(28)
        HS1_u1 => COM1(29)
        HS1_t1 => COM1(30)
        HS1_d1 => COM1(31)
        HS1_ms => COM1(32)
        HS1_re => COM1(33)
        HC1 => COM1(34)
        HC1_u1 => COM1(35)
        HC1_t1 => COM1(36)
        HC1_d1 => COM1(37)
        HC1_ms => COM1(38)
        RT1 => COM1(39)
        RT1_u1 => COM1(40)
        RT1_t1 => COM1(41)
        RT1_ms => COM1(42)
        RT1_re => COM1(43)
        CF1 => COM1(44)
        CF1_u1 => COM1(45)
        CF1_t1 => COM1(46)
        CF1_d1 => COM1(47)
        CF1_ms => COM1(48)
        CF1_re => COM1(49)
        DI1 => COM1(50)
        DI1_u1 => COM1(51)
        DI1_t1 => COM1(52)
        DI1_d1 => COM1(53)
        DI1_s1 => COM1(54)
        DI1_ms => COM1(55)
        DI1_re => COM1(56)
        US1 => COM1(57)
        US1_u1 => COM1(58)
        US1_t1 => COM1(59)
        US1_d1 => COM1(60)
        US1_ms => COM1(61)
        US1_re => COM1(62)
        CQ1 => COM1(63)
        CQ1_u1 => COM1(64)
        CQ1_t1 => COM1(65)
        CQ1_d1 => COM1(66)
        CQ1_ms => COM1(67)
        CQ1_re => COM1(68)
        DE1 => COM1(69)
        DE1_u1 => COM1(70)
        DE1_t1 => COM1(71)
        DE1_d1 => COM1(72)
        DE1_ms => COM1(73)

        X2 => COM2(1)
        U2 => COM2(2)
        T2 => COM2(3)
        D2 => COM2(4)
        S2 => COM2(5)
        AMPl2 => COM2(6)
        U2_uei => COM2(7)
        U2_ms => COM2(8)
        DW2 => COM2(9)
        H2 => COM2(10)
        H2_t2 => COM2(11)
        H2_d2 => COM2(12)
        M2 => COM2(13)
        M2_u2 => COM2(14)
        M2_ms => COM2(15)
        R2 => COM2(16)
        R2_u2 => COM2(17)
        R2_ms => COM2(18)
        V2 => COM2(19)
        V2_u2 => COM2(20)
        V2_ms => COM2(21)
        V2_re => COM2(22)
        HK2 => COM2(23)
        HK2_u2 => COM2(24)
        HK2_t2 => COM2(25)
        HK2_d2 => COM2(26)
        HK2_ms => COM2(27)
        HS2 => COM2(28)
        HS2_u2 => COM2(29)
        HS2_t2 => COM2(30)
        HS2_d2 => COM2(31)
        HS2_ms => COM2(32)
        HS2_re => COM2(33)
        HC2 => COM2(34)
        HC2_u2 => COM2(35)
        HC2_t2 => COM2(36)
        HC2_d2 => COM2(37)
        HC2_ms => COM2(38)
        RT2 => COM2(39)
        RT2_u2 => COM2(40)
        RT2_t2 => COM2(41)
        RT2_ms => COM2(42)
        RT2_re => COM2(43)
        CF2 => COM2(44)
        CF2_u2 => COM2(45)
        CF2_t2 => COM2(46)
        CF2_d2 => COM2(47)
        CF2_ms => COM2(48)
        CF2_re => COM2(49)
        DI2 => COM2(50)
        DI2_u2 => COM2(51)
        DI2_t2 => COM2(52)
        DI2_d2 => COM2(53)
        DI2_s2 => COM2(54)
        DI2_ms => COM2(55)
        DI2_re => COM2(56)
        US2 => COM2(57)
        US2_u2 => COM2(58)
        US2_t2 => COM2(59)
        US2_d2 => COM2(60)
        US2_ms => COM2(61)
        US2_re => COM2(62)
        CQ2 => COM2(63)
        CQ2_u2 => COM2(64)
        CQ2_t2 => COM2(65)
        CQ2_d2 => COM2(66)
        CQ2_ms => COM2(67)
        CQ2_re => COM2(68)
        DE2 => COM2(69)
        DE2_u2 => COM2(70)
        DE2_t2 => COM2(71)
        DE2_d2 => COM2(72)
        DE2_ms => COM2(73)
    end subroutine preptrs
    !*==SETBL.f90  processed by SPAG 7.21DC at 11:25 on 11 Jan 2019


    function setbl()
        use m_xpanel, only: ueset
        use m_xblsys, only: tesys, trchek, blprv, blmid, blkin, blsys, blvar
        use m_spline, only: seval
        use i_xfoil
        use i_xbl
        use s_xfoil, only: mrcl, comset
        implicit none

        logical :: setbl, gcm
        !
        !*** Start of declarations rewritten by SPAG
        !
        ! Local variables
        !
        !DanEli:IVX - number of nodes along BL on one side of airfoil and wake
        !
        real :: ami, chsq, chx, chy, clmr, cte, cte_cte1, cte_cte2, cte_tte1, cte_tte2, cti, d1_a, d2_a, d2_m2, &
                & d2_u2, dds1, dds2, dsi, dswaki, dte, dte_mte1, dte_mte2, dte_ute1, dte_ute2, due1, due2, dule1, &
                & dule2, herat, herat_ms, ma_clmr, mdi, msq_clmr, re_clmr, str, temp, thi, tte, tte_tte1, tte_tte2, &
                & u1_a, u2_a, uei, ule1_a, ule2_a, xi_ule1, xi_ule2, xsi, xtr, ytr
        real, dimension(2 * IVX) :: d1_m, d2_m, u1_m, u2_m, ule1_m, ule2_m, ute1_m, ute2_m
        integer :: i, ibl, icom, ile1, ile2, is, ite1, ite2, iv, iw, j, jbl, js, jv, jvte1, jvte2
        real, dimension(IVX, 2) :: usav
        !
        !*** End of declarations rewritten by SPAG
        !
        !
        !*** Start of declarations rewritten by SPAG
        !
        ! Local variables
        !
        !
        !*** End of declarations rewritten by SPAG

        setbl = .true.
        !DanEli
        gcm = .false. !Enable Global Convergent Method
        !
        !-------------------------------------------------
        !     Sets up the BL Newton system coefficients
        !     for the current BL variables and the edge
        !     velocities received from SETUP. The local
        !     BL system coefficients are then
        !     incorporated into the global Newton system.
        !-------------------------------------------------
        !
        !---- set the CL used to define Mach, Reynolds numbers
        if (LALfa) then
            clmr = CL
        else
            clmr = CLSpec
        endif
        !
        !---- set current MINF(CL)
        call mrcl(clmr, ma_clmr, re_clmr)
        msq_clmr = 2.0 * MINf * ma_clmr
        !
        !---- set compressibility parameter TKLAM and derivative TK_MSQ
        call comset
        !
        !---- set gas constant (= Cp/Cv)
        GAMbl = GAMma
        GM1bl = GAMm1
        !
        !---- set parameters for compressibility correction
        QINfbl = QINf
        TKBl = TKLam
        TKBl_ms = TKL_msq
        !
        !---- stagnation density and 1/enthalpy
        RSTbl = (1.0 + 0.5 * GM1bl * MINf**2)**(1.0 / GM1bl)
        RSTbl_ms = 0.5 * RSTbl / (1.0 + 0.5 * GM1bl * MINf**2)
        !
        HSTinv = GM1bl * (MINf / QINfbl)**2 / (1.0 + 0.5 * GM1bl * MINf**2)
        HSTinv_ms = GM1bl * (1.0 / QINfbl)**2 / (1.0 + 0.5 * GM1bl * MINf**2) &
                - 0.5 * GM1bl * HSTinv / (1.0 + 0.5 * GM1bl * MINf**2)
        !
        !---- set Reynolds number based on freestream density, velocity, viscosity
        herat = 1.0 - 0.5 * QINfbl**2 * HSTinv
        herat_ms = -0.5 * QINfbl**2 * HSTinv_ms
        !
        REYbl = REInf * sqrt(herat**3) * (1.0 + HVRat) / (herat + HVRat)
        REYbl_re = sqrt(herat**3) * (1.0 + HVRat) / (herat + HVRat)
        REYbl_ms = REYbl * (1.5 / herat - 1.0 / (herat + HVRat)) * herat_ms
        !
        IDAmpv = IDAmp
        !
        !---- save TE thickness
        DWTe = WGAp(1)
        !
        if (.not.LBLini) then
            !----- initialize BL by marching with Ue (fudge at separation)
            if (show_output) then
                write (*, *)
                write (*, *) 'Initializing BL ...'
            endif
            !DanEli
            if (gcm) then
                setbl = mrchuegcm()
            else
                setbl = mrchue()
            endif

            if (setbl) then
                LBLini = .true.
            elseif (abort_on_nan) then
                return
            end if
        endif
        !
        if (show_output) write (*, *)
        !
        !---- march BL with current Ue and Ds to establish transition
        setbl = mrchdu()
        if (abort_on_nan .and. .not. setbl) return
        !
        do is = 1, 2
            do ibl = 2, NBL(is)
                usav(ibl, is) = UEDg(ibl, is)
            enddo
        enddo
        !
        call ueset
        !
        do is = 1, 2
            do ibl = 2, NBL(is)
                temp = usav(ibl, is)
                usav(ibl, is) = UEDg(ibl, is)
                UEDg(ibl, is) = temp
            enddo
        enddo
        !
        ile1 = IPAn(2, 1)
        ile2 = IPAn(2, 2)
        ite1 = IPAn(IBLte(1), 1)
        ite2 = IPAn(IBLte(2), 2)
        !
        jvte1 = ISYs(IBLte(1), 1)
        jvte2 = ISYs(IBLte(2), 2)
        !
        dule1 = UEDg(2, 1) - usav(2, 1)
        dule2 = UEDg(2, 2) - usav(2, 2)
        !
        !---- set LE and TE Ue sensitivities wrt all m values
        do js = 1, 2
            do jbl = 2, NBL(js)
                j = IPAn(jbl, js)
                jv = ISYs(jbl, js)
                ule1_m(jv) = -VTI(2, 1) * VTI(jbl, js) * DIJ(ile1, j)
                ule2_m(jv) = -VTI(2, 2) * VTI(jbl, js) * DIJ(ile2, j)
                ute1_m(jv) = -VTI(IBLte(1), 1) * VTI(jbl, js) * DIJ(ite1, j)
                ute2_m(jv) = -VTI(IBLte(2), 2) * VTI(jbl, js) * DIJ(ite2, j)
            enddo
        enddo
        !
        ule1_a = UINv_a(2, 1)
        ule2_a = UINv_a(2, 2)
        !
        TINdex(1) = 0.0
        TINdex(2) = 0.0
        !
        !**** Go over each boundary layer/wake
        do is = 1, 2
            !
            !DanEli
            !---- set logical variables for detecting the current airfoil side
            if (is==1) then
                listop = .true.
                lisbot = .false.
            else
                listop = .false.
                lisbot = .true.
            endif
            !      WRITE(*,*)' MRCHDU: current side is ',IS,', ',
            !     & ' LISTOP=',LISTOP,' LISBOT=',LISBOT 
            !
            !---- there is no station "1" at similarity, so zero everything out
            do js = 1, 2
                do jbl = 2, NBL(js)
                    jv = ISYs(jbl, js)
                    u1_m(jv) = 0.
                    d1_m(jv) = 0.
                enddo
            enddo
            u1_a = 0.
            d1_a = 0.
            !
            due1 = 0.
            dds1 = 0.
            !
            !---- similarity station pressure gradient parameter  x/u du/dx
            ibl = 2
            BULe = 1.0
            !
            AMCrit = ACRit(is)
            !
            !---- set forced transition arc length position
            call xifset(is)
            !
            TRAn = .false.
            TURb = .false.
            !
            !**** Sweep downstream setting up BL equation linearizations
            do ibl = 2, NBL(is)

                !DanEli
                !---- set up a common variable for the panel node IBL to be used to 
                !     read stored in memory variables. The name is COMIBL, in XFOIL.INC
                comibl = ibl
                !
                iv = ISYs(ibl, is)
                !
                SIMi = ibl==2
                WAKe = ibl>IBLte(is)
                TRAn = ibl==ITRan(is)
                TURb = ibl>ITRan(is)
                !
                i = IPAn(ibl, is)
                !
                !---- set primary variables for current station
                xsi = XSSi(ibl, is)
                if (ibl<ITRan(is)) ami = CTAu(ibl, is)
                if (ibl>=ITRan(is)) cti = CTAu(ibl, is)
                uei = UEDg(ibl, is)
                thi = THEt(ibl, is)
                mdi = MASs(ibl, is)
                !
                dsi = mdi / uei
                !
                if (WAKe) then
                    iw = ibl - IBLte(is)
                    dswaki = WGAp(iw)
                else
                    dswaki = 0.
                endif
                !
                !---- set derivatives of DSI (= D2)
                d2_m2 = 1.0 / uei
                d2_u2 = -dsi / uei
                !
                do js = 1, 2
                    do jbl = 2, NBL(js)
                        j = IPAn(jbl, js)
                        jv = ISYs(jbl, js)
                        u2_m(jv) = -VTI(ibl, is) * VTI(jbl, js) * DIJ(i, j)
                        d2_m(jv) = d2_u2 * u2_m(jv)
                    enddo
                enddo
                d2_m(iv) = d2_m(iv) + d2_m2
                !
                u2_a = UINv_a(ibl, is)
                d2_a = d2_u2 * u2_a
                !
                !---- "forced" changes due to mismatch between UEDG and USAV=UINV+dij*MASS
                due2 = UEDg(ibl, is) - usav(ibl, is)
                dds2 = d2_u2 * due2
                !
                call blprv(xsi, ami, cti, thi, dsi, dswaki, uei)
                call blkin
                !
                !---- check for transition and set TRAN, XT, etc. if found
                if (TRAn) then
                    setbl = trchek()
                    if (abort_on_nan .and. .not. setbl) return
                    ami = AMPl2
                endif
                if (ibl==ITRan(is) .and. .not.TRAn .and. show_output) write (*, *) 'SETBL: Xtr???  n1 n2: ', AMPl1, AMPl2
                !
                !---- assemble 10x4 linearized system for dCtau, dTh, dDs, dUe, dXi
                !     at the previous "1" station and the current "2" station
                !
                if (ibl==IBLte(is) + 1) then
                    !
                    !----- define quantities at start of wake, adding TE base thickness to Dstar
                    tte = THEt(IBLte(1), 1) + THEt(IBLte(2), 2)
                    dte = DSTr(IBLte(1), 1) + DSTr(IBLte(2), 2) + ANTe
                    cte = (CTAu(IBLte(1), 1) * THEt(IBLte(1), 1) + CTAu(IBLte(2), 2) * THEt(IBLte(2), 2)) / tte
                    call tesys(cte, tte, dte)
                    !
                    tte_tte1 = 1.0
                    tte_tte2 = 1.0
                    dte_mte1 = 1.0 / UEDg(IBLte(1), 1)
                    dte_ute1 = -DSTr(IBLte(1), 1) / UEDg(IBLte(1), 1)
                    dte_mte2 = 1.0 / UEDg(IBLte(2), 2)
                    dte_ute2 = -DSTr(IBLte(2), 2) / UEDg(IBLte(2), 2)
                    cte_cte1 = THEt(IBLte(1), 1) / tte
                    cte_cte2 = THEt(IBLte(2), 2) / tte
                    cte_tte1 = (CTAu(IBLte(1), 1) - cte) / tte
                    cte_tte2 = (CTAu(IBLte(2), 2) - cte) / tte
                    !
                    !----- re-define D1 sensitivities wrt m since D1 depends on both TE Ds values
                    do js = 1, 2
                        do jbl = 2, NBL(js)
                            j = IPAn(jbl, js)
                            jv = ISYs(jbl, js)
                            d1_m(jv) = dte_ute1 * ute1_m(jv) + dte_ute2 * ute2_m(jv)
                        enddo
                    enddo
                    d1_m(jvte1) = d1_m(jvte1) + dte_mte1
                    d1_m(jvte2) = d1_m(jvte2) + dte_mte2
                    !
                    !----- "forced" changes from  UEDG --- USAV=UINV+dij*MASS  mismatch
                    due1 = 0.
                    dds1 = dte_ute1 * (UEDg(IBLte(1), 1) - usav(IBLte(1), 1)) &
                            + dte_ute2 * (UEDg(IBLte(2), 2) - usav(IBLte(2), 2))
                    !
                else
                    !
                    call blsys
                    !
                endif
                !
                !
                !---- Save wall shear and equil. max shear coefficient for plotting output
                TAU(ibl, is) = 0.5 * R2 * U2 * U2 * CF2
                DIS(ibl, is) = R2 * U2 * U2 * U2 * DI2 * HS2 * 0.5
                CTQ(ibl, is) = CQ2
                DELt(ibl, is) = DE2
                USLp(ibl, is) = 1.60 / (1.0 + US2)
                !
                !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
                !      IF(WAKE) THEN
                !        ALD = DLCON
                !      ELSE
                !       ALD = 1.0
                !      ENDIF
                !C
                !      IF(TURB .AND. .NOT.WAKE) THEN
                !        GCC = GCCON
                !        HKC     = HK2 - 1.0 - GCC/RT2
                !        IF(HKC .LT. 0.01) THEN
                !         HKC = 0.01
                !        ENDIF
                !       ELSE
                !        HKC = HK2 - 1.0
                !       ENDIF
                !C
                !       HR = HKC     / (GACON*ALD*HK2)
                !       UQ = (0.5*CF2 - HR**2) / (GBCON*D2)
                !C
                !       IF(TURB) THEN
                !        IBLP = MIN(IBL+1,NBL(IS))
                !        IBLM = MAX(IBL-1,2      )
                !        DXSSI = XSSI(IBLP,IS) - XSSI(IBLM,IS)
                !        IF(DXXSI.EQ.0.0) DXSSI = 1.0
                !        GUXD(IBL,IS) = -LOG(UEDG(IBLP,IS)/UEDG(IBLM,IS)) / DXSSI
                !        GUXQ(IBL,IS) = -UQ
                !       ELSE
                !        GUXD(IBL,IS) = 0.0
                !        GUXQ(IBL,IS) = 0.0
                !       ENDIF
                !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
                !
                !---- set XI sensitivities wrt LE Ue changes
                if (is==1) then
                    xi_ule1 = SST_go
                    xi_ule2 = -SST_gp
                else
                    xi_ule1 = -SST_go
                    xi_ule2 = SST_gp
                endif
                !
                !---- stuff BL system coefficients into main Jacobian matrix
                !
                do jv = 1, NSYs
                    VM(1, jv, iv) = VS1(1, 3) * d1_m(jv) + VS1(1, 4) * u1_m(jv) + VS2(1, 3) * d2_m(jv) &
                            + VS2(1, 4) * u2_m(jv) + (VS1(1, 5) + VS2(1, 5) &
                            + VSX(1)) * (xi_ule1 * ule1_m(jv) + xi_ule2 * ule2_m(jv))
                enddo
                !
                VB(1, 1, iv) = VS1(1, 1)
                VB(1, 2, iv) = VS1(1, 2)
                !
                VA(1, 1, iv) = VS2(1, 1)
                VA(1, 2, iv) = VS2(1, 2)
                !
                if (LALfa) then
                    VDEl(1, 2, iv) = VSR(1) * re_clmr + VSM(1) * msq_clmr
                else
                    VDEl(1, 2, iv) = (VS1(1, 4) * u1_a + VS1(1, 3) * d1_a) &
                            + (VS2(1, 4) * u2_a + VS2(1, 3) * d2_a) + (VS1(1, 5) + VS2(1, 5) + VSX(1)) &
                            * (xi_ule1 * ule1_a + xi_ule2 * ule2_a)
                endif
                !
                VDEl(1, 1, iv) = VSRez(1) + (VS1(1, 4) * due1 + VS1(1, 3) * dds1) + (VS2(1, 4) * due2 + VS2(1, 3) * dds2) &
                        + (VS1(1, 5) + VS2(1, 5) + VSX(1)) * (xi_ule1 * dule1 + xi_ule2 * dule2)
                !
                !
                do jv = 1, NSYs
                    VM(2, jv, iv) = VS1(2, 3) * d1_m(jv) + VS1(2, 4) * u1_m(jv) &
                            + VS2(2, 3) * d2_m(jv) + VS2(2, 4) * u2_m(jv) &
                            + (VS1(2, 5) + VS2(2, 5) + VSX(2)) * (xi_ule1 * ule1_m(jv) + xi_ule2 * ule2_m(jv))
                enddo
                !
                VB(2, 1, iv) = VS1(2, 1)
                VB(2, 2, iv) = VS1(2, 2)
                !
                VA(2, 1, iv) = VS2(2, 1)
                VA(2, 2, iv) = VS2(2, 2)
                !
                if (LALfa) then
                    VDEl(2, 2, iv) = VSR(2) * re_clmr + VSM(2) * msq_clmr
                else
                    VDEl(2, 2, iv) = (VS1(2, 4) * u1_a + VS1(2, 3) * d1_a) &
                            + (VS2(2, 4) * u2_a + VS2(2, 3) * d2_a) + (VS1(2, 5) + VS2(2, 5) + VSX(2)) &
                            & * (xi_ule1 * ule1_a + xi_ule2 * ule2_a)
                endif
                !
                VDEl(2, 1, iv) = VSRez(2) + (VS1(2, 4) * due1 + VS1(2, 3) * dds1) &
                        + (VS2(2, 4) * due2 + VS2(2, 3) * dds2) &
                        + (VS1(2, 5) + VS2(2, 5) + VSX(2)) * (xi_ule1 * dule1 + xi_ule2 * dule2)
                !
                !
                do jv = 1, NSYs
                    VM(3, jv, iv) = VS1(3, 3) * d1_m(jv) + VS1(3, 4) * u1_m(jv) &
                            + VS2(3, 3) * d2_m(jv) + VS2(3, 4) * u2_m(jv) &
                            + (VS1(3, 5) + VS2(3, 5) + VSX(3)) * (xi_ule1 * ule1_m(jv) + xi_ule2 * ule2_m(jv))
                enddo
                !
                VB(3, 1, iv) = VS1(3, 1)
                VB(3, 2, iv) = VS1(3, 2)
                !
                VA(3, 1, iv) = VS2(3, 1)
                VA(3, 2, iv) = VS2(3, 2)
                !
                if (LALfa) then
                    VDEl(3, 2, iv) = VSR(3) * re_clmr + VSM(3) * msq_clmr
                else
                    VDEl(3, 2, iv) = (VS1(3, 4) * u1_a + VS1(3, 3) * d1_a) &
                            + (VS2(3, 4) * u2_a + VS2(3, 3) * d2_a) &
                            + (VS1(3, 5) + VS2(3, 5) + VSX(3)) * (xi_ule1 * ule1_a + xi_ule2 * ule2_a)
                endif
                !
                VDEl(3, 1, iv) = VSRez(3) + (VS1(3, 4) * due1 + VS1(3, 3) * dds1) + (VS2(3, 4) * due2 + VS2(3, 3) * dds2) &
                        & + (VS1(3, 5) + VS2(3, 5) + VSX(3)) * (xi_ule1 * dule1 + xi_ule2 * dule2)
                !
                !
                if (ibl==IBLte(is) + 1) then
                    !
                    !----- redefine coefficients for TTE, DTE, etc
                    VZ(1, 1) = VS1(1, 1) * cte_cte1
                    VZ(1, 2) = VS1(1, 1) * cte_tte1 + VS1(1, 2) * tte_tte1
                    VB(1, 1, iv) = VS1(1, 1) * cte_cte2
                    VB(1, 2, iv) = VS1(1, 1) * cte_tte2 + VS1(1, 2) * tte_tte2
                    !
                    VZ(2, 1) = VS1(2, 1) * cte_cte1
                    VZ(2, 2) = VS1(2, 1) * cte_tte1 + VS1(2, 2) * tte_tte1
                    VB(2, 1, iv) = VS1(2, 1) * cte_cte2
                    VB(2, 2, iv) = VS1(2, 1) * cte_tte2 + VS1(2, 2) * tte_tte2
                    !
                    VZ(3, 1) = VS1(3, 1) * cte_cte1
                    VZ(3, 2) = VS1(3, 1) * cte_tte1 + VS1(3, 2) * tte_tte1
                    VB(3, 1, iv) = VS1(3, 1) * cte_cte2
                    VB(3, 2, iv) = VS1(3, 1) * cte_tte2 + VS1(3, 2) * tte_tte2
                    !
                endif
                !
                !---- turbulent intervals will follow if currently at transition interval
                if (TRAn) then
                    TURb = .true.
                    !
                    !------ save transition location
                    ITRan(is) = ibl
                    TFOrce(is) = TRForc
                    XSSitr(is) = XT
                    !
                    !------ interpolate airfoil geometry to find transition x/c
                    !-      (for user output)
                    if (is==1) then
                        str = SST - XT
                    else
                        str = SST + XT
                    endif
                    chx = XTE - XLE
                    chy = YTE - YLE
                    chsq = chx**2 + chy**2
                    xtr = seval(str, X, XP, S, N)
                    ytr = seval(str, Y, YP, S, N)
                    XOCtr(is) = ((xtr - XLE) * chx + (ytr - YLE) * chy) / chsq
                    YOCtr(is) = ((ytr - YLE) * chx - (xtr - XLE) * chy) / chsq
                endif
                !
                TRAn = .false.
                !
                if (ibl==IBLte(is)) then
                    !----- set "2" variables at TE to wake correlations for next station
                    !
                    TURb = .true.
                    WAKe = .true.
                    call blvar(3)
                    call blmid(3)
                endif
                !
                do js = 1, 2
                    do jbl = 2, NBL(js)
                        jv = ISYs(jbl, js)
                        u1_m(jv) = u2_m(jv)
                        d1_m(jv) = d2_m(jv)
                    enddo
                enddo
                !
                u1_a = u2_a
                d1_a = d2_a
                !
                due1 = due2
                dds1 = dds2
                !
                if (ibl==ITRan(is) .and. X2>X1) then
                    if (is==1) then
                        TINdex(is) = float(IST - ITRan(is) + 3) - (XT - X1) / (X2 - X1)
                    else
                        TINdex(is) = float(IST + ITRan(is) - 2) + (XT - X1) / (X2 - X1)
                    endif
                endif
                !
                !---- set BL variables for next station
                do icom = 1, NCOM
                    COM1(icom) = COM2(icom)
                enddo
                !
                !---- next streamwise station
            enddo
            !
            if (TFOrce(is)) then
                if (show_output) write (*, 99001) is, XOCtr(is), ITRan(is)
                99001  format (1x, 'Side', i2, ' forced transition at x/c = ', f7.4, i5)
            else
                if (show_output) write (*, 99002) is, XOCtr(is), ITRan(is)
                99002  format (1x, 'Side', i2, '  free  transition at x/c = ', f7.4, i5)
            endif
            !
            !---- next airfoil side
        enddo
        !
    end function setbl
    !*==IBLSYS.f90  processed by SPAG 7.21DC at 11:25 on 11 Jan 2019


    function mrchue()
        use m_xblsys, only: hkin, tesys, trchek, blprv, blkin, blsys, blvar, blmid
        use m_xsolve, only: gauss
        use i_xfoil
        use i_xbl
        implicit none

        logical :: mrchue
        !
        !*** Start of declarations rewritten by SPAG
        !
        ! Local variables
        !
        real :: ami, const, cte, cti, dmax, dsi, dsw, dswaki, dte, dummy, hklim, hktest, hlmax, hmax, htarg, &
                & htest, htmax, msq, ratlen, thi, tsq, tte, ucon, uei, xsi
        logical :: direct
        integer :: ibl, ibm, icom, is, itbl, iw
        !
        !*** End of declarations rewritten by SPAG
        !
        !
        !*** Start of declarations rewritten by SPAG
        !
        ! Local variables
        !
        !
        !*** End of declarations rewritten by SPAG
        !
        !----------------------------------------------------
        !     Marches the BLs and wake in direct mode using
        !     the UEDG array. If separation is encountered,
        !     a plausible value of Hk extrapolated from
        !     upstream is prescribed instead.  Continuous
        !     checking of transition onset is performed.
        !----------------------------------------------------
        !
        mrchue = .true.

        !---- shape parameters for separation criteria
        hlmax = 3.8 !Laminar
        htmax = 2.5 !Turbulent
        !
        do is = 1, 2
            !
            if (show_output) write (*, *) '   side ', is, ' ...'
            !
            AMCrit = ACRit(is)
            !
            !DanEli
            !---- set logical variables for detecting the current airfoil side
                  if (is==1) then 
                   listop = .true.
                   lisbot = .false.
                  else
                   listop = .false.
                   lisbot = .true.
                  endif
            !      WRITE(*,*)' MRCHDU: current side is ',IS,', ',
            !     & ' LISTOP=',LISTOP,' LISBOT=',LISBOT

            !---- set forced transition arc length position
            call xifset(is)
            !
            !---- initialize similarity station with Thwaites' formula
            !DanEli: theta^2 = 0.45 * (x * mu) / (rho * Ue)
            ibl = 2
            xsi = XSSi(ibl, is)
            uei = UEDg(ibl, is)
            !      BULE = LOG(UEDG(IBL+1,IS)/UEI) / LOG(XSSI(IBL+1,IS)/XSI)
            !      BULE = MAX( -.08 , BULE )
            BULe = 1.0
            ucon = uei / xsi**BULe
            tsq = 0.45 / (ucon * (5.0 * BULe + 1.0) * REYbl) * xsi**(1.0 - BULe)
            thi = sqrt(tsq)
            dsi = 2.2 * thi ! DanEli: delta star, see Schlichting, Ch. 6.5
            ami = 0.0 ! disturbance amplification e^N
            !
            !---- initialize Ctau for first turbulent station
            cti = 0.03
            !
            TRAn = .false.
            TURb = .false.
            ITRan(is) = IBLte(is)
            !
            !---- march downstream
            do ibl = 2, NBL(is)
                ibm = ibl - 1
                !
                !DanEli
                !---- set up a common variable for the panel node IBL to be used to 
                !    read stored in memory variables. The name is COMIBL, in XFOIL.INC
                comibl = ibl

                iw = ibl - IBLte(is)
                !
                SIMi = ibl==2
                WAKe = ibl>IBLte(is)
                !
                !------ prescribed quantities
                xsi = XSSi(ibl, is)
                uei = UEDg(ibl, is)
                !
                if (WAKe) then
                    iw = ibl - IBLte(is)
                    dswaki = WGAp(iw) !DanEli WGAP thickness of "dead air" region inside wake just behind TE   
                else
                    dswaki = 0.
                endif
                !
                direct = .true.
                !
                !------ Newton iteration loop for current station
                do itbl = 1, NIL
                    !
                    !DanEli
                    !if (show_output) write (*,*) '   Newton iteration loop ',itbl, ' of ',NIL 
                    !
                    !
                    !-------- assemble 10x3 linearized system for dCtau, dTh, dDs, dUe, dXi
                    !         at the previous "1" station and the current "2" station
                    !         (the "1" station coefficients will be ignored)
                    !
                    !
                    call blprv(xsi, ami, cti, thi, dsi, dswaki, uei)
                    call blkin
                    !
                    !DanEli
                    !if (show_output) write (*,*) '   panel ', ibl, ' X1 ', X1, 'X2', X2 
                    !
                    !-------- check for transition and set appropriate flags and things
                    if ((.not.SIMi) .and. (.not.TURb)) then
                        mrchue = trchek()
                        if (abort_on_nan .and. .not. mrchue) return
                        ami = AMPl2
                        !
                        if (TRAn) then
                            ITRan(is) = ibl
                            if (cti<=0.0) then
                                cti = 0.03
                                S2 = cti
                            endif
                        else
                            ITRan(is) = ibl + 2
                        endif
                        !
                        !
                    endif
                    !
                    if (ibl==IBLte(is) + 1) then
                        tte = THEt(IBLte(1), 1) + THEt(IBLte(2), 2)
                        dte = DSTr(IBLte(1), 1) + DSTr(IBLte(2), 2) + ANTe
                        cte = (CTAu(IBLte(1), 1) * THEt(IBLte(1), 1) + CTAu(IBLte(2), 2) * THEt(IBLte(2), 2)) / tte
                        call tesys(cte, tte, dte)
                    else
                        call blsys
                        !     DanEli
                        !------------------------------------------------------------------
                        !
                        !     Sets up the BL Newton system governing the current interval:
                        !
                        !     |       ||dA1|     |       ||dA2|       |     |
                        !     |  VS1  ||dT1|  +  |  VS2  ||dT2|   =   |VSREZ|
                        !     |       ||dD1|     |       ||dD2|       |     |
                        !              |dU1|              |dU2|
                        !              |dX1|              |dX2|
                        !
                        !        3x5    5x1         3x5    5x1          3x1
                        !
                        !     The system as shown corresponds to a laminar station
                        !     If TRAN, then  dS2  replaces  dA2
                        !     If TURB, then  dS1, dS2  replace  dA1, dA2
                        !
                        !------------------------------------------------------------------
                    endif
                    !

                    if (direct) then
                        !
                        !--------- try direct mode (set dUe = 0 in currently empty 4th line)
                        VS2(4, 1) = 0.
                        VS2(4, 2) = 0.
                        VS2(4, 3) = 0.
                        VS2(4, 4) = 1.0
                        VSRez(4) = 0.
                        !
                        !--------- solve Newton system for current "2" station
                        call gauss(4, 4, VS2, VSRez, 1)
                        !
                        !--------- determine max changes and underrelax if necessary
                        dmax = max(abs(VSRez(2) / thi), abs(VSRez(3) / dsi))
                        if (ibl<ITRan(is)) dmax = max(dmax, abs(VSRez(1) / 10.0))
                        if (ibl>=ITRan(is)) dmax = max(dmax, abs(VSRez(1) / cti))
                        !
                        RLX = 1.0
                        if (dmax>0.3) RLX = 0.3 / dmax
                        !
                        !DanEli
                        !
                        !if (show_output) Then
                        !    write(*,*)'   AMI ',ami,' CTI ',cti,' THI ',thi,' DSI ',dsi
                        !    write(*,*)'   R1 ',VSRez(1),' R2 ',VSRez(2),' R3 ',VSRez(3)
                        !    write(*,*)'   RLX ',RLX
                        !endif 
                        !
                        !--------- see if direct mode is not applicable
                        if (ibl/=IBLte(is) + 1) then
                            !
                            !---------- calculate resulting kinematic shape parameter Hk
                            msq = uei * uei * HSTinv / (GM1bl * (1.0 - 0.5 * uei * uei * HSTinv))
                            htest = (dsi + RLX * VSRez(3)) / (thi + RLX * VSRez(2))
                            call hkin(htest, msq, hktest, dummy, dummy)
                            !
                            !---------- decide whether to do direct or inverse problem based on Hk
                            if (ibl<ITRan(is)) hmax = hlmax
                            if (ibl>=ITRan(is)) hmax = htmax
                            direct = hktest<hmax
                        endif
                        !
                        if (direct) then
                            !---------- update as usual
                            !cc            IF(IBL.LT.ITRAN(IS)) AMI = AMI + RLX*VSREZ(1)
                            if (ibl>=ITRan(is)) cti = cti + RLX * VSRez(1)
                            thi = thi + RLX * VSRez(2)
                            dsi = dsi + RLX * VSRez(3)
                        else
                            !---------- set prescribed Hk for inverse calculation at the current station
                            if (ibl<ITRan(is)) then
                                !----------- laminar case: relatively slow increase in Hk downstream
                                htarg = HK1 + 0.03 * (X2 - X1) / T1
                            elseif (ibl==ITRan(is)) then
                                !----------- transition interval: weighted laminar and turbulent case
                                htarg = HK1 + (0.03 * (XT - X1) - 0.15 * (X2 - XT)) / T1
                            elseif (WAKe) then
                                !----------- turbulent wake case:
                                !-           asymptotic wake behavior with approximate Backward Euler
                                const = 0.03 * (X2 - X1) / T1
                                HK2 = HK1
                                HK2 = HK2 - (HK2 + const * (HK2 - 1.0)**3 - HK1) / (1.0 + 3.0 * const * (HK2 - 1.0)**2)
                                HK2 = HK2 - (HK2 + const * (HK2 - 1.0)**3 - HK1) / (1.0 + 3.0 * const * (HK2 - 1.0)**2)
                                HK2 = HK2 - (HK2 + const * (HK2 - 1.0)**3 - HK1) / (1.0 + 3.0 * const * (HK2 - 1.0)**2)
                                htarg = HK2
                            else
                                !----------- turbulent case: relatively fast decrease in Hk downstream
                                htarg = HK1 - 0.15 * (X2 - X1) / T1
                            endif
                            !
                            !---------- limit specified Hk to something reasonable
                            if (WAKe) then
                                htarg = max(htarg, 1.01)
                            else
                                htarg = max(htarg, hmax)
                            endif
                            !
                            if (show_output) write (*, 99001) ibl, htarg
                            99001              format (' MRCHUE: Inverse mode at', i4, '     Hk =', f8.3)
                            !
                            !---------- try again with prescribed Hk
                            cycle
                            !
                        endif
                        !
                    else
                        !
                        !-------- inverse mode (force Hk to prescribed value HTARG)
                        VS2(4, 1) = 0.
                        VS2(4, 2) = HK2_t2
                        VS2(4, 3) = HK2_d2
                        VS2(4, 4) = HK2_u2
                        VSRez(4) = htarg - HK2
                        !
                        call gauss(4, 4, VS2, VSRez, 1)
                        !
                        !--------- added Ue clamp   MD  3 Apr 03
                        dmax = max(abs(VSRez(2) / thi), abs(VSRez(3) / dsi), abs(VSRez(4) / uei))
                        if (ibl>=ITRan(is)) dmax = max(dmax, abs(VSRez(1) / cti))
                        !
                        RLX = 1.0
                        if (dmax>0.3) RLX = 0.3 / dmax
                        !
                        !DanEli
                        !
                        !if (show_output) Then
                        !   write(*,*)'   AMI ',ami,' CTI ',cti,' THI ',thi,' DSI ',dsi
                        !    write(*,*)'   R1 ',VSRez(1),' R2 ',VSRez(2),' R3 ',VSRez(3)
                        !    write(*,*)'   RLX ',RLX
                        !endif 
                        !
                        !--------- update variables
                        !cc           IF(IBL.LT.ITRAN(IS)) AMI = AMI + RLX*VSREZ(1)
                        if (ibl>=ITRan(is)) cti = cti + RLX * VSRez(1)
                        thi = thi + RLX * VSRez(2)
                        dsi = dsi + RLX * VSRez(3)
                        uei = uei + RLX * VSRez(4)
                        !
                    endif
                    !
                    !-------- eliminate absurd transients
                    if (ibl>=ITRan(is)) then
                        cti = min(cti, 0.30)
                        cti = max(cti, 0.0000001)
                    endif
                    !
                    if (ibl<=IBLte(is)) then
                        hklim = 1.02
                    else
                        hklim = 1.00005
                    endif
                    msq = uei * uei * HSTinv / (GM1bl * (1.0 - 0.5 * uei * uei * HSTinv))
                    dsw = dsi - dswaki
                    call dslim(dsw, thi, uei, msq, hklim)
                    dsi = dsw + dswaki
                    !
                    if (dmax<=1.0E-5) goto 20
                    !
                enddo
                if (show_output) write (*, 99002) ibl, is, dmax
                99002  format (' MRCHUE: Convergence failed at', i4, '  side', i2, '    Res =', e12.4)
                !
                !------ the current unconverged solution might still be reasonable...
                !CC        IF(DMAX .LE. 0.1) GO TO 110
                if (dmax>0.1) then
                    !
                    !------- the current solution is garbage --> extrapolate values instead
                    if (ibl>3) then
                        if (ibl<=IBLte(is)) then
                            thi = THEt(ibm, is) * (XSSi(ibl, is) / XSSi(ibm, is))**0.5
                            dsi = DSTr(ibm, is) * (XSSi(ibl, is) / XSSi(ibm, is))**0.5
                        elseif (ibl==IBLte(is) + 1) then
                            cti = cte
                            thi = tte
                            dsi = dte
                        else
                            thi = THEt(ibm, is)
                            ratlen = (XSSi(ibl, is) - XSSi(ibm, is)) / (10.0 * DSTr(ibm, is))
                            dsi = (DSTr(ibm, is) + thi * ratlen) / (1.0 + ratlen)
                        endif
                        if (ibl==ITRan(is)) cti = 0.05
                        if (ibl>ITRan(is)) cti = CTAu(ibm, is)
                        !
                        uei = UEDg(ibl, is)
                        if (ibl>2 .and. ibl<NBL(is)) uei = 0.5 * (UEDg(ibl - 1, is) + UEDg(ibl + 1, is))
                    endif
                endif
                !
                call blprv(xsi, ami, cti, thi, dsi, dswaki, uei)
                call blkin
                !
                !------- check for transition and set appropriate flags and things
                if ((.not.SIMi) .and. (.not.TURb)) then
                    mrchue = trchek()
                    if (abort_on_nan .and. .not. mrchue) return
                    ami = AMPl2
                    if (TRAn) ITRan(is) = ibl
                    if (.not.TRAn) ITRan(is) = ibl + 2
                endif
                !
                !------- set all other extrapolated values for current station
                if (ibl<ITRan(is)) call blvar(1)
                if (ibl>=ITRan(is)) call blvar(2)
                if (WAKe) call blvar(3)
                !
                if (ibl<ITRan(is)) call blmid(1)
                if (ibl>=ITRan(is)) call blmid(2)
                if (WAKe) call blmid(3)
                !
                !------ pick up here after the Newton iterations
                !
                !------ store primary variables
                20    if (ibl<ITRan(is)) CTAu(ibl, is) = ami
                if (ibl>=ITRan(is)) CTAu(ibl, is) = cti
                THEt(ibl, is) = thi
                DSTr(ibl, is) = dsi
                UEDg(ibl, is) = uei
                MASs(ibl, is) = dsi * uei
                TAU(ibl, is) = 0.5 * R2 * U2 * U2 * CF2
                DIS(ibl, is) = R2 * U2 * U2 * U2 * DI2 * HS2 * 0.5
                CTQ(ibl, is) = CQ2
                DELt(ibl, is) = DE2
                TSTr(ibl, is) = HS2 * T2
                !
                !------ set "1" variables to "2" variables for next streamwise station
                call blprv(xsi, ami, cti, thi, dsi, dswaki, uei)
                call blkin
                do icom = 1, NCOM
                    COM1(icom) = COM2(icom)
                enddo
                !
                !------ turbulent intervals will follow transition interval or TE
                if (TRAn .or. ibl==IBLte(is)) then
                    TURb = .true.
                    !
                    !------- save transition location
                    TFOrce(is) = TRForc
                    XSSitr(is) = XT
                endif
                !
                TRAn = .false.
                !
                if (ibl==IBLte(is)) then
                    thi = THEt(IBLte(1), 1) + THEt(IBLte(2), 2)
                    dsi = DSTr(IBLte(1), 1) + DSTr(IBLte(2), 2) + ANTe
                endif
                !
            enddo
        enddo
        !
    end function mrchue
    !*==MRCHDU.f90  processed by SPAG 7.21DC at 11:25 on 11 Jan 2019


    function mrchuegcm()
        use m_xblsys, only: hkin, tesys, trchek, blprv, blkin, blsys, blvar, blmid
        use m_xsolve, only: gauss
        use i_xfoil
        use i_xbl
        implicit none

        logical :: mrchuegcm
        !
        !
        ! Local variables
        !
        real :: ami, const, cte, cti, dmax, dsi, dsw, dswaki, dte, dummy, hklim, hktest, hlmax, hmax, htarg, &
                & htest, htmax, msq, ratlen, thi, tsq, tte, ucon, uei, xsi
        
        real :: tolf, tolmin, tolx, stmpx, f, jaco(4,4), g(4), sum
        real :: xold(5), xnew (5), fold, p(4), rezold(4)
        logical :: debug, check

        logical :: direct
        integer :: ibl, ibm, icom, is, itbl, iw

        !
        !----------------------------------------------------
        !     Marches the BLs and wake in direct mode using
        !     the UEDG array. If separation is encountered,
        !     a plausible value of Hk extrapolated from
        !     upstream is prescribed instead.  Continuous
        !     checking of transition onset is performed.
        !----------------------------------------------------
        !
        !----------------------------------------------------
        !     A Global Convergent Method called Line Search 
        !     has been included, substituting the default Newton.
        !
        !     elia.daniele@iwes.fraunhofer.de
        !----------------------------------------------------

        mrchuegcm = .true.


    end function mrchuegcm


    function mrchdu()
        use m_xblsys, only: hkin, tesys, trchek, blprv, blkin, blsys, blvar, blmid
        use m_xsolve, only: gauss
        use i_xfoil
        use i_xbl
        implicit none

        logical :: mrchdu
        !
        !*** Start of declarations rewritten by SPAG
        !
        ! Local variables
        !
        real :: ami, cte, cti, dmax, dsi, dsm, dsw, dswaki, dte, dummy, hklim, hkref, msq, ratlen, sennew, &
                & sens, senswt, thi, thm, tte, uei, uem, ueref, xsi
        real, save :: deps
        integer :: ibl, ibm, icom, is, itbl, itrold, iw, k, l
        real, dimension(4, 5) :: vtmp
        real, dimension(4) :: vztmp
        !
        !*** End of declarations rewritten by SPAG
        !
        !
        !*** Start of declarations rewritten by SPAG
        !
        ! Local variables
        !
        !
        !*** End of declarations rewritten by SPAG
        !
        !----------------------------------------------------
        !     Marches the BLs and wake in mixed mode using
        !     the current Ue and Hk.  The calculated Ue
        !     and Hk lie along a line quasi-normal to the
        !     natural Ue-Hk characteristic line of the
        !     current BL so that the Goldstein or Levy-Lees
        !     singularity is never encountered.  Continuous
        !     checking of transition onset is performed.
        !----------------------------------------------------
        !cc   REAL MDI
        !
        data deps/5.0E-6/
        mrchdu = .true.
        !
        !---- constant controlling how far Hk is allowed to deviate
        !-    from the specified value.
        senswt = 1000.0
        !
        do is = 1, 2
            !
            AMCrit = ACRit(is)
            !
            !DanEli
            !---- set logical variables for detecting the current airfoil side
            if (is==1) then 
                listop = .true.
                lisbot = .false.
               else
                listop = .false.
                lisbot = .true.
               endif
            !      WRITE(*,*)' MRCHDU: current side is ',IS,', ',
            !     & ' LISTOP=',LISTOP,' LISBOT=',LISBOT

            !---- set forced transition arc length position
            call xifset(is)
            !
            !---- set leading edge pressure gradient parameter  x/u du/dx
            ibl = 2
            xsi = XSSi(ibl, is)
            uei = UEDg(ibl, is)
            !CC      BULE = LOG(UEDG(IBL+1,IS)/UEI) / LOG(XSSI(IBL+1,IS)/XSI)
            !CC      BULE = MAX( -.08 , BULE )
            BULe = 1.0
            !
            !---- old transition station
            itrold = ITRan(is)
            !
            TRAn = .false.
            TURb = .false.
            ITRan(is) = IBLte(is)
            !
            !---- march downstream
            do ibl = 2, NBL(is)
                ibm = ibl - 1
                !
                !DanEli
                !---- set up a common variable for the panel node IBL to be used to 
                !     read stored in memory variables. The name is COMIBL, in XFOIL.INC
                comibl = ibl
                !
                SIMi = ibl==2
                WAKe = ibl>IBLte(is)
                !
                !------ initialize current station to existing variables
                xsi = XSSi(ibl, is)
                uei = UEDg(ibl, is)
                thi = THEt(ibl, is)
                dsi = DSTr(ibl, is)

                !CC        MDI = MASS(IBL,IS)
                !
                !------ fixed BUG   MD 7 June 99
                if (ibl<itrold) then
                    ami = CTAu(ibl, is)
                    cti = 0.03
                else
                    cti = CTAu(ibl, is)
                    if (cti<=0.0) cti = 0.03
                endif
                !
                !CC        DSI = MDI/UEI
                !
                if (WAKe) then
                    iw = ibl - IBLte(is)
                    dswaki = WGAp(iw)
                else
                    dswaki = 0.
                endif
                !
                if (ibl<=IBLte(is)) dsi = max(dsi - dswaki, 1.02000 * thi) + dswaki
                if (ibl>IBLte(is)) dsi = max(dsi - dswaki, 1.00005 * thi) + dswaki
                !
                !------ Newton iteration loop for current station
                do itbl = 1, NIL
                    !
                    !-------- assemble 10x3 linearized system for dCtau, dTh, dDs, dUe, dXi
                    !         at the previous "1" station and the current "2" station
                    !         (the "1" station coefficients will be ignored)
                    !
                    call blprv(xsi, ami, cti, thi, dsi, dswaki, uei)
                    call blkin
                    !
                    !DanEli
                    if (isnan(HK2_t2)) then
                        write (*,*)'************************************'
                        write (*,*)'* IS: ',IS,' IBL: ',IBL,' IBLTE(IS):',IBLTE(IS),' ITBL: ',ITBL       
                        write (*,99002) X2, AMPL2, S2, T2, D2, U2 
                        99002 format (' MRCHDU: after BLPRV  X2=', E12.4, '   AMPL2=', E12.4, &
                         &       '  S2=', E12.4,'   T2=', E12.4, &
                         &       '  D2=', E12.4,'   U2=', E12.4)
                    endif

                    !-------- check for transition and set appropriate flags and things
                    if ((.not.SIMi) .and. (.not.TURb)) then
                        mrchdu = trchek()
                        if (abort_on_nan .and. .not. mrchdu) return
                        ami = AMPl2
                        if (TRAn) ITRan(is) = ibl
                        if (.not.TRAn) ITRan(is) = ibl + 2
                    endif
                    !
                    if (ibl==IBLte(is) + 1) then
                        tte = THEt(IBLte(1), 1) + THEt(IBLte(2), 2)
                        dte = DSTr(IBLte(1), 1) + DSTr(IBLte(2), 2) + ANTe
                        cte = (CTAu(IBLte(1), 1) * THEt(IBLte(1), 1) + CTAu(IBLte(2), 2) * THEt(IBLte(2), 2)) / tte
                        call tesys(cte, tte, dte)
                    else
                        call blsys
                    endif
                    !
                    !DanEli
                    !
                    if (isnan(vtmp(1,2))) then
                        write(*,99003) VSRez(1), VSRez(2), VSRez(3), VSRez(4) 
                        99003 format(' MRCHDU: After Gauss VSREZ(1)=',E8.4,' VSREZ(2)=',E8.4, &
                            &       ' VSREZ(3)=', E8.4,' VSREZ(4)=', E8.4)
                                   write(*,*)' MRCHDU: After Gauss VS2(1,:)=', &
                            &       VS2(1,1), VS2(1,2), VS2(1,3), VS2(1,4), VS2(1,5), &
                            &       ' MRCHDU: After Gauss VS2(2,:)=', &
                            &       VS2(2,1), VS2(2,2), VS2(2,3), VS2(2,4), VS2(2,5), &
                            &       ' MRCHDU: After Gauss VS2(3,:)=', &
                            &       VS2(3,1), VS2(3,2), VS2(3,3), VS2(3,4), VS2(3,5), &
                            &       ' MRCHDU: After Gauss VS2(4,:)=', &
                            &       VS2(4,1), VS2(4,2), VS2(4,3), VS2(4,4), VS2(4,5)
                    endif
                    !
                    !-------- set stuff at first iteration...
                    if (itbl==1) then
                        !
                        !--------- set "baseline" Ue and Hk for forming  Ue(Hk)  relation
                        ueref = U2
                        hkref = HK2
                        !
                        !--------- if current point IBL was turbulent and is now laminar, then...
                        if (ibl<ITRan(is) .and. ibl>=itrold) then
                            !---------- extrapolate baseline Hk
                            uem = UEDg(ibl - 1, is)
                            dsm = DSTr(ibl - 1, is)
                            thm = THEt(ibl - 1, is)
                            msq = uem * uem * HSTinv / (GM1bl * (1.0 - 0.5 * uem * uem * HSTinv))
                            call hkin(dsm / thm, msq, hkref, dummy, dummy)
                        endif
                        !
                        !--------- if current point IBL was laminar, then...
                        if (ibl<itrold) then
                            !---------- reinitialize or extrapolate Ctau if it's now turbulent
                            if (TRAn) CTAu(ibl, is) = 0.03
                            if (TURb) CTAu(ibl, is) = CTAu(ibl - 1, is)
                            if (TRAn .or. TURb) then
                                cti = CTAu(ibl, is)
                                S2 = cti
                            endif
                        endif
                        !
                    endif
                    !
                    !
                    if (SIMi .or. ibl==IBLte(is) + 1) then
                        !
                        !--------- for similarity station or first wake point, prescribe Ue
                        VS2(4, 1) = 0.
                        VS2(4, 2) = 0.
                        VS2(4, 3) = 0.
                        VS2(4, 4) = U2_uei
                        VSRez(4) = ueref - U2
                        !
                    else
                        !
                        !********* calculate Ue-Hk characteristic slope
                        !
                        do k = 1, 4
                            vztmp(k) = VSRez(k)
                            do l = 1, 5
                                vtmp(k, l) = VS2(k, l)
                            enddo
                        enddo
                        !
                        !--------- set unit dHk
                        vtmp(4, 1) = 0.
                        vtmp(4, 2) = HK2_t2
                        vtmp(4, 3) = HK2_d2
                        vtmp(4, 4) = HK2_u2 * U2_uei
                        vztmp(4) = 1.0
                        !
                        !DanEli
                        !
                        if (isnan(HK2_t2)) then
                            WRITE(*,99004) HK2_t2, HK2_d2, HK2_u2, U2_uei 
                            99004 format(' MRCHDU: set unit dHk HK2_T2=',E12.4,'   HK2_D2=',E12.4, &
                                &       '  HK2_U2=', E12.4,'   U2_UEI=', E12.4)
                        endif
                                   
                        if (ISNAN(vztmp(2))) then
                            write(*,*)'************************************'
                            write(*,*)'* IS: ',is,' IBL: ',ibl,' IBLTE(IS):',IBLte(is), &
                                & ' ITBL: ',itbl       
                            write(*,99005) vztmp(1), vztmp(2), vztmp(3), vztmp(4) 
                            99005 format(' MRCHDU: Bef Gauss VZTMP(1)=',E12.4,'   VZTMP(2)=',E12.4, &
                                &       '  VZTMP(3)=', E12.4,'   VZTMP(4)=', E12.4)
                            write(*,*)' MRCHDU: After Gauss VTMP(1,:)=', &
                                &       vtmp(1,1), vtmp(1,2), vtmp(1,3), vtmp(1,4), vtmp(1,5), &
                                &       ' MRCHDU: After Gauss VTMP(2,:)=', &
                                &       vtmp(2,1), vtmp(2,2), vtmp(2,3), vtmp(2,4), vtmp(2,5), &
                                &       ' MRCHDU: After Gauss VTMP(3,:)=', &
                                &       vtmp(3,1), vtmp(3,2), vtmp(3,3), vtmp(3,4), vtmp(3,5), &
                                &       ' MRCHDU: After Gauss VTMP(4,:)=', &
                                &       vtmp(4,1), vtmp(4,2), vtmp(4,3), vtmp(4,4), vtmp(4,5)
                        endif
                        !
                        !--------- calculate dUe response
                        call gauss(4, 4, vtmp, vztmp, 1)
                        !
                        if (ISNAN(vztmp(4))) then
                            write(*,*)'************************************'
                            write(*,*)'* IS: ',is,' IBL: ',ibl,' IBLTE(IS):',IBLte(is), &
                                & ' ITBL: ',itbl       
                            write(*,99006) vztmp(1), vztmp(2), vztmp(3), vztmp(4) 
                            99006 format(' MRCHDU: Bef Gauss VZTMP(1)=',E12.4,'   VZTMP(2)=',E12.4, &
                                &       '  VZTMP(3)=', E12.4,'   VZTMP(4)=', E12.4)
                            write(*,*)' MRCHDU: After Gauss VTMP(1,:)=', &
                                &       vtmp(1,1), vtmp(1,2), vtmp(1,3), vtmp(1,4), vtmp(1,5), &
                                &       ' MRCHDU: After Gauss VTMP(2,:)=', &
                                &       vtmp(2,1), vtmp(2,2), vtmp(2,3), vtmp(2,4), vtmp(2,5), &
                                &       ' MRCHDU: After Gauss VTMP(3,:)=', &
                                &       vtmp(3,1), vtmp(3,2), vtmp(3,3), vtmp(3,4), vtmp(3,5), &
                                &       ' MRCHDU: After Gauss VTMP(4,:)=', &
                                &       vtmp(4,1), vtmp(4,2), vtmp(4,3), vtmp(4,4), vtmp(4,5)
                        endif
                        !
                        !--------- set  SENSWT * (normalized dUe/dHk)
                        sennew = senswt * vztmp(4) * hkref / ueref
                        if (itbl<=5) then
                            sens = sennew
                        elseif (itbl<=15) then
                            sens = 0.5 * (sens + sennew)
                        endif
                        !
                        ! DanEli 2018.10.24
                        ! IF (ISNAN(SENS)) THEN
                        !    WRITE(*,1355) SENSWT, SENNEW, VZTMP(4), ITBL, SENS 
                        !    1355 FORMAT(' MRCHDU: SENSWT=',E12.4,'  SENNEW=',E12.4,
                        !      &       '  VZTMP(4)=', E12.4,'  ITBL=', I4,'  SENS=', E12.4)
                        !            ENDIF
                        !--------- set prescribed Ue-Hk combination
                        VS2(4, 1) = 0.
                        VS2(4, 2) = HK2_t2 * hkref
                        VS2(4, 3) = HK2_d2 * hkref
                        VS2(4, 4) = (HK2_u2 * hkref + sens / ueref) * U2_uei
                        VSRez(4) = -(hkref**2) * (HK2 / hkref - 1.0) - sens * (U2 / ueref - 1.0)
                        !
                        !DanEli 2018.10.24
                        ! IF (ISNAN(VSREZ(4))) THEN
                        !    WRITE(*,1354) HKREF, HK2, UEREF, U2, SENS 
                        !    1354 FORMAT(' MRCHDU: HKREF=',E12.4,'  HK2=',E12.4,
                        !        &       '  UEREF=', E12.4,'  U2=', E12.4,'  SENS=', E12.4)
                        ! ENDIF
                    endif
                    !
                    !DanEli 2018.10.24
                    ! IF (ISNAN(VSREZ(1)) .OR. ISNAN(VSREZ(2))) THEN
                    !    WRITE(*,1353) VSREZ(1), VSREZ(2), VSREZ(3), VSREZ(4) 
                    !    1353   FORMAT(' MRCHDU: Before Gauss Res(1)=',E12.4,'  Res(2)=',E12.4,
                    !        &         '  Res(3)=', E12.4,'  Res(4)=', E12.4)
                    ! ENDIF
                    !
                    !-------- solve Newton system for current "2" station
                    call gauss(4, 4, VS2, VSRez, 1)
                    !
                    ! DanEli 2018.10.24
                    ! IF (ISNAN(VSREZ(1)) .OR. ISNAN(VSREZ(2))) THEN
                    !     WRITE(*,1352) VSREZ(1), VSREZ(2), VSREZ(3), VSREZ(4) 
                    !     1352   FORMAT(' MRCHDU: After Gauss Res(1)=',E12.4,'  Res(2)=',E12.4,
                    !         &         '  Res(3)=', E12.4,'  Res(4)=', E12.4)
                    ! ENDIF
                    !
                    !-------- determine max changes and underrelax if necessary
                    !-------- (added Ue clamp   MD  3 Apr 03)
                    dmax = max(abs(VSRez(2) / thi), abs(VSRez(3) / dsi), abs(VSRez(4) / uei))
                    if (ibl>=ITRan(is)) dmax = max(dmax, abs(VSRez(1) / (10.0 * cti)))
                    !
                    RLX = 1.0
                    if (dmax>0.3) RLX = 0.3 / dmax
                    !
                    !-------- update as usual
                    if (ibl<ITRan(is)) ami = ami + RLX * VSRez(1)
                    if (ibl>=ITRan(is)) cti = cti + RLX * VSRez(1)
                    thi = thi + RLX * VSRez(2)
                    dsi = dsi + RLX * VSRez(3)
                    uei = uei + RLX * VSRez(4)
                    !
                    !-------- eliminate absurd transients
                    if (ibl>=ITRan(is)) then
                        cti = min(cti, 0.30)
                        cti = max(cti, 0.0000001)
                    endif
                    !
                    if (ibl<=IBLte(is)) then
                        hklim = 1.02
                    else
                        hklim = 1.00005
                    endif
                    msq = uei * uei * HSTinv / (GM1bl * (1.0 - 0.5 * uei * uei * HSTinv))
                    dsw = dsi - dswaki
                    call dslim(dsw, thi, uei, msq, hklim)
                    dsi = dsw + dswaki
                    !
                    if (dmax<=deps) goto 20
                    !
                enddo
                !
                if (show_output) write (*, 99001) ibl, is, dmax
                99001  format (' MRCHDU: Convergence failed at', i4, '  side', i2, '    Res =', e12.4)
                !
                !DanEli 2018.10.24
                !         WRITE(*,1351) VSREZ(1), VSREZ(2), VSREZ(3), VSREZ(4) 
                !  1351   FORMAT(' MRCHDU: Res(1)=',E12.4,'  Res(2)=',E12.4,
                !      &         '  Res(3)=', E12.4,'  Res(4)=', E12.4)
                !
                !------ the current unconverged solution might still be reasonable...
                !CC        IF(DMAX .LE. 0.1) GO TO 110
                if (dmax>0.1) then
                    !
                    !------- the current solution is garbage --> extrapolate values instead
                    if (ibl>3) then
                        if (ibl<=IBLte(is)) then
                            thi = THEt(ibm, is) * (XSSi(ibl, is) / XSSi(ibm, is))**0.5
                            dsi = DSTr(ibm, is) * (XSSi(ibl, is) / XSSi(ibm, is))**0.5
                            uei = UEDg(ibm, is)
                        elseif (ibl==IBLte(is) + 1) then
                            cti = cte
                            thi = tte
                            dsi = dte
                            uei = UEDg(ibm, is)
                        else
                            thi = THEt(ibm, is)
                            ratlen = (XSSi(ibl, is) - XSSi(ibm, is)) / (10.0 * DSTr(ibm, is))
                            dsi = (DSTr(ibm, is) + thi * ratlen) / (1.0 + ratlen)
                            uei = UEDg(ibm, is)
                        endif
                        if (ibl==ITRan(is)) cti = 0.05
                        if (ibl>ITRan(is)) cti = CTAu(ibm, is)
                    endif
                endif
                !
                call blprv(xsi, ami, cti, thi, dsi, dswaki, uei)
                call blkin
                !
                !------- check for transition and set appropriate flags and things
                if ((.not.SIMi) .and. (.not.TURb)) then
                    mrchdu = trchek()
                    if (abort_on_nan .and. .not. mrchdu) return
                    ami = AMPl2
                    if (TRAn) ITRan(is) = ibl
                    if (.not.TRAn) ITRan(is) = ibl + 2
                endif
                !
                !------- set all other extrapolated values for current station
                if (ibl<ITRan(is)) call blvar(1)
                if (ibl>=ITRan(is)) call blvar(2)
                if (WAKe) call blvar(3)
                !
                if (ibl<ITRan(is)) call blmid(1)
                if (ibl>=ITRan(is)) call blmid(2)
                if (WAKe) call blmid(3)
                !
                !------ pick up here after the Newton iterations
                !
                20    sens = sennew
                !
                !------ store primary variables
                if (ibl<ITRan(is)) CTAu(ibl, is) = ami
                if (ibl>=ITRan(is)) CTAu(ibl, is) = cti
                THEt(ibl, is) = thi
                DSTr(ibl, is) = dsi
                UEDg(ibl, is) = uei
                MASs(ibl, is) = dsi * uei
                TAU(ibl, is) = 0.5 * R2 * U2 * U2 * CF2
                DIS(ibl, is) = R2 * U2 * U2 * U2 * DI2 * HS2 * 0.5
                CTQ(ibl, is) = CQ2
                DELt(ibl, is) = DE2
                TSTr(ibl, is) = HS2 * T2
                !
                !------ set "1" variables to "2" variables for next streamwise station
                call blprv(xsi, ami, cti, thi, dsi, dswaki, uei)
                call blkin
                do icom = 1, NCOM
                    COM1(icom) = COM2(icom)
                enddo
                !
                !
                !------ turbulent intervals will follow transition interval or TE
                if (TRAn .or. ibl==IBLte(is)) then
                    TURb = .true.
                    !
                    !------- save transition location
                    TFOrce(is) = TRForc
                    XSSitr(is) = XT
                endif
                !
                TRAn = .false.
                !
            enddo
            !
        enddo
        !
    end function mrchdu
    !*==XIFSET.f90  processed by SPAG 7.21DC at 11:25 on 11 Jan 2019


    subroutine xifset(Is)
        use m_spline, only: splind, sinvrt
        use i_xfoil
        use i_xbl
        implicit none
        !
        !*** Start of declarations rewritten by SPAG
        !
        ! Dummy arguments
        !
        integer :: Is
        intent (in) Is
        !
        ! Local variables
        !
        real :: chsq, chx, chy, str
        integer :: i
        !
        !*** End of declarations rewritten by SPAG
        !
        !
        !*** Start of declarations rewritten by SPAG
        !
        ! Dummy arguments
        !
        !
        ! Local variables
        !
        !
        !*** End of declarations rewritten by SPAG
        !
        !-----------------------------------------------------
        !     Sets forced-transition BL coordinate locations.
        !-----------------------------------------------------
        !
        if (XSTrip(Is)>=1.0) then
            XIForc = XSSi(IBLte(Is), Is)
            return
        endif
        !
        chx = XTE - XLE
        chy = YTE - YLE
        chsq = chx**2 + chy**2
        !
        !---- calculate chord-based x/c, y/c
        do i = 1, N
            W1(i) = ((X(i) - XLE) * chx + (Y(i) - YLE) * chy) / chsq
            W2(i) = ((Y(i) - YLE) * chx - (X(i) - XLE) * chy) / chsq
        enddo
        !
        call splind(W1, W3, S, N, -999.0, -999.0)
        call splind(W2, W4, S, N, -999.0, -999.0)
        !
        if (Is==1) then
            !
            !----- set approximate arc length of forced transition point for SINVRT
            str = SLE + (S(1) - SLE) * XSTrip(Is)
            !
            !----- calculate actual arc length
            call sinvrt(str, XSTrip(Is), W1, W3, S, N)
            !
            !----- set BL coordinate value
            XIForc = min((SST - str), XSSi(IBLte(Is), Is))
            !
        else
            !----- same for bottom side
            !
            str = SLE + (S(N) - SLE) * XSTrip(Is)
            call sinvrt(str, XSTrip(Is), W1, W3, S, N)
            XIForc = min((str - SST), XSSi(IBLte(Is), Is))
            !
        endif
        !
        if (XIForc<0.0) then
            if (show_output) write (*, 99001) Is
            99001 format (/' ***  Stagnation point is past trip on side', i2, '  ***')
            XIForc = XSSi(IBLte(Is), Is)
        endif
        !
    end subroutine xifset
    !*==UPDATE.f90  processed by SPAG 7.21DC at 11:25 on 11 Jan 2019


    subroutine update
        use i_xfoil
        implicit none
        !
        !*** Start of declarations rewritten by SPAG
        !
        ! Local variables
        !
        real :: ag, ag_ac, ag_ms, beta, beta_msq, bfac, bfac_msq, ca, cginc, clnew, cl_a, cl_ac, cl_ms, cpc_cpi, &
                & cpg1, cpg1_ac, cpg1_ms, cpg2, cpg2_ac, cpg2_ms, cpi_q, dac, dalmax, dalmin, dclmax, dclmin, &
                & dctau, ddstr, dhi, dlo, dmass, dn1, dn2, dn3, dn4, dsw, dswaki, dthet, duedg, dui, dui_ac, dx, &
                & dx_a, hklim, hstinv, msq, rdn1, rdn2, rdn3, rdn4, sa, ue_m, uinv_ac
        integer :: i, ibl, ip, is, iv, iw, j, jbl, js, jv, kbl
        real, dimension(IQX) :: qnew, q_ac
        real, dimension(IVX, 2) :: unew, u_ac
        real, dimension(3, 2, IZX) :: va_copy, vb_copy
        !
        !*** End of declarations rewritten by SPAG
        !
        !
        !*** Start of declarations rewritten by SPAG
        !
        ! Local variables
        !
        !
        !*** End of declarations rewritten by SPAG
        !
        !------------------------------------------------------------------
        !      Adds on Newton deltas to boundary layer variables.
        !      Checks for excessive changes and underrelaxes if necessary.
        !      Calculates max and rms changes.
        !      Also calculates the change in the global variable "AC".
        !        If LALFA=.TRUE. , "AC" is CL
        !        If LALFA=.FALSE., "AC" is alpha
        !------------------------------------------------------------------
        equivalence (va_copy(1, 1, 1), unew(1, 1))
        equivalence (vb_copy(1, 1, 1), qnew(1))
        equivalence (va_copy(1, 1, IVX), u_ac(1, 1))
        equivalence (vb_copy(1, 1, IVX), q_ac(1))
        va_copy = VA
        vb_copy = VB
        !
        !---- max allowable alpha changes per iteration
        dalmax = 0.5 * DTOr
        dalmin = -0.5 * DTOr
        !
        !---- max allowable CL change per iteration
        dclmax = 0.5
        dclmin = -0.5
        if (MATyp/=1) dclmin = max(-0.5, -0.9 * CL)
        !
        hstinv = GAMm1 * (MINf / QINf)**2 / (1.0 + 0.5 * GAMm1 * MINf**2)
        !
        !---- calculate new Ue distribution assuming no under-relaxation
        !-    also set the sensitivity of Ue wrt to alpha or Re
        do is = 1, 2
            do ibl = 2, NBL(is)
                i = IPAn(ibl, is)
                !
                dui = 0.
                dui_ac = 0.
                do js = 1, 2
                    do jbl = 2, NBL(js)
                        j = IPAn(jbl, js)
                        jv = ISYs(jbl, js)
                        ue_m = -VTI(ibl, is) * VTI(jbl, js) * DIJ(i, j)
                        dui = dui + ue_m * (MASs(jbl, js) + VDEl(3, 1, jv))
                        dui_ac = dui_ac + ue_m * (-VDEl(3, 2, jv))
                    enddo
                enddo
                !
                !-------- UINV depends on "AC" only if "AC" is alpha
                if (LALfa) then
                    uinv_ac = 0.
                else
                    uinv_ac = UINv_a(ibl, is)
                endif
                !
                unew(ibl, is) = UINv(ibl, is) + dui
                u_ac(ibl, is) = uinv_ac + dui_ac
                !
            enddo
        enddo
        !
        !---- set new Qtan from new Ue with appropriate sign change
        do is = 1, 2
            do ibl = 2, IBLte(is)
                i = IPAn(ibl, is)
                qnew(i) = VTI(ibl, is) * unew(ibl, is)
                q_ac(i) = VTI(ibl, is) * u_ac(ibl, is)
            enddo
        enddo
        !
        !---- calculate new CL from this new Qtan
        sa = sin(ALFa)
        ca = cos(ALFa)
        !
        beta = sqrt(1.0 - MINf**2)
        beta_msq = -0.5 / beta
        !
        bfac = 0.5 * MINf**2 / (1.0 + beta)
        bfac_msq = 0.5 / (1.0 + beta) - bfac / (1.0 + beta) * beta_msq
        !
        clnew = 0.
        cl_a = 0.
        cl_ms = 0.
        cl_ac = 0.
        !
        i = 1
        cginc = 1.0 - (qnew(i) / QINf)**2
        cpg1 = cginc / (beta + bfac * cginc)
        cpg1_ms = -cpg1 / (beta + bfac * cginc) * (beta_msq + bfac_msq * cginc)
        !
        cpi_q = -2.0 * qnew(i) / QINf**2
        cpc_cpi = (1.0 - bfac * cpg1) / (beta + bfac * cginc)
        cpg1_ac = cpc_cpi * cpi_q * q_ac(i)
        !
        do i = 1, N
            ip = i + 1
            if (i==N) ip = 1
            !
            cginc = 1.0 - (qnew(ip) / QINf)**2
            cpg2 = cginc / (beta + bfac * cginc)
            cpg2_ms = -cpg2 / (beta + bfac * cginc) * (beta_msq + bfac_msq * cginc)
            !
            cpi_q = -2.0 * qnew(ip) / QINf**2
            cpc_cpi = (1.0 - bfac * cpg2) / (beta + bfac * cginc)
            cpg2_ac = cpc_cpi * cpi_q * q_ac(ip)
            !
            dx = (X(ip) - X(i)) * ca + (Y(ip) - Y(i)) * sa
            dx_a = -(X(ip) - X(i)) * sa + (Y(ip) - Y(i)) * ca
            !
            ag = 0.5 * (cpg2 + cpg1)
            ag_ms = 0.5 * (cpg2_ms + cpg1_ms)
            ag_ac = 0.5 * (cpg2_ac + cpg1_ac)
            !
            clnew = clnew + dx * ag
            cl_a = cl_a + dx_a * ag
            cl_ms = cl_ms + dx * ag_ms
            cl_ac = cl_ac + dx * ag_ac
            !
            cpg1 = cpg2
            cpg1_ms = cpg2_ms
            cpg1_ac = cpg2_ac
        enddo
        !
        !---- initialize under-relaxation factor
        RLX = 1.0
        !
        if (LALfa) then
            !===== alpha is prescribed: AC is CL
            !
            !----- set change in Re to account for CL changing, since Re = Re(CL)
            dac = (clnew - CL) / (1.0 - cl_ac - cl_ms * 2.0 * MINf * MINf_cl)
            !
            !----- set under-relaxation factor if Re change is too large
            if (RLX * dac>dclmax) RLX = dclmax / dac
            if (RLX * dac<dclmin) RLX = dclmin / dac
            !
        else
            !===== CL is prescribed: AC is alpha
            !
            !----- set change in alpha to drive CL to prescribed value
            dac = (clnew - CLSpec) / (0.0 - cl_ac - cl_a)
            !
            !----- set under-relaxation factor if alpha change is too large
            if (RLX * dac>dalmax) RLX = dalmax / dac
            if (RLX * dac<dalmin) RLX = dalmin / dac
            !
        endif
        !
        RMSbl = 0.
        RMXbl = 0.
        !
        dhi = 1.5
        dlo = -.5
        !
        !---- calculate changes in BL variables and under-relaxation if needed
        do is = 1, 2
            do ibl = 2, NBL(is)
                iv = ISYs(ibl, is)
                !


                !-------- set changes without underrelaxation
                dctau = VDEl(1, 1, iv) - dac * VDEl(1, 2, iv)
                dthet = VDEl(2, 1, iv) - dac * VDEl(2, 2, iv)
                dmass = VDEl(3, 1, iv) - dac * VDEl(3, 2, iv)
                duedg = unew(ibl, is) + dac * u_ac(ibl, is) - UEDg(ibl, is)
                ddstr = (dmass - DSTr(ibl, is) * duedg) / UEDg(ibl, is)
                !
                !-------- normalize changes
                if (ibl<ITRan(is)) dn1 = dctau / 10.0
                if (ibl>=ITRan(is)) dn1 = dctau / CTAu(ibl, is)
                dn2 = dthet / THEt(ibl, is)
                dn3 = ddstr / DSTr(ibl, is)
                dn4 = abs(duedg) / 0.25
                !
                !-------- accumulate for rms change
                RMSbl = RMSbl + dn1**2 + dn2**2 + dn3**2 + dn4**2
                !
                !-------- see if Ctau needs underrelaxation
                rdn1 = RLX * dn1
                if (abs(dn1)>abs(RMXbl)) then
                    RMXbl = dn1
                    if (ibl<ITRan(is)) VMXbl = 'n'
                    if (ibl>=ITRan(is)) VMXbl = 'C'
                    IMXbl = ibl
                    ISMxbl = is
                endif
                if (rdn1>dhi) RLX = dhi / dn1
                if (rdn1<dlo) RLX = dlo / dn1
                !
                !-------- see if Theta needs underrelaxation
                rdn2 = RLX * dn2
                if (abs(dn2)>abs(RMXbl)) then
                    RMXbl = dn2
                    VMXbl = 'T'
                    IMXbl = ibl
                    ISMxbl = is
                endif
                if (rdn2>dhi) RLX = dhi / dn2
                if (rdn2<dlo) RLX = dlo / dn2
                !
                !-------- see if Dstar needs underrelaxation
                rdn3 = RLX * dn3
                if (abs(dn3)>abs(RMXbl)) then
                    RMXbl = dn3
                    VMXbl = 'D'
                    IMXbl = ibl
                    ISMxbl = is
                endif
                if (rdn3>dhi) RLX = dhi / dn3
                if (rdn3<dlo) RLX = dlo / dn3
                !
                !-------- see if Ue needs underrelaxation
                rdn4 = RLX * dn4
                if (abs(dn4)>abs(RMXbl)) then
                    RMXbl = duedg
                    VMXbl = 'U'
                    IMXbl = ibl
                    ISMxbl = is
                endif
                if (rdn4>dhi) RLX = dhi / dn4
                if (rdn4<dlo) RLX = dlo / dn4
                !
            enddo
        enddo
        !
        !---- set true rms change
        RMSbl = sqrt(RMSbl / (4.0 * float(NBL(1) + NBL(2))))
        !
        !
        if (LALfa) then
            !----- set underrelaxed change in Reynolds number from change in lift
            CL = CL + RLX * dac
        else
            !----- set underrelaxed change in alpha
            ALFa = ALFa + RLX * dac
            ADEg = ALFa / DTOr
        endif
        !
        !---- update BL variables with underrelaxed changes
        do is = 1, 2
            do ibl = 2, NBL(is)
                iv = ISYs(ibl, is)
                !
                dctau = VDEl(1, 1, iv) - dac * VDEl(1, 2, iv)
                dthet = VDEl(2, 1, iv) - dac * VDEl(2, 2, iv)
                dmass = VDEl(3, 1, iv) - dac * VDEl(3, 2, iv)
                duedg = unew(ibl, is) + dac * u_ac(ibl, is) - UEDg(ibl, is)
                ddstr = (dmass - DSTr(ibl, is) * duedg) / UEDg(ibl, is)
                !
                CTAu(ibl, is) = CTAu(ibl, is) + RLX * dctau
                THEt(ibl, is) = THEt(ibl, is) + RLX * dthet
                DSTr(ibl, is) = DSTr(ibl, is) + RLX * ddstr
                UEDg(ibl, is) = UEDg(ibl, is) + RLX * duedg
                !
                if (ibl>IBLte(is)) then
                    iw = ibl - IBLte(is)
                    dswaki = WGAp(iw)
                else
                    dswaki = 0.
                endif
                !
                !-------- eliminate absurd transients
                if (ibl>=ITRan(is)) CTAu(ibl, is) = min(CTAu(ibl, is), 0.25)
                !
                if (ibl<=IBLte(is)) then
                    hklim = 1.02
                else
                    hklim = 1.00005
                endif
                msq = UEDg(ibl, is)**2 * hstinv / (GAMm1 * (1.0 - 0.5 * UEDg(ibl, is)**2 * hstinv))
                dsw = DSTr(ibl, is) - dswaki
                call dslim(dsw, THEt(ibl, is), UEDg(ibl, is), msq, hklim)
                DSTr(ibl, is) = dsw + dswaki
                !
                !-------- set new mass defect (nonlinear update)
                MASs(ibl, is) = DSTr(ibl, is) * UEDg(ibl, is)
                !
            enddo
            !
            !------ make sure there are no "islands" of negative Ue
            do ibl = 3, IBLte(is)
                if (UEDg(ibl - 1, is)>0.0 .and. UEDg(ibl, is)<=0.0) then
                    UEDg(ibl, is) = UEDg(ibl - 1, is)
                    MASs(ibl, is) = DSTr(ibl, is) * UEDg(ibl, is)
                endif
            enddo
        enddo
        !
        !
        !---- equate upper wake arrays to lower wake arrays
        do kbl = 1, NBL(2) - IBLte(2)
            CTAu(IBLte(1) + kbl, 1) = CTAu(IBLte(2) + kbl, 2)
            THEt(IBLte(1) + kbl, 1) = THEt(IBLte(2) + kbl, 2)
            DSTr(IBLte(1) + kbl, 1) = DSTr(IBLte(2) + kbl, 2)
            UEDg(IBLte(1) + kbl, 1) = UEDg(IBLte(2) + kbl, 2)
            TAU(IBLte(1) + kbl, 1) = TAU(IBLte(2) + kbl, 2)
            DIS(IBLte(1) + kbl, 1) = DIS(IBLte(2) + kbl, 2)
            CTQ(IBLte(1) + kbl, 1) = CTQ(IBLte(2) + kbl, 2)
            DELt(IBLte(1) + kbl, 1) = DELt(IBLte(2) + kbl, 2)
            TSTr(IBLte(1) + kbl, 1) = TSTr(IBLte(2) + kbl, 2)
        enddo
        !
        VA = va_copy
        VB = vb_copy
    end subroutine update
    !*==DSLIM.f90  processed by SPAG 7.21DC at 11:25 on 11 Jan 2019


    subroutine dslim(Dstr, Thet, Uedg, Msq, Hklim)
        use m_xblsys, only: hkin
        implicit none
        !
        !*** Start of declarations rewritten by SPAG
        !
        ! Dummy arguments
        !
        real :: Dstr, Hklim, Msq, Thet, Uedg
        intent (in) Hklim, Thet
        intent (inout) Dstr
        !
        ! Local variables
        !
        real :: dh, h, hk, hk_h, hk_m
        !
        !*** End of declarations rewritten by SPAG
        !
        !
        !*** Start of declarations rewritten by SPAG
        !
        ! Dummy arguments
        !
        !
        ! Local variables
        !
        !
        !*** End of declarations rewritten by SPAG
        !
        !
        h = Dstr / Thet
        call hkin(h, Msq, hk, hk_h, hk_m)
        !
        dh = max(0.0, Hklim - hk) / hk_h
        Dstr = Dstr + dh * Thet
        !
    end subroutine dslim
    !*==BLPINI.f90  processed by SPAG 7.21DC at 11:25 on 11 Jan 2019


    subroutine blpini
        use i_blpar
        implicit none
        !
        !*** Start of declarations rewritten by SPAG
        !
        !*** End of declarations rewritten by SPAG
        !
        !
        !*** Start of declarations rewritten by SPAG
        !
        !*** End of declarations rewritten by SPAG
        !
        !
        SCCon = 5.6
        !GACon = 6.70
        !GBCon = 0.75
        GCCon = 18.0
        DLCon = 0.9
        !
        !     DanEli 2018.10.24
        !---- original value of Drela     GACON = 6.70
        !---- original value of Drela     GBCON = 0.75
        !---- new values of van Rooij, section 3.3.4, page 16-17: GACON = 6.95, GBCON = 0.95
        !---- new values of van Rooij, section 3.3.4, page 16-17
        GACon = 5.5 !6.0 !6.7 !5.5 !5.0 !6.5 !6.75 !6.7 !6.75
        GBCon = 0.9 !0.75 !0.9 !0.83 !0.75 !0.83
        !good for DU97W300 with GACON=5.5, GBCON=0.8 and (A=0.3,B=2.5)
        !
        CTRcon = 1.8
        CTRcex = 3.3
        !
        DUXcon = 1.0
        !
        CTCon = 0.5 / (GACon**2 * GBCon)
        !
        CFFac = 1.0
        !
    end subroutine blpini

    subroutine statusvg(is)
        !     DanEli 2018.10.24
        !--------------------------------------------------------------------
        !     Determine the status of the vortex generator.
        !     The VG could be active only if its position lies ahead of:
        !     - the forced transition location
        !     - the natural transition location
        !
        !     - IS is the actual airfoil side, 1 top, 2 bottom
        !     - VGA VG activity, 1 true, 0 false
        !--------------------------------------------------------------------
            use i_xfoil
            use i_xbl
        !      
            !DO IS=1, 2
            IF ( XVG(IS) .LT. XOCTR(IS) ) THEN
                if (show_output) write (*,*) 'Side ',IS,' Vortex Generator is ahead of both', &
                                 &  ' natural and/or force transition: POSSIBLE BY-PASS TRANSITION'
                LVGS(IS) = .TRUE.
            ELSE
                if (show_output) write(*,*) 'Side ',IS,' Vortex Generator is beyond both', &
                                 &  ' natural and/or force transition: NO BY-PASS TRANSITION'
                IF ( XVG(IS) .LT. XTE ) THEN ! XVG on the surface
                    if (show_output) write(*,*) 'Side ',IS,' Vortex Generator is promoting', &
                                     &  ' an enhanced mixing in the turbulent BL'
                    LENHMIX(IS) = .TRUE.
                ENDIF
            ENDIF
            !ENDDO
        !      
            RETURN
    end subroutine statusvg

    subroutine xivgset(is)
        !-----------------------------------------------------
        !     Sets VG BL coordinate locations.
        !-----------------------------------------------------
        use m_spline, only: splind, sinvrt
        use i_xfoil
        use i_xbl

        IF(XVG(IS).GE.1.0) THEN
            XIVG(IS) = XSSI(IBLTE(IS),IS)
            RETURN
        ENDIF
        !
        CHX = XTE - XLE
        CHY = YTE - YLE
        CHSQ = CHX**2 + CHY**2
        !
        !---- calculate chord-based x/c, y/c
        do I=1, N
            W1(I) = ((X(I)-XLE)*CHX + (Y(I)-YLE)*CHY) / CHSQ
            W2(I) = ((Y(I)-YLE)*CHX - (X(I)-XLE)*CHY) / CHSQ
        end do
        !
           CALL SPLIND(W1,W3,S,N,-999.0,-999.0)
           CALL SPLIND(W2,W4,S,N,-999.0,-999.0)
        !
        IF(IS.EQ.1) THEN
            !
            !----- set approximate arc length of VG point for SINVRT
            STR = SLE + (S(1)-SLE)*XVG(IS)
            !
            !----- calculate actual arc length
            CALL SINVRT(STR,XVG(IS),W1,W3,S,N)
            !
            !----- set BL coordinate value
            XIVG(IS) = MIN( (SST - STR) , XSSI(IBLTE(IS),IS) )
            !
        ELSE
            !----- same for bottom side
            !
            STR = SLE + (S(N)-SLE)*XVG(IS)
            CALL SINVRT(STR,XVG(IS),W1,W3,S,N)
            XIVG(IS) = MIN( (STR - SST) , XSSI(IBLTE(IS),IS) )
            !
        ENDIF
        !
        IF(XIVG(IS) .LT. 0.0) THEN
            WRITE(*,1000) IS
            1000  FORMAT(/' ***  Stagnation point is past VG on side',I2,'  ***')
            XIVG(IS) = XSSI(IBLTE(IS),IS)
        ENDIF
        !
        !---- determine BL array index at vortex generator
        !
        do IVG = 1, IBLTE(IS)
            !        WRITE(*,*)'IVG',IVG,'of IBLTE(IS)',IBLTE(IS),
            !      &  'XIVG(IS)',XIVG(IS),' ~ XSSI(IVG,IS)',XSSI(IVG,IS) 
            IF (XIVG(IS) .LE. XSSI(IVG,IS)) THEN
                IBLVG(IS) = IVG;
                EXIT
            ENDIF
        end do  
        !
        RETURN
    end subroutine xivgset

    subroutine trvg(XPR, YPR, UPR, LOUT, IPR,  LTRVG)
        !-----------------------------------------------------------------
        !     Check for by-pass transition occurrence due to VG.
        !     The criteria follows the one stated in AIAA-2003-0211.
        !
        ! Inputs:
        ! - XPR, YPR, UPR    Dummy inputs
        ! - LOUT  Logical: true to enable the WRITE(*,*) commands
        !
        ! Output:
        ! - LTRVG Logical: true if by-pass transition occurs
        !-----------------------------------------------------------------
        use m_spline, only: sinvrt, seval, deval
        use i_xfoil
        use i_xbl
        real, dimension(*)::xpr, ypr, upr
        logical::lout, ltrvg
        intent (in) lout
        intent (out) ltrvg
        !      
        NPR = 2
        !DO IPR = 1, NPR
        !
        IF    (IPR .EQ. 1) THEN
            IF (LOUT) WRITE(*,*) 'XVG Top: ', XVG(1)
            XOC = XVG(1)
            SGN = 1.0
        ELSEIF(IPR .EQ. 2) THEN
            IF (LOUT) WRITE(*,*) 'XVG Bot: ', XVG(2)
            XOC = XVG(2)
            SGN = -1.0
        ENDIF
        !
        IF (NINPUT .GT. 0) THEN
            DPR = RINPUT(1)
        ELSE
            DPR = 0.01
        ENDIF
        !       
        IF(SGN .GT. 0.0) THEN
            SPR = SLE + (S(1)-SLE)*XOC
        ELSE
            SPR = SLE + (S(N)-SLE)*XOC
        ENDIF
        !
        XPRI = XLE + (XTE-XLE)*XOC
        CALL SINVRT(SPR,XPRI,X,XP,S,N)
        !
        DOFF = 0.00001*(S(N)-S(1))
        !       
        XPR(IPR) = SEVAL(SPR,X,XP,S,N) + DOFF*DEVAL(SPR,Y,YP,S,N)
        YPR(IPR) = SEVAL(SPR,Y,YP,S,N) - DOFF*DEVAL(SPR,X,XP,S,N)
        !
        CALL UBLGET(XPR(IPR),YPR(IPR),HVG(IPR),.false., show_output, UPR(IPR))
        !
        !----- display velocity profile value at HVG
        IF (LOUT) THEN
            IF    (IPR .EQ. 1) THEN
                WRITE(*,*) 'UVG Top: ', UPR(IPR)!,' at XPR=',XPR(IPR)
            ELSEIF(IPR .EQ. 2) THEN
                WRITE(*,*) 'UVG Bot: ', UPR(IPR)!,' at XPR=',XPR(IPR)
            ENDIF
        ENDIF
        !
        !----- check if by-pass transition occurs
        IF (abs(UPR(IPR))*HVG(IPR)*REYBL .GE. 600.) THEN
            !----- by-pass transition occurs       
            LTRVG = .TRUE.
            IF (LOUT) THEN
                WRITE(*,*) 'VG promotes by-pass transition'
                WRITE(*,9000) abs(UPR(IPR))*HVG(IPR)*REYBL, 600.
                9000 FORMAT('RE_VG =', F12.4, ' >= RE_CR =', F12.4)  
                XSTRIP(IPR) = XVG(IPR)
            ELSE
                if (show_output) write(*,*) 'Side ',IPR,' Transition forced at VG'
                XSTRIP(IPR) = XVG(IPR)
            ENDIF
        ELSE
            LTRVG = .FALSE.
            IF (LOUT) THEN
                WRITE(*,*) 'VG does not promote by-pass transition'
                WRITE(*,9001) abs(UPR(IPR))*HVG(IPR)*REYBL, 600. 
                9001 FORMAT('RE_VG =', F12.4, ' < RE_CR =', F12.4)
            ENDIF
        ENDIF
        !ENDDO
        RETURN

    end subroutine trvg

    SUBROUTINE UBLGET(XPR,YPR,HPR,DEBUG,LOUT,UPR)
        !-----------------------------------------------------------------
        !     DanEli 2018.10.24
        !     Display velocity profile taken from flow solution at a 
        !     certain distance from wall HPR.
        !
        !   XPR,YPR  coordinates of point through which profile axis passes
        !   HPR      distance from wall for sampling
        !   LOUT     Logical: true to enable the WRITE(*,*) commands
        !   UPR      velocity profile value at HPR distance from wall
        !-----------------------------------------------------------------
        use i_xfoil
        use m_xblsys, only: hkin
        !m_profil module is not converted to FORTRAN90 yet
        use m_profil, only: prwall, uwall, fs     
        
        PARAMETER (KPRX=129)
        DIMENSION YY(KPRX), UU(KPRX), FFS(KPRX), SFS(KPRX)
        
        CHARACTER*1 KCHAR
        LOGICAL TURB, DEBUG, LOUT
        INTEGER KVG ! index of HPR position within normal coordinate Y
        REAL WFUE1, WFUE2 ! weighting factor for interpolation of U edge
        !
        XC = XPR
        YC = YPR
        !
        !---- find nearest airfoil surface point
        RSQMIN = 1.0E23
        ISMIN = 0
        IBLMIN = 0
        DOFF = 0.00001*(S(N)-S(1))
        DO IS = 1, 2
            DO IBL = 2, IBLTE(IS)
                I = IPAN(IBL,IS)
                XSURF = X(I) + DOFF*YP(I)
                YSURF = Y(I) - DOFF*XP(I)
                RSQ = (XC-XSURF)**2 + (YC-YSURF)**2
                IF(RSQ .LE. RSQMIN) THEN
                    RSQMIN = RSQ
                    ISMIN = IS
                    IBLMIN = IBL
                ENDIF
            ENDDO
        ENDDO
        !
        IS = ISMIN
        IBL = IBLMIN
        !
        I = IPAN(IBL,IS)
        CRSP = (XC-X(I))*NY(I) - (YC-Y(I))*NX(I)
        IF(IS.EQ.2) CRSP = -CRSP
            !
            IF(CRSP.GT.0.0) THEN
                IBLP = IBL+1
                IBLO = IBL
            ELSE
                IBLP = IBL
                IBLO = IBL-1
            ENDIF
        ISP = IS
        ISO = IS
        !
        IF(IBLP.GT.IBLTE(IS)) THEN
            IBLP = IBLTE(IS)
            IBLO = IBLP-1
            IBL = IBLTE(IS)
        ELSEIF(IBLO.LT.2) THEN
            IBLO = 2
            IF(ISO.EQ.1) THEN
                ISO = 2
            ELSE
                ISO = 1
            ENDIF
        ENDIF
        !
        IP = IPAN(IBLP,ISP)
        IO = IPAN(IBLO,ISO)
        !
        !---- set interpolation fraction at profile location
        DX = X(IP) - X(IO)
        DY = Y(IP) - Y(IO)
        VX = XC - X(IO)
        VY = YC - Y(IO)
        FRAC = (DX*VX + DY*VY)/(DX*DX+DY*DY)
        FRAC = MIN( MAX( FRAC , 0.0 ) , 1.0 )
        !
        !---- set averaged displacement vector at profile location
        CA = FRAC*NY(IP) + (1.0-FRAC)*NY(IO)
        SA = FRAC*NX(IP) + (1.0-FRAC)*NX(IO)
        CSMOD = SQRT(CA**2 + SA**2)
        CA = CA/CSMOD
        SA = SA/CSMOD
        !
        X0 = FRAC*X(IP) + (1.0-FRAC)*X(IO)
        Y0 = FRAC*Y(IP) + (1.0-FRAC)*Y(IO)
        !
        DS = FRAC*DSTR(IBLP,ISP) + (1.0-FRAC)*DSTR(IBLO,ISO)
        TH = FRAC*THET(IBLP,ISP) + (1.0-FRAC)*THET(IBLO,ISO)
        UE = FRAC*UEDG(IBLP,ISP) + (1.0-FRAC)*UEDG(IBLO,ISO)
        !
        XI = FRAC*XSSI(IBLP,ISP) + (1.0-FRAC)*XSSI(IBLO,ISO)
        TURB = XI .GT. XSSITR(IS)
        !
        !---- 1 / (total enthalpy)
        HSTINV = GAMM1*(MINF/QINF)**2 / (1.0 + 0.5*GAMM1*MINF**2)
        !
        !---- Sutherland's const./To   (assumes stagnation conditions are at STP)
        HVRAT = 0.35
        !
        !---- fill Rtheta arrays
        UEC = UE * (1.0-TKLAM) / (1.0 - TKLAM*(UE/QINF)**2)
        HERAT = (1.0 - 0.5*HSTINV*UEC **2)/(1.0 - 0.5*HSTINV*QINF**2)
        RHOE = HERAT ** (1.0/GAMM1)
        AMUE = SQRT(HERAT**3) * (1.0+HVRAT)/(HERAT+HVRAT)
        RTHETA = REINF * RHOE*UE*TH/AMUE
        !
        AMSQ = UEC*UEC*HSTINV / (GAMM1*(1.0 - 0.5*UEC*UEC*HSTINV))
        CALL HKIN( DS/TH, AMSQ, HK, DUMMY, DUMMY)
        !
        !---- calculate kinematic shape parameter (assuming air)
        !     (from Whitfield )
        !
        IF (DEBUG) THEN
            WRITE(*,9100) X0,Y0, DS, RTHETA, HK
            9100  FORMAT(1X,'x y =', 2F8.4,'    Delta* =', G12.4, &
                        &'    Rtheta =', F10.2,'    Hk =', F9.4)
        ENDIF
        !
        IF(IS.EQ.1) THEN
            UDIR = 1.0
        ELSE
            UDIR = -1.0
        ENDIF
        !
        UEI = UE/QINF
        UN = 0.0
        !
        NN = KPRX
        UO = 1.0
        !---- compute kinematic displacement thickness including compressibility (rho)
        !     effects by means of HK, the kinematic shape factor, see HKIN 
        DK = HK*TH
        CT = 0.
        !
        IF (DEBUG) THEN
            WRITE(*,9200) DK, TH, UE, QINF
            9200 FORMAT(1X,'Delta* kin. =', G12.4,'    Theta kin. =', G12.4, &
             &       '    U edge =', F10.6,'    Q inf =', F9.4)
        ENDIF
        !     
        IF(TURB) THEN
             !----- set Spalding + power-law turbulent profile
            CALL PRWALL(DK,TH,UO,RTHETA,AMSQ,CT, BB, &
             &        DE, DE_DS, DE_TH, DE_UO, DE_RT, DE_MS, &
             &        US, US_DS, US_TH, US_UO, US_RT, US_MS, &
             &        HS, HS_DS, HS_TH, HS_UO, HS_RT, HS_MS, &
             &        CF, CF_DS, CF_TH, CF_UO, CF_RT, CF_MS, &
             &        CD, CD_DS, CD_TH, CD_UO, CD_RT, CD_MS, &
             &        CD_CT)
            CALL UWALL(TH,UO,DE,US,RTHETA,CF,BB, YY,UU,NN)
            !      SUBROUTINE UWALL(TH,UO,DO,UI,RT,CF,BB, Y,U,N)
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
            DO K=1, NN
                UU(K) = UU(K)*UEI
            ENDDO
        ELSE
            !----- set Falkner-Skan profile
            INORM = 3
            ISPEC = 2
            HSPEC = HK
            ETAE = 1.5*(3.15 + 1.72/(HK-1.0) + HK)
            GEO = 1.0
            CALL FS(INORM,ISPEC,BU,HSPEC,NN,ETAE,GEO,YY,FFS,UU,SFS,DEFS)
            !      SUBROUTINE FS(INORM,ISPEC,BSPEC,HSPEC,N,ETAE,GEO,ETA,F,U,S,DELTA)
            !-----------------------------------------------------
            !     Routine for solving the Falkner-Skan equation.
            !
            !     Input:
            !     ------
            !      INORM   1: eta = y / sqrt(vx/Ue)  "standard" Falkner-Skan coordinate
            !              2: eta = y / sqrt(2vx/(m+1)Ue)  Hartree's coordinate
            !              3: eta = y / Theta  momentum thickness normalized coordinate
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
            !----- boundary layer thickness delta edge DE
            DE = ETAE*TH
            DO K=1, NN
                YY(K) = YY(K)*TH
                UU(K) = UU(K)*UEI
            ENDDO
        ENDIF
        !
        !---- check if VG lies within the boundary layer thickness
        IF (YY(NN) .LT. HPR .and. LOUT) THEN
            WRITE(*,*) '*************************************************'
            WRITE(*,*) '*              UBLGET in dplot.f                *'
            WRITE(*,*) '*             VG lies outside BL                *'
            WRITE(*,*) '* DELTA = ', YY(NN)
            WRITE(*,*) '* HVG   = ', HPR
            WRITE(*,*) '*          VG Shrinked to BL height.            *'
            WRITE(*,*) '*            Consider new sizing!               *'
            WRITE(*,*) '*************************************************'
            HPR = YY(NN)
            IF (DEBUG) THEN
                WRITE(*,*) 'VG lies outside BL: Consider new sizing!'
            ENDIF 
        ELSE
            IF (DEBUG) THEN
                WRITE(*,*) 'VG lies inside BL'
            ENDIF
        ENDIF
        !      
        !---- detect velocity value at input distance from wall HPR
        !
        DO K = 1, NN
            !------ check if YY(K) is bigger than HPR
            IF (YY(K) .GE. HPR) THEN
                KVG = K
                EXIT
            ENDIF
        ENDDO
        !
        !---- summary of information
        !WRITE(*,*) KVG
        WFUE1 =  (YY(KVG) - HPR)  / (YY(KVG) - YY(KVG-1))
        WFUE2 = (HPR - YY(KVG-1)) / (YY(KVG) - YY(KVG-1))
        UPR   = WFUE1*UU(KVG - 1) + WFUE2*UU(KVG)
        !
        IF (DEBUG) THEN
            WRITE(*,9300) HPR, UPR
            9300  FORMAT(1X,'HVG =', F8.4, '    UVG =', F8.4) 
            !
            WRITE(*,9400) DE
            9400  FORMAT(1X,'Delta edge =', G12.4)      
            !
            WRITE(*,*) 'Y(i) normal coordinate, U(i) velocity profile'
            DO K = 1, NN
                WRITE(*,'(1x,5f10.6)') YY(K), UU(K)
            ENDDO
        ENDIF
        !      
        RETURN
    END subroutine UBLGET

end module m_xbl