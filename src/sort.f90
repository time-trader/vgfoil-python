SUBROUTINE HSORT(N, A, INDX)
    DIMENSION A(*)
    DIMENSION INDX(*)
    !--------------------------------------
    !     Heapsort algorithm.
    !     Returns INDX(.) such that
    !
    !       A(INDX(i)) < A(INDX(i+1))
    !
    !     Stolen from Numerical Recipes.
    !--------------------------------------
    !
    DO I = 1, N
        INDX(I) = I
    ENDDO
    !
    IF(N <= 1) RETURN
    !
    L = N / 2 + 1
    IR = N
    !
    10   CONTINUE
    IF(L > 1) THEN
        L = L - 1
        INDXT = INDX(L)
        Q = A(INDXT)
    ELSE
        INDXT = INDX(IR)
        Q = A(INDXT)
        INDX(IR) = INDX(1)
        !
        IR = IR - 1
        IF(IR == 1) THEN
            INDX(1) = INDXT
            RETURN
        ENDIF
    ENDIF
    !
    I = L
    J = L + L
    !
    20   IF(J <= IR) THEN
        IF(J < IR) THEN
            IF(A(INDX(J)) < A(INDX(J + 1))) J = J + 1
        ENDIF
        IF(Q < A(INDX(J))) THEN
            INDX(I) = INDX(J)
            !
            I = J
            J = J + J
        ELSE
            J = IR + 1
        ENDIF
        GO TO 20
    ENDIF
    !
    INDX(I) = INDXT
    GO TO 10
END

SUBROUTINE ASORT(N, A, INDX, ATMP)
    DIMENSION A(*), ATMP(*)
    DIMENSION INDX(*)
    !-----------------------------------------------
    !     Applies sorted index array to reorder A.
    !-----------------------------------------------
    DO I = 1, N
        ATMP(I) = A(I)
    ENDDO
    !
    DO I = 1, N
        ISORT = INDX(I)
        A(I) = ATMP(ISORT)
    ENDDO
    !
    RETURN
END

SUBROUTINE REMD(N, A, INDX, TOL, NNEW)
    DIMENSION A(*)
    DIMENSION INDX(*)
    !----------------------------------------------------
    !     Sets index array, such that
    !     duplicate A values are left out
    !----------------------------------------------------
    K = 1
    INDX(K) = 1
    !
    DO I = 2, N
        IF(ABS(A(I) - A(I - 1)) > TOL) THEN
            K = K + 1
            INDX(K) = I
        ENDIF
    ENDDO
    !
    NNEW = K
    !
    RETURN
END
! REMD


SUBROUTINE SORTDUP(KK, S, W)
    !--- Sort arrays in S with no removal of duplicates
    DIMENSION S(KK), W(KK)
    LOGICAL DONE
    !
    !---- sort arrays
    DO IPASS = 1, 1234
        DONE = .TRUE.
        DO N = 1, KK - 1
            NP = N + 1
            IF(S(NP) >= S(N)) GO TO 101
            TEMP = S(NP)
            S(NP) = S(N)
            S(N) = TEMP
            TEMP = W(NP)
            W(NP) = W(N)
            W(N) = TEMP
            DONE = .FALSE.
        101   CONTINUE
        end do
        IF(DONE) GO TO 11
    end do
    WRITE(*, *) 'Sort failed'
    !
    11 CONTINUE
    RETURN
END


SUBROUTINE FIXDUP(KK, S, W)
    !--- Check arrays in S by removing leading and ending duplicates
    !    eliminate extra duplicates (more than one duplicate point) elsewhere
    DIMENSION S(KK), W(KK)
    LOGICAL DONE
    !
    5    CONTINUE
    DONE = .TRUE.

    !---- Check first elements for dups
    IF(S(2) == S(1)) THEN
        DO N = 1, KK - 1
            S(N) = S(N + 1)
            W(N) = W(N + 1)
        END DO
        KK = KK - 1
        DONE = .FALSE.
    ENDIF
    !
    !---- Check last elements for dups
    IF(S(KK) == S(KK - 1)) THEN
        S(KK - 1) = S(KK)
        W(KK - 1) = W(KK)
        KK = KK - 1
        DONE = .FALSE.
    ENDIF
    !
    !--- Eliminate more than 2 succeeding identical elements
    10   CONTINUE
    DO N = 1, KK - 2
        IF(S(N) == S(N + 1) .AND. S(N) == S(N + 2)) THEN
            DO I = N, KK - 1
                S(I) = S(I + 1)
                W(I) = W(I + 1)
            END DO
            KK = KK - 1
            GO TO 10
        ENDIF
    END DO
    !
    IF(DONE) THEN
        RETURN
    ELSE
        GO TO 5
    ENDIF

END
! FIXDUP



SUBROUTINE SORT(KK, S, W)
    DIMENSION S(KK), W(KK)
    LOGICAL DONE
    !
    !---- sort arrays
    DO IPASS = 1, 1234
        DONE = .TRUE.
        DO N = 1, KK - 1
            NP = N + 1
            IF(S(NP) >= S(N)) GO TO 101
            TEMP = S(NP)
            S(NP) = S(N)
            S(N) = TEMP
            TEMP = W(NP)
            W(NP) = W(N)
            W(N) = TEMP
            DONE = .FALSE.
        101   CONTINUE
        end do
        IF(DONE) GO TO 11
    end do
    WRITE(*, *) 'Sort failed'
    !
    !---- search for duplicate pairs and eliminate each one
    11 KKS = KK
    DO K = 1, KKS
        IF(K >= KK) RETURN
        IF(S(K) /= S(K + 1)) GO TO 20
        !------- eliminate pair
        KK = KK - 2
        DO KT = K, KK
            S(KT) = S(KT + 2)
            W(KT) = W(KT + 2)
        end do
    20 CONTINUE
    end do
    !
    RETURN
END


SUBROUTINE SORTOL(TOL, KK, S, W)
    DIMENSION S(KK), W(KK)
    LOGICAL DONE
    !
    !---- sort arrays
    DO IPASS = 1, 1234
        DONE = .TRUE.
        DO N = 1, KK - 1
            NP = N + 1
            IF(S(NP) < S(N)) THEN
                TEMP = S(NP)
                S(NP) = S(N)
                S(N) = TEMP
                TEMP = W(NP)
                W(NP) = W(N)
                W(N) = TEMP
                DONE = .FALSE.
            ENDIF
        END DO
        IF(DONE) GO TO 10
    END DO
    WRITE(*, *) 'Sort failed'
    !
    !---- search for near-duplicate pairs and eliminate extra points
    !---- Modified 4/24/01 HHY to check list until ALL duplicates removed
    !     This cures a bug for sharp LE foils where there were 3 LE points in
    !     camber, thickness lists from GETCAM.
    !
    10   KKS = KK
    DONE = .TRUE.
    DO K = 1, KKS
        IF(K >= KK) GO TO 20
        DSQ = (S(K) - S(K + 1))**2 + (W(K) - W(K + 1))**2
        IF(DSQ >= TOL * TOL) GO TO 20
        !------- eliminate extra point pairs
        !cc         write(*,*) 'extra on point ',k,kks
        KK = KK - 1
        DO KT = K + 1, KK
            S(KT) = S(KT + 1)
            W(KT) = W(KT + 1)
        END DO
        DONE = .FALSE.
    20 CONTINUE
    end do
    IF(.NOT.DONE) GO TO 10
    !
    RETURN
END