      DOUBLE PRECISION FUNCTION READA(A,ISTART)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      CHARACTER*1 A(80)
   10 FORMAT('   IN READA')
      NINE=ICHAR('9')
      IZERO=ICHAR('0')
      MINUS=ICHAR('-')
      IDOT=ICHAR('.')
      IDIG=0
      K1=0
      K2=0
      ONE=1.D0
      X = 1.D0
      DO 20 J=ISTART,80
         N=ICHAR(A(J))
         IF(N.LE.NINE.AND.N.GE.IZERO .OR. N.EQ.MINUS.OR.N.EQ.IDOT)GOTO 3
     10
   20 CONTINUE
      READA=0.D0
      RETURN
   30 CONTINUE
      DO 40 I=J,80
         N=ICHAR(A(I))
         IF(N.LE.NINE.AND.N.GE.IZERO) THEN
C#         WRITE (*,*) N
            IDIG=IDIG+1
            IF (IDIG.GT.10) GOTO 70
            K1=K1*10+N-IZERO
         ELSEIF(N.EQ.MINUS.AND.I.EQ.J) THEN
            ONE=-1.D0
         ELSEIF(N.EQ.IDOT) THEN
            GOTO 50
         ELSE
            GOTO 70
         ENDIF
   40 CONTINUE
   50 CONTINUE
      IDIG=0
      DO 60 II=I+1,80
         N=ICHAR(A(II))
         IF(N.LE.NINE.AND.N.GE.IZERO) THEN
C#         WRITE (*,*) N
            IDIG=IDIG+1
            IF (IDIG.GT.10) GOTO 70
            K2=K2*10+N-IZERO
            X = X /10
         ELSEIF(N.EQ.MINUS.AND.II.EQ.I) THEN
            X=-X
         ELSE
            GOTO 70
         ENDIF
   60 CONTINUE
C
C PUT THE PIECES TOGETHER
C
   70 CONTINUE
C#      WRITE (*,*) ONE*(K1+K2*X)
      READA= ONE * ( K1 + K2 * X)
      RETURN
      END
