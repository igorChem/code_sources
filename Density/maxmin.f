      SUBROUTINE MAXMIN(M,GRID,MDIM,N)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/INFO/AMAX,AMIN,STEP
      DIMENSION GRID(MDIM,*)
***********************************************************************
* MAXMIN LOCATES THE LOCAL MAXIMA AND MINIMA IN THE ARRAY GRID.
*        THE LIMITS OF GRID ARE DEFINED AS 0.0 - 1.0 IN BOTH DIRECTIONS
* IN ORDER, THE ARGUMENTS ARE:
*   M      =   1  ALL MINIMA ARE LOCATED.
*   M      =  -1  ALL MAXIMA ARE LOCATED.
*   GRID   =   2-D ARRAY.
*   MDIM   =   SIZE OF FIRST DIMENSION OF MDIM.
*   N      =   SIZE OF ARRAY OF POINTS WITHIN GRID.
*              (CAN BE THE SAME AS MDIM)
*
***********************************************************************
      AMAX=0.D0
      IF(M.EQ.-1) WRITE(6,'(/15X,''CALCULATION OF MAXIMA'')')
      IF(M.EQ. 1) WRITE(6,'(/15X,''CALCULATION OF MINIMA'')')
      IF (M.EQ.1) AMIN=1.D0
      IF (M.LT.0) THEN
         DO 10 I=1,N
            DO 10 J=1,N
   10    GRID(I,J)=-GRID(I,J)
      ENDIF
      NM1=N-1
      F=1.D0/NM1
      WRITE(6,20)
   20 FORMAT(//12X,'LOCAL TURNING POINTS',/,'  X-DIRECTION  Y-DIRECTION
     1   VALUE',/)
      DO 30 I=2,NM1
         DO 30 J=2,NM1
            IF(GRID(I,J).GT.GRID(I,J+1)
     1       .OR.GRID(I,J).GT.GRID(I,J-1))GOTO 30
            IF(GRID(I,J).GT.GRID(I+1,J)
     1       .OR.GRID(I,J).GT.GRID(I-1,J))GOTO 30
C
C     I AND J ARE IN THE REGION OF A TURNING POINT
C     NOW TO WORK OUT THE VALUE OF THE TURNING POINT.
            A=GRID(I,J-1)
            B=GRID(I,J)
            C=GRID(I,J+1)
            IF(DABS(A+C-B-B).LT.1.D-8)GOTO 30
            X=J-1.0+0.5*(A-C)/(A+C-B-B)
            D=(C-A)*(C-A)/(8*(C+A-B-B))
            A=GRID(I-1,J)
            C=GRID(I+1,J)
            IF(DABS(A+C-B-B).LT.1.D-8)GOTO 30
            Y=I-1.0+0.5*(A-C)/(A+C-B-B)
            X1=X*F
            Y1=Y*F
            POINT=B-D-(C-A)*(C-A)/(8*(C+A-B-B))
            POINT=POINT*M
C
C  COORDINATES OF TURNING POINT =(X1,Y1)
C  VALUE OF TURNING POINT       = POINT
C
            WRITE(6,'(F11.4,F13.4,F13.4)')X1,Y1,POINT
            IF (M.EQ.1) THEN
               IF (AMIN.GT.POINT) AMIN=POINT
            ELSE
               IF (AMAX.LT.POINT) AMAX=POINT
            ENDIF
   30 CONTINUE
      IF(M.LT.0) THEN
         DO 40 I=1,N
            DO 40 J=1,N
   40    GRID(I,J)=-GRID(I,J)
      ENDIF
      RETURN
      END
