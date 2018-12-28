      SUBROUTINE DRAW(IZ,JZ,JDIR,A,B,C,PZ,I1,I2,I3,MDIM)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      LOGICAL B(25000),C(18000,2)
      DIMENSION A(MDIM,MDIM), IDIRX(6), IDIRY(6)
      LOGICAL AI,AJ,AK,LC(4)
***********************************************************************
*
*   DRAW DRAWS CONTOURS, STARTING AT POINT IZ,JZ.
*   JDIR = STARTING DIRECTION, IF 1 THEN IN +X DIRECTION
*                                 2 THEN IN +Y DIRECTION
*                                 3 THEN IN -X DIRECTION
*                                 4 THEN IN -Y DIRECTION
*    A   = ARRAY TO BE PLOTTED
*    B,C = WORK SPACES OF SIZE SUITABLE TO HOLD 5000 AND 4000 LOGICAL
*          ELEMENTS.
*    PZ  = VALUE OF THIRD DIMENSION. SET TO ZERO IF NOT WANTED.
*    I1,I2,I3 = ORDER OF CARTESIAN DIRECTIONS, X=1,Y=2,Z=3. FIRST TWO
*          ARE PLOTTED, SO TO DRAW THE X-Y PLOT USE 1,2,3.
*    MDIM= SIZE OF FIRST DIMENSION OF A.
***********************************************************************
      COMMON/CNTR/R1,IMAX,JMAX
      EQUIVALENCE (LC(1),ILC)
      DATA IDIRX/1,0,-1,0,1,0/,IDIRY/0,1,0,-1,0,1/
      ICOUNT =-5
      IDIR = JDIR
      ILC =0
      IPEN=2
      I=IZ
      J=JZ
   10 AA=A(I,J)
      AI= (AA.LT.0.)
      IDX= IDIRX(IDIR)
      IDY= IDIRY(IDIR)
      AB= A(I+IDX,J+IDY)
      FACTOR= AA/(AA-AB)
      PY = (IMAX-I-FACTOR*IDX)/(IMAX-1)
      PX = (JMAX-J-FACTOR*IDY)/(JMAX-1)
      CALL EULER(PX,PY,PZ,I1,I2,I3,IPEN)
      IF(IPEN.EQ.3.OR.R1.GT.0.D0) R1=R1+DSQRT((CX-PX)**2+(CY-PY)**2)
      CY=PY
      CX=PX
      ICOUNT= ICOUNT+1
   20 IF (IDIR/2*2.NE.IDIR) GOTO 50
      IF (IDIR.NE.2) GOTO 30
      NUMBER=J
      M=1
      GOTO 40
   30 NUMBER= J-1
      M= IMAX
   40 K= (NUMBER-1)*IMAX +I
      IF (.NOT.B(K)) RETURN
      B(K)= .FALSE.
      IF (I.EQ.M) RETURN
      GOTO 60
   50 IF (J.NE.1.AND.J.NE.JMAX) GOTO 60
      NUMBER= IDIR*J
      IF (NUMBER.NE.3.AND.NUMBER.NE.JMAX) GOTO 60
      IF (NUMBER.EQ.3) THEN
         C(I-1,1) = .FALSE.
      ELSE
         C(I,2)= .FALSE.
      ENDIF
      RETURN
   60 IPEN=3
      IDDX= I+IDIRX(IDIR+1)
      IDDY= J+ IDIRY(IDIR+1)
      AC= A(IDDX+IDX,IDDY+IDY)
      AJ= (AC.LT.0.)
      AD= A(IDDX,IDDY)
      AK= (AD.LT.0.)
      FA=1
      IF (AJ.AND.AK) GOTO 90
      IF (.NOT.(AJ.OR.AK)) GOTO 100
      FA=0.
      IF ((AI.OR..NOT.AK).AND.(.NOT.AI.OR.AK)) GOTO 80
      FA= AA*AC-AB*AD
      IF (FA.GE.0.) GOTO 80
   70 IDIR = IDIR+1
      IF (IDIR.EQ.5) IDIR =1
      GO TO 10
   80 I=IDDX
      J=IDDY
      IF(FA.EQ.0)GO TO 10
      I=I+IDX
      J=J+IDY
      IDIR=IDIR-1
      IF(IDIR.EQ.0) IDIR=4
      GO TO 10
   90 IF(AI)GO TO 80
      GO TO 70
  100 IF(AI)GO TO 70
      GO TO 80
      END
