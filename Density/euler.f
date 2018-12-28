      SUBROUTINE EULER(PX,PY,PZ,I1,I2,I3,IPEN)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
************************************************************************
*
* EULER DOES A SIMPLE EULERIAN TRANSFORM ON THE POINT PX,PY,PZ TO
* PRODUCE A POINT FOR PLOTTING. IT HAS EXTRA CODE TO ALLOW THE USER
* TO SPECIFY WHICH TWO DIMENSIONS ARE TO BE PLOTTED.
* ON INPUT:  IF I1 IS ZERO THEN
*            PX,PY,PZ = COMPONENTS OF VECTOR PERPENDICULAR TO THE PLOT
*                       OF THE GRAPH. THIS WOULD BE (0,0,1) FOR THE
*                       NORMAL X-Y PLOT.
*            I2       = IF NOT EQUAL TO ZERO THEN THE DASH LENGTH IN A
*                       DASHED LINE (USE ZERO - THIS CODE IS
*                       BADLY WRITTEN)
*            I3       = IF NOT EQUAL TO ZERO THEN THE GAP LENGTH IN A
*                       DASHED LINE (USE ZERO - THIS CODE IS
*                       BADLY WRITTEN)
*    THE FIRST CALL OF EULER MUST HAVE I1 EQUAL TO ZERO IN ORDER TO
*    SET THE PLANE OF THE PLOT
*            IF I1 IS NON-ZERO THEN
*            PX,PY,PZ = POINT IN 3-D SPACE TO MOVE THE PEN TO.
*            I1,I2,I3:  NORMALLY THE X-Y PLOT IS TO BE DRAWN, IN
*                       WHICH CASE THESE SHOULD BE 1,2,3. IF THE X-Z
*                       PLOT IS TO BE DRAWN PUT 1,3,2; IF Y-Z PUT 2,3,1.
*                       IF Y-X THEN 2 1 3.
*            IPEN     = 2 DO NOT DRAW A LINE, = 3 DRAW A LINE.
************************************************************************
      DIMENSION C(3), BOUNDS(2,4)
      DATA      DASH,GAP/1.D0,0.D0/
      DATA BOUNDS/0.D0,0.D0,0.D0,1.D0,1.D0,0.D0,1.D0,1.D0/
      IF(I1.EQ.0) THEN
         DASH=1.D0/MAX(I2,1)
         GAP =1.D0/MAX(I3,1)
*
* SET UP COS AND SINE ANGLES FOR EULARIAN TRANSFORM.
*
         X=PX
         Y=PY
         Z=PZ
         XY=X*X+Y*Y
         IF(XY.GT.1.D-15) THEN
            R1=DSQRT(XY+Z*Z)
            XY=DSQRT(XY)
            CA=X/XY
            CB=Z/R1
            SA=Y/XY
            SB=XY/R1
         ELSE
         CA=1.D0
         CB=1.D0
         SA=0.D0
         SB=0.D0
         ENDIF
         RETURN
      ELSEIF(I1.LT.0)THEN
         XMIN=1.D6
         YMIN=1.D6
         XMAX=-1.D6
         YMAX=-1.D6
         DO 44 I=1,4
         CX=-SA*BOUNDS(1,I)+CA*BOUNDS(2,I)
         CY=CA*CB*BOUNDS(1,I)+SA*CB*BOUNDS(2,I) -SB*PZ
         XMIN=MIN(CX,XMIN)
         YMIN=MIN(CY,YMIN)
         XMAX=MAX(CX,XMAX)
         YMAX=MAX(CY,YMAX)
  44     SCALE=MIN(1.D0/(XMAX-XMIN+1.D-6),1.D0/(YMAX-YMIN+1.D-6))
         RETURN
         ELSE
*
*  SWAP AROUND THE POINTS TO BE PLOTTED.
*
         C(I1)=PX
         C(I2)=PY
         C(I3)=PZ
         X=C(1)
         Y=C(2)
         Z=C(3)
*
*  NOW DO THE EULARIAN TRANSFORM ITSELF.
*
         CX=-SA*X+CA*Y
         CY=CA*CB*X+SA*CB*Y-SB*Z
         CX=(CX-XMIN)*SCALE
         CY=(CY-YMIN)*SCALE
*G         CALL PLOTGR(IPEN,CX,CY,DASH,GAP)
         RETURN
      ENDIF
      END
