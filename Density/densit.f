      PROGRAM DENSIT
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      INCLUDE 'SIZES'
************************************************************************
*
* GENERAL ELECTRON DENSITY DRAWING PROGRAM
*
************************************************************************
      LOGICAL LPIC
      COMMON /LINES / PX, PY, PZ, CX, CY, CZ, R, IPSI
      COMMON /VECTRS/ VECTRS(MAXORB,MAXORB)
      COMMON /DENSTY/ P(MAXORB,MAXORB)
      COMMON /KEYWRD/ KEYWRD
      COMMON /TVECTS/ TVEC(3,3), ID
      COMMON /SPHERE/ AVERAG(MAXORB)
      COMMON /COORD / XYZ(3,NUMATM)
      COMMON /MOLDAT/ NATOMS, NORBS, NELECS, NFIRST(NUMATM),
     1                NLAST(NUMATM), IDUMY, EMU(NUMATM,3), NPQ(NUMATM),
     2                NAT(NUMATM)
      COMMON /OCCUP / IOCC(MAXORB)
      COMMON /UCELL / L1L,L2L,L3L,L1U,L2U,L3U
      COMMON /CNTVAL/ CNTVAL
      DIMENSION PSI(MAXORB), ZEROS(MAXORB), HERE(3), IWORK(200),
     1 CONSTS(NUMATM), CONSTP(NUMATM), CONSTD(NUMATM),
     2 EMUS(NUMATM), EMUP(NUMATM), EMUD(NUMATM), PQN(NUMATM), SETVAL(20)
     3 , RSYM(NUMATM), ISYM(NUMATM), JSYM(NUMATM), PAA(MAXORB)
*
* ARRAYS WHICH CANNOT BE PARAMETER DIMENSIONED
*
      DIMENSION  IJK(3,8), FA(20), EDENS(40000), LIMS(3,2)
      LOGICAL  BONDS, MANUAL, DEBUG, FINE, MONO, CNTRS,
     1 ADD, EUCLID
      CHARACTER ELEMNT(99)*2, GETNAM*80
      CHARACTER TITLE*80, KEYWRD*80
      EQUIVALENCE (EMU(1,1),EMUS(1)),
     1            (EMU(1,2),EMUP(1)),
     2            (EMU(1,3),EMUD(1))
      EQUIVALENCE (L1L,LIMS(1,1)),(HERE(1),X),(HERE(2),Y),
     1(HERE(3),Z)
      DATA IJK/1,1,1,1,1,0,1,0,1,1,0,0,0,1,1,0,1,0,0,0,1,0,0,0/
      DATA ELEMNT/' H','++',
     1 'Li','Be',' B',' C',' N',' O',' F',' +',
     2 'Na','Mg','Al','Si',' P',' S','Cl','--',
     3 ' K','Ca','Sc','Ti',' V','Cr','Mn','Fe','Co','Ni','Cu',
     4 'Zn','Ga','Ge','As','Se','Br',' -',
     5 'Rb','Sr',' Y','Zr','Nb','Mo','Tc','Ru','Rh','Pd','Ag',
     6 'Cd','In','Sn','Sb','Te',' I','Xe',
     7 'Cs','Ba','La','Ce','Pr','Nd','Pm','Sm','Eu','Gd','Tb','Dy',
     8 'Ho','Er','Tm','Yb','Lu','Hf','Ta',' W','Re','Os','Ir','Pt',
     9 'Au','Hg','Tl','Pb','Bi','Po','At','Rn',
     1 'Fr','Ra','Ac','Th','Pa',' U','Np','Pu','Am','Cm','Bk','Cf','XX'/
      DATA SETVAL/1.D-4, 2.5D-4, 5.D-4, 1.D-3, 2.5D-3, 5.D-3,
     1            1.D-2, 2.5D-2, 5.D-2, 1.D-1, 2.5D-1, 5.D-1,
     2            1.D 0, 2.5D 0, 5.D 0, 1.D 1, 2.5D 1, 5.D 1,
     3            1.D 2, 2.5D 2/
      DATA ISYM/NUMATM*0/, JSYM/NUMATM*0/
      DATA RSYM/NUMATM*0.5D0/
      DATA ZEROS/MAXORB*0.D0/
      IC=0
      JC=0
*
* SOME SIMPLE BOOK-KEEPING - SET UP AN ARRAY OF FACTORIALS
      FA(2)=DSQRT(2.D0)
      DO 10 I=4,20,2
   10 FA(I)=FA(I-2)*DSQRT(I*I-I*1.D0)
      LPIC=.TRUE.
*
*  FIRST, READ IN THE KEY-WORDS
*
      OPEN(UNIT=5,FILE=GETNAM('FOR005'))
      OPEN(UNIT=6,FILE=GETNAM('FOR006'))
      READ(5,'(A)')KEYWRD,TITLE
      WRITE(6,'('' '',76(''*''),
     1/,'' *'',74X,''*'',
     2/,'' *'',18X,''ELECTRON DENSITY MAP''
     3        ,20X,''VERSION'',F7.2,''  *'',
     4/,'' *'',74X,''*'',
     5/,'' '',76(''*''))')VERSON
      WRITE(6,'(//1X,A)')TITLE
      DEBUG=(INDEX(KEYWRD,'DEBUG').NE.0)
      MONO =(INDEX(KEYWRD,'MONO').NE.0)
      MANUAL=(INDEX(KEYWRD,'MANUAL').NE.0)
      CALL READIN (MANUAL)
      DO 20 I=1,ID
         LIMS(I,1)=-1
   20 LIMS(I,2)= 1
      DO 30 I=ID+1,3
         LIMS(I,1)=0
   30 LIMS(I,2)=0
C
C  ALL THE WORKING IS IN ANGSTROMS, THEREFORE CONVERT EXPONENTS INTO
C  ANGSTROMS.
      DO 40 I=1,NUMATM
         DO 40 J=1,3
   40 EMU(I,J)=MAX(0.01D0,EMU(I,J)/0.529167D0)
*
*
* SETTING UP OF ORBITAL AND GEOMETRIC CONSTANTS FOR DENSITY MAP
*
*
      DO 50 I=1,NATOMS
         J=2*NPQ(I)
         PQN(I)=NPQ(I)
         IF(J.GT.6)PQN(I)=PQN(I)-1.D0
         K=PQN(I)*2.01
C
C  0.282095  =  SQRT( 1/(4*PI))
C  0.48860   =  SQRT( 3/(4*PI))
C  1.092547  =  SQRT(15/(4*PI))
C
C   FOR SLATER-TYPE ORBITALS RADIAL NORMALISATION CONSTANT
C
C    2**(PQN+1/2)*SQRT(FACTORIAL((2*PQN)))
C
C   ANGULAR NORMALISATION CONSTANTS
C
C   S:   1,   P:  SQRT(3),   D: SQRT(15)
C
         CONSTS(I)=(2.D0*EMUS(I))**(NPQ(I)+0.5D0)/FA(J)*0.282095
         CONSTP(I)=(2.D0*EMUP(I))**(NPQ(I)+0.5D0)/FA(J)*0.48860
   50 CONSTD(I)=(2.D0*EMUD(I))**(PQN(I)+0.5D0)/FA(K)*1.092547
      WRITE(6,60)
   60 FORMAT(//25X,'COORDINATES',//,20X,'X', 9X,'Y',9X,'Z',/)
      WRITE(6,70)(I,ELEMNT(NAT(I)),(XYZ(J,I),J=1,3),I=1,NATOMS)
   70 FORMAT(I14,A2,3F10.5)
*
* SLATER ORBITALS INVOLVE R**(N-1) SO MAY AS WELL DO THE "-1" NOW.
      DO 80 I=1,NATOMS
         PQN(I)=PQN(I)-1
   80 NPQ(I)=NPQ(I)-1
      ADD=.FALSE.
      TX=0.D0
      TY=0.D0
      TZ=1.D0
*G      CALL PLOTGR(1,0.D0,0.D0)
*
*                       START OF A NEW PICTURE
*
   90 CONTINUE
      DO 100 I=1,NORBS
  100 IOCC(I)=0
      NELEC=NELECS/2
      DO 110 I=1,NELEC
  110 IOCC(I)=2
      IF(NELEC*2.NE.NELECS)IOCC(NELEC+1)=1
*
* READ IN NEW DATA FOR A PICTURE.
*
*G      CALL SLEEP(1)
*G      CALL PLOTGR(6,0.D0,0.D0)
      CALL DATIN
      I=INDEX(KEYWRD,'AXIS')
      CNTRS=(INDEX(KEYWRD,'NO-CONTOURS').EQ.0)
      IF(I.NE.0) THEN
         TZ=READA(KEYWRD,I)
         TX=1.D0-TZ
         TY=TX*0.33333D0
      ENDIF
      CALL EULER(TX,TY,TZ,0,0,0,0)
*
* PLOT IS EUCLIDEAN IF TZ .GT. 0.999
*
      EUCLID=(TZ.GT.0.999D0)
      FINE  =(INDEX(KEYWRD,'FINE').NE.0)
      IF(ADD)THEN
         ADDOLD=1.D0
      ELSE
         ADDOLD=0.D0
      ENDIF
      ADD=(INDEX(KEYWRD,'ADD').NE.0)
      ONE=1.D0
      IF(INDEX(KEYWRD,'PHASE').NE.0)ONE=-1.D0
      I=INDEX(KEYWRD,'PHASE=')
      IF(I.NE.0)ONE=READA(KEYWRD,I)
      BONDS =(INDEX(KEYWRD,'BONDS').NE.0)
      IF(BONDS .AND. IPSI.NE.0) THEN
         WRITE(6,'('' BONDS WITH A M.O. IS NOT PLOTTABLE'')')
         GOTO 90
      ENDIF
      IF( .NOT. MANUAL) THEN
         IF (IPSI.EQ.0) THEN
            IF(BONDS)THEN
               WRITE(6,'(/10X,''ELECTRON DENSITIES DUE TO'',
     1'' ATOMS ARE TO BE SUBTRACTED FROM DENSITY PLOT'',/)')
               IBONDS=1
            ELSE
               IBONDS=0
            ENDIF
C
C   CONSTRUCT A DENSITY MATRIX FROM ORBITAL OCCUPANCIES
C
            DO 140 I=1,NORBS
               DO 130 J=I,NORBS
                  SUM=0.D0
                  DO 120 K=1,NORBS
  120             SUM=SUM+IOCC(K)*VECTRS(I,K)*VECTRS(J,K)
                  P(J,I)=SUM
  130          P(I,J)=SUM
  140       P(I,I)=P(I,I)-IBONDS*AVERAG(I)
         ELSE
            DO 150 I=1,NORBS
  150       PAA(I)=VECTRS(I,IPSI)
            IF(DEBUG) THEN
               WRITE(6,'(''  COEFFICIENTS OF M.O. TO BE PLOTTED'')')
               WRITE(6,'(6F12.6)')(PAA(I),I=1,NORBS)
            ENDIF
         ENDIF
      ENDIF
      IF(CX.EQ.0.D0.AND.CY.EQ.0.D0)CZ=1.D0
      IF(R.EQ.0.D0)R=2.D0
*
* CALCULATE DIRECTION COSINES AND SINES.
*
      XY=CX*CX+CY*CY
      IF(XY.GT.1.D-15) THEN
         R1=DSQRT(XY+CZ*CZ)
         XY=DSQRT(XY)
         CA=CX/XY
         CB=CZ/R1
         SA=CY/XY
         SB=XY/R1
      ELSE
         CA=1.D0
         CB=1.D0
         SA=0.D0
         SB=0.D0
      ENDIF
      SIZE=0.5D0*R
      NPTS=60
      STEP =SIZE/(NPTS-1.D0)*2.D0
      NPT=NPTS
      IF(NPT.GT.27)NPT=27
      NP=(NPTS-NPT)/2+1
      NPT=NP-1+NPT
      PMAX=-100000.D0
      PMIN= 100000.D0
*
*
*
***********************************************************************
*
*   START OF CALCULATION OF ELECTRON DENSITY MAP
*
      IF(DEBUG) THEN
         WRITE(6,'(''   DENSITY MATRIX USED BY MAP'')')
         CALL MATOUT (P,ZEROS,NORBS,NORBS,MAXORB)
      ENDIF
      M1=0
      M2=0
      M3=0
      IJ=0
      DO 320 II=1,NPTS
         X2=-SIZE+(II-1)*STEP
         DO 310 JJ=1,NPTS
            IJ=IJ+1
            Y2=-SIZE+(JJ-1)*STEP
            SUM=JJ*1.D-20
            X=CA*CB*X2-SA*Y2+PX
            Y=SA*CB*X2+CA*Y2+PY
            Z=-SB*X2+PZ
            DO 160 I=1,3
  160       HERE(I)=HERE(I)+TVEC(I,1)*M1+TVEC(I,2)*M2+TVEC(I,3)*M3
            RMIN=1.D6
            DO 200 I=1,10
               NEARER=0
               DO 190 L1=L1L,L1U
                  DO 190 L2=L2L,L2U
                     DO 190 L3=L3L,L3U
                        R1=0.D0
                        DO 170 M=1,3
  170                   R1=R1+(HERE(M)+
     1TVEC(M,1)*L1+TVEC(M,2)*L2+TVEC(M,3)*L3)**2
                        IF(R1.LT.RMIN-1.D-4)THEN
                           RMIN=R1
                           M1=M1+L1
                           M2=M2+L2
                           M3=M3+L3
                           NEARER=1
                           DO 180 J=1,3
  180                      HERE(J)=HERE(J)+TVEC(J,1)*L1+TVEC(J,2)*L2+TVE
     1C(J,3)*L3
                        ENDIF
  190          CONTINUE
  200       IF(NEARER.EQ.0)GOTO 210
  210       CONTINUE
            ICO=0
            DO 270 L=1,NATOMS
               RMIN=1.D6
               DO 230 L1=L1L,L1U
                  DO 230 L2=L2L,L2U
                     DO 230 L3=L3L,L3U
                        R1=0.D0
                        DO 220 M=1,3
  220                   R1=R1+(HERE(M)-XYZ(M,L)+
     1TVEC(M,1)*L1+TVEC(M,2)*L2+TVEC(M,3)*L3)**2
                        IF(RMIN.GT.R1)THEN
                           XK=X-XYZ(1,L)+TVEC(1,1)*L1+TVEC(1,2)*L2+TVEC(
     11,3)*L3
                           YK=Y-XYZ(2,L)+TVEC(2,1)*L1+TVEC(2,2)*L2+TVEC(
     12,3)*L3
                           ZK=Z-XYZ(3,L)+TVEC(3,1)*L1+TVEC(3,2)*L2+TVEC(
     13,3)*L3
                           RMIN=R1
                        ENDIF
  230          CONTINUE
               R1=SQRT(XK*XK+YK*YK+ZK*ZK)
               IF(RSYM(L).GT.R1) THEN
                  RSYM(L)=R1
                  ISYM(L)=IC
                  JSYM(L)=JC
               ENDIF
               CONS=CONSTS(L)*R1**NPQ(L)*DEXP(-EMUS(L)*R1)
               CONP=CONSTP(L)*R1**(NPQ(L)-1)*DEXP(-EMUP(L)*R1)
               I=NLAST(L)-NFIRST(L)+1
               GOTO  (260,250,250,250,240,240,240,240,240),  I
C
C
  240          COND=CONSTD(L)*R1**(PQN(L)-2)*DEXP(-EMUD(L)*R1)
               PSI(ICO+9)=               COND*XK*YK
               PSI(ICO+8)=               COND*YK*ZK
               PSI(ICO+7)=0.2886751346D0*COND*(2.D0*ZK*ZK-YK*YK-XK*XK)
               PSI(ICO+6)=               COND*XK*ZK
               PSI(ICO+5)=0.50D0*COND*(XK*XK-YK*YK)
  250          PSI(ICO+4)=CONP*ZK
               PSI(ICO+3)=CONP*YK
               PSI(ICO+2)=CONP*XK
  260          PSI(ICO+1)=CONS
C
C
               ICO=ICO+I
  270       CONTINUE
            IF(IPSI.EQ.0)  THEN
               DO 280 I=1,ICO
                  SUM=SUM-PSI(I)*PSI(I)*P(I,I)*0.5D0
                  DO 280 J=1,I
  280          SUM=SUM+PSI(I)*PSI(J)*P(I,J)
               SUM=SUM+SUM
            ELSE
  290          CONTINUE
               DO 300 I=1,ICO
  300          SUM=SUM+PAA(I)*PSI(I)
            ENDIF
            EDENS(IJ)=EDENS(IJ)*ADDOLD+SUM*ONE
            IF(EDENS(IJ).GT.PMAX)PMAX=EDENS(IJ)
            IF(EDENS(IJ).LT.PMIN)PMIN=EDENS(IJ)
  310    CONTINUE
  320 CONTINUE
      IF (ADD) GOTO 90
      PMAX=MAX(PMAX,-PMIN)
      IF(PMAX.LT.1.D-5) WRITE(6,'('' DENSITY OF PLOT IS VERY'',
     1'' LOW- SUGGEST YOU CHECK YOUR DATA'')')
      IF(PMAX.GT.1.D 5) WRITE(6,'('' DENSITY OF PLOT IS VERY'',
     1'' HIGH- SUGGEST YOU CHECK YOUR DATA'')')
      DO 330 I=1,20
  330 IF(PMAX.LT.SETVAL(I)) GOTO 340
  340 STEP=SETVAL(I)*0.1D0
      LPIC=.FALSE.
      WRITE(6,350)STEP
  350 FORMAT(//20X,'INTERVAL BETWEEN CONTOURS IS',F8.5,/,20X,
     1'   ELECTRONS PER CUBIC ANGSTROM',/)
      STEP=STEP*0.5D0
      I=PMAX/STEP+1
      IF(FINE) THEN
         DELLH=0.249999D0
      ELSE
         DELLH=1.D0
      ENDIF
      J=I/DELLH
      OPEN(UNIT=15,FILE=GETNAM('FOR015'))
      WRITE(15,'('' NUMBER OF CONTOURS ='',I4)')J
      WRITE(15,'(A)')TITLE
      WRITE(15,'('' CONTOUR INTERVAL ='',F13.6,''      VERSION'',F9.2)')
     1     STEP*DELLH,   VERSON
      WRITE(15,'(A)')KEYWRD
      SCALE=1
      I=INDEX(KEYWRD,'MULT=')
      IF(I.NE.0)SCALE=READA(KEYWRD,I)
      CONST=-0.01D0*SCALE
         CONST2=CONST/STEP
            CALL EULER(0.D0,0.D0,-PMIN*CONST2,-1,0,0,0)
*G         CALL PLOTGR(99,1.1D0,0.D0)
      IF(INDEX(KEYWRD,'GRID') .NE. 0 )THEN
*
*   WE WANT DO DRAW AN X-Y GRID OVER THE PICTURE
*
         FACT=1.D0/(NPTS-1)
         NPTS1=NPTS-1
         DO 370 I=0,NPTS1
            II=I*NPTS
            IJ=II+1
            XVECT=1.D0-I*FACT
            CALL EULER(XVECT,1.D0,(EDENS(IJ)-PMIN)*CONST2,1,2,3,2)
            DO 360 J=2,NPTS
               YVECT=1-(J-1)*FACT
               IJ=I*NPTS+J
               CALL EULER(XVECT,YVECT,(EDENS(IJ)-PMIN)*CONST2,1,2,3,3)
  360       CONTINUE
  370    CONTINUE
         DO 390 I=1,NPTS
            IJ=I
            XVECT=1.D0-(I-1)*FACT
            CALL EULER(1.D0,XVECT,(EDENS(IJ)-PMIN)*CONST2,1,2,3,2)
            DO 380 J=1,NPTS1
               YVECT=J*FACT
               IJ=J*NPTS+I
               CALL EULER(1.D0-YVECT,XVECT,(EDENS(IJ)-PMIN)*CONST2
     1        ,1,2,3,3)
  380       CONTINUE
  390    CONTINUE
      ENDIF
C
C   EXPAND EDENS TO DOUBLE THE MESH
C
      NPTS2=2*NPTS-1
      DO 400 I=NPTS,1,-1
         DO 400 J=NPTS,1,-1
  400 EDENS((2*I-2)*NPTS2+2*J-1)=EDENS((I-1)*NPTS+J)
      NPTS=NPTS*2-1
      DO 420 I=1,NPTS,2
         IROW=(I-1)*NPTS
         EDENS(IROW+2)=0.5D0*(EDENS(IROW+1)+EDENS(IROW+3))
         EDENS(I*NPTS-1)=0.5D0*(EDENS(I*NPTS)+EDENS(I*NPTS-2))
         DO 410 J=3,NPTS-4,2
  410    EDENS(IROW+J+1)=0.0625D0*(-EDENS(IROW+J-2)-EDENS(IROW+J+4)
     1 +9.D0*(EDENS(IROW+J)+EDENS(IROW+J+2)))
  420 CONTINUE
      NPTS2=NPTS*NPTS
      DO 440 I=1,NPTS
         EDENS(I+NPTS )=0.5D0*(EDENS(I)+EDENS(I+NPTS*2))
         EDENS(I+NPTS2-2*NPTS)=
     10.5D0*(EDENS(I+NPTS2-NPTS)+EDENS(I+NPTS2-3*NPTS))
         DO 430 J=3,NPTS-4,2
  430    EDENS(I+J*NPTS)=0.0625D0*(-EDENS(I+(J-1)*NPTS)-
     1EDENS(I+(J+3)*NPTS)
     2 +9.D0*(EDENS(I+(J-1)*NPTS)+EDENS(I+(J+1)*NPTS)))
  440 CONTINUE
      SHIFT=-PMIN*CONST/STEP
      STEP=STEP*DELLH
      CONST=CONST*DELLH
      IF(CNTRS)THEN
      DO 450 I=1,100
         CNTVAL=(I-1)*STEP
*G        IF(.NOT.MONO)CALL PLOTGR(99,I*1.D0,0.D0)
         IF(CNTVAL.LT.PMIN) GOTO 450
         IF(CNTVAL.GT.PMAX) GOTO 450
         HEIGHT=(I-1)*CONST+SHIFT
         CALL CNTOUR(EDENS,NPTS,NPTS,NPTS,HEIGHT,CNTVAL,1,2,3,X)
  450 CONTINUE
      DO 460 I=1,20
         CNTVAL=-STEP*I
         IF(CNTVAL.LT.PMIN) GOTO 460
         IF(CNTVAL.GT.PMAX) GOTO 460
         HEIGHT=-I*CONST+SHIFT
  460 CALL CNTOUR(EDENS,NPTS,NPTS,NPTS,HEIGHT,CNTVAL,1,2,3,X)
      ENDIF
      CNTVAL=99.999
      IF(EUCLID) THEN
*
*  DRAW A BOX AROUND THE PLOT
*
*G         CALL PLOTGR(99,1.D0,0.D0)
*G         CALL PLOTGR(2,0.D0,0.D0)
*G         CALL PLOTGR(3,1.D0,0.D0)
*G         CALL PLOTGR(3,1.D0,1.D0)
*G         CALL PLOTGR(3,0.D0,1.D0)
*G         CALL PLOTGR(3,0.D0,0.D0)
      ENDIF
      ENPTS=NPTS
      IF(EUCLID) THEN
         DO 470 I=1,NATOMS
*
* WRITE ELEMENT NAMES
*
            IF(ISYM(I).NE.0)
     1    WRITE(15,'(F5.3,F6.3,A3,''  0.0000'')')
     2    1-JSYM(I)/ENPTS,1-ISYM(I)/ENPTS,ELEMNT(NAT(I))
  470    CONTINUE
      ENDIF
      CLOSE(15)
      STEP=4.D0/STEP
      WRITE(6,480)STEP
  480 FORMAT(//10X,'DENSITY MATRIX, MATRIX ELEMENTS ARE MULTIPLIED BY',
     1F8.3,/)
      J=0
      DO 500 K=1,NPTS
         DO 490 I=1,NPTS
            J=J+1
  490    IWORK(I)=EDENS(J)*STEP
         WRITE(6,'(1X)')
  500 WRITE(6,'(1X,41I3)')(IWORK(I),I=NP,NPT)
      CALL MAXMIN(1,EDENS,NPTS,NPTS)
      CALL MAXMIN(-1,EDENS,NPTS,NPTS)
      GOTO 90
      END
