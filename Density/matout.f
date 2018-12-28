      SUBROUTINE MATOUT (A,B,NC,NR,NDIM)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      INCLUDE 'SIZES'
      DIMENSION A(NDIM,NDIM), B(NDIM)
      COMMON /MOLKST/ NUMAT,NAT(NUMATM),NFIRST(NUMATM),NMIDLE(NUMATM),
     1                NLAST(NUMATM), NORBS, NELECS,NALPHA,NBETA,
     2                NCLOSE,NOPEN
C**********************************************************************
C
C      MATOUT PRINTS A SQUARE MATRIX OF EIGENVECTORS AND EIGENVALUES
C
C    ON INPUT A CONTAINS THE MATRIX TO BE PRINTED.
C             B CONTAINS THE EIGENVALUES.
C             NC NUMBER OF MOLECULAR ORBITALS TO BE PRINTED.
C             NR IS THE SIZE OF THE SQUARE ARRAY TO BE PRINTED.
C             NDIM IS THE ACTUAL SIZE OF THE SQUARE ARRAY "A".
C             NFIRST AND NLAST CONTAIN ATOM ORBITAL COUNTERS.
C             NAT = ARRAY OF ATOMIC NUMBERS OF ATOMS.
C
C
C***********************************************************************
      CHARACTER*2 ELEMNT(99), ATORBS(9), ITEXT(MAXORB), JTEXT(MAXORB)
      DIMENSION NATOM(MAXORB)
      DATA ATORBS/' S','PX','PY','PZ','X2','XZ','Z2','YZ','XY'/
      DATA ELEMNT/'H','He',
     1 'Li','Be','B','C','N','O','F','Ne',
     2 'Na','Mg','Al','Si','P','S','Cl','Ar',
     3 'K','Ca','Sc','Ti','V','Cr','Mn','Fe','Co','Ni','Cu',
     4 'Zn','Ga','Ge','As','Se','Br','Kr',
     5 'Rb','Sr','Y','Zr','Nb','Mo','Tc','Ru','Rh','Pd','Ag',
     6 'Cd','In','Sn','Sb','Te','I','Xe',
     7 'Cs','Ba','La','Ce','Pr','Nd','Pm','Sm','Eu','Gd','Tb','Dy',
     8 'Ho','Er','Tm','Yb','Lu','Hf','Ta','W','Re','Os','Ir','Pt',
     9 'Au','Hg','Tl','Pb','Bi','Po','At','Rn',
     1 'Fr','Ra','Ac','Th','Pa','U','Np','Pu','Am','Cm','Bk','Cf','XX'/
      IF(NLAST(NUMAT).NE.NR) GOTO 30
      DO 20 I=1,NUMAT
         JLO=NFIRST(I)
         JHI=NLAST(I)
         L=NAT(I)
         K=0
         DO 10 J=JLO,JHI
            K=K+1
            ITEXT(J)=ATORBS(K)
            JTEXT(J)=ELEMNT(L)
            NATOM(J)=I
   10    CONTINUE
   20 CONTINUE
      GOTO 50
   30 CONTINUE
      NR=ABS(NR)
      DO 40 I=1,NR
         ITEXT(I)='  '
         JTEXT(I)='  '
   40 NATOM(I)=I
   50 CONTINUE
      KA=1
      KC=6
   60 KB=MIN0(KC,NC)
      WRITE (6,100) (I,I=KA,KB)
      WRITE (6,110) (B(I),I=KA,KB)
      WRITE (6,120)
      LA=1
      LC=40
   70 LB=MIN0(LC,NR)
      DO 80 I=LA,LB
         IF(ITEXT(I).EQ.' S')WRITE(6,120)
         WRITE (6,130) ITEXT(I),JTEXT(I),NATOM(I),(A(I,J),J=KA,KB)
   80 CONTINUE
      IF (LB.EQ.NR) GO TO 90
      LA=LC+1
      LC=LC+40
      WRITE (6,140)
      GO TO 70
   90 IF (KB.EQ.NC) RETURN
      KA=KC+1
      KC=KC+6
      IF (NR.GT.25) WRITE (6,140)
      GO TO 60
C
  100 FORMAT (////,3X,9H ROOT NO.,I5,9I12)
  110 FORMAT (/8X,10F12.6)
  120 FORMAT (2H  )
  130 FORMAT (1H ,2(1X,A2),I3,F10.6,10F12.6)
  140 FORMAT (1H1)
C
      END
