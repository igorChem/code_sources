      SUBROUTINE READIN(MANUAL)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      INCLUDE 'SIZES'
************************************************************************
*
* READS IN DATA FROM A FILE OR DISK
*
*  THE DATA TO BE PUT INTO COMMON BLOCKS ARE AS FOLLOWS:
*  1 ATOMIC DATA: NUMBER OF ATOMS, ORBITALS, ELECTRONS, CARTESIAN
*                 COORDINATES, EXPONENTS, PQN'S, ORBITAL COUNTERS
*  2 IF MANUAL THEN A DENSITY MATRIX WHICH WAS READ IN DIRECTLY,
*    OR MADE FROM A MOLECULAR ORBITAL.
*    OTHERWISE THE NORMALIZED EIGENVECTORS WHICH, IN TURN, WERE
*    GENERATED FROM THE UN-RENORMALIZED EIGENVECTORS AND
*    INVERSE-SQUARE-ROOT OF THE
*    OVERLAP MATRIX, WHICH WERE READ OFF DISK
*  3 IF NOT MANUAL, THEN THE SPHERICAL-AVERAGE ATOMIC ORBITAL DENSITY
*
************************************************************************
*
*   ARRAYS: VECTRS  -  NORMALIZED EIGENVECTORS
*           DENSTY  -  DENSITY MATRIX
*           AVERAG  -  SPHERICAL AVERAGE ATOMIC ORBITAL OCCUPANCY
*
*              MOLECULAR  DATA
*
*           XYZ     -  CARTESIAN COORDINATES
*           NFIRST  -  STARTING ORBITAL COUNTERS
*           NLAST   -  ENDING ORBITAL COUNTERS
*           EMU     -  ORBITAL EXPONENTS
*           NPQ     -  PRINCIPAL QUANTUM NUMBERS
************************************************************************
      COMMON /VECTRS/ VECTRS(MAXORB,MAXORB)
      COMMON /DENSTY/ P(MAXORB,MAXORB)
      COMMON /LINES / XDUMY(7), IPSI
      COMMON /KEYWRD/ KEYWRD
      COMMON /TVECTS/ TVEC(3,3), ID
      COMMON /SPHERE/ AVERAG(MAXORB)
      COMMON /COORD / XYZ(3,NUMATM)
      COMMON /MOLDAT/ NATOMS, NORBS, NELECS, NFIRST(NUMATM),
     1                NLAST(NUMATM), IDUMY, EMU(NUMATM,3), NPQ(NUMATM),
     2                NAT(NUMATM)
      COMMON /SOLID/ RX, RY, RZ, ALPHA, BETA, GAMMA
      DIMENSION GETMU(3,10), NP1(10), IWORK(10),
     1 HALFS(MPACK*2), PSI(MAXORB), NPQREF(54)
      CHARACTER*80 KEYWRD, GETNAM
      LOGICAL DMAT, MO, DEBUG, MANUAL
      DATA PSI/MAXORB*0.D0/
      DATA NPQREF/1,0, 2,2,2,2,2,2,2,0,
     1 2,2,2,2,2,2,2,0,
     2 4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,0,
     3 5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5/
      DEBUG=(INDEX(KEYWRD,'DEBUG') .NE.0)
      DMAT=(INDEX(KEYWRD,' DMAT') .NE. 0)
      MO=(INDEX(KEYWRD,' M.O.') .NE. 0)
      IF(DEBUG )WRITE(6,'(''  DEBUG - DEBUG OUTPUT TO BE PRINTED'')')
      IF(DMAT  )WRITE(6,'(''  DMAT  - DENSITY MATRIX TO BE INPUT'')')
      IF(MO    )WRITE(6,'(''  M.O.  - MOLECULAR ORBITAL TO BE INPUT'')')
      IF(MANUAL)WRITE(6,'(''  MANUAL- ALL DATA TO BE READ OFF '',
     1''THIS FILE'')')
      IF(MANUAL .AND. (.NOT.MO.AND..NOT.DMAT)) THEN
*
*  A KEY-WORD FOR THIS IS OBLIGATORY.
*
         WRITE(6,'(////10X,''you MUST supply '',
     1         ''EITHER the key-word M.O. or DMAT.'')')
         STOP
      ENDIF
      IF (MANUAL) THEN
         READ(5,*)NATOMS,NORBS,NELECS
         WRITE(6,10)NATOMS,NORBS,NELECS
   10    FORMAT(//20X,'NUMBER OF ATOMS =',I4,/27X,'ORBITALS ='
     1    ,I4,/22X,'FILLED LEVELS =',I4,//)
C
C   NOW TO HANDLE SOLIDS
C
C#      READ(5,'(6F10.5)')RX,RY,RZ,ALPHA,BETA,GAMMA
         IF(ALPHA.EQ.0.D0)ALPHA=90.D0
         IF(BETA.EQ.0.D0)BETA=90.D0
         IF(GAMMA.EQ.0.D0)GAMMA=90.D0
         IF(RX+RY+RZ.NE.0.D0) WRITE(6,20)RX,RY,RZ,ALPHA,BETA,GAMMA
   20    FORMAT(//34X,'A =',F8.4,/34X,'B =',F8.4,/34X,'C =',F8.4,/30X
     1,'ALPHA =',F8.4,/31X,'BETA =',F8.4,/30X,'GAMMA =',F8.4,///)
         NCELLS=8
         IF(RX+RY+RZ.EQ.0.D0)NCELLS=1
C
C   SOLID WORK COMPLETE HERE
C
         WRITE(6,'(/10X,''COORDINATES'',/)')
         DO 30 J=1,NATOMS
            READ(5,*)(XYZ(I,J),I=1,3)
            WRITE(6,'(3F12.6)')(XYZ(I,J),I=1,3)
   30    CONTINUE
         WRITE(6,'(/,10X,'' ATOM LABELS'',/)')
         READ(5,*)(NFIRST(I),I=1,NATOMS)
         WRITE(6,'(20I4)')(NFIRST(I),I=1,NATOMS)
         NUNI=1
         DO 40 I=1,NATOMS
   40    NUNI=MAX(NFIRST(I),NUNI)
         WRITE(6,'(/,10X,'' ATOMIC NUMBERS'',/)')
         READ(5,*)(NP1(I),I=1,NUNI)
         DO 50 I=1,NATOMS
   50    NAT(I)=NP1(NFIRST(I))
         WRITE(6,'(20I4)')(NAT(I),I=1,NATOMS)
         WRITE(6,'(/10X,''PRINCIPAL QUANTUM NUMBERS'',/)')
         READ(5,*)(NP1(I),I=1,NUNI)
         WRITE(6,'(10X,10I4)')(NP1(I),I=1,NUNI)
         WRITE(6,'(/10X,'' NUMBER OF ORBITALS PER LABELLED ATOM'',/)')
         READ(5,*)(IWORK(I),I=1,NUNI)
         WRITE(6,'(10X,10I4)')(IWORK(I),I=1,NUNI)
         WRITE(6,'(/10X,''ORBITAL EXPONENTS'',/)')
         DO 60 J=1,NUNI
            READ(5,'(3F10.4)')(GETMU(I,J),I=1,3)
   60    WRITE(6,'(3F12.5)')(GETMU(I,J),I=1,3)
         WRITE(6,'(//10X,''SUMMARY OF ATOMIC DATA, PER ATOM'')')
         WRITE(6,'(/,''   ATOM  START  STOP  PQN EXPONENTS'')')
         NORBS=0
         DO 80 I=1,NATOMS
            K=NFIRST(I)
            NFIRST(I)=NORBS+1
            NPQ(I)=NP1(K)
            NLAST(I)=NORBS+IWORK(K)
            NORBS=NLAST(I)
            DO 70 J=1,3
   70       EMU(I,J)=GETMU(J,K)
   80    WRITE(6,'(4I6,3F9.4)')I,NFIRST(I),NLAST(I),NPQ(I),
     1         (EMU(I,J),J=1,3)
         IF (MO) THEN
*
*  READ IN ONE MOLECULAR ORBITAL
*
            READ(5,*)(AVERAG(I),I=1,NORBS)
            WRITE(6,'(/10X,'' MOLECULAR ORBITAL COEFFICIENTS'')')
            WRITE(6,'(8F10.5)')(AVERAG(I),I=1,NORBS)
            DO 90 I=1,NORBS
               DO 90 J=1,NORBS
                  VECTRS(I,J)=AVERAG(I)
   90       P(I,J)=AVERAG(I)*AVERAG(J)
         ENDIF
         IF ( DMAT ) THEN
*
*   READ IN  ENTIRE DENSITY MATRIX
*
            DO 110 I=1,NORBS
               READ(5,*)(P(I,J),J=1,I)
               WRITE(6,'(8F10.5)')(P(I,J),J=1,I)
               DO 100 J=1,I
  100          P(J,I)=P(I,J)
  110       CONTINUE
         ENDIF
      ELSE
*
*  ALL DATA HERE ARE READ IN UNFORMATTED
*
         OPEN(UNIT=13,FILE=GETNAM('FOR013'),FORM='UNFORMATTED')
         READ(13)NATOMS,NORBS,NELECS,((XYZ(I,J),J=1,NATOMS),I=1,3)
         READ(13)(NLAST(I),NFIRST(I),I=1,NATOMS)
         READ(13)((EMU(J,I),J=1,NATOMS),I=1,3),(NAT(I),I=1,NATOMS)
         DO 120 I=1,NATOMS
  120    NPQ(I)=NPQREF(NAT(I))
         LINEAR=NORBS*NORBS
*
*  NEED TO READ IN UN-NORMALISED EIGENVECTORS AND
*  INVERSE OF OVERLAP MATRIX
*
         READ(13)((P(J,I),J=1,NORBS),I=1,NORBS)
         READ(13)(HALFS(I),I=1,LINEAR)
         READ(13)ID,TVEC
         WRITE(6,'(//20X,''    NUMBER OF ATOMS ='',I4,/
     1                 20X,''           ORBITALS ='',I4,/
     2                 20X,''NUMBER OF ELECTRONS ='',I4,//)')
     3                 NATOMS,NORBS,NELECS
         NELEC=NELECS/2
         JJ=NELEC+1
         FRACT=0.5D0*(NELECS-NELEC*2)
         DO 140 I=1,NORBS
            X=0.D0
            DO 130 J=1,NELEC
  130       X=X+P(I,J)**2
            X=X+P(I,JJ)**2*FRACT
  140    AVERAG(I)=X*2
*
* LOOP TO CALCULATE SPHERICAL-AVERAGE ATOMIC ORBITAL OCCUPANCY
*
         DO 220 I=1,NATOMS
            IL=NFIRST(I)
            IU=NLAST(I)
            IR=IU-IL+1
            GOTO (210,180,180,180,150,150,150,150,150),IR
  150       X=0.D0
            DO 160 J=1,5
               JI=J+IL+3
  160       X=X+AVERAG(JI)
            X=X*0.2D0
            DO 170 J=1,5
               JI=J+IL+3
  170       AVERAG(JI)=X
  180       X=0.D0
            DO 190 J=1,3
               JI=J+IL
  190       X=X+AVERAG(JI)
            X=X*0.333333D0
            DO 200 J=1,3
               JI=J+IL
  200       AVERAG(JI)=X
  210       CONTINUE
  220    CONTINUE
*
* MATRIX MULTIPLY EIGENVECTORS BY INVERSE SQUARE-ROOT OF OVERLAP MATRIX
*
         IF(DEBUG) THEN
            WRITE(6,'(''   STARTING EIGENVECTORS'')')
            CALL MATOUT (P,PSI,NORBS,NORBS,MAXORB)
            WRITE(6,'(''   STARTING INVERSE-SQUARE-ROOT OVERLAP MATRIX''
     1)')
            CALL MATOUT (HALFS,PSI,NORBS,NORBS,NORBS)
         ENDIF
         DO 250 I=1,NORBS
            L=0
            DO 240 J=1,NORBS
               X=0.D0
               DO 230 K=1,NORBS
                  L=L+1
  230          X=X+P(K,I)*HALFS(L)
  240       PSI(J)=X
            DO 250 J=1,NORBS
  250    P(J,I)=PSI(J)
         DO 260 I=1,NORBS
            DO 260 J=1,NORBS
  260    VECTRS(J,I)=P(J,I)
      ENDIF
      IF(DEBUG) THEN
         WRITE(6,'(''   RE-NORMALISED EIGENVECTORS'')')
         DO 280 I=1,NORBS
            SUM=0.D0
            DO 270 J=1,NORBS
  270       SUM=SUM+VECTRS(J,I)**2
  280    PSI(I)=SUM
         CALL MATOUT (VECTRS,PSI,NORBS,NORBS,MAXORB)
      ENDIF
      RETURN
      END
