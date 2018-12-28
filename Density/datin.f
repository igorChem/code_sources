      SUBROUTINE DATIN
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      INCLUDE 'SIZES'
      COMMON /LINES / PX, PY, PZ, CX, CY, CZ, R, IPSI
      COMMON /KEYWRD/ KEYWRD
      COMMON /OCCUP / IOCC(MAXORB)
      COMMON /COORD / XYZ(3,NUMATM)
      COMMON /MOLDAT/ NATOMS, NORBS, NELECS, NFIRST(NUMATM),
     1                NLAST(NUMATM), IDUMY, EMU(NUMATM,3), NPQ(NUMATM),
     2                NAT(NUMATM)
************************************************************************
*
*  DATIN FOR GRAPHICS READS IN ALL DATA FOR ONE PICTURE.
*
************************************************************************
      CHARACTER*80 KEYWRD
      WRITE(6,'(80(''*''))')
      READ(5,'(A)',END=99,ERR=99)KEYWRD
      CALL UPCASE(KEYWRD)
      DO 10 I=1,80
   10 IF(KEYWRD(I:I) .NE. ' ') GOTO 20
      WRITE(6,'(''  END OF GRAPHICAL DATA'')')
      GOTO 99
   20 CONTINUE
      WRITE(6,'(''   DATA  PROVIDED BY INPUT FILE (PARAPHRASED)'')')
      I=INDEX(KEYWRD,'CENTER=(')
      IF(I.NE.0) THEN
         PX=READA(KEYWRD,I)
         I=INDEX(KEYWRD,',')
         KEYWRD(I:I)=' '
         PY=READA(KEYWRD,I)
         I=INDEX(KEYWRD,',')
         KEYWRD(I:I)=' '
         PZ=READA(KEYWRD,I)
      ELSE
         I=INDEX(KEYWRD,'CENTER')
         IF(I.EQ.0) THEN
            WRITE(6,'(//10X,'' KEY-WORD "CENTER" IS MISSING'')')
            GOTO 99
         ENDIF
         I=READA(KEYWRD,I)
         PX=XYZ(1,I)
         PY=XYZ(2,I)
         PZ=XYZ(3,I)
      ENDIF
      I=INDEX(KEYWRD,'LINE=(')
      IF(I.NE.0) THEN
         CX=READA(KEYWRD,I)
         I=INDEX(KEYWRD,',')
         KEYWRD(I:I)=' '
         CY=READA(KEYWRD,I)
         I=INDEX(KEYWRD,',')
         KEYWRD(I:I)=' '
         CZ=READA(KEYWRD,I)
      ELSE
         I=INDEX(KEYWRD,'LINE')
         IF(I.EQ.0) THEN
            WRITE(6,'(//10X,'' KEY-WORD "LINE" IS MISSING'')')
            GOTO 99
         ENDIF
         I=READA(KEYWRD,I)
         CX=XYZ(1,I)-PX
         CY=XYZ(2,I)-PY
         CZ=XYZ(3,I)-PZ
      ENDIF
      R=2.D0
      I=INDEX(KEYWRD,'EDGE')
      IF(I.NE.0)R=READA(KEYWRD,I)
      WRITE(6,30)PX,PY,PZ,CX,CY,CZ,R
   30 FORMAT(9X,'  CENTER OF GRAPH           AXIS OF GRAPH      RADIUS',
     1/9X,      'PX      PY      PZ        CX      CY      CZ',
     2/,5X,3F8.3,F10.3,2F8.3,F10.3,///)
      IMOS=0
      IPSI=0
      I=INDEX(KEYWRD,' PSI')
      IF(I.NE.0) THEN
         IMOS=IMOS+1
         IPSI=READA(KEYWRD,I)
      ENDIF
      I=INDEX(KEYWRD,' HOMO')
      IF(I.NE.0) THEN
         IMOS=IMOS+1
         IPSI=NELECS/2
      ENDIF
      I=INDEX(KEYWRD,' LUMO')
      IF(I.NE.0) THEN
         IMOS=IMOS+1
         IPSI=NELECS/2+1
      ENDIF
      IF(IMOS.GT.1) THEN
*
*   MADE A MISTAKE
*
         WRITE(6,'(//10X,'' INCOMPATIBLE KEY-WORDS'')')
         GOTO 99
      ENDIF
      IF(IPSI.NE.0) THEN
         WRITE(6,40)IPSI
   40    FORMAT(//10X,'M.O. NUMBER ',I4,'  IS TO BE PLOTTED',/)
      ENDIF
      I=INDEX(KEYWRD,' OCCUPA')
      IF(I.NE.0) THEN
         READ(5,'(80I1)')(IOCC(I),I=1,NORBS)
         WRITE(6,'('' MOLECULAR ORBITAL OCCUPANCIES FOR DENSITY MAP'')')
         WRITE(6,'(1X,80I1)')IOCC
      ENDIF
      WRITE(6,'(80(''*''))')
      RETURN
   99 CALL SLEEP(5)
*G      CALL PLOTGR(0,0.D0,0.D0)
      STOP 'Exit from DATIN'
      END
