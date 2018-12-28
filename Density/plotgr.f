      SUBROUTINE PLOTGR( IND, X, Y)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      INCLUDE 'SIZES'
      REAL XLX, YLY, XCOL
      COMMON /CNTVAL/ CNTVAL
      EXTERNAL XWIN
      IF(IND.EQ.6) XCOL=0.0
*
*
C   CODES FOR IND:
C
C     IND = 0   =>  REMOVE ALL GRAPHICS TO QUIT
C     IND = 1   =>  ANY ONE TIME ONLY INITIALIZATION
C     IND = 2   =>  MOVE GRAPHICS TO POINT (X,Y) -- DO NOT DRAW LINE
C     IND = 3   =>  DRAW LINE FROM LAST POINT TO NEW (X,Y)
C     IND = 4   =>  DRAW DARK LINE (ERASES A LINE)
C     IND = 5   =>  DRAW BRIGHT LINE (REGULAR SOLID LINE)
C     IND = 6   =>  INITIALIZE & SET-UP FOR NEXT PICTURE
C     IND = 8   =>  TEMPORARY RELEASE FROM GRAPHICS
C     IND = 9   =>  RETURN TO GRAPHICS FROM TEMP RELEASE
C
*     IND = 99  =>  SET NEW COLOR TO DRAW ( COLOR IS IN X )
*
      IF(IND.EQ.2.OR.IND.EQ.3)THEN
      XLX = MIN(1.0,MAX(0.0,REAL(X)))
      YLY = MIN(1.0,MAX(0.0,REAL(Y)))
      ELSE
      XLX=X
      YLY=Y
      ENDIF
      LIND = IND
      CALL XWIN(XLX,YLY,LIND)
         CALL HPPLOT(XLX,YLY,LIND)
      RETURN
      END
      SUBROUTINE HPPLOT(X,Y,IND)
* FOR HPGL DEVICES
      LOGICAL SORT, ERASE
      LOGICAL ISINIT
      CHARACTER*80 FILPLT, GETNAM
      CHARACTER*1  ESC, CTLZ
*
*
      DATA ISINIT / .FALSE. /
      DATA SORT   / .TRUE. /
C
C   CODES FOR IND:
C
C     IND = 0   =>  REMOVE ALL GRAPHICS TO QUIT
C     IND = 1   =>  ANY ONE TIME ONLY INITIALIZATION
C     IND = 2   =>  MOVE GRAPHICS TO POINT (X,Y) -- DO NOT DRAW LINE
C     IND = 3   =>  DRAW LINE FROM LAST POINT TO NEW (X,Y)
C     IND = 4   =>  DRAW DARK LINE
C     IND = 5   =>  DRAW BRIGHT SOLID LINE
C     IND = 6   =>  INITIALIZE & SET-UP FOR NEXT PICTURE
C     IND = 8   =>  TEMPORARY RELEASE FROM GRAPHICS
C     IND = 9   =>  RETURN TO GRAPHICS FROM TEMP RELEASE
C
*     IND = 99  =>  SET NEW COLOR (VALUE IN X)
*
C
      IF (IND.EQ.0) THEN
C DONE DRAWING PICTURE
*  PUT PEN TO HOME
         IF ( SORT ) CALL HPSORT( IXLX, IYLY, 0, 101 )
         WRITE ( 11, *) 'PU;SP0;'
         WRITE ( 11, *) ESC//'.Z'
         ISINIT = .FALSE.
         CLOSE(UNIT=11)
         ISINIT = .FALSE.

      ELSEIF (IND.EQ.1) THEN
C  ONE TIME ONLY INITIALIZATION OF TERMINAL
         ESC=CHAR(27)
         CTLZ=CHAR(26)
         IPAPER=100
         IPAGE=100
         ERASE = .FALSE.
         PIXEL = 7900. * IPAPER / 100.
         NCOLOR = 7
         LCOLOR = 1
      ELSEIF (IND.EQ.2 ) THEN
* Move pen to absolute position.
         IXLX = X * PIXEL
         IYLY = Y * PIXEL
         IF ( ERASE ) RETURN
         IF ( SORT )  THEN
           CALL HPSORT( IXLX, IYLY, 2, LCOLOR)
         ELSE
           WRITE ( 11, '('' PU;PA'', I5, '','', I5,'';'')') IXLX, IYLY
         ENDIF

      ELSEIF (IND.EQ.3) THEN
C DRAW LINE FROM LAST POINT TO NEW POINT
         IXLX = X * PIXEL
         IYLY = Y * PIXEL
         IF ( ERASE ) RETURN
         IF ( SORT ) THEN
           CALL HPSORT( IXLX, IYLY, 3, LCOLOR)
         ELSE
           WRITE ( 11, '('' PD;PA'', I5, '','', I5,'';'')') IXLX, IYLY
         ENDIF

      ELSEIF (IND.EQ.4) THEN
* NO ABILITY TO ERASE FIGURE FOR PEN PLOTTER
        ERASE= .TRUE.

      ELSEIF (IND.EQ.5) THEN
* NO ABILITY TO ERASE FIGURE FOR PEN PLOTTER
        ERASE = .FALSE.

      ELSEIF (IND.EQ.6) THEN
C  INITIALIZE FOR DRAWING NEXT PICTURE
         ERASE = .FALSE.
         LCOLOR = 1
         NCOLOR = 7
         IF ( ISINIT ) THEN
         IF ( SORT ) CALL HPSORT( IXLX, IYLY, 0, 101 )
         WRITE ( 11, *) 'PU;SP0;'
         WRITE ( 11, *) ESC//'.Z'
         ISINIT = .FALSE.
         CLOSE(UNIT=11)
         ENDIF
         ISINIT = .TRUE.
         FILPLT=GETNAM('plot')
         CALL LOWER(FILPLT,80)
         J=INDEX(FILPLT,' ')
         INO=0
         JNO=0
         FILPLT(J:)='.plt'
         GOTO 34
  33     CONTINUE
         INO=INO+1
         IF(INO.GT.9)THEN
         JNO=JNO+1
         INO=-1
         ENDIF
         IF(JNO.EQ.0)THEN
         FILPLT(J:)='_'//CHAR(ICHAR('0')+INO)//'.plt'
         ELSE
         FILPLT(J:)='_'//CHAR(ICHAR('0')+JNO)//
     1    CHAR(ICHAR('0')+INO)//'.plt'
         ENDIF
  34     OPEN( UNIT=11, FILE=FILPLT,STATUS='NEW',IOSTAT=I33 )
         IF(I33.NE.0)GOTO 33
         REWIND 11
         WRITE ( 11, '(A)') ' '//ESC//'.Y;'//
     .     ESC//'.M:'//ESC//'.I080:'//ESC//'.N0:'//
     .     'DF;CS01;SS;PU;SP1;VS10;'

         WRITE ( 11, '(A)') ' VA;PU;   '

         PIXEL = 7900. * IPAPER / 100.
         IF ( SORT ) CALL HPSORT( IXLX, IYLY, 0, -4)

      ELSEIF (IND.EQ.8) THEN
C TEMPORARY RELEASE FROM GRAPHICS  (USUALLY FOR TEXT)

      ELSEIF (IND.EQ.9) THEN
C RETURN TO GRAPHICS FROM TEMPORARY RELEASE

      ELSEIF (IND.EQ.10) THEN
C DONE DRAWING PICTURE
*  PUT PEN TO HOME
         IF ( SORT ) CALL HPSORT( IXLX, IYLY, 0, 101 )
         WRITE ( 11, *) 'PU;SP0;'
         WRITE ( 11, *) ESC//'.Z'
         ISINIT = .FALSE.
         CLOSE(UNIT=11)
      ELSEIF ( IND.EQ.99) THEN
* SELECT NEW COLOR, THE VALUE IS IN X
         ERASE = .FALSE.
         ICOLOR = INT( X )
         IF ( ICOLOR .GT. NCOLOR ) THEN
            ICOLOR = NCOLOR
         ELSEIF( ICOLOR .LT. 1 ) THEN
            ICOLOR = 1
         ENDIF
         IF ( ICOLOR .NE. LCOLOR) THEN
            IF(.NOT. SORT)THEN
          WRITE ( 11, '('' PU;SP'',I1,'';'')' ) ICOLOR
            ELSE
          CALL HPSORT( IXLX, IYLY, 2, ICOLOR)
          ENDIF
           LCOLOR = ICOLOR
         ENDIF
      ELSE
C WE HAVE AN ERROR IN IND
         WRITE ( *, *) ' ERROR IN PLOT, IND=',IND

      ENDIF
      RETURN
      END

      SUBROUTINE HPSORT( X, Y, PEN, COLOR)
      IMPLICIT INTEGER (A-Z)
      CHARACTER*2  STPEN
      PARAMETER (STORES=150, MAXCOL=8)

*  HERE I CREATE A SMALL STORAGE AREA TO SORT COLORS
*
      DIMENSION SX(STORES,MAXCOL), SY(STORES,MAXCOL)
      DIMENSION SPEN(STORES,MAXCOL), ISORT(MAXCOL)
      LOGICAL PENUP
      DATA LASCOL / 0 /

      IF( STORES.LT.1) RETURN

      IF ( COLOR .LT. 0) THEN
* INITIALIZE ARRAY
         DO 10 I=1, MAXCOL
            ISORT( I) = 0
 10      CONTINUE
         LASCOL = 0
         PENUP = .TRUE.

      ELSEIF( COLOR .GT. 100 ) THEN
* FLUSH REST OF DATA
         DO 29 ICOL= MAXCOL, 1, -1
            IF ( ISORT( ICOL) .GT. 0 ) THEN
               IF ( LASCOL .NE. ICOL) THEN
                  WRITE ( 11, '('' PU;SP'',I1,'';'')' ) ICOL
                  PENUP = .TRUE.
                  LASCOL = ICOL
               ENDIF
               DO 25 I=1, ISORT( ICOL)
                  IF ( SPEN( I, ICOL) .EQ. 2) THEN
                     STPEN = 'PU'
                     PENUP = .TRUE.
                  ELSE
                     STPEN = 'PD'
                     PENUP = .FALSE.
                  ENDIF
                  WRITE(11,'( '' '',A2,'';PA'', I5, '','', I5,'';'')') 
     1                STPEN, SX( I, ICOL), SY( I, ICOL)
 25           CONTINUE
           ENDIF
 29     CONTINUE
        LASCOL = 0

      ELSEIF ( COLOR.LT.1 ) THEN
* ERROR HERE:
      WRITE(6,*)' Color:',COLOR
      ELSEIF ( COLOR.GT.MAXCOL ) THEN
      WRITE(6,*)' Color:',COLOR
      ELSE
* STORE VALUE
        ITEMP = ISORT( COLOR) + 1
        ISORT( COLOR) = ITEMP
        SX( ITEMP, COLOR) = X
        SY( ITEMP, COLOR) = Y
        SPEN( ITEMP, COLOR) = PEN
        IF ( ( ITEMP.GT.STORES-20 .AND.PEN.EQ.2) .OR.
     .           ITEMP.GE.STORES ) THEN
           IF ( LASCOL .NE. COLOR) THEN
              WRITE ( 11, '('' PU;SP'',I1,'';'')' ) COLOR
              PENUP = .TRUE.
              LASCOL = COLOR
           ENDIF
           DO 50 I=1, ITEMP
              IF ( SPEN( I, COLOR) .EQ. 2) THEN
                 IF ( .NOT. PENUP ) THEN
                   STPEN = 'PU'
                   PENUP = .TRUE.
                 ELSE
                   STPEN = '  '
                 ENDIF
              ELSE
                 STPEN = 'PD'
                 PENUP = .FALSE.
              ENDIF
              WRITE(11,'( '' '',A2,'';PA'', I5, '','', I5,'';'')') 
     1                STPEN, SX( I, COLOR), SY( I, COLOR)
 50        CONTINUE
           ISORT( COLOR) = 1
           SX( 1, COLOR) = SX( ITEMP, COLOR)
           SY( 1, COLOR) = SY( ITEMP, COLOR)
           SPEN( 1, COLOR) = 2
        ENDIF
      ENDIF
      RETURN
      END
      SUBROUTINE LOWER(A,N)
      CHARACTER*(*)A
      LOWA=ICHAR('a')
      LUPA=ICHAR('A')
      LUPZ=ICHAR('Z')
      DO 10 I=1,N
      IF(ICHAR(A(I:I)).GE.LUPA.AND.ICHAR(A(I:I)).LE.LUPZ)THEN
      J=ICHAR(A(I:I))+LOWA-LUPA
      A(I:I)=CHAR(J)
      ENDIF
  10  CONTINUE
      RETURN
      END
