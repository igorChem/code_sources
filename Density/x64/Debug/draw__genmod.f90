        !COMPILER-GENERATED INTERFACE MODULE: Tue Mar 27 23:54:12 2018
        MODULE DRAW__genmod
          INTERFACE 
            SUBROUTINE DRAW(IZ,JZ,JDIR,A,B,C,PZ,I1,I2,I3,MDIM)
              INTEGER(KIND=4) :: MDIM
              INTEGER(KIND=4) :: IZ
              INTEGER(KIND=4) :: JZ
              INTEGER(KIND=4) :: JDIR
              REAL(KIND=8) :: A(MDIM,MDIM)
              LOGICAL(KIND=4) :: B(25000)
              LOGICAL(KIND=4) :: C(18000,2)
              REAL(KIND=8) :: PZ
              INTEGER(KIND=4) :: I1
              INTEGER(KIND=4) :: I2
              INTEGER(KIND=4) :: I3
            END SUBROUTINE DRAW
          END INTERFACE 
        END MODULE DRAW__genmod
