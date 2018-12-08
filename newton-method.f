CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C               ROOT-FINDING: NEWTON'S METHOD 
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C For: MATH 116 at Dartmouth College Winter '17
C 
C Uses Newton's method to find the root of the user-defined
C functions F(X) and DF(X).
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
      PROGRAM NEWTON
C
      DL = 1.0E-06
      A  = 0.5
      B  = 1.0 
      DX = B - A
      X0 = (A+B)/2.0
      ISTEP = 0
      DO    100  WHILE (ABS(DX).GT.DL)
        X1 = X0 - F(X0)/DF(X0)
        DX = X1 - X0
        X0 = X1
        ISTEP = ISTEP + 1
        WRITE (6,999) ISTEP,X0,DX
  100 END DO
      STOP
  999 FORMAT (I4,2F16.8)
      END
C
      FUNCTION F(X)
        A = 0
        F = X ** 3 - A 
      RETURN
      END
C
      FUNCTION DF(X)
        A  = 0
        DF = (3 * (X ** 2)) 
      RETURN
      END
