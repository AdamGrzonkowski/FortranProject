      PROGRAM Linear_least_squares_fitting

      REAL x(20), y(20), a, b, xAvg, D, yDelta, aDelta, bDelta
      INTEGER n
      CHARACTER*(1) answer

C     Program fits a straight line to a set of experimental points
C     using the linear least squares fitting.
C     Also, it calculates standard deviations of both parameters: a & b.


C     Getting number of experimental points
 10   PRINT *, 'Insert number of experimental points. (min=3, max=20)..'
      READ *, n
      IF ( (n.GT.20) .OR. (n.LT.3) ) THEN
        PRINT *, 'Number out of range!'
        STOP
      ENDIF

C     Getting coordinates of the points
      PRINT *, ''
      PRINT *, 'Insert the coordinates of the points (alternately x & y'
      PRINT *, 'separated by a space or ENTER without parentheses and'
      PRINT *, 'commas. The entire values separated by a dot from'
      PRINT *, 'decimal ones. For example: 2 3 2.34 5'
      READ *, (x(i), y(i), i=1, n)

C     Counting average value of x point
      xAvg = 0
      DO i=1, n
        xAvg = xAvg + x(i)
      ENDDO
      xAvg = xAvg / n

C     Counting a numerator of standard deviation
      D = 0
      DO i=1, n
        D = D + ( x(i) - xAvg )**2
      ENDDO

      IF (D.EQ.0) THEN
        PRINT *, 'Searched line is most likely parallel to the y-axis.'
        PRINT *, 'Can not count parameters a & b of the equation.'
        STOP
      ENDIF

C     Counting parameter 'a' of the searched straight line
C      a = 0
C      DO i=1, n
C        a = a + y(i) * ( x(i) - xAvg )
C      ENDDO
C      a = a / D

      a = a_(x, y, xAvg, D, n)


C     Counting parameter 'b' of the searched straight line
      b = 0
      DO i=1, n
        b = b + ( y(i) - a*x(i) )
      ENDDO
      b = b / n

C     Counting measurement uncertainty
      yDelta = 0
      DO i=1, n
        yDelta = yDelta + ( y(i) - a*x(i) - b)**2
      ENDDO
      yDelta = yDelta / (n-2)
      yDelta = SQRT(yDelta)

      aDelta = yDelta / SQRT( D )
      bDelta = yDelta * SQRT( 1/n + (xAvg**2)/D )

C     Displaying results on the screen

      PRINT *, ''
      PRINT *, 'Inserted experimental points are best fitted by a line:'
      PRINT *, '     y =', a, ' x +', b
      PRINT *, ''
      PRINT *, 'while deltas of a & b have values:'
      PRINT *, '     ', aDelta, '  ,  ', bDelta

C     Saving results to the file
      PRINT *, ''
      PRINT *, 'Do you want to export the result to the file? [Y / N]'
      READ *, answer
      IF (answer.EQ.'Y' .OR. answer.EQ.'y') THEN
        OPEN (30, FILE = 'result.dat')
        WRITE(30,*) 'The result is y =', a, ' x +', b
        WRITE(30,*) 'deltaA =',aDelta, ' ,  deltaB =',bDelta
        WRITE(30,*) ''
        WRITE(30,*) 'Experimental points:'
        WRITE(30,*) '     x              y'
        DO i=1, n
          WRITE(30,100) x(i), y(i)
        ENDDO
100     format(F10.3,5x,F10.3)
        CLOSE( UNIT = 30)
        PRINT *, 'Results written to the file "result.dat"'
      ENDIF

C     Chance to repeat the program
      PRINT *, ''
      PRINT *, 'Do you want to repeat the program? [Y / N]'
      READ *, answer
      IF (answer.EQ.'Y' .OR. answer.EQ.'y') THEN
        PRINT *, ''
        GOTO 10
      ELSE
        PRINT *, 'Thanks, have a great day!'
      ENDIF

      ENDPROGRAM


*     Counting parameter 'a' - function
      REAL FUNCTION a_(x,y,xAvg,D,n)
      INTEGER i, n
      REAL xAvg, D, x(n), y(n)

        a_ = 0
        DO i=1, n
          a_ = a_ + y(i) * ( x(i) - xAvg )
        ENDDO
        a_ = a_ / D

        RETURN
      END
