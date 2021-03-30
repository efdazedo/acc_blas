      SUBROUTINE DGER  ( M, N, ALPHA, X, INCX, Y, INCY, A, LDA )
#ifdef _OPENACC
!$acc routine vector
#else
!$omp declare target
#endif
!     .. Scalar Arguments ..
      DOUBLE PRECISION, value ::    ALPHA
      INTEGER, value ::            INCX, INCY, LDA, M, N
!     .. Array Arguments ..
      DOUBLE PRECISION, intent(in)    ::   X( * ), Y( * )
      DOUBLE PRECISION, intent(inout) ::   A( LDA, * )

!     ..
!
!  Purpose
!  =======
!
!  DGER   performs the rank 1 operation
!
!     A := alpha*x*y' + A,
!
!  where alpha is a scalar, x is an m element vector, y is an n element
!  vector and A is an m by n matrix.
!
!  Parameters
!  ==========
!
!  M      - INTEGER.
!           On entry, M specifies the number of rows of the matrix A.
!           M must be at least zero.
!           Unchanged on exit.
!
!  N      - INTEGER.
!           On entry, N specifies the number of columns of the matrix A.
!           N must be at least zero.
!           Unchanged on exit.
!
!  ALPHA  - DOUBLE PRECISION.
!           On entry, ALPHA specifies the scalar alpha.
!           Unchanged on exit.
!
!  X      - DOUBLE PRECISION array of dimension at least
!           ( 1 + ( m - 1 )*abs( INCX ) ).
!           Before entry, the incremented array X must contain the m
!           element vector x.
!           Unchanged on exit.
!
!  INCX   - INTEGER.
!           On entry, INCX specifies the increment for the elements of
!           X. INCX must not be zero.
!           Unchanged on exit.
!
!  Y      - DOUBLE PRECISION array of dimension at least
!           ( 1 + ( n - 1 )*abs( INCY ) ).
!           Before entry, the incremented array Y must contain the n
!           element vector y.
!           Unchanged on exit.
!
!  INCY   - INTEGER.
!           On entry, INCY specifies the increment for the elements of
!           Y. INCY must not be zero.
!           Unchanged on exit.
!
!  A      - DOUBLE PRECISION array of DIMENSION ( LDA, n ).
!           Before entry, the leading m by n part of the array A must
!           contain the matrix of coefficients. On exit, A is
!           overwritten by the updated matrix.
!
!  LDA    - INTEGER.
!           On entry, LDA specifies the first dimension of A as declared
!           in the calling (sub) program. LDA must be at least
!           max( 1, m ).
!           Unchanged on exit.
!
!
!  Level 2 Blas routine.
!
!  -- Written on 22-October-1986.
!     Jack Dongarra, Argonne National Lab.
!     Jeremy Du Croz, Nag Central Office.
!     Sven Hammarling, Nag Central Office.
!     Richard Hanson, Sandia National Labs.
!
!
!     .. Parameters ..
      DOUBLE PRECISION   ZERO
      PARAMETER        ( ZERO = 0.0D+0 )
!     .. Local Scalars ..
      DOUBLE PRECISION   TEMP
      INTEGER            I, INFO, IX, IX0, J, JY, JY0, KX 
#if (0)
!     .. External Subroutines ..
      EXTERNAL           XERBLA
#endif
!     .. Intrinsic Functions ..
      INTRINSIC          MAX
!     ..
!     .. Executable Statements ..
!
!     Test the input parameters.
!
      INFO = 0
      IF     ( M.LT.0 )THEN
         INFO = 1
      ELSE IF( N.LT.0 )THEN
         INFO = 2
      ELSE IF( INCX.EQ.0 )THEN
         INFO = 5
      ELSE IF( INCY.EQ.0 )THEN
         INFO = 7
      ELSE IF( LDA.LT.MAX( 1, M ) )THEN
         INFO = 9
      END IF
      IF( INFO.NE.0 )THEN
         CALL XERBLA( 'DGER  ', INFO )
         goto 999
      END IF
!
!     Quick return if possible.
!
      IF( ( M.EQ.0 ).OR.( N.EQ.0 ).OR.( ALPHA.EQ.ZERO ) )
     $   goto 999
!
!     Start the operations. In this version the elements of A are
!     accessed sequentially with one pass through A.
!
      IF( INCY.GT.0 )THEN
         JY = 1
      ELSE
         JY = 1 - ( N - 1 )*INCY
      END IF
      JY0 = JY
      IF( INCX.EQ.1 )THEN
#ifdef _OPENACC
!$acc    loop  vector collapse(2) private(JY,TEMP)
#else
!$omp parallel do simd collapse(2) private(JY,TEMP)
#endif
         DO J = 1, N
         DO I = 1, M
            JY = JY0 + (J-1)*INCY
            TEMP = ALPHA*Y( JY )
            A( I, J ) = A( I, J ) + X( I )*TEMP
         ENDDO
         ENDDO
      ELSE
         IF( INCX.GT.0 )THEN
            KX = 1
         ELSE
            KX = 1 - ( M - 1 )*INCX
         END IF
         IX0  = KX
#ifdef _OPENACC
!$acc loop vector collapse(2) private(JY,TEMP,IX)
#else
!$omp parallel do simd collapse(2) private(JY,TEMP,IX)
#endif
         DO J = 1, N
         DO I = 1, M
            JY = JY0 + (J-1)*INCY
            TEMP = ALPHA*Y( JY )
            IX = IX0 + (I-1)*INCX
            A( I, J ) = A( I, J ) + X( IX )*TEMP
         ENDDO
         ENDDO
      END IF
!
 999  continue
      RETURN
!
!     End of DGER  .
!
      END SUBROUTINE DGER
