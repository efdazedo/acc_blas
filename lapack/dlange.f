!> \brief \b DLANGE returns the value of the 1-norm, Frobenius norm, infinity-norm, or the largest absolute value of any element of a general rectangular matrix.
!
!  =========== DOCUMENTATION ===========
!
! Online html documentation available at
!            http://www.netlib.org/lapack/explore-html/
!
!> \htmlonly
!> Download DLANGE + dependencies
!> <a href="http://www.netlib.org/cgi-bin/netlibfiles.tgz?format=tgz&filename=/lapack/lapack_routine/dlange.f">
!> [TGZ]</a>
!> <a href="http://www.netlib.org/cgi-bin/netlibfiles.zip?format=zip&filename=/lapack/lapack_routine/dlange.f">
!> [ZIP]</a>
!> <a href="http://www.netlib.org/cgi-bin/netlibfiles.txt?format=txt&filename=/lapack/lapack_routine/dlange.f">
!> [TXT]</a>
!> \endhtmlonly
!
!  Definition:
!  ===========
!
!       DOUBLE PRECISION FUNCTION DLANGE( NORM, M, N, A, LDA, WORK )
!
!       .. Scalar Arguments ..
!       CHARACTER          NORM
!       INTEGER            LDA, M, N
!       ..
!       .. Array Arguments ..
!       DOUBLE PRECISION   A( LDA, * ), WORK( * )
!       ..
!
!
!> \par Purpose:
!  =============
!>
!> \verbatim
!>
!> DLANGE  returns the value of the one norm,  or the Frobenius norm, or
!> the  infinity norm,  or the  element of  largest absolute value  of a
!> real matrix A.
!> \endverbatim
!>
!> \return DLANGE
!> \verbatim
!>
!>    DLANGE = ( max(abs(A(i,j))), NORM = 'M' or 'm'
!>             (
!>             ( norm1(A),         NORM = '1', 'O' or 'o'
!>             (
!>             ( normI(A),         NORM = 'I' or 'i'
!>             (
!>             ( normF(A),         NORM = 'F', 'f', 'E' or 'e'
!>
!> where  norm1  denotes the  one norm of a matrix (maximum column sum),
!> normI  denotes the  infinity norm  of a matrix  (maximum row sum) and
!> normF  denotes the  Frobenius norm of a matrix (square root of sum of
!> squares).  Note that  max(abs(A(i,j)))  is not a consistent matrix norm.
!> \endverbatim
!
!  Arguments:
!  ==========
!
!> \param[in] NORM
!> \verbatim
!>          NORM is CHARACTER*1
!>          Specifies the value to be returned in DLANGE as described
!>          above.
!> \endverbatim
!>
!> \param[in] M
!> \verbatim
!>          M is INTEGER
!>          The number of rows of the matrix A.  M >= 0.  When M = 0,
!>          DLANGE is set to zero.
!> \endverbatim
!>
!> \param[in] N
!> \verbatim
!>          N is INTEGER
!>          The number of columns of the matrix A.  N >= 0.  When N = 0,
!>          DLANGE is set to zero.
!> \endverbatim
!>
!> \param[in] A
!> \verbatim
!>          A is DOUBLE PRECISION array, dimension (LDA,N)
!>          The m by n matrix A.
!> \endverbatim
!>
!> \param[in] LDA
!> \verbatim
!>          LDA is INTEGER
!>          The leading dimension of the array A.  LDA >= max(M,1).
!> \endverbatim
!>
!> \param[out] WORK
!> \verbatim
!>          WORK is DOUBLE PRECISION array, dimension (MAX(1,LWORK)),
!>          where LWORK >= M when NORM = 'I'; otherwise, WORK is not
!>          referenced.
!> \endverbatim
!
!  Authors:
!  ========
!
!> \author Univ. of Tennessee
!> \author Univ. of California Berkeley
!> \author Univ. of Colorado Denver
!> \author NAG Ltd.
!
!> \date December 2016
!
!> \ingroup doubleGEauxiliary
!
!  =====================================================================
      DOUBLE PRECISION FUNCTION DLANGE( NORM, M, N, A, LDA, WORK )
      implicit none
#ifdef _OPENACC
!$acc routine vector 
#else
!$omp declare target
#endif
!
!  -- LAPACK auxiliary routine (version 3.7.0) --
!  -- LAPACK is a software package provided by Univ. of Tennessee,    --
!  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..--
!     December 2016
!
!     .. Scalar Arguments ..
      CHARACTER          NORM
      INTEGER            LDA, M, N
!     ..
!     .. Array Arguments ..
      DOUBLE PRECISION   A( LDA, * ), WORK( * )
!     ..
!
! =====================================================================
!
!     .. Parameters ..
      DOUBLE PRECISION   ONE, ZERO
      PARAMETER          ( ONE = 1.0D+0, ZERO = 0.0D+0 )
!     ..
!     .. Local Scalars ..
      INTEGER            I, J
      DOUBLE PRECISION   SCALE, SUM, VALUE, TEMP
#if (0)
!     ..
!     .. External Subroutines ..
      EXTERNAL           DLASSQ
!     ..
!     .. External Functions ..
      LOGICAL            LSAME, DISNAN
      EXTERNAL           LSAME, DISNAN
#endif
      logical :: is_norm_M, is_norm_O, is_norm_I
      logical :: is_norm_E, is_norm_F
!     ..
!     .. Intrinsic Functions ..
      INTRINSIC          ABS, MIN, SQRT
!     ..
!     .. Executable Statements ..
!
      is_norm_M = (NORM.eq.'M').or.(NORM.eq.'m')
      is_norm_O = (NORM.eq.'O').or.(NORM.eq.'o')
      is_norm_I = (NORM.eq.'I').or.(NORM.eq.'i')
      is_norm_E = (NORM.eq.'E').or.(NORM.eq.'e')
      is_norm_F = (NORM.eq.'F').or.(NORM.eq.'f')


      IF( MIN( M, N ).EQ.0 ) THEN
         VALUE = ZERO
      ELSE IF( is_norm_M ) THEN
!
!        Find max(abs(A(i,j))).
!
         VALUE = ZERO
#ifdef _OPENACC
!$acc  loop vector                                                       &
!$acc& collapse(2) private(TEMP) reduction(max:VALUE)
#else
!$omp  parallel do simd collapse(2) private(TEMP) reduction(max:VALUE)
#endif
         DO 20 J = 1, N
            DO 10 I = 1, M
               TEMP = ABS( A( I, J ) )
               IF( VALUE.LT.TEMP .OR. DISNAN( TEMP ) ) VALUE = TEMP
   10       CONTINUE
   20    CONTINUE
      ELSE IF( ( is_norm_O ) .OR. ( NORM.EQ.'1' ) ) THEN
!
!        Find norm1(A).
!
         VALUE = ZERO
#ifdef _OPENACC
!$acc loop vector private(I,SUM) reduction(max:VALUE)
#else
!$omp parallel do simd private(I,SUM) reduction(max:VALUE)
#endif
         DO 40 J = 1, N

            SUM = ZERO
            DO 30 I = 1, M
               SUM = SUM + ABS( A( I, J ) )
   30       CONTINUE

            IF( VALUE.LT.SUM .OR. DISNAN( SUM ) ) VALUE = SUM
   40    CONTINUE
      ELSE IF( is_norm_I ) THEN
!
!        Find normI(A).
!

#ifdef _OPENACC
!$acc loop vector
#else
!$omp parallel do simd
#endif
         DO 50 I = 1, M
            WORK( I ) = ZERO
   50    CONTINUE


         DO 70 J = 1, N
#ifdef _OPENACC
!$acc loop vector
#else
!$omp parallel do simd
#endif
            DO 60 I = 1, M
               WORK( I ) = WORK( I ) + ABS( A( I, J ) )
   60       CONTINUE
   70    CONTINUE

         VALUE = ZERO

#ifdef _OPENACC
!$acc loop vector private(TEMP) reduction(max:VALUE)
#else
!$omp parallel do simd private(TEMP) reduction(max:VALUE)
#endif
         DO 80 I = 1, M
            TEMP = WORK( I )
            IF( VALUE.LT.TEMP .OR. DISNAN( TEMP ) ) VALUE = TEMP
   80    CONTINUE
      ELSE IF( ( is_norm_F ) .OR. ( is_norm_E ) ) THEN
#if defined(OMP_TARGET) || defined(_OPENACC)
      VALUE = ZERO


#ifdef _OPENACC
!$acc loop vector collapse(2) reduction(+:VALUE)
#else
!$omp parallel do simd collapse(2) reduction(+:VALUE)
#endif
      do j=1,N
      do i=1,M
        VALUE = VALUE + abs(A(I,J))**2
      enddo
      enddo
      VALUE = sqrt( VALUE )
#else
!
!        Find normF(A).
!
         SCALE = ZERO
         SUM = ONE
         DO 90 J = 1, N
            CALL DLASSQ( M, A( 1, J ), 1, SCALE, SUM )
   90    CONTINUE
         VALUE = SCALE*SQRT( SUM )
#endif
      END IF
!
      DLANGE = VALUE
      RETURN
!
!     End of DLANGE
!
      END
