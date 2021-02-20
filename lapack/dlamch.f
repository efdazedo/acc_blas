!> \brief \b DLAMCH
!
!  =========== DOCUMENTATION ===========
!
! Online html documentation available at
!            http://www.netlib.org/lapack/explore-html/
!
!  Definition:
!  ===========
!
!      DOUBLE PRECISION FUNCTION DLAMCH( CMACH )
!
!
!> \par Purpose:
!  =============
!>
!> \verbatim
!>
!> DLAMCH determines double precision machine parameters.
!> \endverbatim
!
!  Arguments:
!  ==========
!
!> \param[in] CMACH
!> \verbatim
!>          Specifies the value to be returned by DLAMCH:
!>          = 'E' or 'e',   DLAMCH := eps
!>          = 'S' or 's ,   DLAMCH := sfmin
!>          = 'B' or 'b',   DLAMCH := base
!>          = 'P' or 'p',   DLAMCH := eps*base
!>          = 'N' or 'n',   DLAMCH := t
!>          = 'R' or 'r',   DLAMCH := rnd
!>          = 'M' or 'm',   DLAMCH := emin
!>          = 'U' or 'u',   DLAMCH := rmin
!>          = 'L' or 'l',   DLAMCH := emax
!>          = 'O' or 'o',   DLAMCH := rmax
!>          where
!>          eps   = relative machine precision
!>          sfmin = safe minimum, such that 1/sfmin does not overflow
!>          base  = base of the machine
!>          prec  = eps*base
!>          t     = number of (base) digits in the mantissa
!>          rnd   = 1.0 when rounding occurs in addition, 0.0 otherwise
!>          emin  = minimum exponent before (gradual) underflow
!>          rmin  = underflow threshold - base**(emin-1)
!>          emax  = largest exponent before overflow
!>          rmax  = overflow threshold  - (base**emax)*(1-eps)
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
!> \ingroup auxOTHERauxiliary
!
!  =====================================================================
      DOUBLE PRECISION FUNCTION DLAMCH( CMACH )
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
      CHARACTER          CMACH
!     ..
!
! =====================================================================
!
!     .. Parameters ..
      DOUBLE PRECISION   ONE, ZERO
      PARAMETER          ( ONE = 1.0D+0, ZERO = 0.0D+0 )
!     ..
!     .. Local Scalars ..
      DOUBLE PRECISION   RND, EPS, SFMIN, SMALL, RMACH
      logical :: is_CE,is_CS,is_CB,is_CP,is_CN
      logical :: is_CR,is_CM,is_CU,is_CL,is_CO
#if (0)
!     ..
!     .. External Functions ..
      LOGICAL            LSAME
      EXTERNAL           LSAME
#endif
!     ..
!     .. Intrinsic Functions ..
      INTRINSIC          DIGITS, EPSILON, HUGE, MAXEXPONENT,
     $                   MINEXPONENT, RADIX, TINY
!     ..
!     .. Executable Statements ..
!
!
!     Assume rounding, not chopping. Always.
!
      RND = ONE
!
      is_CE = (CMACH.eq.'E').or.(CMACH.eq.'e')
      is_CS = (CMACH.eq.'S').or.(CMACH.eq.'s')
      is_CB = (CMACH.eq.'B').or.(CMACH.eq.'b')
      is_CP = (CMACH.eq.'P').or.(CMACH.eq.'p')
      is_CN = (CMACH.eq.'N').or.(CMACH.eq.'n')
      
      is_CR = (CMACH.eq.'R').or.(CMACH.eq.'r')
      is_CM = (CMACH.eq.'M').or.(CMACH.eq.'m')
      is_CU = (CMACH.eq.'U').or.(CMACH.eq.'u')
      is_CL = (CMACH.eq.'L').or.(CMACH.eq.'l')
      is_CO = (CMACH.eq.'O').or.(CMACH.eq.'o')

      IF( ONE.EQ.RND ) THEN
         EPS = EPSILON(ZERO) * 0.5
      ELSE
         EPS = EPSILON(ZERO)
      END IF
!
      IF( is_CE ) THEN
         RMACH = EPS
      ELSE IF( is_CS ) THEN
         SFMIN = TINY(ZERO)
         SMALL = ONE / HUGE(ZERO)
         IF( SMALL.GE.SFMIN ) THEN
!
!           Use SMALL plus a bit, to avoid the possibility of rounding
!           causing overflow when computing  1/sfmin.
!
            SFMIN = SMALL*( ONE+EPS )
         END IF
         RMACH = SFMIN
      ELSE IF( is_CB ) THEN
         RMACH = RADIX(ZERO)
      ELSE IF( is_CP ) THEN
         RMACH = EPS * RADIX(ZERO)
      ELSE IF( is_CN ) THEN
         RMACH = DIGITS(ZERO)
      ELSE IF( is_CR ) THEN
         RMACH = RND
      ELSE IF( is_CM ) THEN
         RMACH = MINEXPONENT(ZERO)
      ELSE IF( is_CU ) THEN
         RMACH = tiny(zero)
      ELSE IF( is_CL ) THEN
         RMACH = MAXEXPONENT(ZERO)
      ELSE IF( is_CO ) THEN
         RMACH = HUGE(ZERO)
      ELSE
         RMACH = ZERO
      END IF
!
      DLAMCH = RMACH
      RETURN
!
!     End of DLAMCH
!
      END
!***********************************************************************
!> \brief \b DLAMC3
!> \details
!> \b Purpose:
!> \verbatim
!> DLAMC3  is intended to force  A  and  B  to be stored prior to doing
!> the addition of  A  and  B ,  for use in situations where optimizers
!> might hold one of these in a register.
!> \endverbatim
!> \author LAPACK is a software package provided by Univ. of Tennessee, Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..
!> \date December 2016
!> \ingroup auxOTHERauxiliary
!>
!> \param[in] A
!> \verbatim
!>          A is a DOUBLE PRECISION
!> \endverbatim
!>
!> \param[in] B
!> \verbatim
!>          B is a DOUBLE PRECISION
!>          The values A and B.
!> \endverbatim
!>
      DOUBLE PRECISION FUNCTION DLAMC3( A, B )
#ifdef _OPENACC
!$acc routine vector
#else
!$omp declare target
#endif
!
!  -- LAPACK auxiliary routine (version 3.7.0) --
!     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
!     November 2010
!
!     .. Scalar Arguments ..
      DOUBLE PRECISION   A, B
!     ..
! =====================================================================
!
!     .. Executable Statements ..
!
      DLAMC3 = A + B
!
      RETURN
!
!     End of DLAMC3
!
      END
!
!***********************************************************************
