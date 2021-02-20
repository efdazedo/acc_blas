      real*8 function dnrm2(n,x,incx)
#ifdef _OPENACC
!$acc routine vector
#else
!$omp declare target
#endif
      implicit none
      integer, intent(in) :: n, incx
      real*8, intent(in) :: x(*)

      integer :: i,ix
      real*8 :: dsum

      dsum = 0
#ifdef _OPENMP
!$acc loop vector private(ix)
#else
!$omp parallel do simd private(ix)
#endif
      do i=1,n
        ix = 1 + (i-1)*incx
        dsum = dsum + x(i)*x(i)
      enddo

      dnrm2 = sqrt( dsum )
      return
      end function dnrm2


