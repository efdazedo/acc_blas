      real*8 function dnrm2(n,x,incx)
      implicit none
#ifdef _OPENACC
!$acc routine vector
#else
!$omp declare target
#endif
      integer, intent(in) :: n, incx
      real*8, intent(in) :: x(*)

      integer :: i,ix
      real*8 :: dsum

      dsum = 0
#ifdef _OPENACC
!$acc loop vector private(ix) reduction(+:dsum)
#else
!$omp parallel do simd private(ix) reduction(+:dsum)
#endif
      do i=1,n
        ix = 1 + (i-1)*incx
        dsum = dsum + x(i)*x(i)
      enddo

      dnrm2 = sqrt( dsum )
      return
      end function dnrm2


