      subroutine dscal(n,da,dx,incx)
      implicit none
#ifdef _OPENACC
!$acc routine vector
#else
!$omp declare target
#endif
      integer, value :: n, incx
      real*8, value :: da
      real*8, intent(inout) :: dx(*)

      integer :: i, ix

#ifdef _OPENACC
!$acc loop vector private(ix)
#else
!$omp parallel do simd private(ix)
#endif
      do i=1,n
        ix = 1 + (i-1)*incx
        dx(ix) = dx(ix) * da
      enddo
      return
      end subroutine dscal
