      real*8 function ddot(n,dx,incx,dy,incy)
#ifdef _OPENACC
!$acc routine vector
#else
!$omp declare target
#endif
      implicit none
      integer, intent(in) :: n,incx,incy
      real*8, intent(in) :: dx(*), dy(*)

      integer :: i,ix,iy
      real*8 :: dsum

      dsum = 0
#ifdef _OPENACC
!$acc loop vector private(ix,iy) reduction(+:dsum)
#else
!$omp parallel do simd private(ix,iy) reduction(+:dsum)
#endif
      do i=1,n
        ix = 1 + (i-1)*incx
        iy = 1 + (i-1)*incy
        dsum = dsum + dx(ix)*dy(iy)
      enddo

      ddot = dsum
      return
      end function ddot
