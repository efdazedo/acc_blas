      subroutine dcopy(n,dx,incx,dy,incy)
      implicit none
#ifdef _OPENACC
!$acc routine vector
#else
!$omp declare target
#endif
      integer, value :: n,incx,incy
      real*8, intent(in) :: dx(*)
      real*8, intent(inout) :: dy(*)

      integer :: i,ix,iy

#ifdef _OPENACC
!$acc loop vector private(ix,iy)
#else
!$omp parallel do simd private(ix,iy)
#endif
      do i=1,n
       ix = 1 + (i-1)*incx
       iy = 1 + (i-1)*incy
       dy(iy) = dx(ix)
      enddo

      return
      end subroutine dcopy
