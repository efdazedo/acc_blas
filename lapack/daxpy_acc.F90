      subroutine daxpy(n,da,dx,incx,dy,incy)
      implicit none
#ifdef _OPENACC
!$acc routine vector
#else
!$omp declare target
#endif
      integer, value :: n,incx,incy
      real*8, value :: da
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

        dy(iy) = dy(iy) + da * dx(ix) 
      enddo
      return
      end subroutine daxpy
