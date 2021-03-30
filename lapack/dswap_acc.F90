      subroutine dswap(n,dx,incx,dy,incy)
      implicit none
#ifdef _OPENACC
!$acc routine vector
#else
!$omp declare target
#endif
      integer, value :: n,incx,incy
      real*8, intent(inout) :: dx(*), dy(*)

      integer i,ix,iy
      real*8 :: temp

#ifdef _OPENACC
!$acc loop vector private(ix,iy,temp)
#else
!$omp parallel do simd private(ix,iy,temp)
#endif
      do i=1,n
       ix = 1 + (i-1)*incx
       iy = 1 + (i-1)*incy

       temp = dx(ix)
       dx(ix) = dy(iy)
       dy(iy) = temp
      enddo

      return
      end subroutine dswap
