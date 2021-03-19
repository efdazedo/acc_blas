      integer function idamax(n,dx,incx)
      implicit none
#ifdef _OPENACC
!$acc routine vector
#else
!$omp declare target
#endif

      integer, intent(in) :: n, incx
      real*8, intent(in) :: dx(*)

      integer :: i,ix,ipos
      real*8 :: dx_max

      if (n <= 0) then
              return
      endif

      dx_max = abs(dx(1))
#ifdef _OPENACC
!$acc loop vector private(ix) reduction(max:dx_max)
#else
!$omp parallel do simd private(ix) reduction(max:dx_max)
#endif
      do i=1,n
       ix = 1 + (i-1)*incx
       dx_max = max(dx_max, abs(dx(ix)))
      enddo

      ipos = n+1
#ifdef _OPENACC
!$acc loop vector private(ix) reduction(min:ipos)
#else
!$omp parallel do simd private(ix) reduction(min:ipos)
#endif
      do i=1,n
        ix = 1 + (i-1)*incx
        if (abs(dx(ix)).eq.dx_max) then
                ipos = i
        endif
      enddo

      idamax = ipos
      return
      end function idamax
