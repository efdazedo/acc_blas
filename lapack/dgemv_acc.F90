      subroutine dgemv( trans, m,n,alpha, A,lda, x,incx,beta,y,incy)
#ifdef _OPENACC
!$acc routine vector
#else
!$omp declare target
#endif
      implicit none
      character, intent(in) :: trans
      integer, intent(in) :: m,n,lda,incx,incy
      real*8, intent(in) :: alpha,beta
      real*8, intent(in) :: x(*)
      real*8, intent(inout) :: y(*)
      real*8, intent(in) :: A(lda,*)

!  -----------------------
!  if notrans, y(1:m) = beta * y(1:m) + A(1:m,1:n) * x(1:n)
!  otherwise,  y(1:n) = beta * y(1:n) + trans(A(1:m,1:n))*x(1:m)
!  -----------------------

      integer :: i,j,jx,iy
      logical :: is_TT,is_TC,is_TN, istrans,notrans
      integer :: nrowx, nrowy
      real*8 :: yi, aij

      is_TT = (trans.eq.'T').or.(trans.eq.'t')
      is_TC = (trans.eq.'C').or.(trans.eq.'c')
      is_TN = (trans.eq.'N').or.(trans.eq.'n')

      istrans = (is_TT.or.is_TC)
      notrans = is_TN .or. (.not. istrans)

      if (notrans) then
!  ------------------------
!  if notrans, y(1:m) = beta * y(1:m) + A(1:m,1:n) * x(1:n)
!  ------------------------

#ifdef _OPENACC
!$acc loop vector private(yi,j,jx,iy)
#else
!$omp parallel do simd private(yi,j,jx,iy)
#endif
         do i=1,m

          yi = 0
          do j=1,n
            jx = 1 + (j-1)*incx
            yi = yi + A(i,j) * x(jx)
          enddo

          iy = 1 + (i-1)*incy
          if (beta.eq.0) then
                  y(iy) = alpha * yi
          else
                  y(iy) = beta*y(iy) + alpha * yi
          endif

          enddo
       else
!  -------------
!  otherwise,  y(1:n) = beta * y(1:n) + trans(A(1:m,1:n))*x(1:m)
!  -------------
#ifdef _OPENACC
!$acc loop vector private(yi,j,jx,iy)
#else
!$omp parallel do simd private(yi,j,jx,iy)
#endif
               do i=1,n

                 yi = 0
                 do j=1,m
                   jx = 1 + (j-1)*incx
                   yi = yi + A(j,i) * x(jx)
                 enddo

                 iy = 1 + (i-1)*incy
                 if (beta.eq.0) then
                    y(iy) = alpha * yi
                 else
                    y(iy) = beta * y(iy) + alpha * yi
                 endif

               enddo
       endif
       return
       end subroutine dgemv
