        subroutine dger(m,n,alpha,x,incx,y,incy,A,lda)
        implicit none
#ifdef _OPENACC
!$acc routine vector
#else
!$omp declare target
#endif
! ---------------------------------------------
! perform rank-1 update A = alpha * x * y' + A
! ---------------------------------------------
        integer, value :: m,n,incx,incy,lda
        real*8, value :: alpha
        real*8, intent(in) :: x(*), y(*)
        real*8, intent(inout) :: A(lda,*)

        integer :: i,j
        real*8 :: xi, yj


#ifdef _OPENACC
!$acc loop vector  collapse(2) private(xi,yj)
#else
!$omp parallel do simd  collapse(2) private(xi,yj)
#endif
        do j=1,n
        do i=1,m
          xi = x(1 + (i-1)*incx)
          yj = y(1 + (j-1)*incy)
          A(i,j) = A(i,j) + alpha * xi * yj
        enddo
        enddo

        return
        end subroutine dger
