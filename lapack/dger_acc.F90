        subroutine dger(m,n,alpha,x,incx,y,incy,A,lda)
#ifdef _OPENACC
!$acc routine vector
#else
!$omp declare target
#endif
        implicit none
        integer, intent(in) :: m,n,incx,incy,lda
        real*8, intent(in) :: alpha
        real*8, intent(in) :: x(*), y(*)
        real*8, intent(inout) :: A(lda,*)

        integer :: i,j,ix,jy,ij


#ifdef _OPENACC
!$acc loop vector  private(i,j,ix,jy)
#else

!$omp parallel do simd  private(i,j,ix,jy) 

#endif

        do ij=1,n*m
          ! ----------------
          ! ij = i + (j-1)*m
          ! ----------------
          i = mod( (ij-1),m) + 1
          j = int( (ij-i)/m) + 1
          ix = 1 + (i-1)*incx
          jy = 1 + (j-1)*incy
          A(i,j) = A(i,j) + alpha * x(ix) * y(jy)
        enddo

        return
        end subroutine dger
