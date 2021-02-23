      subroutine dgemm(transA,transB,m,n,kk,                              &
     & alpha, A,lda, B,ldb, beta, C,ldc )
#ifdef _OPENACC
!$acc routine vector
#else
!$omp declare target
#endif
      implicit none
      character, intent(in) :: transA,transB
      integer, intent(in) :: m,n,kk,lda,ldb,ldc
      real*8, intent(in) :: alpha,beta
      real*8, intent(in) :: A(lda,*), B(ldb,*)
      real*8, intent(inout) :: C(ldc,*)

      integer, parameter :: nb = 64
      integer :: i,j,k
      integer :: istart,iend, jstart,jend
      real*8 :: cij,aik,bkj
      logical :: is_transA, is_transB

      is_transA = (transA.eq.'T').or.(transA.eq.'t')
      is_transB = (transB.eq.'T').or.(transB.eq.'t')
#ifdef _OPENACC
!$acc loop vector collapse(2)
#else
!$omp parallel do simd  collapse(2)
#endif
      do j=1,n
      do i=1,m
        if (beta.eq.0) then
                C(i,j) = 0
        else
                C(i,j) = beta * C(i,j)
        endif
      enddo
      enddo

      do jstart=1,n,nb
      do istart=1,m,nb
         jend = min(n,jstart+nb-1)
         iend = min(m,istart+nb-1)
#ifdef _OPENACC
!$acc loop vector collapse(2) private(cij,aik,bkj,k)
#else
!$omp parallel do simd collapse(2) private(cij,aik,bkj,k)
#endif
      do j=jstart,jend
      do i=istart,iend
        cij = 0
        do k=1,kk
          if (is_transA) then
                  aik = A(k,i)
          else
                  aik = A(i,k)
          endif
          if (is_transB) then
                  bkj = B(j,k)
          else
                  bkj = B(k,j)
          endif
          cij = cij + aik*bkj
        enddo
        C(i,j) = C(i,j) + alpha*cij
      enddo
      enddo

      enddo
      enddo

      return
      end subroutine dgemm
