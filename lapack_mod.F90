      module lapack_mod
      implicit none

      interface
      subroutine dgemm(transA,transB,m,n,k,                              &
     & alpha,A,lda, B,ldb, beta, C,ldc)
      implicit none
      character transA,transB
      integer m,n,k,lda,ldb,ldc
      real*8 alpha,beta
      real*8 A(lda,*),B(ldb,*),C(ldc,*)
      end subroutine dgemm
      end interface

      end module lapack_mod
