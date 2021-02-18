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



      interface
      subroutine dgbmv(trans,m,n,kl,ku,alpha,A,lda,                       &
     &                 x,incx,beta,y,incy)
      implicit none
      character trans
      integer m,n,kl,ku,lda,incx,incy
      real*8 alpha,beta,x(*),y(*)
      real*8 A(lda,*)
      end subroutine dgbmv

      subroutine dgbsv(n,kl,ku,nrhs,AB,ldab,ipiv,B,ldb,info)
      implicit none
      integer n,kl,ku,nrhs,ldab,ldb
      integer ipiv(*)
      integer info
      real*8 AB(ldab,*)
      real*8 B(ldb,*)
      end subroutine dgbsv

      end interface

      end module lapack_mod
