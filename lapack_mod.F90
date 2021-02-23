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

      subroutine dtrmv(uplo,trans,diag,n,A,lda,X,incx)
      implicit none
      character uplo,trans,diag
      integer n,lda,incx
      real*8 A(lda,*)
      real*8 X(*)
      end subroutine dtrmv

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

      interface
      subroutine dgeqrf(m,n,A,lda,tau,work,lwork,info)
      implicit none
      integer m,n,lda,lwork,info
      real*8 A(lda,*), tau(*)
      real*8 work(*)
      end subroutine dgeqrf

      subroutine dgeqrs(m,n,nrhs,A,lda,tau,B,ldb,work,lwork,info)
      implicit none
      integer m,n,nrhs,lda,ldb,lwork,info
      real*8 A(lda,*), tau(*), B(ldb,*), work(*)
      end subroutine dgeqrs

      end interface

      interface
      subroutine dgesv(n,nrhs,A,lda,ipiv,B,ldb,info)
      implicit none
      integer n,nrhs,lda,ldb,info
      integer ipiv(*)
      real*8 A(lda,*),B(ldb,*)
      end subroutine dgesv

      subroutine dgetrf(m,n,A,lda,ipiv,info)
      implicit none
      integer m,n,lda,info
      integer ipiv(*)
      real*8 A(lda,*)
      end subroutine dgetrf

      subroutine dgetrs(trans,n,nrhs, A,lda,ipiv,B,ldb,info)
      implicit none
      character trans
      integer n,nrhs,lda,ldb,info
      integer ipiv(*)
      real*8 A(lda,*), B(ldb,*)
      end subroutine dgetrs

      end interface


      end module lapack_mod
