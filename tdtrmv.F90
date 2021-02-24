
!     simple test of calling dtrmv
      program tdtrmv
      use lapack_mod, only : dtrmv_cpu => dtrmv
      use lapack_acc, only : dtrmv_acc => dtrmv
      implicit none
      integer, parameter :: idebug = 0
      double precision, parameter :: tol = 1d-6

      integer :: n,nmat
      integer :: nthreads
      double precision, allocatable, dimension(:,:,:) :: A
      double precision, allocatable :: X(:,:), x_cpu(:,:), x_acc(:,:)
      

      integer :: imat,incx
      
      integer :: iuplo,itrans,idiag
      character :: uplo, trans, diag
      integer :: lda, ldx

      double precision :: maxerr
      integer :: nmax
      integer :: nerrors
      logical :: isok


      nerrors = 0

      nthreads = 1
!$omp parallel
!$omp master
!$    nthreads = omp_get_num_threads()
!$omp end master
!$omp end parallel

      nmat = 4 
      nmax = 100
      n = nmax
      print*,'n,nmat,nthreads',n,nmat,nthreads

      ldx = 10*nmax
      allocate( A(nmax,nmax,nmat)) 
      allocate( X(ldx,nmat), x_cpu(ldx,nmat), x_acc(ldx,nmat))
      lda = size(A,1)

         call random_number(A)
         call random_number(X)

!$acc    enter data copyin(A,X)

      do iuplo=0,1
      do itrans=0,1
      do idiag=0,1
      do incx=1,2

      uplo  = merge('U','L',iuplo.eq.0)
      trans = merge('N','T',itrans.eq.0)
      diag  = merge('N','U',idiag.eq.0)

      x_cpu = X
      x_acc = X

!$acc data copy(x_acc)

      
!$acc kernels  present(A)
!$acc loop independent gang 
      do imat=1,nmat
         call dtrmv_acc(uplo,trans,diag,n,A(:,:,imat),lda,                &
     &                 x_cpu(:,imat),incx)
      enddo
!$acc end kernels

!$acc end data

!$omp parallel do
      do imat=1,nmat
        call dtrmv_cpu(uplo,trans,diag,n,A(:,:,imat), lda,               &
     &                 x_acc(:,imat),incx)
      enddo

      maxerr = maxval( abs(x_cpu - x_acc) )
      isok = (maxerr <= tol * maxval(abs(x_cpu)) )
      if (.not.isok) then
              nerrors=nerrors + 1
              print*,'uplo,trans,diag,incx,maxerr:',                     &
     &                uplo,trans,diag,incx,maxerr
      endif

      enddo
      enddo
      enddo
      enddo

      if (nerrors.eq.0) then
              print*,'ALL OK '
      else
              print*,'nerrors = ', nerrors
      endif
      stop
      end program tdtrmv
