#if _OPENMP >= 201511
#define OMP_TARGET
#endif

      program tdgels
#if defined(_OPENACC)
      use lapack_acc, only :                                             &
     &  dgels_par => dgels,                                               &
     &  dgemm_par => dgemm
#elif defined(OMP_TARGET)
      use lapack_acc, only :                                             &
     &  dgels_par => dgels,                                               &
     &  dgemm_par => dgemm
#else
      use lapack_mod, only :                                             &
     &  dgels_par => dgels,                                               &
     &  dgemm_par => dgemm
#endif
      implicit none




      integer :: tstart,tend,count_rate
      real*8 :: ttime
      logical :: isok
      integer :: m,nmat
      integer :: n,nrhs,lda,ldb,ldx,ioffset
      integer :: mm,nn,kk,ld1,ld2,ld3
      character :: transA,transB

      integer :: imat
      real*8 :: errX
      real*8 :: alpha,beta
      real*8, allocatable :: A(:,:,:), B(:,:,:), X(:,:,:)
      real*8, allocatable :: A_org(:,:,:), B_org(:,:,:)
      integer, allocatable :: info(:)
      real*8, allocatable :: work(:,:)
      integer :: lwork
      integer, parameter :: nb = 512

      nmat = 8
      nrhs = 8
      m = 40
      n = m*m
      lda = n
      ldb = n
      ldx = ldb
      lwork = max(n,nrhs)*nb

      print*,'n,nmat ', n,nmat

      allocate( A(lda,n,nmat), A_org(lda,n,nmat) )
      allocate( X(ldx,nrhs,nmat),B(ldb,nrhs,nmat),B_org(ldb,nrhs,nmat))
      allocate( work(lwork,nmat), info(nmat))

      call random_number(A)
      A_org = A

      call random_number(X)
      work = 0

      alpha = 1
      beta = 0
      transA = 'N'
      transB = 'N'
       mm = n
       nn = nrhs
       kk = n
       ld1 = size(A,1)
       ld2 = size(X,1)
       ld3 = size(B,1)

#if defined(_OPENACC)
!$acc  data copyin(A,X)  copyout(B) create(work)                         &
!$acc& copyin(n,nrhs,transA,transB,mm,nn,kk,alpha,beta,ld1,ld2,ld3)
!$acc& copyin(lda,ldb,ldx)
#elif defined(OMP_TARGET)
!$omp  target data map(to:A,X) map(alloc:work) map(from:B,info)          &
!$omp& map(to:n,nrhs,transA,transB,mm,nn,kk,alpha,beta,ld1,ld2,ld3)      &
!$omp& map(to:lda,ldb,ldx)
#endif



#if defined(_OPENACC)
!$acc kernels  num_gangs(nmat) vector_length(128)
!$acc loop independent gang 
#elif defined(OMP_TARGET)
!$omp target teams
!$omp distribute
#else
!$omp parallel 
!$omp do
#endif
      do imat=1,nmat
       call dgemm_par(transA,transB,mm,nn,kk,                            &
     &   alpha, A(:,:,imat),ld1, X(:,:,imat),ld2,                        &
     &   beta,  B(:,:,imat),ld3 )
      enddo

#if defined(_OPENACC)
!$acc end kernels
!$acc wait
#elif defined(OMP_TARGET)
!$omp end target teams
!$omp taskwait
#else
!$omp end parallel
!$omp barrier
#endif



#if defined(_OPENACC)
!$acc update host(B)
#elif defined(OMP_TARGET)
!$omp target update host(B)
#endif

      B_org = B
      

      info = 0
      call system_clock(tstart,count_rate)
#if defined(_OPENACC)
!$acc kernels  num_gangs(nmat) vector_length(128)
!$acc loop independent gang 
#elif defined(OMP_TARGET)
!$omp target teams
!$omp distribute
#else
!$omp parallel 
!$omp do 
#endif
      do imat=1,nmat
       call dgels_par(transA,n,n,nrhs,A(:,:,imat),lda,                    &
     &            B(:,:,imat),ldb,work(:,imat),lwork,info(imat))
      enddo

#if defined(_OPENACC)
!$acc end kernels
!$acc wait
#elif defined(OMP_TARGET)
!$omp end teams
!$omp taskwait
#else
!$omp end parallel
!$omp barrier
#endif

      call system_clock(tend,count_rate)

#if defined(_OPENACC)
!$acc end data
#elif defined(OMP_TARGET)
!$omp end target data
#endif

      ttime = dble(tend-tstart)/dble(count_rate)
#if defined(_OPENACC)
      print*,'time for OpenACC dgels_par ',ttime
#elif defined(OMP_TARGET)
      print*,'time for OMP target dgels_cpu ',ttime
#else
      print*,'time for OMP dgels_cpu ',ttime
#endif

      isok = all( info.eq.0)
      if (.not.isok) then
          print*,'problem in ', count(info.ne.0),' cases'
          stop '** error ** '
      endif

      errX = maxval( abs(X - B) )
      print*,'max diff in X ', maxval( abs(X-B) )


      stop
      end program tdgels
