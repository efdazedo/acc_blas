      module test_dgbsv_mod
      implicit none
      contains
      subroutine test_dgbsv(m,nrhs,nmat)

#if  defined(OMP_TARGET) || defined(_OPENACC)
      use lapack_acc, only : dgbsv_acc => dgbsv, dgbmv_acc => dgbmv,     &
     &     dgbsv_strided_batched
#else
      use lapack_mod, only : dgbsv_acc => dgbsv, dgbmv_acc => dgbmv
#endif
      use iso_c_binding
      implicit none

      integer, intent(in) :: m,nrhs,nmat
      integer(kind=c_long) :: strideAB, strideB

      integer :: tstart,tend,count_rate
      real*8 :: ttime
      logical :: isok
      integer :: kl,ku,n,ldab,ldb,ldx,ioffset
      integer :: nrowA,ncolA
      integer :: imat,irhs, inc1,inc2
      real*8 :: errX
      real*8 :: alpha,beta
      integer, allocatable :: ipiv(:,:), info(:)
      real*8, allocatable :: AB(:,:,:), B(:,:,:),X(:,:,:),B_org(:,:,:)
      real*8, allocatable :: AB_org(:,:,:)
      character :: trans
      integer, parameter :: idebug = 0

      kl = m
      ku = m
      n = m * m
      ldab = 2*kl+ku+1
      ldb = n
      ldx = ldb

      print*,'n,kl,ku,nmat ', n,kl,ku,nmat

      allocate( AB(ldab,n,nmat), AB_org(ldab,n,nmat))
      allocate( X(ldx,nrhs,nmat),B(ldb,nrhs,nmat),B_org(ldb,nrhs,nmat))
      allocate(ipiv(n,nmat),info(nmat))

      call random_number(AB)
      AB = 2*AB-1
      AB_org = AB

      call random_number(X)
      ipiv = 0

      nrowA = n
      ncolA = n
      inc1 = 1
      inc2 = 1
      alpha = 1
      beta = 0
      trans = 'N'
      ioffset = kl+1
      irhs = 1

      B = 0
      info = 0
      ipiv = 0

#ifdef _OPENACC
!$acc data copyin(AB,X) copy(B,info,ipiv)                                &
!$acc& copyin(trans,nrowA,ncolA,alpha,beta,ldab,ldb,ldx,n,nrhs,kl,ku)    &
!$acc& copyin(ioffset,inc1,inc2)
#elif defined(OMP_TARGET)
!$omp target data map(to:AB,X)  map(tofrom:B,info,ipiv)                  &
!$omp& map(to:trans,nrowA,ncolA,alpha,beta,ldab,ldb,ldx,n,nrhs,kl,ku)    &
!$omp& map(to:ioffset,inc1,inc2)
#else
#endif


#ifdef _OPENACC
!$acc kernels
!$acc loop independent gang collapse(2)
#elif defined(OMP_TARGET)
!$omp target teams
!$omp distribute collapse(2)
#else
!$omp parallel 
!$omp do collapse(2)
#endif

      do imat=1,nmat
      do irhs=1,nrhs
       call dgbmv_acc( trans, nrowA,ncolA,kl,ku,alpha,                   &
     &            AB(ioffset,1,imat),ldab,                               &
     &            X(:,irhs,imat),inc1,beta,B(:,irhs,imat),inc2)
      enddo
      enddo
#ifdef _OPENACC
!$acc end kernels
!$acc wait
!$acc update host(B)
!$acc wait
#elif defined(OMP_TARGET)
!$omp end target teams
!$omp taskwait
!$omp target update from(B)
!$omp taskwait
#else
!$omp end parallel
!$omp barrier
#endif


      B_org = B
      


      call system_clock(tstart,count_rate)
#if defined(_OPENACC) || defined(OMP_TARGET)
        strideAB = size(AB,1)
        strideAB = strideAB * size(AB,2)
        strideB = size(B,1)
        strideB = strideB * size(B,2)
        call dgbsv_strided_batched(n,kl,ku,nrhs,AB,ldab,strideAB,        &
     &         ipiv,B,ldb,strideB,info,nmat)
        if (idebug >= 1) then
          print*,'dgbsv_strided_batched used '
        endif
#else



#ifdef _OPENACC
!$acc kernels
!$acc loop independent gang 
#elif defined(OMP_TARGET)
!$omp target teams 
!$omp distribute
#else
!$omp parallel
!$omp do
#endif
      do imat=1,nmat
       call dgbsv_acc(n,kl,ku,nrhs,AB(1,1,imat),ldab,ipiv(1,imat),           &
     &            B(1,1,imat),ldb,info(imat))
      enddo
      if (idebug >= 1) then
         print*,'dgbsv_acc used '
      endif

#ifdef _OPENACC
!$acc end kernels
!$acc wait
#elif defined(OMP_TARGET)
!$omp end target teams
!$omp taskwait
#else
!$omp end parallel
!$omp barrier
#endif



#endif
      call system_clock(tend,count_rate)

#ifdef _OPENACC
!$acc end data
#elif defined(OMP_TARGET)
!$omp end target data
#else
#endif


      ttime = dble(tend-tstart)/dble(count_rate)

#ifdef _OPENACC
      print*,'time for OpenACC dgbsv ',ttime
#elif defined(OMP_TARGET)
      print*,'time for OpenMP target dgbsv ',ttime
#else
      print*,'time for OpenMP dgbsv ',ttime
#endif

      isok = all( info.eq.0)
      if (.not.isok) then
          print*,'problem in ', count(info.ne.0),' cases'
          stop '** error ** '
      endif

      errX = maxval( abs(X - B) )
      print*,'max diff in X ', maxval( abs(X-B) )


      deallocate( AB, AB_org )
      deallocate( X, B, B_org )
      deallocate( ipiv, info )

      return
      end subroutine  test_dgbsv
      end module test_dgbsv_mod

      program tdgbsv
      use test_dgbsv_mod
      implicit none
      integer :: m, nrhs, nmat
      integer :: icase, ncase


      m = 40
      nrhs = 8
      ncase = 7

      nmat = 16
      do icase=1,ncase
         
         print*,'m,nrhs,nmat ', m,nrhs,nmat
         call test_dgbsv(m,nrhs,nmat)

         nmat = 2*nmat
      enddo

      stop
      end program tdgbsv
