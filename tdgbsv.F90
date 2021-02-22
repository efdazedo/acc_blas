      program tdgbsv
      use lapack_acc, only : dgbsv_gpu => dgbsv, dgbmv_gpu => dgbmv
      use lapack_mod, only : dgbsv_cpu => dgbsv, dgbmv_cpu => dgbmv
      implicit none

      integer :: tstart,tend,count_rate
      real*8 :: ttime
      logical :: isok
      integer :: m,nmat
      integer :: kl,ku,n,nrhs,ldab,ldb,ldx,ioffset
      integer :: nrowA,ncolA
      integer :: imat,irhs, inc1,inc2
      real*8 :: errX
      real*8 :: alpha,beta
      integer, allocatable :: ipiv(:,:), info(:)
      real*8, allocatable :: AB(:,:,:), B(:,:,:),X(:,:,:),B_org(:,:,:)
      real*8, allocatable :: AB_org(:,:,:)
      character :: trans

      nmat = 64
      nrhs = 1
      m = 40
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

#ifdef _OPENACC
!$acc data copyin(AB,X) create(ipiv) copy(B,info)
#else
!$omp parallel do collapse(2)
#endif

      do imat=1,nmat
      do irhs=1,nrhs
#ifdef _OPENACC
       call dgbmv_gpu( trans, nrowA,ncolA,kl,ku,alpha,                   &
     &            AB(ioffset,1,imat),ldab,                               &
     &            X(:,irhs,imat),inc1,beta,B(:,irhs,imat),inc2)
#else
       call dgbmv_cpu( trans, nrowA,ncolA,kl,ku,alpha,                   &
     &            AB(ioffset,1,imat),ldab,                               &
     &            X(:,irhs,imat),inc1,beta,B(:,irhs,imat),inc2)
#endif
      enddo
      enddo
      B_org = B
      

      info = 0
      call system_clock(tstart,count_rate)
!$acc kernels
!$acc loop independent gang 
      do imat=1,nmat
#ifdef _OPENACC
       call dgbsv_gpu(n,kl,ku,nrhs,AB(:,:,imat),ldab,ipiv(:,imat),           &
     &            B(:,:,imat),ldb,info(imat))
#else
       call dgbsv_cpu(n,kl,ku,nrhs,AB(:,:,imat),ldab,ipiv(:,imat),           &
     &            B(:,:,imat),ldb,info(imat))
#endif
      enddo
!$acc end kernels
!$acc wait
      call system_clock(tend,count_rate)
!$acc end data
      ttime = dble(tend-tstart)/dble(count_rate)
#ifdef _OPENACC
      print*,'time for dgbsv_gpu ',ttime
#else
      print*,'time for dgbsv_cpu ',ttime
#endif

      isok = all( info.eq.0)
      if (.not.isok) then
          print*,'problem in ', count(info.ne.0),' cases'
          stop '** error ** '
      endif

      errX = maxval( abs(X - B) )
      print*,'max diff in X ', maxval( abs(X-B) )


      stop
      end program tdgbsv
