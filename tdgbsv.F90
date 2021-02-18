      program tdgbsv
      use lapack_mod, only : dgbsv_cpu => dgbsv, dgbmv_cpu => dgbmv
      use lapack_acc, only : dgbsv_acc => dgbsv, dgbmv_gpu => dgbmv
      implicit none

      logical :: isok
      integer :: m,nmat
      integer :: kl,ku,n,nrhs,ldab,ldb,ldx
      integer :: nrowA,ncolA
      integer :: imat,irhs, inc1,inc2
      real*8 :: errX
      real*8 :: alpha,beta
      integer, allocatable :: ipiv(:,:), info(:)
      real*8, allocatable :: AB(:,:,:), B(:,:,:),X(:,:,:),B_org(:,:,:)
      real*8, allocatable :: AB_org(:,:,:)
      character :: trans

      nmat = 1
      nrhs = 1
      m = 4
      kl = m
      ku = m
      n = m * m
      ldab = 2*kl+ku+1
      ldb = n
      ldx = ldb

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
      do imat=1,nmat
      do irhs=1,nrhs
       call dgbmv( trans, nrowA,ncolA,kl,ku,alpha,                       &
     &            AB(ioffset,1,imat),ldab,                               &
     &            X(:,irhs,imat),inc1,beta,B(:,irhs,imat),inc2)
      enddo
      enddo
      B_org = B
      

      info = 0
      do imat=1,nmat
       call dgbsv(n,kl,ku,nrhs,AB(:,:,imat),ldab,ipiv(:,imat),           &
     &            B(:,:,imat),ldb,info(imat))
      enddo

      isok = all( info.eq.0)
      if (.not.isok) then
          print*,'problem in ', count(info.ne.0),' cases'
          stop '** error ** '
      endif

      errX = maxval( abs(X - B) )
      print*,'max diff in X ', maxval( abs(X-B) )


      stop
      end program tdgbsv
