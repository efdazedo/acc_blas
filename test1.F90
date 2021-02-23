!     simple test of calling dgemm
      program test1
      use lapack_acc
      implicit none

      integer :: n,nwalkers
      double precision, allocatable, dimension(:,:,:) :: A,B,C,hC
      

      integer, parameter :: idebug = 0
      integer :: i,j,walker
      integer :: t1,t2,count_rate
      integer :: ld1,ld2,ld3, mm,nn,kk
      double precision :: alpha,beta
      double precision :: abserr,maxerr
      double precision :: hCij, Cij
      double precision :: gflops,ttime 

#ifdef USE_MANAGED
      attributes(managed) :: A,B,C
#endif

      nwalkers = 1024 
      n = 2*128
      print*,'n,nwalkers',n,nwalkers

      allocate( A(n,n,nwalkers), B(n,n,nwalkers), C(n,n,nwalkers))
      allocate( hC(n,n,nwalkers) )
      hC(:,:,:) = 0
!$acc enter data copyin(n,nwalkers)
#ifdef USE_MANAGED
#else
!$acc enter data create(A,B,C) copyin(n,nwalkers)
#endif

!$acc parallel default(none) present(n,nwalkers,A,B,C)
!$acc loop collapse(3) private(i,j,walker)
      do walker=1,nwalkers
      do j=1,n
      do i=1,n
        A(i,j,walker) = i + j + walker
        B(i,j,walker) = 1.0d0/(i+j+walker)
        C(i,j,walker) = 0.0d0
      enddo
      enddo
      enddo
!$acc end parallel

        mm = n
        nn = n
        kk = n
        ld1 = size(A,1)
        ld2 = size(B,1)
        ld3 = size(C,1)
        alpha = 1
        beta = 0
      call system_clock(t1,count_rate)
!$acc  parallel default(none) present(n,nwalkers,A,B,C)                  &
!$acc& pcopyin(mm,nn,kk,ld1,ld2,ld3,alpha,beta)
!$acc  loop independent gang                                                         &
!$acc& private(walker)
      do walker=1,nwalkers
        call  dgemm('N','N', mm,nn,kk,                                   &
     &             alpha, A(1,1,walker), ld1, B(1,1,walker), ld2,        &
     &             beta, C(1,1,walker), ld3 )
      enddo
!$acc end parallel
      call system_clock(t2,count_rate)

      ttime = dble(t2-t1)/dble(count_rate)
      gflops = 2.0d0*mm*nn*kk*nwalkers*1.0d-9
      print*,'GPU time = ', ttime, ' Gflops/sec=',gflops/ttime

! ---------------------------
! perform computations on CPU
! ---------------------------

!$acc update host(A,B,C)
!$acc wait

      call system_clock(t1,count_rate)
!$omp parallel default(none) shared(n,nwalkers,A,B,hC)
!$omp do private(walker)
      do walker=1,nwalkers
        hC(1:n,1:n,walker)=matmul(A(1:n,1:n,walker),B(1:n,1:n,walker))
      enddo
!$omp end parallel
      call system_clock(t2,count_rate)

      ttime = dble(t2-t1)/dble(count_rate)
      print*,'CPU time = ',ttime,' Gflops/sec=',gflops/ttime

! ----------------
! check difference
! ----------------
      if ((idebug >= 1).and.(n * n * nwalkers <= 1000)) then
          do walker=1,nwalkers
          do j=1,n
          do i=1,n
            Cij = C(i,j,walker)
            print*,'C(',i,',',j,')=',Cij,';'
          enddo
          enddo
          enddo

          do walker=1,nwalkers
          do j=1,n
          do i=1,n
            hCij = hC(i,j,walker)
            print*,'hC(',i,',',j,')=',hCij,';'
          enddo
          enddo
          enddo
      endif

      maxerr = 0
      do walker=1,nwalkers
      do j=1,n
      do i=1,n
        abserr = abs(hC(i,j,walker)-C(i,j,walker))
        if (abserr > maxerr) then
         if (idebug >= 2) then
           print*,'i,j,walker,abserr',i,j,walker,abserr
         endif

         maxerr = abserr
        endif
      enddo
      enddo
      enddo

      print*,'max diff ', maxerr

      stop
      end program test1




