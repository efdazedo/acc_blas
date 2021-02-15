
!     simple test of calling dgemm
      program test2
      use lapack_acc, only : dgemm_acc => dgemm
      use omp_lib
      implicit none

      integer :: n,nwalkers
      integer :: nthreads
      double precision, allocatable, dimension(:,:,:) :: A,B,C,hC
      

      integer, parameter :: idebug = 0
      integer :: i,j,walker
      integer :: t1,t2,count_rate
      integer :: ld1,ld2,ld3 
      integer :: ibeta,ialpha,mm,nn,kk,itransA,itransB
      character :: transA,transB

      double precision :: time_gpu, time_cpu
      double precision :: alpha,beta
      double precision :: abserr,maxerr
      double precision :: hCij, Cij
      double precision :: gflops
      integer :: nmax
      double precision :: normhC

#ifdef USE_MANAGED
      attributes(managed) :: A,B,C
#endif

!$acc init
      nthreads = 1
!$omp parallel
!$omp master
!$    nthreads = omp_get_num_threads()
!$omp end master
!$omp end parallel

      nwalkers = 1024 
      nmax = 100
      n = nmax
      print*,'n,nwalkers,nthreads',n,nwalkers,nthreads

      allocate( A(nmax,nmax,nwalkers)) 
      allocate( B(nmax,nmax,nwalkers)) 
      allocate( C(nmax,nmax,nwalkers))
      allocate( hC(nmax,nmax,nwalkers))

         call random_number(A)
         call random_number(B)
!$acc    enter data copyin(A,B)


      do mm=1,nmax,nmax/3
      do nn=1,nmax,nmax/3
      do kk=1,nmax,nmax/3
      do ibeta=0,2
      do ialpha=1,2
      do itransA=1,2
      do itransB=1,2

         call random_number(C)
         hC = C

         transA = merge( 'T','N', (itransA.eq.1))
         transB = merge( 'T','N', (itransB.eq.1))

         beta = dble(ibeta)
         alpha = dble(ialpha)
         ld1 = size(A,1)
         ld2 = size(B,1)
         ld3 = size(C,1)

         
!$acc   data   pcopy(C)                                      &
!$acc&  pcopyin(transA,transB,mm,nn,kk,alpha,beta,ld1,ld2,ld3) 

         call system_clock(t1,count_rate)
!$acc    kernels  present(A,B,C)
!$acc    loop gang private(walker)
         do walker=1,nwalkers
           call dgemm_acc(transA,transB,mm,nn,kk,                        &
     &            alpha,A(:,:,walker),ld1,                               &
     &                  B(:,:,walker),ld2,                               &
     &            beta, C(:,:,walker),ld3 )
          enddo  
!$acc    end kernels
!$acc    wait
         call system_clock(t2,count_rate)
!$acc    end data
         time_gpu = dble(t2-t1)/dble(count_rate)


         call system_clock(t1,count_rate)
!$omp    parallel do private(walker,ld1,ld2,ld3)
         do walker=1,nwalkers
           ld1 = size(A,1)
           ld2 = size(B,1)
           ld3 = size(hC,1)
           call dgemm(transA,transB,mm,nn,kk,                            &
     &            alpha,A(:,:,walker),ld1,                               &
     &                  B(:,:,walker),ld2,                               &
     &            beta, hC(:,:,walker),ld3 )
          enddo  
         call system_clock(t2,count_rate)
         time_cpu = dble(t2-t1)/dble(count_rate)

         gflops = 2.0*mm*nn*kk*nwalkers*1.0d-9

         print*,transA,transB,mm,nn,kk,alpha,beta
!    ----------------
!    check difference
!    ----------------

         maxerr = maxval( abs( C(1:mm,1:nn,1:nwalkers) -                &
     &                        hC(1:mm,1:nn,1:nwalkers) ))

         normhC = maxval( abs(hC(1:mm,1:nn,1:nwalkers)))

         print*,'Gflops_gpu=',gflops/time_gpu,                           &
     &          ' Gflops_cpu=',gflops/time_cpu,                          &
     &          ' maxerr=',maxerr,' normhC ',normhC

         enddo
         enddo
         enddo
         enddo
         enddo
         enddo
         enddo


      stop
      end program test2




