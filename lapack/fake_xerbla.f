       subroutine fake_xerbla(info)
       implicit none
#ifdef _OPENACC
!$acc  routine vector
#else
!$omp declare target
#endif
       integer info
       print*,'fake xerbla:' ,info
       return
       end subroutine fake_xerbla
