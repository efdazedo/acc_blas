       subroutine fake_xerbla(info)
#ifdef _OPENACC
!$acc  routine vector
#else
!$omp declare target
#endif
       integer info
       print*,'xerbla:' ,info
       return
       end subroutine fake_xerbla
