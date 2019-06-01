       subroutine fake_xerbla(info)
!$acc  routine(fake_xerbla) seq  
       integer info
       print*,'xerbla:' ,info
       return
       end subroutine fake_xerbla
