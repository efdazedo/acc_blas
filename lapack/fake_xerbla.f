       subroutine fake_xerbla(info)
!$acc  routine vector
       integer info
       print*,'xerbla:' ,info
       return
       end subroutine fake_xerbla
