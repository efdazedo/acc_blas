      logical function lsame(charA, charB)
!$acc routine vector
      implicit none
      character, intent(in) :: charA, charB

      character :: lcharA,lcharB
      call tolower(charA,lcharA)
      call tolower(charB,lcharB)

      lsame = (lcharA.eq.lcharB)
      return
      end function lsame


