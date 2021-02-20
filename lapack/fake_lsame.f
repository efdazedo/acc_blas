      logical function lsame(charA, charB)
#ifdef _OPENACC
!$acc routine vector
#else
!$omp declare target
#endif
      implicit none
      character, intent(in) :: charA, charB

      character :: lcharA,lcharB
      call tolower(charA,lcharA)
      call tolower(charB,lcharB)

      lsame = (lcharA.eq.lcharB)
      return
      end function lsame


