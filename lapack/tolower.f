      subroutine tolower( charA, lcharA )
!$acc routine vector
      implicit none
      character, intent(in) :: charA
      character, intent(out) :: lcharA

      select case (charA)
      case ('A')
              lcharA = 'a'
      case ('B')
              lcharA = 'b'
      case ('C')
              lcharA = 'c'
      case ('D')
              lcharA = 'd'
      case ('E')
              lcharA = 'e'

      case ('F')
              lcharA = 'f'
      case ('G')
              lcharA = 'g'
      case ('H')
              lcharA = 'h'
      case ('I')
              lcharA = 'i'
      case ('J')
              lcharA = 'j'

      case ('K')
              lcharA = 'k'
      case ('L')
              lcharA = 'l'
      case ('M')
              lcharA = 'm'
      case ('N')
              lcharA = 'n'
      case ('O')
              lcharA = 'o'

      case ('P')
              lcharA = 'p'
      case ('Q')
              lcharA = 'q'
      case ('R')
              lcharA = 'r'
      case ('S')
              lcharA = 's'
      case ('T')
              lcharA = 't'

      case ('U')
              lcharA = 'u'
      case ('V')
              lcharA = 'v'
      case ('W')
              lcharA = 'w'
      case ('X')
              lcharA = 'x'
      case ('Y')
              lcharA = 'y'

      case ('Z')
              lcharA = 'z'

      case default
              lcharA = charA
      end select
      return
      end subroutine tolower
