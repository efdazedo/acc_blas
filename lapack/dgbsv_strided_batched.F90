      subroutine dgbsv_strided_batched( n, kl, ku, nrhs, pAB, ldab,      &
     &   strideAB, ipiv, pB, ldb, strideB, info, batchCount )            &
     &   bind(C,name='dgbsv_strided_batched')
      use iso_c_binding
      implicit none
      integer(c_int), value :: n,kl,ku,nrhs,ldab,ldb,batchCount
      integer(c_long), value :: strideAB, strideB
      integer(c_int) :: info(batchCount)
      integer(c_int) :: ipiv(*)
      real(c_double) :: pAB(*)
      real(c_double) :: pB(*)

      integer :: ibatch
#ifdef _OPENACC
!$acc kernels
!$acc loop independent gang
#elif defined(OMP_TARGET)
!$omp target teams
!$omp distribute
#else
!$omp parallel
!$omp do
#endif
      do ibatch=1,batchCount
        call dgbsv(n,kl,ku,nrhs, pAB(1 + (ibatch-1)*strideAB),ldab,      &
     &         ipiv(1+(ibatch-1)*n),                                     &
     &         pB(1 + (ibatch-1)*strideB ), ldb, info(ibatch) )
      enddo

#ifdef _OPENACC
!$acc end kernels
#elif OMP_TARGET
!$omp end target teams
#else
!$omp end parallel
#endif

      return
      end subroutine dgbsv_strided_batched
