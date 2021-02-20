       module lapack_acc
       implicit none
       public
#define XERBLA(msg,info) fake_xerbla(info)
#define xerbla(msg,info) fake_xerbla(info)

#define ILAENV( ISPEC, NAME, OPTS, N1, N2, N3, N4) (64)
#define ilaenv( ISPEC, NAME, OPTS, N1, N2, N3, N4) (64)

       contains
#include "lapack/fake_xerbla.f"
!#include "lapack/tolower.f"
!#include "lapack/fake_lsame.f"

!#include "lapack/dgemm.f"
#include "lapack/dgemm_acc.F90"

!#include "lapack/idamax.f"
#include "lapack/idamax_acc.F90"


!#include "lapack/dscal.f"
#include "lapack/dscal_acc.F90"

!#include "lapack/dcopy.f"
#include "lapack/dcopy_acc.F90"

!#include "lapack/daxpy.f"
#include "lapack/daxpy_acc.F90"

!#include "lapack/ddot.f"
#include "lapack/ddot_acc.F90"

!#include "lapack/dswap.f"
#include "lapack/dswap_acc.F90"

#include "lapack/dger.f"
#include "lapack/dgemv.f"
#include "lapack/dtrsm.f"
#include "lapack/dtrmm.f"

#include "lapack/dlaswp.f"
#include "lapack/dgbtf2.f"
#include "lapack/dtbsv.f"

#include "lapack/dgbtrs.f"
#include "lapack/dgbtrf.f"
#include "lapack/dgbsv.f"
#include "lapack/dgbmv.f"


#include "lapack/dgetf2.f"
#include "lapack/dgetrs.f"
#include "lapack/dgetrf.f"

       end module lapack_acc
