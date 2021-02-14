       module lapack_acc
       implicit none
       public
#define XERBLA(msg,info) fake_xerbla(info)
#define ILAENV( ISPEC, NAME, OPTS, N1, N2, N3, N4) (64)

       contains
#include "lapack/fake_xerbla.f"
#include "lapack/idamax.f"
#include "lapack/lsame.f"
#include "lapack/dscal.f"
#include "lapack/dcopy.f"
#include "lapack/daxpy.f"
#include "lapack/ddot.f"
#include "lapack/dswap.f"
#include "lapack/dger.f"
#include "lapack/dgemm.f"
#include "lapack/dtrsm.f"
#include "lapack/dtrmm.f"


#include "lapack/dgbtf2.f"
#include "lapack/dgbtrf.f"

#include "lapack/dtbsv.f"
#include "lapack/dgbtrs.f"

#if (0)


 


#include "lapack/dlaswp.f"
#include "lapack/dgetf2.f"
#include "lapack/dgetrs.f"
#include "lapack/dgetrf.f"



#include "lapack/dgbsv.f"

#endif

       end module lapack_acc
