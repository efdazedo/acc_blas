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

!#include "lapack/dnrm2.f"
#include "lapack/dnrm2_acc.F90"


!#include "lapack/dger.f"
#include "lapack/dger_acc.F90"

!#include "lapack/dgemv.f"
#include "lapack/dgemv_acc.F90"



#include "lapack/dtrsm.f"
#include "lapack/dtrmm.f"

#include "lapack/dtrmv.f"

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
#include "lapack/dgesv.f"




#include "lapack/dlamch.f"
#include "lapack/dlaisnan.f"
#include "lapack/disnan.f"
#include "lapack/dlapy2.f"

#include "lapack/iladlc.f"
#include "lapack/iladlr.f"
#include "lapack/dlarf.f"

#include "lapack/dlarfg.f"
#include "lapack/dlarfb.f"
#include "lapack/dlarft.f"


#include "lapack/dorm2r.f"
#include "lapack/dorml2.f"
#include "lapack/dlabad.f"
#include "lapack/dlaset.f"
#include "lapack/dlascl.f"
#include "lapack/dlassq.f"
#include "lapack/dlange.f"
#include "lapack/dtrtrs.f"
#include "lapack/dormlq.f"
#include "lapack/dormqr.f"
#include "lapack/dgeqr2.f"
#include "lapack/dgeqrf.f"

#include "lapack/dgelq2.f"
#include "lapack/dgelqf.f"

#include "lapack/dgels.f"


#include "lapack/dgbsv_strided_batched.F90"

       end module lapack_acc
