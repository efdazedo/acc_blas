#include <cstdlib>
#include <iostream>
#include <ctime>

#ifdef __cplusplus
 	extern  "C" {
#else
        extern
#endif
	void dgbmv_( char * const * trans, 
			     int * const * nrowA, 
			     int * const * ncolA, 
			     int * const * kl,
			     int * const * ku,
			     double * const * alpha,
			     double * const * AB_,
			     int * const * ldab,
			     double * const * X_,
			     int * const * inc1,
			     double * const * beta,
			     double * B_,
			     int * const * inc1 );



	void dgbsv_strided_batched( 
			int const n,
			int const kl,
			int const ku,
			int const nrhs,
			double *pAB,
			int const ldab,
			long const strideAB,
			int *ipiv,
			double *pB,
			int const ldb,
			long strideB,
			int *info,
			int batchCount );

#ifdef __cplusplus
	};
#endif



main()
{
 // -----------------
 // problem dimension
 // -----------------
 int const nmat = 16;
 int const nrhs = 8;
 int const m = 40;
 int const kl = m;
 int const ku = m;
 int const n = m * m;
 int const ldab = 2*kl + ku + 1;
 int const ldb = n;
 int const ldx = ldb;

 thrust::host_vector<double> AB_(ldab * n * nmat);
 thrust::host_vector<double> B_(ldb * nrhs * nmat);
 thrust::host_vector<double> X_(ldx * nrhs * nmat);
 thrust::host_vector<int>    ipiv_(ldab * n * nmat);

 auto AB = [=](int const i, int const j, int const imat) -> double& {
	 return( AB_[ (i-1) + (j-1)*ldab + (imat-1)*(ldab*n) ]);
 };

 auto B = [=](int const i, int const j, int const imat) -> double& {
	 return( B_[ (i-1) + (j-1)*ldb + (imat-1)*(ldb*nrhs) ]);
 };

 auto X = [=](int const i, int const j, int const imat) -> double& {
	 return( X_[ (i-1) + (j-1)*ldx + (imat-1)*(ldx*nrhs) ]);
 };


 std::srand(1);
 auto drand = [=]() -> double {
	 return( std::rand() / RAND_MAX  );
 };

 #pragma omp parallel
 for(int imat=1; imat <= nmat; imat++)  
 for(int j=1; j <= n; j++)  
 for(int i=1; i <= ldab; i++)  {
	 AB(i,j,imat) = drand();
 };

 #pragma omp parallel
 for(int imat=1; imat <= nmat; imat++)  
 for(int j=1; j <= nrhs; j++)  
 for(int i=1; i <= ldx; i++)  {
	 B(i,j,imat) = drand();
 };

 thrust::host_vector<double>B_org{B};
 thrust::host_vector<double>AB_org{AB};

 #pragma omp parallel
 for(int imat=1; imat <= nmat; imat++ )
 for(int irhs=1; irhs <= nrhs; irhs++) {
	 int const nrowA = n;
	 int const ncolA = n;
	 int const inc1 = 1;
	 int const inc2 = 1;
	 double const alpha = 1;
	 double const beta = 0;
	 char const trans = 'N';
	 int const ioffset = kl + 1;


	 dgbmv_(&trans,&nrowA,&ncolB,&kl,&ku,&alpha,
		 &(AB(ioffset,1,imat), &ldab,
		 &(X(1,irhs,imat)), &inc1, &beta, &(B(1,irhs,imat), &inc2);
	 };


  thrust::device_vector<double>dB_{B_org};
  thrust::device_vector<double>dAB_{AB_};

  long const strideAB = ldab * n;
  long const strideB = ldb * nrhs;

  dgbsv_strided_batched(n,kl,ku,nrhs,&(dAB_[0]),ldab,strideAB,         
			&(ipiv[0]),&(dB_[0]),ldb,strideB,info,nmat);

  // ----------------
  // copy results back
  // ----------------
  thrust::copy( dAB_.begin(), dAB_.end(), AB_.begin());
  thrust::copy( dB_.begin(), dB_.end(), B_.begin());

  // ----------------
  // check difference
  // ----------------
  // maxval(X-B)
  double max_err = 0;
  for(int imat=1; imat <= nmat; imat++) 
  for(int irhs=1; irhs <= nrhs; irhs++) 
  for(int i=1; i <= n; i++) {
	  double const abs_err = std::abs( X(i,irhs,imat) - B(i,irhs,imat) );
	  max_err = std::max( max_err, abs_err );
  };

  std::cout << " max_err = " 
	    << max_err 
	    << "\n";

}
