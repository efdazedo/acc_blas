echo "build tdgbsv_acc"; date;
rm lapack_acc.mod lapack_acc.o lapack_mod.mod lapack_mod.o
nvfortran -Mfixed \
	-Minline=reshape,size:99 \
	-Minfo=all -acc   -ta=tesla \
	-o tdgbsv_acc \
	lapack_mod.F90 lapack_acc.F90 \
	tdgbsv.F90 

echo "build tdgbsv_acc_noinline"; date;
rm lapack_acc.mod lapack_acc.o lapack_mod.mod lapack_mod.o
nvfortran -Mfixed \
	-Minfo=all -acc   -ta=tesla \
	-o tdgbsv_acc_noinline \
	lapack_mod.F90 lapack_acc.F90 \
	tdgbsv.F90 



echo "build tdgbsv_omp"; date;
rm lapack_acc.mod lapack_acc.o lapack_mod.mod lapack_mod.o
nvfortran -Mfixed \
	-DOMP_TARGET  \
	-Minline=reshape,size:99 \
	-mp=gpu -ta=tesla -Minfo=all \
	-o tdgbsv_omp \
	lapack_mod.F90 lapack_acc.F90 \
	tdgbsv.F90 

echo "build tdgbsv_omp_noinline"; date;
rm lapack_acc.mod lapack_acc.o lapack_mod.mod lapack_mod.o
nvfortran -Mfixed \
	-DOMP_TARGET  \
	-mp=gpu -ta=tesla -Minfo=all \
	-o tdgbsv_omp_noinline \
	lapack_mod.F90 lapack_acc.F90 \
	tdgbsv.F90 


echo "build tdgbsv_fake_acc"; date;
rm lapack_acc.mod lapack_acc.o lapack_mod.mod lapack_mod.o
nvfortran -Mfixed -D_OPENACC \
	-o tdgbsv_fake_acc \
	lapack_mod.F90 lapack_acc.F90 \
	tdgbsv.F90 

echo "build tdgbsv_fake_omp"; date;
rm lapack_acc.mod lapack_acc.o lapack_mod.mod lapack_mod.o
nvfortran -Mfixed -D_OPENMP=201511 \
	-o tdgbsv_fake_omp \
	lapack_mod.F90 lapack_acc.F90 \
	tdgbsv.F90  


echo "build tdgbsv_serial"; date;
rm lapack_acc.mod lapack_acc.o lapack_mod.mod lapack_mod.o
nvfortran -Mfixed -U_OPENMP -U_OPENACC \
	-o tdgbsv_serial \
	lapack_mod.F90 lapack_acc.F90 \
	tdgbsv.F90   -llapack -lblas



