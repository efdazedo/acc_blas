gfortran -O3  -ffixed-form -c lapack_mod.F90
gfortran -O3  -ffixed-form -c lapack_acc.F90
gfortran -O3  -o tdgbsv lapack_mod.o lapack_acc.o tdgbsv.F90 -llapack -lblas

gfortran -O3  -D_OPENACC -o tdgesv_acc  lapack_acc.o tdgesv.F90 
gfortran -O3  -D_OPENMP=201107  -o tdgesv_target  lapack_acc.o tdgesv.F90 
gfortran -O3  -U_OPENMP -U_OPENACC -o tdgesv lapack_mod.o  tdgesv.F90 -llapack -lblas
