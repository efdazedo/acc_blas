


gfortran -Wall  -ffixed-form -D_OPENACC -o test1_acc lapack_acc.F90 lapack_mod.F90 test1.F90
gfortran -Wall  -ffixed-form -D_OPENMP=201511 -o test1_omp lapack_acc.F90 lapack_mod.F90 test1.F90
gfortran -Wall  -ffixed-form -U_OPENMP -U_OPENACC -o test1_serial lapack_acc.F90 lapack_mod.F90 test1.F90


