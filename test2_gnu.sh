


gfortran -Wall  -ffixed-form -D_OPENACC -o test2_acc lapack_acc.F90 lapack_mod.F90 test2.F90 -lblas
gfortran -Wall  -ffixed-form -D_OPENMP=201511 -o test2_omp lapack_acc.F90 lapack_mod.F90 test2.F90 -lblas
gfortran -Wall  -ffixed-form -U_OPENMP -U_OPENACC -o test2_serial lapack_acc.F90 lapack_mod.F90 test2.F90 -lblas


