


gfortran -Wall  -ffixed-form -D_OPENACC -o tdtrmv_acc lapack_acc.F90 lapack_mod.F90 tdtrmv.F90 -lblas
gfortran -Wall  -ffixed-form -D_OPENMP=201511 -o tdtrmv_omp lapack_acc.F90 lapack_mod.F90 tdtrmv.F90 -lblas
gfortran -Wall  -ffixed-form -U_OPENMP -U_OPENACC -o tdtrmv_serial lapack_acc.F90 lapack_mod.F90 tdtrmv.F90 -lblas


