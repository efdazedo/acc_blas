gfortran -fbounds-check -ffixed-form -c lapack_mod.F90
gfortran -fbounds-check -ffixed-form -c lapack_acc.F90
gfortran -fbounds-check -o tdgbsv lapack_mod.o lapack_acc.o tdgbsv.F90 -llapack -lblas
