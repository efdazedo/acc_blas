nvfortran -Mfixed -c lapack_mod.F90
nvfortran -Mfixed -c -acc -gpu=cc60 lapack_acc.F90
nvfortran -Minline=size:99 -Minfo=all -acc -gpu=cc60 -o tdgbsv lapack_mod.o lapack_acc.o tdgbsv.F90
