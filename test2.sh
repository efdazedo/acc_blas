
pgf90 -Minline=size:999,reshape -UUSE_MANAGED -Minfo=all -c -acc -Mcuda -ta=tesla,cc60,managed -Mfixed lapack_acc.F90 
pgf90  -o test2 -Minfo=all -mp -acc -Mcuda -ta=tesla,cc60,managed lapack_mod.F90 lapack_acc.o test2.F90  -llapack -lblas
