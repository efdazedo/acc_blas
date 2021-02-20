pgf90 -Minfo=all -c -acc -Mcuda -ta=tesla,managed,keepgpu -Mfixed lapack_acc.F90 
pgf90  -Minfo=all -mp -acc -Mcuda -ta=tesla,managed,keepgpu lapack_acc.o -o test1 test1.F90  -llapack -lblas
