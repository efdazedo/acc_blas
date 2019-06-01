pgf90 -Minfo=all -c -acc -Mcuda -ta=tesla,cc60,managed,keepgpu -Mfixed lapack_acc.F90 
pgf90  -Minfo=all -mp -acc -Mcuda -ta=tesla,cc60,managed,keepgpu lapack_acc.o test1.F90 
