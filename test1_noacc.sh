pgf90 -Minfo=all -c   -Mfixed lapack_acc.F90 
pgf90  -Minfo=all -mp   lapack_acc.o test1.F90 
