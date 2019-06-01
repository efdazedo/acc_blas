pgf90 -Minfo=all -c -mp -acc  -ta=multicore -Mfixed lapack_acc.F90 
pgf90  -Minfo=all -mp -acc -ta=multicore lapack_acc.o test1.F90 
