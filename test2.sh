
pgf90 -Minfo=all -c -acc:verystrict -Mcuda -ta=tesla,cc60,managed -Mfixed lapack_acc.F90 
pgf90  -o test2 -Minfo=all -mp -acc:verystrict -Mcuda -ta=tesla,cc60,managed lapack_acc.o test2.F90  -lopenblas
