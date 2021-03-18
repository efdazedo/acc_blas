echo "== serial == "
date
pgf90 -c -Mfixed lapack_mod.F90
pgf90 -c -Mfixed lapack_acc.F90
pgf90 -Mfixed -o tdgesv_serial lapack_mod.o lapack_acc.o tdgesv.F90 -llapack -lblas



echo "== fake OpenACC == "
date
pgf90 -g -Mbounds -Mfixed -D_OPENACC -o tdgesv_acc_fake lapack_acc.f90 tdgesv.F90 

echo "== OpenACC multicore== "
date
pgf90 -P -E -cpp -D_OPENACC lapack_acc.F90 > lapack_acc.f90
pgf90 -acc=verystrict -ta=multicore -Minfo=all -Mfixed -o tdgesv_acc_multicore lapack_acc.f90  tdgesv.F90 

echo "== OpenACC tesla== "
date
pgf90 -P -E -cpp -D_OPENACC lapack_acc.F90 > lapack_acc.f90
pgf90 -acc=verystrict -ta=tesla -Minfo=all -Mfixed -o tdgesv_acc_tesla lapack_acc.f90  tdgesv.F90 

date
