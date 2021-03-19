echo "== serial == "
date
pgf90 -c -Mfixed lapack_mod.F90
pgf90 -c -Mfixed lapack_acc.F90
pgf90 -fast -Mfixed -o tdgels_serial lapack_mod.o lapack_acc.o tdgels.F90 -llapack -lblas



echo "== fake OpenACC == "
date
pgf90 -Mbounds  -Mfixed -D_OPENACC -o tdgels_acc_fake lapack_acc.f90 tdgels.F90 

echo "== OpenACC multicore== "
date
pgf90 -P -E -cpp -D_OPENACC lapack_acc.F90 > lapack_acc.f90
pgf90 -fast -acc=verystrict -ta=multicore -Minfo=all -Mfixed -o tdgels_acc_multicore lapack_acc.f90  tdgels.F90 

echo "== OpenACC tesla== "
date
pgf90 -P -E -cpp -D_OPENACC lapack_acc.F90 > lapack_acc.f90
pgf90 -fast -Minline=size:99,reshape -acc=verystrict -ta=tesla -Minfo=all -Mfixed \
        -o tdgels_acc_tesla \
        lapack_acc.f90  tdgels.F90 

date
