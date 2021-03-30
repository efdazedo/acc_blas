module unload xl cuda
module load pgi cuda essl
touch lapack_mod.mod lapack_acc.mod
rm lapack_mod.mod lapack_acc.mod

date

echo " === tdgbsv_acc ===="
pgfortran -fast -Mfixed -c lapack_mod.F90
pgfortran -fast -Minline=size:99 -Mfixed -c -acc -ta=tesla,cc70 -Minfo=accel lapack_acc.F90
pgfortran -fast -Minline=size:99 -Minfo=all -acc  -ta=tesla,cc70 -Minfo=accel \
  -o tdgbsv_acc \
  lapack_acc.o tdgbsv.F90  \
  -L$OLCF_ESSL_ROOT/lib64 -lessl

./tdgbsv_acc

echo " === tdgbsv_fake_acc ===="
pgfortran -Mfixed -g -Mbounds -D_OPENACC \
  -o tdgbsv_fake_acc \
  lapack_acc.F90 tdgbsv.F90 \
  -L$OLCF_ESSL_ROOT/lib64 -lessl
./tdgbsv_fake_acc


  
echo " === tdgbsv_serial ===="
pgfortran -Mfixed -fast -U_OPENACC \
  -o tdgbsv_serial \
  lapack_mod.F90  tdgbsv.F90 \
  -L$OLCF_ESSL_ROOT/lib64 -lessl


./tdgbsv_serial
date
