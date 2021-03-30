module unload pgi cuda
module load xl cuda essl
module load netlib-lapack
touch lapack_mod.mod lapack_acc.mod
rm lapack_mod.mod lapack_acc.mod

date

echo "=== tdgbsv_omp_gpu === "
xlf90_r -Dsimd= -DSIMD=    -DOMP_TARGET -U_OPENACC -g -qcheck=all   -qstrict -qfixed -qsmp=omp -qoffload -c lapack_acc.F90
xlf90_r -Dsimd= -DSIMD=    -DOMP_TARGET -U_OPENACC -g -qcheck=all   -qstrict -qsmp=omp -qoffload \
          -o tdgbsv_omp_gpu \
          lapack_acc.o tdgbsv.F90   \
          -L$OLCF_ESSL_ROOT/lib64 -lessl
./tdgbsv_omp_gpu

echo "=== tdgbsv_omp_gpu == "
xlf90_r -Dsimd= -DSIMD=    -UOMP_TARGET -U_OPENACC -g -qcheck=all   -qstrict -qfixed -qsmp=omp  -c lapack_mod.F90
xlf90_r -Dsimd= -DSIMD=    -UOMP_TARGET -U_OPENACC -g -qcheck=all   -qstrict -qsmp=omp  \
      -o tdgbsv_omp_cpu \
      lapack_mod.o  tdgbsv.F90   \
      -L$OLCF_ESSL_ROOT/lib64 -lessl

export OMP_STACKSIZE=2G
export OMP_NUM_THREADS=4
./tdgbsv_omp_cpu


echo "=== tdgbsv_serial === "
xlf90_r -Dsimd= -DSIMD=    -UOMP_TARGET -U_OPENACC -g -qcheck=all   -qstrict -qfixed   -c lapack_mod.F90
xlf90_r -Dsimd= -DSIMD=    -UOMP_TARGET -U_OPENACC -g -qcheck=all   -qstrict \
  -o tdgbsv_serial \
  lapack_mod.o tdgbsv.F90 \
  -L$OLCF_ESSL_ROOT/lib64 -lessl
./tdgbsv_serial


echo "=== tdgbsv_fake_acc === "
xlf90_r -Dsimd= -DSIMD=  -g -qcheck=all -qstrict -qfixed -D_OPENACC -UOMP_TARGET \
  -o tdgbsv_fake_acc \
  lapack_acc.F90 tdgbsv.F90 \
  -L$OLCF_ESSL_ROOT/lib64 -lessl

./tdgbsv_fake_acc


echo "=== tdgbsv_fake_noacc === "
xlf90_r -Dsimd= -DSIMD=  -g -qcheck=all -qstrict -qfixed -U_OPENACC -UOMP_TARGET \
  -o tdgbsv_fake_noacc \
  lapack_acc.F90  tdgbsv.F90 \
  -L$OLCF_ESSL_ROOT/lib64 -lessl

./tdgbsv_fake_noacc

date
