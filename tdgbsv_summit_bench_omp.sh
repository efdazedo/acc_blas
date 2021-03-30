module unload pgi cuda
module load xl cuda essl
module load netlib-lapack
touch lapack_mod.mod lapack_acc.mod
rm lapack_mod.mod lapack_acc.mod

export OMP_NUM_THREADS=7
export OMP_STACKSIZE=2G

date

echo "=== tdgbsv_omp_gpu === "
xlf90_r   -DOMP_TARGET -Dsimd= -DSIMD= -U_OPENACC -O3   -qstrict -qfixed -qsmp=omp -qoffload -c lapack_acc.F90
xlf90_r   -DOMP_TARGET -Dsimd= -DSIMD= -U_OPENACC -O3   -qstrict -qsmp=omp -qoffload \
          -o tdgbsv_omp_gpu \
          lapack_acc.o tdgbsv.F90   \
          -L$OLCF_ESSL_ROOT/lib64 -lessl

jsrun -n 1 -c 7 -a 1 -g 1 ./tdgbsv_omp_gpu >& tdgbsv_omp_gpu.out



echo "=== tdgbsv_serial === "
xlf90_r   -UOMP_TARGET -Dsimd= -DSIMD= -U_OPENACC -O3   -qsimp=omp -qstrict -qfixed   -c lapack_mod.F90
xlf90_r   -UOMP_TARGET -Dsimd= -DSIMD= -U_OPENACC -O3   -qsimp=omp -qstrict \
  -o tdgbsv_serial \
  lapack_mod.o tdgbsv.F90 \
  -L$OLCF_ESSL_ROOT/lib64 -lessl
jsrun -n 1 -c 7 -a 1 -g 1 -EOMP_NUM_THREADS=7 -EOMP_STACKSIZE=2G ./tdgbsv_serial >& tdgbsv_serial.out


date
