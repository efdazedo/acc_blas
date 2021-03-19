echo "== serial == "
date
export OPENBLAS_MODULE=openblas-0.3.7-gcc-7.4.0-nh3kll3
module load $OPENBLAS_MODULE
gfortran -ffixed-form -pg -o tdgels_serial lapack_mod.F90 lapack_acc.F90 tdgels.F90 \
 -lopenblas



echo "== fake OpenACC == "
date
gfortran -ffixed-form -pg -D_OPENACC -o tdgels_acc_fake  lapack_acc.F90 tdgels.F90  \
  -lopenblas

