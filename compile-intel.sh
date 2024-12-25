#!/bin/bash

# Environments; platform: nersc perlmutter
module load PrgEnv-intel/8.5.0
module load cray-mpich/8.1.28

# files
prog=hybrid_prog
src=main.f90

# debug version
# -qmkl
ftn -L${MKLROOT}/lib/intel64 -lmkl_scalapack_lp64 -lmkl_blacs_intelmpi_lp64 -lmkl_core -lpthread -lmkl_intel_lp64 -lmkl_sequential \
    -qopenmp -O2 -g -traceback -o $prog $src

# work version [print optimization info]
ftn -L${MKLROOT}/lib/intel64 -lmkl_scalapack_lp64 -lmkl_blacs_intelmpi_lp64 -lmkl_core -lpthread -lmkl_intel_lp64 -lmkl_sequential \
    -qopenmp -O3 -qopt-report=2 -o $prog $src
