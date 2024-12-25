salloc --nodes 1 --qos interactive --time 04:00:00 --constraint cpu --account=m4588

module load PrgEnv-intel/8.5.0
module load intel-oneapi/2023.2.0
module load cray-mpich/8.1.28
module load valgrind/3.23.0
module load gdb4hpc/4.16.0
pdir=
prog=$pdir/hybrid_prog

ulimit -s unlimited
export OMP_PLACES=cores
export OMP_PROC_BIND=close
export OMP_NUM_THREADS=8
srun -N 1 --ntasks-per-node=1 --cpus-per-task=$OMP_NUM_THREADS $prog
