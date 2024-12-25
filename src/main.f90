PROGRAM HYBRID_MAIN
  USE OMP_LIB
  IMPLICIT NONE
  INCLUDE 'mpif.h'

  ! mpi...
  INTEGER                        :: mpierr, err_code, io_pid, pid_idx, shm_rank, shm_size, shm_comm
  CHARACTER(LEN=24)              :: proc_name, last_proc_name
  CHARACTER(LEN=24), ALLOCATABLE :: proc_names(:)
  ! openmp...
  INTEGER :: tid, n_thread, pid, n_proc, proc_name_len, n_core
  ! output..
  INTEGER          :: io_fid, io_err_fid
  CHARACTER(LEN=8) :: log_file, log_err_file
  ! calc...
  INTEGER, PARAMETER :: M1 = 1024, M2 = 512, M3 = 256, n_loop = 2041
  REAL*8,  ALLOCATABLE :: matr1(:,:), matr2(:,:), matr3(:,:), a1d(:)
  REAL*8  :: t1, t2, t3, t4, c, t_beg, t_end, t_start_cpu, t_start_wall, t_finish_cpu, t_finish_wall
  INTEGER :: i, j, k, l, n_loop_per_process
  LOGICAL :: output

  CALL MPI_INIT( mpierr )
  CALL MPI_COMM_SIZE( mpi_comm_world, n_proc, mpierr )
  CALL MPI_COMM_RANK( mpi_comm_world, pid, mpierr )
  ! Split the communicator by shared memory
  CALL MPI_COMM_SPLIT_TYPE( mpi_comm_world, mpi_comm_type_shared, 0, mpi_info_null, shm_comm, mpierr )
  ! ! Get rank and size in the new shared memory communicator
  CALL MPI_COMM_RANK( shm_comm, shm_rank, mpierr )
  CALL MPI_COMM_SIZE( shm_comm, shm_size, mpierr )
  ! ! Print information
  ! print *, "Global rank:", rank, "Shared memory rank:", shm_rank, "Shared memory size:", shm_size
  ! ! Free the shared memory communicator
  ! CALL MPI_COMM_FREE(shm_comm, ierr)
  
  OUTPUT       = .FALSE.
  io_pid       = 0
  io_fid       = 1001
  log_file     = 'log'
  io_err_fid   = 3001
  log_err_file = 'log_err'
  OPEN( UNIT = io_err_fid, FILE = log_file, STATUS = 'REPLACE' )
  IF( pid .EQ. 0)  THEN
    OUTPUT  = .TRUE.
    OPEN( UNIT = io_fid, FILE = log_file, STATUS = 'REPLACE' )
    WRITE( io_fid, '(A/)')  'Begin.'
  ENDIF
  CALL MPI_GET_PROCESSOR_NAME( proc_name, proc_name_len, MPIERR )
  ALLOCATE( proc_names( n_proc +1 ) )
  proc_names( n_proc +1 ) = 'END'
  CALL MPI_GATHER( proc_name, 16, MPI_CHARACTER, proc_names, 16, MPI_CHARACTER, io_pid, mpi_comm_world, mpierr )
  IF(OUTPUT)  THEN
    WRITE( io_fid, '(A,/2X,A)', ADVANCE = 'NO' )  '__MPI info__:', 'Task/Process      1-'
    pid_idx        = 1
    last_proc_name = proc_names(1)
    DO pid_idx = 2, n_proc +1
       IF( proc_names( pid_idx ) .NE. last_proc_name )  THEN
         WRITE( io_fid, '(I7,A16,A16)', ADVANCE = 'YES')  pid_idx -1, 'running on host', proc_names( pid_idx -1 )
         IF( pid_idx .NE. n_proc +1 )  WRITE( io_fid, '(A14,I7,A)', ADVANCE = 'NO' )  'Task/Process', pid_idx, '-'
       ENDIF
       last_proc_name = proc_names( pid_idx )
    ENDDO ! ENDS pid_idx
  ENDIF
  IF( ALLOCATED( proc_names ) )  DEALLOCATE( proc_names )

  IF( MOD( n_loop, n_proc ) .NE. 0 )  THEN
    IF(OUTPUT)  WRITE( io_err_fid, '(A,I0,A)') '__ERROR: n_loop (', n_loop, ') should be able to be divided by number of mpi processes!'
    CALL MPI_ABORT( MPI_COMM_WORLD, err_code, mpierr )
  ENDIF

  n_loop_per_process = n_loop / n_proc
  ! Get the number of available processors (cores)
  n_core = omp_get_num_procs()
  CALL MPI_GET_PROCESSOR_NAME( proc_name, proc_name_len, MPIERR )
  n_thread = OMP_GET_NUM_THREADS()
  tid      = OMP_GET_THREAD_NUM()
  IF(OUTPUT)  THEN
    WRITE( io_fid, '(A24,I12)')  'mpi processes', n_proc
    WRITE( io_fid, '(A24,I12)')  'processors/cores', n_core
    !$OMP PARALLEL
    IF( omp_get_thread_num() .EQ. 0 )  &
    WRITE( io_fid, '(A24,I12)')  'threads per mpi process', omp_get_num_threads()
    !$OMP END PARALLEL
    !WRITE( io_fid, '(A24,I12)')  'threads in the current region', n_thread
    WRITE( io_fid, '(A24,4I12)') 'problem size', M1, M2, M3, n_loop
  ENDIF

  ! Initialize arrays
  ALLOCATE( matr1( M1, M2 ) )
  ALLOCATE( matr2( M2, M3 ) )
  ALLOCATE( matr3( M1, M3 ) )
  ALLOCATE( a1d( n_loop ) )
  DO i = 1, n_loop
     CALL RANDOM_NUMBER( a1d( i ) )
  ENDDO ! ENDS i
  DO j = 1, M2
     DO i = 1, M1
        CALL RANDOM_NUMBER( matr1( i, j ) )
     ENDDO ! ENDS i
  ENDDO ! ENDS j
  DO j = 1, M3
     DO i = 1, M2
        CALL RANDOM_NUMBER( matr2( i, j ) )
     ENDDO ! ENDS i
  ENDDO ! ENDS j

  t_start_wall = OMP_GET_WTIME()
  CALL CPU_TIME( t_start_cpu )
  CALL CPU_TIME( t_beg )
  t1 = OMP_GET_WTIME()
  DO l = 1, n_loop
     CALL DGEMM( 'N', 'N', M1, M3, M2, a1d( l ), matr1, M1, matr2, M2, 0.0, matr3, M1 )
  ENDDO ! ENDS l
  t2 = OMP_GET_WTIME()
  CALL CPU_TIME( t_end )
  IF(OUTPUT)  WRITE( io_fid, '(A24,F14.1,F24.1)')  'CPU serial time', t2 - t1, t_end - t_beg

  CALL CPU_TIME( t_beg )
  t1 = OMP_GET_WTIME()
  !$OMP PARALLEL 
  !$OMP DO
  DO l = 1, n_loop
     CALL DGEMM( 'N', 'N', M1, M3, M2, a1d( l ), matr1, M1, matr2, M2, 0.0, matr3, M1 )
  ENDDO ! ENDS l
  !$OMP END DO
  !$OMP END PARALLEL
  t2 = OMP_GET_WTIME()
  CALL CPU_TIME( t_end )
  IF(OUTPUT)  WRITE( io_fid, '(A24,F14.1,F24.1)') 'omp time', t2 - t1, t_end - t_beg

  !*...Free spaces...*!
  IF( ALLOCATED( matr1 ) )  DEALLOCATE( matr1 )
  IF( ALLOCATED( matr2 ) )  DEALLOCATE( matr2 )
  IF( ALLOCATED( matr3 ) )  DEALLOCATE( matr3 )
  IF( ALLOCATED( a1d ) )    DEALLOCATE( a1d )

  CALL  CPU_TIME( t_finish_cpu )
  t_finish_wall = OMP_GET_WTIME()
  IF(OUTPUT)  THEN
    WRITE( io_fid, '(A24,F14.1,F24.1)')  'Total time', t_finish_wall - t_start_wall, t_finish_cpu - t_start_cpu
    WRITE( io_fid, '(/A)')  'Done.'
  ENDIF
  
  IF(OUTPUT)  CLOSE( io_fid )
  CLOSE( io_err_fid )
  CALL MPI_FINALIZE( mpierr )
END PROGRAM HYBRID_MAIN
