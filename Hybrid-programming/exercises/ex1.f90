program hello
  use mpi
  use omp_lib
  implicit none

  integer :: rank, msize, prov_supp, rc
  integer :: thread, num_threads

  integer :: msg, i

  call mpi_init_thread(MPI_THREAD_MULTIPLE, prov_supp, rc)
  if (prov_supp < MPI_THREAD_MULTIPLE) then
     write(*,*) 'MPI version does not support MPI_THREAD_MULTIPLE'
     !call mpi_abort(mpi_comm_world, rc)
  end if


  call mpi_comm_size(MPI_COMM_WORLD, msize, rc)
  call mpi_comm_rank(MPI_COMM_WORLD, rank, rc)

!$omp parallel private(thread, num_threads)
  thread = omp_get_thread_num()
  num_threads = omp_get_num_threads()

  if (thread == 0) then
     !omp single
     write(*,*) num_threads,' threads in master rank'
     !omp end single
     do i = 1,msize-1
        call mpi_send(thread, 1, MPI_INTEGER, i, thread, MPI_COMM_WORLD, rc)
     end do
  else
     call mpi_recv(msg, 1, MPI_INTEGER, 0, thread, &
          MPI_COMM_WORLD, MPI_STATUS_IGNORE, rc) 
     write(*,*) 'rank:',rank,'thread:',thread,'received:',msg
  end if
!$omp end parallel

  call mpi_finalize(rc)
end program
