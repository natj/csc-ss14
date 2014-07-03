program hello
  use mpi
  implicit none
  ! include 'mpif.h' ! this is the place for include

  integer :: rank, size, rc, i

  call mpi_init(rc)
  call mpi_comm_size(MPI_COMM_WORLD, size, rc)
  call mpi_comm_rank(MPI_COMM_WORLD, rank, rc)

  write(*,*) 'I am rank', rank, 'in group of',size

  call mpi_barrier(MPI_COMM_WORLD, rc)
  if(rank == 0) then
     write(*,*) 'In total there were', size, 'processes'
     write(*,*) ' '
     write(*,*) 'and now in order:'
  end if


  do i=0,size-1
     if(rank == i) then
        write(*,*) 'I am rank', rank
     end if
     call mpi_barrier(MPI_COMM_WORLD, rc)
  end do

  call mpi_finalize(rc)
end program
