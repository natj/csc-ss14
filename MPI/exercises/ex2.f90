program ex2
  use mpi
  implicit none

  integer, parameter :: dp = selected_real_kind(12)
  integer :: rc, size, rank
  real(kind=dp), dimension(:), allocatable :: data, rdata

  ! basic mpi stuff
  call mpi_init(rc)
  call mpi_comm_size(mpi_comm_world, size, rc)
  call mpi_comm_rank(mpi_comm_world, rank, rc)

  !allocate and initialize data vector
  allocate(data(1:10))
  allocate(rdata(1:10))
  data = real(rank, dp)

  write(*,*) rank,' has ',data(1)

  !call mpi_send(buf, count, datatype, dest, tag, comm, rc)
  !call mpi_recv(buf, count, datatype, source, tag, comm, status, rc)
  if (rank == 0) then
     call mpi_send(data, 10, MPI_DOUBLE_PRECISION, 1, 1, mpi_comm_world, rc)
     call mpi_recv(rdata, 10, MPI_DOUBLE_PRECISION, 1, 1, mpi_comm_world, MPI_STATUS_IGNORE, rc)
     !call mpi_recv(rdata, 10, MPI_DOUBLE_PRECISION, 1, 1, mpi_comm_world, rc)
  elseif (rank == 1) then
     call mpi_send(data, 10, MPI_DOUBLE_PRECISION, 0, 1, mpi_comm_world, rc)
     call mpi_recv(rdata, 10, MPI_DOUBLE_PRECISION, 0, 1, mpi_comm_world, MPI_STATUS_IGNORE, rc)
     !call mpi_recv(rdata, 10, MPI_DOUBLE_PRECISION, 0, 1, mpi_comm_world, rc)
  end if

  call mpi_barrier(mpi_comm_world, rc)

  write(*,*) 'after exchange', rank, 'has', rdata(1)

  call mpi_finalize(rc)
end program
