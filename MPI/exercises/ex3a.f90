program ex3
  use mpi
  implicit none

  integer :: size, rank, rc, count, rtag, rsrc
  integer :: status(MPI_STATUS_SIZE)
  integer, parameter :: p = 10
  integer, dimension(p) :: data, rdata

  call mpi_init(rc)
  call mpi_comm_size(mpi_comm_world, size, rc)
  call mpi_comm_rank(mpi_comm_world, rank, rc)

  data = rank

  if(rank < size-1) then
     call mpi_send(data, p, MPI_INTEGER, rank+1, rank+1, mpi_comm_world, rc)
     write(*,*) rank,' send ', p, 'elements to', rank+1
  end if

  if(rank > 0) then
     !call mpi_recv(rdata, p, MPI_INTEGER, rank-1, rank, mpi_comm_world, status, rc)

     ! receive from any
     call mpi_recv(rdata, p, MPI_INTEGER, MPI_ANY_SOURCE, MPI_ANY_TAG, mpi_comm_world, status, rc)

     ! get info of the data received
     call mpi_get_count(status, mpi_integer, count, rc)
     rtag = status(MPI_TAG)
     rsrc = status(MPI_SOURCE)

     write(*,*) rank,' received ', rdata(1),' that is ', count, 'long with tag',rtag, 'from', rsrc

  end if


  call mpi_finalize(rc)
end program
