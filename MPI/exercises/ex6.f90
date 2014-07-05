program ex6
  use mpi
  implicit none

  integer :: size, rank, rc, count, rtag, rsrc
  integer :: source, dest
  integer :: status(MPI_STATUS_SIZE)
  integer, parameter :: p = 10
  integer, dimension(p) :: data, rdata
  integer :: request, request2

  call mpi_init(rc)
  call mpi_comm_size(mpi_comm_world, size, rc)
  call mpi_comm_rank(mpi_comm_world, rank, rc)

  !SEND
  data = rank
  if(rank == size-1) then
     dest = MPI_PROC_NULL
  else 
     dest = rank+1
  end if
  !RECEIVE
  if(rank == 0) then
     source = MPI_PROC_NULL
  else
     source = rank-1
  end if

  call mpi_isend(data, p, MPI_INTEGER, dest, rank+1, mpi_comm_world, request2, rc)
  write(*,*) rank,' send ', p, 'elements to', rank+1

  call mpi_irecv(rdata, p, MPI_INTEGER, source, MPI_ANY_TAG, mpi_comm_world, request, rc)
  call mpi_wait(request, status, rc)

  ! get info of the data received
  call mpi_get_count(status, mpi_integer, count, rc)
  rtag = status(MPI_TAG)
  rsrc = status(MPI_SOURCE)

  write(*,*) rank,'received', rdata(1),'that is', count, 'long with tag',rtag, 'from', rsrc

  call mpi_finalize(rc)
end program
