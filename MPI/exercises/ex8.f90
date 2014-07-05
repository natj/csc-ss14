program ex3
  use mpi
  implicit none

  integer :: size, rank, rc, count, rtag, rsrc
  integer :: status(MPI_STATUS_SIZE)
  integer :: gridcomm
  integer :: source, dest
  integer, parameter :: p = 10
  integer, dimension(p) :: data, rdata
  integer :: request, request2

  call mpi_init(rc)
  call mpi_comm_size(mpi_comm_world, size, rc)
  call mpi_comm_rank(mpi_comm_world, rank, rc)

  data = rank

  ! create cartesian grid and get neighbours
  call mpi_cart_create(mpi_comm_world, 1, (/ size /), (/ .true. /), .true., gridcomm, rc) 
  call mpi_cart_shift(gridcomm, 1, 1, source, dest, rc)

  !send & receive
  call mpi_isend(data, p, MPI_INTEGER, dest, rank+1, gridcomm, request2, rc)
  write(*,*) rank,' send ', p, 'elements to', rank+1

  call mpi_irecv(rdata, p, MPI_INTEGER, source, MPI_ANY_TAG, gridcomm, request, rc)
  call mpi_wait(request, status, rc)


  ! get info of the data received
  call mpi_get_count(status, mpi_integer, count, rc)
  rtag = status(MPI_TAG)
  rsrc = status(MPI_SOURCE)

  write(*,*) rank,' received ', rdata(1),' that is ', count, 'long with tag',rtag, 'from', rsrc

  call mpi_finalize(rc)
end program
