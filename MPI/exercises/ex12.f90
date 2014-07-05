program ex3
  use mpi
  implicit none

  integer :: size, rank, rc, count
  integer :: status(MPI_STATUS_SIZE)
  integer(kind=MPI_ADDRESS_KIND) :: winsize, disp
  integer :: win
  integer, parameter :: p = 10
  integer, dimension(p) :: data

  call mpi_init(rc)
  call mpi_comm_size(mpi_comm_world, size, rc)
  call mpi_comm_rank(mpi_comm_world, rank, rc)

  data = rank

  winsize=sizeof(data)*p
  call mpi_win_create(data, winsize, sizeof(data), &
       MPI_INFO_NULL, mpi_comm_world, win, rc)

  !start epoch
  call mpi_win_fence(0, win, rc)

  if(rank < size-1) then
     disp = 0
     call mpi_put(data, p, MPI_INTEGER, rank+1, disp, p, MPI_INTEGER, win, rc)
     write(*,*) rank,'put',p,'elements to',rank+1
  end if

  !end epoch
  call mpi_win_fence(0, win, rc)


  write(*,*) rank,' received ', data(1)

  call mpi_finalize(rc)
end program
