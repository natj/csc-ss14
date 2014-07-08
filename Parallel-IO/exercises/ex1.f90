program pario
  use mpi
  implicit none

  integer :: i
  integer :: rank, ranks, size, sizes, col, rc
  integer :: comm
  integer, parameter :: p=100
  integer :: vec(p)
  integer, dimension(:), allocatable :: buff 
  character(len=20) :: filen

  integer(kind=mpi_offset_kind) :: displ
  integer :: filehandle
  integer :: intsize

  filen='ex1.dat'

  call mpi_init(rc)
  call mpi_comm_size(MPI_COMM_WORLD, size, rc)
  call mpi_comm_rank(MPI_COMM_WORLD, rank, rc)

  if (mod(rank,2) == 0) then
     col = 1
     filen='ex1a.dat'
  else
     col = 2
     filen='ex1b.dat'
  end if

  call mpi_comm_split(MPI_COMM_WORLD, col, 0, comm, rc)
  call mpi_comm_rank(comm, ranks, rc)
  call mpi_comm_size(comm, sizes, rc)
  !allocate buffer to size times rank numbers
  allocate(buff(1:p))

  vec = rank
  buff = -1

  ! send to root 0
  !call mpi_gather(vec, p, MPI_INTEGER, &
  !     buff, p, MPI_INTEGER, 0, comm, rc)

  !debug prints
!  if(rank == 0) then
  call mpi_file_open(comm, filen, &
       MPI_MODE_CREATE+MPI_MODE_WRONLY, MPI_INFO_NULL, filehandle, rc)
  call mpi_type_size(MPI_INTEGER, intsize, rc)
  
  displ = ranks*intsize*p
  !write(*,*) 'displ:', displ,' of rank', ranks
  !displ=0
  call mpi_file_write_at(filehandle, displ, vec, p, &
       MPI_INTEGER, MPI_STATUS_IGNORE, rc)
  
  call mpi_file_close(filehandle, rc)
  !end if

  call mpi_finalize(rc)
end program
