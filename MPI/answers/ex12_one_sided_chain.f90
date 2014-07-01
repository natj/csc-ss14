program basic
  implicit none
  include 'mpif.h'
  integer, parameter :: size = 100
  integer :: rc, myid, ntasks
  integer :: message(size)
  integer :: receiveBuffer(size)
  
  integer(kind=MPI_ADDRESS_KIND) :: winsize, disp
  integer :: window

  call MPI_INIT(rc)
  call MPI_COMM_RANK(MPI_COMM_WORLD, myid, rc)
  call MPI_COMM_SIZE(MPI_COMM_WORLD, ntasks, rc)

  message = myid

  winsize = sizeof(receiveBuffer) * size
  ! Create window corresponding to the receive buffer
  call MPI_WIN_CREATE(receiveBuffer, winsize, sizeof(receiveBuffer), &
                      MPI_INFO_NULL, MPI_COMM_WORLD, window, rc)

  ! Send and receive as defined in the assignment
  call MPI_WIN_FENCE(0, window, rc)
  if ( myid < ntasks-1 ) then
     disp = 0
     call MPI_PUT(message, size, MPI_INTEGER, myid + 1, disp, size, &
                  MPI_INTEGER, window, rc)
     write(*,'(A10,I3,A20,I8,A,I3)') 'Origin: ', myid, &
          ' Put elements: ',size, &
          '. Target: ', myid+1
  end if
  call MPI_WIN_FENCE(0, window, rc)

  if ( myid > 0 ) then
     write(*,'(A10,I3,A,I3)') 'Target: ', myid, &
          ' First element: ', receiveBuffer(1)
  end if

  call MPI_WIN_FREE(window, rc)
  call MPI_FINALIZE(rc)

end program basic
