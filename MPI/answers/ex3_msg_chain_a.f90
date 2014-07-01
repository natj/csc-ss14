program basic
  implicit none
  include 'mpif.h'
  integer, parameter :: size = 100
  integer :: rc, myid, ntasks, count
  integer :: status(MPI_STATUS_SIZE)
  integer :: message(size)
  integer :: receiveBuffer(size)

  call MPI_INIT(rc)
  call MPI_COMM_RANK(MPI_COMM_WORLD, myid, rc)
  call MPI_COMM_SIZE(MPI_COMM_WORLD, ntasks, rc)

  message = myid

  ! Send and receive as defined in the assignment
  if ( myid < ntasks-1 ) then
     call MPI_SEND(message, size, MPI_INTEGER, myid+1, &
          myid+1, MPI_COMM_WORLD, rc)
     write(*,'(A10,I3,A20,I8,A,I3,A,I3)') 'Sender: ', myid, &
          ' Sent elements: ',size, &
          '. Tag: ', myid+1, '. Receiver: ', myid+1
  end if

  if ( myid > 0 ) then
     call MPI_RECV(receiveBuffer, size, MPI_INTEGER, myid-1,  &
          myid, MPI_COMM_WORLD, status, rc)
     call MPI_GET_COUNT(status, MPI_INTEGER, count, rc)
     write(*,'(A10,I3,A,I3)') 'Receiver: ', myid, &
          ' First element: ', receiveBuffer(1)
  end if

  call MPI_FINALIZE(rc)

end program basic
