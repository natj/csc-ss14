program basic
  implicit none
  include 'mpif.h'
  integer, parameter :: size = 100
  integer :: rc, myid, ntasks, count
  integer :: status(MPI_STATUS_SIZE)
  integer :: message(size)
  integer :: receiveBuffer(size)
  integer :: source, destination

  call MPI_INIT(rc)
  call MPI_COMM_RANK(MPI_COMM_WORLD, myid, rc)
  call MPI_COMM_SIZE(MPI_COMM_WORLD, ntasks, rc)

  message = myid

  if (myid < ntasks-1) then
     destination = myid + 1
  else
     destination = MPI_PROC_NULL
  end if

  ! Send and receive as defined in exercises
  if (myid > 0) then
     source = myid - 1
  else
     source = MPI_PROC_NULL
  end if

  call MPI_SENDRECV(message, size, MPI_INTEGER, destination, myid + 1, &
       receiveBuffer, size, MPI_INTEGER,source, MPI_ANY_TAG, &
       MPI_COMM_WORLD, status, rc)

  write(*,'(A10,I3,A20,I8,A,I3,A,I3)') 'Sender: ', myid, &
       ' Sent elements: ', size, &
       '. Tag: ', myid + 1, '. Receiver: ', destination

  call MPI_GET_COUNT(status, MPI_INTEGER, count, rc)
  write(*,'(A10,I3,A20,I8,A,I3,A,I3)') 'Receiver: ', myid, &
       'received elements: ', count, &
       '. Tag: ', status(MPI_TAG), &
       '. Sender:   ', status(MPI_SOURCE)

  call MPI_FINALIZE(rc)
end program basic
