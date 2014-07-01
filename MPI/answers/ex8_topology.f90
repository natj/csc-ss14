program basic
  use mpi
  implicit none
!  include 'mpif.h'
  integer, parameter :: size = 100
  integer :: rc, myid, ntasks, count
  integer :: status(MPI_STATUS_SIZE,2)
  integer :: message(size)
  integer :: receiveBuffer(size)
  integer :: source, destination
  integer :: requests(2)

  integer :: cart_comm
  integer :: dims(1)
  logical :: periods(1) = (/.FALSE./)


  call MPI_INIT(rc)
  call MPI_COMM_SIZE(MPI_COMM_WORLD, ntasks, rc)

  ! Create cartesian topology
  dims = ntasks
  call MPI_CART_CREATE(MPI_COMM_WORLD, 1, dims, periods, .true., cart_comm, rc)
  call MPI_COMM_RANK(cart_comm, myid, rc)


  message = myid

  call MPI_CART_SHIFT(cart_comm, 0, 1, source, destination, rc)

  ! Send and receive as defined in exercise

  call MPI_IRECV(receiveBuffer, size, MPI_INTEGER,source,  &
       MPI_ANY_TAG, MPI_COMM_WORLD, requests(1), rc)
  call MPI_ISEND(message, size, MPI_INTEGER, destination, &
       myid + 1, MPI_COMM_WORLD, requests(2), rc)
  write(*,'(A10,I3,A20,I8,A,I3,A,I3)') 'Sender: ', myid, &
       ' Sent elements: ', size, &
       '. Tag: ', myid + 1, '. Receiver: ', destination

  call MPI_WAITALL(2, requests, status, rc)

  call MPI_GET_COUNT(status(:,1), MPI_INTEGER, count, rc)
  write(*,'(A10,I3,A20,I8,A,I3,A,I3)') 'Receiver: ', myid, &
       'received elements: ', count, &
       '. Tag: ', status(MPI_TAG, 1), &
       '. Sender:   ', status(MPI_SOURCE, 1)

  call MPI_FINALIZE(rc)
end program basic
