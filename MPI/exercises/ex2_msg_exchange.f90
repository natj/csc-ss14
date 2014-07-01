program exchange
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

  ! TODO start
  ! Send and receive as defined in the assignment
  if ( myid == 0 ) then

     write(*,'(A10,I3,A10,I3)') 'Rank: ', myid, &
          ' received ', receiveBuffer(1)
  else if (myid == 1) then

     write(*,'(A10,I3,A10,I3)') 'Rank: ', myid, &
          ' received ', receiveBuffer(1)
  end if

  ! TODO end

  call MPI_FINALIZE(rc)

end program exchange
