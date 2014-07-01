program neighborhood_collectives
  use mpi
  implicit none
  integer, dimension(:), allocatable :: test_send, test_recv
  integer :: n, i
  integer :: my_id, ntasks, rc, comm2d, coord(2)

  call init_comm(ntasks, my_id, comm2d, coord)

! case neighbor_allgather
  n = 2
  allocate (test_send(n), test_recv(n*4))
  do i = 1, n
     test_send(i) = my_id*10 + i
  end do
  test_recv = 0
  call mpi_neighbor_allgather(test_send, n, mpi_integer, &
       test_recv, n, mpi_integer, comm2d, rc)
  do i = 0, ntasks-1
     if (my_id == i) then
        write (*,'(I4,A2,2I2,A1)') my_id, '=(', coord(:), ')'
        write (*,'(A,20I4)') 'Data:', test_send(:)
        write (*,'(A,20I4)') 'Recv:', test_recv(:)
     end if
     call mpi_barrier(mpi_comm_world, rc)
  end do
  deallocate(test_send, test_recv)

! case neighbor_alltoall

  n = 4
  allocate (test_send(n), test_recv(n))
  do i = 1, n
     test_send(i) = my_id*10 + i
  end do
  test_recv = 0
  call mpi_neighbor_alltoall(test_send, n/4, mpi_integer, &
       test_recv, n/4, mpi_integer, comm2d, rc)
 
  do i = 0, ntasks-1
     if (my_id == i) then
        write (*,'(I4,A2,2I2,A1)') my_id, '=(', coord(:), ')'
        write (*,'(A,20I4)') 'Data:', test_send(:)
        write (*,'(A,20I4)') 'Recv:', test_recv(:)
     end if
     call mpi_barrier(mpi_comm_world, rc)
  end do
  deallocate(test_send, test_recv)

  call mpi_finalize(rc)

contains

  subroutine init_comm(ntask, id, comm2d, coord)
    implicit none
    integer,  intent(out) :: ntask, id, comm2d, coord(0:1)
    integer :: npx, npy, dims(0:1)
    logical :: period(2) = (/ .true., .true. /)
    call mpi_init(rc)
    if (rc /= MPI_SUCCESS) then
       write(*,*) 'MPI init failed'
       call mpi_abort(MPI_COMM_WORLD, 1, rc)
    end if
    call mpi_comm_size(MPI_COMM_WORLD, ntask, rc)
    call mpi_comm_rank(MPI_COMM_WORLD, id, rc)
    ! determine the process grid (dims(0) x dims(1) = ntask)
    if (ntask < 16) then
       dims(0) = 2
    else if (ntask >= 16 .and. ntask < 64) then
       dims(0) = 4
    else if (ntask >= 64 .and. ntask < 256) then
       dims(0) = 8
    else
       dims(0) = 16
    end if
    dims(1) = ntask/dims(0)
    if (dims(0)*dims(1) /= ntask) then
       write(*,'(A,I3,A1,I3,A2,I4)') 'sorry, no go', dims(0), 'x', &
            dims(1),'/=', ntask
       call mpi_abort(mpi_comm_world, 1, rc)
    end if
    ! create the 2D Cartesian communicator
    call mpi_cart_create(mpi_comm_world,2,dims,period,.true.,comm2d, rc)
    ! find out & store also the Cartesian coordinates of a rank
    call mpi_cart_coords(comm2d,my_id,2,coord,rc)
  end subroutine init_comm

end program neighborhood_collectives
