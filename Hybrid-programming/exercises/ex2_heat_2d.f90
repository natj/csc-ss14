!  2D heat equation
!
!  Authors: Jussi Enkovaara, Mikko Byckling, ...
!  Copyright (C) 2014  CSC - IT Center for Science Ltd.
!
!  Licensed under the terms of the GNU General Public License as published by
!  the Free Software Foundation, either version 3 of the License, or
!  (at your option) any later version.
!
!  Code is distributed in the hope that it will be useful,
!  but WITHOUT ANY WARRANTY; without even the implied warranty of
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!  GNU General Public License for more details.
!
!  Copy of the GNU General Public License can be onbtained from
!  see <http://www.gnu.org/licenses/>.

module heat_mpi
  use mpi
  use omp_lib
  implicit none

  integer, parameter :: dp = SELECTED_REAL_KIND(12) 
  real(kind=dp), parameter :: DX = 0.01, DY = 0.01  ! Fixed grid spacing

  type :: field
     integer :: nx          ! local dimension of the field
     integer :: ny
     integer :: nx_full     ! global dimension of the field
     integer :: ny_full
     real(kind=dp) :: dx
     real(kind=dp) :: dy
     real(kind=dp) :: dx2
     real(kind=dp) :: dy2
     real(kind=dp), dimension(:,:), allocatable :: data
  end type field

  type :: parallel_data
     integer :: size
     integer :: rank
     integer :: nup, ndown, nleft, nright  ! Ranks of neighbouring MPI tasks
     integer :: comm
     integer :: rowtype                    ! MPI Datatype for communication of rows
     integer :: columntype                 ! MPI Datatype for communication of columns
     integer :: subarraytype               ! MPI Datatype for communication of inner region 
  end type parallel_data

contains

  ! Initialize the field type metadata
  ! Arguments:
  !   field0 (type(field)): input field
  !   nx, ny, dx, dy: field dimensions and spatial step size
  subroutine initialize_field_metadata(field0, nx, ny, parallel)
    implicit none

    type(field), intent(out) :: field0
    integer, intent(in) :: nx, ny
    type(parallel_data), intent(in) :: parallel

    integer :: nx_local, ny_local

    nx_local = nx / sqrt(real(parallel%size))
    ny_local = ny / sqrt(real(parallel%size))

    field0%dx = DX
    field0%dy = DY
    field0%dx2 = DX**2
    field0%dy2 = DY**2
    field0%nx = nx_local
    field0%ny = ny_local
    field0%nx_full = nx
    field0%ny_full = ny

  end subroutine initialize_field_metadata

  ! Initialize parallelization data
  subroutine parallel_initialize(parallel, nx, ny)
    implicit none

    type(parallel_data), intent(out) :: parallel
    integer, intent(in) :: nx, ny

    integer :: nx_local, ny_local
    integer :: world_size
    integer :: dims(2)
    logical :: periods(2) = (/.False., .FALSE./)

    integer, dimension(2) :: sizes, subsizes, offsets

    integer :: ierr

    call mpi_comm_size(MPI_COMM_WORLD, world_size, ierr)

    dims(1) = sqrt(real(world_size))
    dims(2) = dims(1)
    if (dims(1) * dims(2) /= world_size) then
       write(*,*) 'Cannot make square MPI grid, please use number of CPUs which is power of two', & 
                   dims(1), dims(2), world_size
       call mpi_abort(MPI_COMM_WORLD, -1, ierr)
    end if

    nx_local = nx / dims(1)
    ny_local = ny / dims(2)
    
    if (nx_local * dims(1) /= nx) then
       write(*,*) 'Cannot divide grid evenly to processors in x-direction', &
                   nx_local, dims(1), nx
       call mpi_abort(MPI_COMM_WORLD, -2, ierr)
    end if

    if (ny_local * dims(2) /= ny) then
       write(*,*) 'Cannot divide grid evenly to processors in y-direction', &
                   ny_local, dims(2), ny
       call mpi_abort(MPI_COMM_WORLD, -2, ierr)
    end if

    ! Create cartesian communicator based on dims variable
    call mpi_cart_create(MPI_COMM_WORLD, 2, dims, periods, .true., parallel%comm, ierr)

    ! Find the neighbouring MPI tasks (parallel%nup, parallel%ndown, 
    ! parallel%nleft, parallel%nright) using MPI_Cart_shift
    call mpi_cart_shift(parallel%comm, 1, 1, parallel%nleft, parallel%nright, ierr)
    call mpi_cart_shift(parallel%comm, 0, 1, parallel%nup, parallel%ndown, ierr)

    !  Determine parallel%size and parallel%rank from newly created Cartesian comm
    call mpi_comm_size(parallel%comm, parallel%size, ierr)
    call mpi_comm_rank(parallel%comm, parallel%rank, ierr)

    ! Create datatypes for halo exchange
    ! Datatype for communication of rows (parallel%rowtype)
    call mpi_type_vector(ny_local+2, 1, nx_local+2, MPI_DOUBLE_PRECISION, parallel%rowtype, ierr)

    ! Datatype for communication of columns (parallel%columntype)
    call mpi_type_contiguous(nx_local+2, MPI_DOUBLE_PRECISION, parallel%columntype, ierr)

    ! commit newly defined types
    call mpi_type_commit(parallel%rowtype, ierr)
    call mpi_type_commit(parallel%columntype, ierr)

    ! Create datatype for subblock needed in I/O
    subsizes(1) = nx_local
    subsizes(2) = ny_local
    offsets(1) = 0
    offsets(2) = 0

    ! Rank 0 uses datatype for receiving data into full array
    if (parallel%rank == 0) then
       sizes(1) = nx
       sizes(2) = ny
    ! Other ranks use datatype for sending the inner part of array
    else
       sizes(1) = nx_local+2
       sizes(2) = ny_local+2
    end if

    call mpi_type_create_subarray(2, sizes, subsizes, offsets, MPI_ORDER_C, &
         MPI_DOUBLE_PRECISION, parallel%subarraytype, ierr)
    call mpi_type_commit(parallel%subarraytype, ierr)

  end subroutine parallel_initialize

  ! Initialize the field to default values:
  !   zero temperature inside the region and
  !   5, 20, 45, 85 degrees on different boundaries
  subroutine initialize(field0, parallel)
    implicit none

    type(field), intent(inout) :: field0
    type(parallel_data), intent(in) :: parallel

    integer, dimension(2) :: dims, coords
    logical :: periods(2)
    integer :: ierr

    ! The arrays for field contain also a halo region
    allocate(field0%data(0:field0%nx+1, 0:field0%ny+1))

    field0%data(:,:) = 0.0_dp

    call mpi_cart_get(parallel%comm, 2, dims, periods, coords, ierr)

    ! Left boundary
    if (coords(2) == 0) then
       field0%data(:,0) = 30.0_dp
    end if

    ! Upper boundary
    if (coords(1) == 0) then
       field0%data(0,:) = 15.0_dp
    end if

    ! Right boundary
    if (coords(2) == dims(2) - 1) then
       field0%data(:,field0%ny+1) = -15.0_dp
    end if

    ! Lower boundary
    if (coords(1) == dims(1) - 1) then
       field0%data(field0%nx+1,:) = -25.0_dp
    end if

  end subroutine initialize

  ! Swap the data fields of two variables of type field
  ! Arguments:
  !   curr, prev (type(field)): the two variables that are swapped
  subroutine swap_fields(curr, prev)
    implicit none

    type(field), intent(inout) :: curr, prev
    real(kind=dp), allocatable, dimension(:,:) :: tmp

    call move_alloc(curr%data, tmp)
    call move_alloc(prev%data, curr%data)
    call move_alloc(tmp, prev%data)
  end subroutine swap_fields

  ! Copy the data from one field to another
  ! Arguments:
  !   from_field (type(field)): variable to copy from
  !   to_field (type(field)): variable to copy to
  subroutine copy_fields(from_field, to_field)
    implicit none

    type(field), intent(in) :: from_field
    type(field), intent(out) :: to_field

    ! Consistency checks
    if (.not.allocated(from_field%data)) then
       write (*,*) "Can not copy from a field without allocated data"
       stop
    end if
    if (.not.allocated(to_field%data)) then
       ! Target is not initialize, allocate memory
       allocate(to_field%data(lbound(from_field%data,1):ubound(from_field%data,1), &
            & lbound(from_field%data,2):ubound(from_field%data,2)))
    else if (any(shape(from_field%data) /= shape(to_field%data))) then
       write (*,*) "Wrong field data sizes in copy routine"
       print *, shape(from_field%data), shape(to_field%data)
       stop
    end if

    to_field%data = from_field%data

    to_field%nx = from_field%nx
    to_field%ny = from_field%ny
    to_field%nx_full = from_field%nx_full
    to_field%ny_full = from_field%ny_full
    to_field%dx = from_field%dx
    to_field%dy = from_field%dy
    to_field%dx2 = from_field%dx2
    to_field%dy2 = from_field%dy2
  end subroutine copy_fields

  ! Compute one time step of temperature evolution
  ! Arguments:
  !   curr (type(field)): current temperature values
  !   prev (type(field)): values from previous time step
  !   a (real(dp)): update equation constant
  !   dt (real(dp)): time step value
  subroutine evolve(curr, prev, a, dt)
    implicit none

    type(field), intent(inout) :: curr, prev
    real(kind=dp) :: a, dt
    integer :: nx, ny

    nx = curr%nx
    ny = curr%ny

!$omp parallel
!$omp workshare
    curr%data(1:nx, 1:ny) = prev%data(1:nx, 1:ny) + a * dt * &
         & ((prev%data(0:nx-1, 1:ny) - 2.0 * prev%data(1:nx, 1:ny) + &
         &   prev%data(2:nx+1, 1:ny)) / curr%dx2 + &
         &  (prev%data(1:nx, 0:ny-1) - 2.0 * prev%data(1:nx, 1:ny) + &
         &   prev%data(1:nx, 2:ny+1)) / curr%dy2)
!$omp end workshare
!$omp end parallel

  end subroutine evolve

  ! Exchange the boundary data between MPI tasks
  subroutine exchange(field0, parallel)
    implicit none

    type(field), intent(inout) :: field0
    type(parallel_data), intent(in) :: parallel

    integer :: ierr

    ! XXX It is really much more simple to use sendrecv to avoid deadlocks
    ! USE THEM!

    ! Send to left, receive from right
    call mpi_sendrecv(field0%data(0, 1), 1, parallel%columntype, &
         parallel%nleft, 11, &
         field0%data(0, field0%ny + 1), 1, parallel%columntype, &
         parallel%nright, 11, &
         parallel%comm, MPI_STATUS_IGNORE, ierr)

    ! Send to right, receive from left
    call mpi_sendrecv(field0%data(0, field0%ny), 1, parallel%columntype, &
         parallel%nright, 12, &
         field0%data(0, 0), 1, parallel%columntype, &
         parallel%nleft, 12, &
         parallel%comm, MPI_STATUS_IGNORE, ierr)
    
    ! Send to up receive from down
     call mpi_sendrecv(field0%data(1,0), 1, parallel%rowtype, &
          parallel%nup, 13, field0%data(field0%nx+1,0), 1, parallel%rowtype, &
          parallel%ndown, 13, parallel%comm, MPI_STATUS_IGNORE, ierr)

     ! Send to the down, receive from up
     call mpi_sendrecv(field0%data(field0%nx,0), 1, parallel%rowtype, &
          parallel%ndown, 14, field0%data(0,0), 1, parallel%rowtype, &
          parallel%nup, 14, parallel%comm, MPI_STATUS_IGNORE, ierr)

  end subroutine exchange

  ! Output routine, saves the temperature distribution as a png image
  ! Arguments:
  !   curr (type(field)): variable with the temperature data
  !   iter (integer): index of the time step
  subroutine output(curr, iter, parallel)
    use, intrinsic :: ISO_C_BINDING
    implicit none

    type(field), intent(in) :: curr
    integer, intent(in) :: iter
    type(parallel_data), intent(in) :: parallel

    character(len=85) :: filename
    ! Interface for save_png C-function
    interface
       ! The C-function definition is
       !   int save_png(double *data,
       !                const int nx, const int ny,
       !                const char *fname, const char lang)
       function save_png(data, nx, ny, fname, lang) &
            & bind(C,name="save_png") result(stat)
         use, intrinsic :: ISO_C_BINDING
         implicit none
         real(kind=C_DOUBLE) :: data(*)
         integer(kind=C_INT), value, intent(IN) :: nx, ny
         character(kind=C_CHAR), intent(IN) :: fname(*)
         character(kind=C_CHAR), value, intent(IN) :: lang
         integer(kind=C_INT) :: stat
       end function save_png
    end interface   

    ! The actual write routine takes only the actual data
    ! (without ghost layers) so we need array for that
    integer :: full_nx, full_ny, stat
    real(kind=dp), dimension(:,:), allocatable, target :: full_data

    integer :: coords(2)
    integer :: ix, jy
    integer :: p, ierr

    full_nx = curr%nx_full
    full_ny = curr%ny_full

    if (parallel%rank == 0) then
       allocate(full_data(full_nx, full_ny))

       ! Copy own inner data
       full_data(1:curr%nx, 1:curr%ny) = curr%data(1:curr%nx, 1:curr%ny)

       ! Receive data from other ranks
       do p = 1, parallel%size - 1
          call mpi_cart_coords(parallel%comm, p, 2, coords, ierr)
          ix = coords(1) * curr%nx + 1
          jy = coords(2) * curr%ny + 1
          call mpi_recv(full_data(ix, jy), 1, parallel%subarraytype, p, 22, &
               & parallel%comm, MPI_STATUS_IGNORE, ierr)
       end do
    else
       ! Send data
       call mpi_ssend(curr%data(1,1), 1, parallel%subarraytype, 0, 22, &
            & parallel%comm, ierr)
    end if

    if (parallel%rank == 0) then
       write(filename,'(A5,I5.5,A4,A)')  'heat_', iter, '.png'
       stat = save_png(full_data, full_nx, full_ny, &
            & trim(filename) // C_NULL_CHAR, 'F')
       deallocate(full_data)
    end if


  end subroutine output

  ! Clean up routine for field type
  ! Arguments:
  !   field0 (type(field)): field variable to be cleared
  subroutine finalize(field0)
    implicit none

    type(field), intent(inout) :: field0

    deallocate(field0%data)
  end subroutine finalize

  ! Reads the temperature distribution from an input file
  ! Arguments:
  !   field0 (type(field)): field variable that will store the
  !                         read data
  !   filename (char): name of the input file
  ! Note that this version assumes the input data to be in C memory layout
  subroutine read_input(field0, filename, parallel)
    implicit none

    type(field), intent(out) :: field0
    character(len=85), intent(in) :: filename
    type(parallel_data), intent(out) :: parallel

    integer :: nx, ny, i, p, ierr
    character(len=2) :: dummy

    real(kind=dp), dimension(:,:), allocatable :: full_data
    integer :: coords(2)
    integer :: ix, jy

    open(10, file=filename)
    ! Read the header
    read(10, *) dummy, nx, ny

    call parallel_initialize(parallel, nx, ny)
    call initialize_field_metadata(field0, nx, ny, parallel)

    ! The arrays for temperature field contain also a halo region
    allocate(field0%data(0:field0%nx+1, 0:field0%ny+1))

    if (parallel%rank == 0) then 
       allocate(full_data(nx, ny))
       ! Read the data
       do i = 1, nx
          read(10, *) full_data(i, 1:ny)
       end do
       ! Copy own local part
       field0%data(1:field0%nx, 1:field0%ny) = full_data(1:field0%nx, 1:field0%ny)
       ! Send to other process
       do p=1, parallel%size - 1 
          call mpi_cart_coords(parallel%comm, p, 2, coords, ierr)
          ix = coords(1) * field0%nx + 1
          jy = coords(2) * field0%ny + 1
          call mpi_send(full_data(ix, jy), 1, parallel%subarraytype, p, 44, &
               &   parallel%comm, ierr)
       end do
    else
       ! Receive data
       call mpi_recv(field0%data(1, 1), 1, parallel%subarraytype, 0, 44, &
            &        parallel%comm, MPI_STATUS_IGNORE, ierr)
    end if


    ! Set the boundary values
    field0%data(1:field0%nx,   0     ) = field0%data(1:field0%nx, 1     )
    field0%data(1:field0%nx,     field0%ny+1) = field0%data(1:field0%nx,   field0%ny  )
    field0%data(0,      0:field0%ny+1) = field0%data(1,    0:field0%ny+1)
    field0%data(  field0%nx+1, 0:field0%ny+1) = field0%data(  field0%nx, 0:field0%ny+1)

    close(10)
    if (parallel%rank == 0) then
       deallocate(full_data)
    end if

  end subroutine read_input

  subroutine parallel_finalize(parallel)
    implicit none

    type(parallel_data), intent(inout) :: parallel

  end subroutine parallel_finalize

end module heat_mpi
