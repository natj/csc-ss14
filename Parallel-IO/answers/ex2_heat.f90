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

    nx_local = nx
    ny_local = ny / parallel%size

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
    integer, intent(in), optional :: nx, ny

    integer :: ny_local
    integer :: ierr

    parallel%comm = MPI_COMM_WORLD
    call mpi_comm_size(parallel%comm, parallel%size, ierr)

    if (present(ny)) then
       ny_local = ny / parallel%size
       if (ny_local * parallel%size /= ny) then
          write(*,*) 'Cannot divide grid evenly to processors'
          call mpi_abort(parallel%comm, -2, ierr)
       end if
    end if
       
    call mpi_comm_rank(parallel%comm, parallel%rank, ierr)

    parallel%nleft = parallel%rank - 1
    parallel%nright = parallel%rank + 1

    if (parallel%nleft < 0) then
       parallel%nleft = MPI_PROC_NULL
    end if
    if (parallel%nright > parallel%size - 1) then
       parallel%nright = MPI_PROC_NULL
    end if

  end subroutine parallel_initialize

  ! Initialize the field to default values:
  !   zero temperature inside the region and
  !   5, 20, 45, 85 degrees on different boundaries
  subroutine initialize(field0, parallel)
    implicit none

    type(field), intent(inout) :: field0
    type(parallel_data), intent(in) :: parallel

    ! The arrays for field contain also a halo region
    allocate(field0%data(0:field0%nx+1, 0:field0%ny+1))

    field0%data(:,:) = 0.0_dp

    if (parallel % rank == 0) then
       field0%data(:,0) = 85.0_dp
    else if (parallel % rank == parallel%size - 1) then
       field0%data(:,field0%ny+1) = 45.0_dp
    end if
    field0%data(0,:) = 5.0_dp
    field0%data(field0%nx+1,:) = 20.0_dp
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

    curr%data(1:nx, 1:ny) = prev%data(1:nx, 1:ny) + a * dt * &
         & ((prev%data(0:nx-1, 1:ny) - 2.0 * prev%data(1:nx, 1:ny) + &
         &   prev%data(2:nx+1, 1:ny)) / curr%dx2 + &
         &  (prev%data(1:nx, 0:ny-1) - 2.0 * prev%data(1:nx, 1:ny) + &
         &   prev%data(1:nx, 2:ny+1)) / curr%dy2)
  end subroutine evolve

  ! Exchange the boundary data between MPI tasks
  subroutine exchange(field0, parallel)
    implicit none

    type(field), intent(inout) :: field0
    type(parallel_data), intent(in) :: parallel

    integer :: ierr

    ! Send to left, receive from right
    call mpi_sendrecv(field0%data(:, 1), field0%nx + 2, MPI_DOUBLE_PRECISION, &
                      parallel%nleft, 11, &
                      field0%data(:, field0%ny + 1), field0%nx + 2, MPI_DOUBLE_PRECISION, &
                      parallel%nright, 11, &
                      parallel%comm, MPI_STATUS_IGNORE, ierr)

    ! Send to right, receive from left
    call mpi_sendrecv(field0%data(:, field0%ny), field0%nx + 2, MPI_DOUBLE_PRECISION, &
                      parallel%nright, 12, &
                      field0%data(:, 0), field0%nx + 2, MPI_DOUBLE_PRECISION, &
                      parallel%nleft, 12, &
                      parallel%comm, MPI_STATUS_IGNORE, ierr)

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

    integer :: stat
    real(kind=dp), dimension(:,:), allocatable, target :: full_data
    integer :: p, ierr

    if (parallel%rank == 0) then
       allocate(full_data(curr%nx_full, curr%ny_full))
       ! Copy rand #0 data to the global array
       full_data(1:curr%nx, 1:curr%ny) = curr%data(1:curr%nx, 1:curr%ny)
       
       ! Receive data from other ranks
       do p = 1, parallel%size - 1
          call mpi_recv(full_data(1:curr%nx, p*curr%ny + 1:(p + 1) * curr%ny), &
               curr%nx * curr%ny, MPI_DOUBLE_PRECISION, p, 22, &
               parallel%comm, MPI_STATUS_IGNORE, ierr)
       end do
    else
       ! Send data
       call mpi_send(curr%data(1:curr%nx, 1:curr%ny), curr%nx * curr%ny, MPI_DOUBLE_PRECISION, 0, 22, &
            parallel%comm, ierr)
    end if

    if (parallel%rank == 0) then
       write(filename,'(A5,I5.5,A4,A)')  'heat_', iter, '.png'
       stat = save_png(full_data, curr%nx_full, curr%ny_full, &
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

    integer :: nx, ny, i, ierr
    character(len=2) :: dummy

    real(kind=dp), dimension(:,:), allocatable :: full_data, inner_data

    open(10, file=filename)
    ! Read the header
    read(10, *) dummy, nx, ny

    call parallel_initialize(parallel, nx, ny)
    call initialize_field_metadata(field0, nx, ny, parallel)

    ! The arrays for temperature field contain also a halo region
    allocate(field0%data(0:field0%nx+1, 0:field0%ny+1))

    allocate(inner_data(field0%nx, field0%ny))

    if (parallel%rank == 0) then 
       allocate(full_data(nx, ny))
       ! Read the data
       do i = 1, nx
          read(10, *) full_data(i, 1:ny)
       end do
    end if

    call mpi_scatter(full_data, nx * field0%ny, MPI_DOUBLE_PRECISION, inner_data, &
                     nx * field0%ny, MPI_DOUBLE_PRECISION, 0, parallel%comm, ierr)
    ! Copy to full array containing also boundaries
    field0%data(1:field0%nx, 1:field0%ny) = inner_data(:,:) 

    ! Set the boundary values
    field0%data(1:field0%nx,   0     ) = field0%data(1:field0%nx, 1     )
    field0%data(1:field0%nx,     field0%ny+1) = field0%data(1:field0%nx,   field0%ny  )
    field0%data(0,      0:field0%ny+1) = field0%data(1,    0:field0%ny+1)
    field0%data(  field0%nx+1, 0:field0%ny+1) = field0%data(  field0%nx, 0:field0%ny+1)

    close(10)
    deallocate(inner_data)
    if (parallel%rank == 0) then
       deallocate(full_data)
    end if

  end subroutine read_input

  subroutine parallel_finalize(parallel)
    implicit none

    type(parallel_data), intent(inout) :: parallel

  end subroutine parallel_finalize

  subroutine read_restart(temp, temp0, parallel, step0)
    implicit none

    type(field), intent(inout) :: temp, temp0
    type(parallel_data), intent(inout) :: parallel
    integer, intent(out) :: step0

    integer :: full_nx, full_ny
    integer :: fp
    integer(kind=MPI_OFFSET_KIND) :: disp
    integer :: ierr

    call mpi_file_open(parallel%comm, "HEAT_RESTART.dat", MPI_MODE_RDONLY, &
                MPI_INFO_NULL, fp, ierr)

    ! read in the restart header (dimensions, number of preceding steps) by the
    ! rank#0 and broadcast it to other ranks
    if (parallel%rank == 0) then
       disp = 0
       call mpi_file_read_at(fp, disp, full_nx, 1, MPI_INTEGER, MPI_STATUS_IGNORE, ierr)
       disp = sizeof(full_nx)
       call mpi_file_read_at(fp, disp, full_ny, 1, MPI_INTEGER, MPI_STATUS_IGNORE, ierr)
       if (mod(full_ny, parallel%size) /= 0) then
          write (*,*) 'Unable to divide the grid with the number of processes'
          call mpi_abort(parallel%comm, -1, ierr)
       end if
       disp = disp + sizeof(step0)
       call mpi_file_read_at(fp, disp, step0, 1, MPI_INTEGER, MPI_STATUS_IGNORE, ierr)
    end if
    call mpi_bcast(full_nx, 1, MPI_INTEGER, 0, parallel%comm, ierr)
    call mpi_bcast(full_ny, 1, MPI_INTEGER, 0, parallel%comm, ierr)
    call mpi_bcast(step0, 1, MPI_INTEGER, 0, parallel%comm, ierr)

 
    call initialize_field_metadata(temp, full_nx, full_ny, parallel)
    call initialize_field_metadata(temp0, full_nx, full_ny, parallel)

    allocate(temp%data(0:temp%nx+1, 0:temp%ny+1))
    allocate(temp0%data(0:temp%nx+1, 0:temp%ny+1))

    temp%data(:,:) = 0.0
    temp0%data(:,:) = 0.0

    ! read in restart data
    ! Skip three integers in the beginning of file
    disp = 3*sizeof(full_nx)
    disp = disp + parallel%rank * temp0%nx * temp0%ny * sizeof(temp0%data(1,1))
    call mpi_file_read_at_all(fp, disp, temp0%data(1:temp0%nx,1:temp0%ny), temp0%nx*temp0%ny, &
                         MPI_DOUBLE_PRECISION, MPI_STATUS_IGNORE, ierr)
    call mpi_file_close(fp, ierr)

  end subroutine read_restart

  subroutine write_restart(temp, parallel, iter)
    implicit none

    type(field), intent(in) :: temp
    type(parallel_data), intent(in) :: parallel
    integer, intent(in) :: iter

    integer :: fp
    integer(kind=MPI_OFFSET_KIND) :: disp
    integer :: ierr

    call mpi_file_open(parallel%comm, "HEAT_RESTART.dat",  &
                       MPI_MODE_CREATE + MPI_MODE_WRONLY, &
                       MPI_INFO_NULL, fp, ierr)

    ! write the restart header: grid size and the number of preceding time steps by the rank#0
    if (parallel%rank == 0) then
       disp = 0
       call mpi_file_write_at(fp, disp, temp%nx_full, 1, MPI_INTEGER, MPI_STATUS_IGNORE, ierr)
       disp = sizeof(temp%nx_full)
       call mpi_file_write_at(fp, disp, temp%ny_full, 1, MPI_INTEGER, MPI_STATUS_IGNORE, ierr)
       disp = disp + sizeof(iter)
       call mpi_file_write_at(fp, disp, iter, 1, MPI_INTEGER, MPI_STATUS_IGNORE, ierr)
    end if

    ! For simplicity, skip three integers at the beginning of file in all tasks
    disp = 3*sizeof(temp%nx_full)
    ! find the writing location
    disp = disp + parallel%rank * temp%ny * temp%nx * sizeof(temp%data(1,1))
    ! everyone writes their part of the grid
    call mpi_file_write_at_all(fp, disp, temp%data(1:temp%nx,1:temp%ny), temp%nx*temp%ny, &
         MPI_DOUBLE_PRECISION, MPI_STATUS_IGNORE, ierr)
    call mpi_file_close(fp, ierr)


  end subroutine write_restart

end module heat_mpi
