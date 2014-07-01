!  2D heat equation
!  Parallel version using Fortran coarrays
!
!  Authors: Jussi Enkovaara, Mikko Byckling, Pekka Manninen
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

module heat
  implicit none

  integer, parameter :: dp = SELECTED_REAL_KIND(12)

  real(kind=dp), parameter :: DX = 0.01, DY = 0.01  ! Fixed grid spacing

  type :: field
     integer :: nx
     integer :: ny
     real(kind=dp) :: dx
     real(kind=dp) :: dy
     real(kind=dp) :: dx2
     real(kind=dp) :: dy2
     real(kind=dp), dimension(:,:), allocatable :: data
  end type field

contains

  ! Initialize the field type metadata
  ! Arguments:
  !   field0 (type(field)): input field
  !   nx, ny, dx, dy: field dimensions and spatial step size
  subroutine initialize_field_metadata(field0, nx, ny, dx, dy)
    implicit none

    type(field), intent(out) :: field0[*]
    integer, intent(in) :: nx, ny
    real(kind=dp), intent(in) :: dx, dy

    field0%dx = dx
    field0%dy = dy
    field0%dx2 = dx**2
    field0%dy2 = dy**2
    field0%nx = nx
    field0%ny = ny
  end subroutine initialize_field_metadata

  ! Initialize the field to default values:
  !   zero temperature inside the region and
  !   5, 20, 45, 85 degrees on different boundaries
  subroutine initialize(field0)
    implicit none

    type(field), intent(inout) :: field0[*]

    ! The arrays for field contain also a halo region
    allocate(field0%data(0:field0%nx+1, 0:field0%ny+1))

    field0%data(:,:) = 0.0_dp

    ! first column of the full field = first column
    ! of the first image
    if (this_image() == 1) then
       field0%data(:,0) = 85.0_dp
    end if
    ! last column of the full field = last column
    ! of the last image
    if (this_image() == num_images()) then
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

    type(field), intent(inout) :: curr[*], prev[*]
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

    type(field), intent(in) :: from_field[*]
    type(field), intent(out) :: to_field[*]

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
    to_field%dx = from_field%dx
    to_field%dy = from_field%dy
    to_field%dx2 = from_field%dx2
    to_field%dy2 = from_field%dy2
  end subroutine copy_fields

  subroutine exchange(from_field)
    implicit none
    type(field), intent(inout) :: from_field[*]
    integer, save :: lb_x[*], lb_y[*], ub_x[*], ub_y[*] ! upper and lower boundary indices of the local arrays
                                                  ! accessible by other images
    integer :: me

    me = this_image()
    lb_y = lbound(from_field%data,2)
    ub_y = ubound(from_field%data,2)

    sync all
    ! read the leftmost column of the image on the right to the right halo region
    if (me < num_images()) then ! the board is non-periodic, hence the last image does not read
       from_field%data(:,ub_y) = from_field[me+1]%data(:,lb_y[me+1]+1)
    end if
    ! read the rightmost column of the image on the left to the left halo region
    if (me > 1) then ! the board is non-periodic, hence the first image does not read
       from_field%data(:,lb_y) = from_field[me-1]%data(:,ub_y[me-1]-1)
    end if
  end subroutine exchange
    

  ! Compute one time step of temperature evolution
  ! Arguments:
  !   curr (type(field)): current temperature values
  !   prev (type(field)): values from previous time step
  !   a (real(dp)): update equation constant
  !   dt (real(dp)): time step value
  ! Unchanged in the coarrays implementation, each images update their local grid
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

  ! Output routine, saves the temperature distribution as a png image
  ! Arguments:
  !   curr (type(field)): variable with the temperature data
  !   iter (integer): index of the time step
  ! The coarrays implementation just writes one png file per coarray image
  subroutine output(curr, iter)
    use pngwriter
    implicit none

    type(field), intent(in) :: curr[*]
    integer, intent(in) :: iter

    character(len=85) :: filename

    ! The actual write routine takes only the actual data
    ! (without ghost layers) so we need array for that
    integer :: full_nx, full_ny, stat
    real(kind=dp), dimension(:,:), allocatable, target :: full_data

    full_nx = curr%nx
    full_ny = curr%ny

    allocate(full_data(full_nx, full_ny))
    full_data(1:curr%nx, 1:curr%ny) = curr%data(1:curr%nx, 1:curr%ny)

    write(filename,'(A5,I5.5,A1,I4.4,A4,A)')  'heat_', iter, '_', &
         this_image(), '.png'
    stat = save_png(full_data, full_nx, full_ny, filename)
    deallocate(full_data)
  end subroutine output


  ! Clean up routine for field type
  ! Arguments:
  !   field0 (type(field)): field variable to be cleared
  subroutine finalize(field0)
    implicit none

    type(field), intent(inout) :: field0[*]

    deallocate(field0%data)
  end subroutine finalize

  ! Reads the temperature distribution from an input file
  ! Arguments:
  !   field0 (type(field)): field variable that will store the
  !                         read data
  !   filename (char): name of the input file
  subroutine read_input(field0, filename)
    implicit none

    type(field), intent(out) :: field0[*]
    character(len=85), intent(in) :: filename
    integer, save :: nx[*], ny[*]
    integer :: i, me, ylow, yup, im
    character(len=2) :: dummy
    real(kind=dp), dimension(:,:), allocatable :: full_grid

    me = this_image()
    ! i/o only from the first image
    if (me == 1) then
       open(10, file=filename)
       ! Read the header
       read(10, *) dummy, nx, ny
       ! allocate the helper array
       allocate(full_grid(nx,ny))
       ! Read the data to the helper array
       do i = 1, nx
          read(10, *) full_grid(i, 1:ny)
       end do
       close(10)
    end if
    ! find the decomposition over coarrays (compare with the case of command-line input for nx and ny)
    sync all
    ny = ny[1]
    nx = nx[1]
    if (mod(ny,num_images()) < me) then
       ny = ny / num_images() + 1
    else
       ny = ny / num_images()
    end if

    call initialize_field_metadata(field0, nx, ny, DX, DY)
    ! The arrays for temperature field contain also a halo region
    allocate(field0%data(0:field0%nx+1, 0:field0%ny+1))

    ! distribute the data
    sync all
    if (me == 1) then
       ylow = 1
       do im = 1, num_images()
          yup = ylow + ny[im] - 1
          field0[im]%data(1:nx,1:ny[im]) = full_grid(1:nx,ylow:yup)
          ylow = yup + 1
       end do
    end if
    sync all

    ! Set the boundary values
    field0%data(1:nx,   0     ) = field0%data(1:nx, 1     )
    field0%data(1:nx,     ny+1) = field0%data(1:nx,   ny  )
    field0%data(0,      0:ny+1) = field0%data(1,    0:ny+1)
    field0%data(  nx+1, 0:ny+1) = field0%data(  nx, 0:ny+1)

  end subroutine read_input

end module heat
