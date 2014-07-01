!  2D heat equation
!  Parallelization with Fortran coarrays
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

program heat_solve
  use heat
  implicit none

  real(kind=dp), parameter :: a = 0.5 ! Diffusion constant
  type(field) :: current[*], previous[*]    ! Current and previus temperature fields
                                            ! declared as coarrays

  real(kind=dp) :: dt     ! Time step
  integer :: nsteps[*]       ! Number of time steps
  integer, parameter :: image_interval = 10 ! Image output interval

  integer :: nx[*], ny[*]  ! Field dimensions

  character(len=85) :: input_file[*], arg  ! Input file name and command line arguments

  integer :: iter
  integer :: me

  logical :: using_input_file[*]

  ! Default values for grid size and time steps
  nx = 100
  ny = 100
  nsteps = 500
  using_input_file = .false.

  me = this_image()
  ! Read in the command line arguments and
  ! set up the needed variables
  ! we will need to do this from Image 1
  if (me == 1) then
     select case(command_argument_count())
     case(0) ! No arguments -> default values
     case(1) ! One argument -> input file name
        using_input_file = .true.
        call get_command_argument(1, input_file)
     case(2) ! Two arguments -> input file name and number of steps
        using_input_file = .true.
        call get_command_argument(1, input_file)
        call get_command_argument(2, arg)
        read(arg, *) nsteps
     case(3) ! Three arguments -> nx, ny and nsteps
        call get_command_argument(1, arg)
        read(arg, *) nx
        call get_command_argument(2, arg)
        read(arg, *) ny
        call get_command_argument(3, arg)
        read(arg, *) nsteps
     case default
        call usage()
        stop
     end select
  end if
  sync all
  using_input_file = using_input_file[1]
  nsteps = nsteps[1]
  nx = nx[1]
  ny = ny[1]
! divide the grid in y-direction (to have contiguous memory blocks)
! give all images ny (user input) / #images columns
! possible remainder r is divided to r first images, one column each
  if (mod(ny,num_images()) < me) then
     ny = ny / num_images() + 1
  else
     ny = ny / num_images()
  end if

  ! Initialize the fields according the command line arguments
  if (using_input_file) then
!     call get_command_argument(1, input_file)
     call read_input(previous, input_file)
     call copy_fields(previous, current)
  else
     call initialize_field_metadata(previous, nx, ny, DX, DY)
     call initialize_field_metadata(current, nx, ny, DX, DY)
     call initialize(previous)
     call initialize(current)
  end if

  ! Draw the picture of the initial state
  call output(current, 0)

  ! Largest stable time step
  dt = current%dx2 * current%dy2 / &
       & (2.0 * a * (current%dx2 + current%dy2))

  ! Main iteration loop, save a picture every
  ! image_interval steps
  do iter = 1, nsteps
     call exchange(previous)
     call evolve(current, previous, a, dt)
     if (mod(iter, image_interval) == 0) then
        call output(current, iter)
     end if
     call swap_fields(current, previous)
  end do

  call finalize(current)
  call finalize(previous)

contains

  ! Helper routine that prints out a simple usage if
  ! user gives more than three arguments
  subroutine usage()
    implicit none
    character(len=256) :: buf

    call get_command_argument(0, buf)
    write (*,'(A)') 'Usage:'
    write (*,'(A, " (default values will be used)")') trim(buf)
    write (*,'(A, " <filename>")') trim(buf)
    write (*,'(A, " <filename> <nsteps>")') trim(buf)
    write (*,'(A, " <nx> <ny> <nsteps>")') trim(buf)
  end subroutine usage

end program
