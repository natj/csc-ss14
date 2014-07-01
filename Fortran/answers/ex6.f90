program ex6
  implicit none

  integer, parameter :: dp = selected_real_kind(12)
  type :: field
     integer :: nx
     integer :: ny
     real(kind=dp) :: dx
     real(kind=dp) :: dy
     real(kind=dp) :: dx2
     real(kind=dp) :: dy2
     real(kind=dp), dimension(:,:), allocatable :: data
  end type field

  type(field) :: fld

  call initialize_field_metadata(fld, 256, 256, real(0.01,dp), real(0.01,dp))
  write (*,'("nx=",I0,X,"ny=",I0)') fld % nx, fld % ny
  write (*,'("dx=",ES9.2,X,"dy=",ES9.2)') fld % dx, fld % dy
  write (*,'("dx2=",ES9.2,X,"dy2=",ES9.2)') fld % dx2, fld % dy2
  write (*,'("Data allocated? ",L)') allocated(fld % data)

  contains 

    subroutine initialize_field_metadata(field0, nx, ny, dx, dy)
      implicit none
      
      type(field), intent(out) :: field0
      integer, intent(in) :: nx, ny
      real(kind=dp), intent(in) :: dx, dy
      
      field0 % dx = dx
      field0 % dy = dy
      field0 % dx2 = dx**2
      field0 % dy2 = dy**2
      field0 % nx = nx
      field0 % ny = ny
      
      if (allocated(field0 % data)) deallocate(field0 % data)
    end subroutine initialize_field_metadata
end program ex6
