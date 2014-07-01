program ex7
  implicit none

  integer, parameter :: dp = selected_real_kind(12)
  integer, parameter :: nx = 256, ny = 256
  real(kind=dp), parameter :: dx = real(0.01,dp), dy=real(0.01,dp)

    ! TODO: Declare field-type as in exercise 6

  type(field) :: prev, curr

  call initialize_field_metadata(prev, nx, ny, dx, dy)
  call initialize_field(prev)

  call initialize_field_metadata(curr, nx, ny, dx, dy)
  call initialize_field(curr)

  call apply_fd_laplacian(curr, prev)
  call output(curr)

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

    subroutine initialize_field(fld)
      implicit none
      
      type(field) :: fld
      integer :: allocstat
      
      ! TODO: Implement type initialization as in exercise 2
    end subroutine initialize_field

    subroutine apply_fd_laplacian(curr, prev)
      implicit none
      type(field), intent(in) :: prev
      type(field) :: curr
      
      ! TODO: Implement as in exercise 5
    end subroutine apply_fd_laplacian

    subroutine output(fld)
      use, intrinsic :: ISO_C_BINDING
      implicit none
      
      type(field), intent(in) :: fld
      integer :: stat
      
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
      
      stat = save_png(fld%data(1:fld%nx, 1:fld%ny), fld%nx, fld%ny, &
           & 'ex7.png' // C_NULL_CHAR, 'F')
      if (stat == 0) write(*,*) 'Output written to ex7.png'
    end subroutine output

end program ex7
