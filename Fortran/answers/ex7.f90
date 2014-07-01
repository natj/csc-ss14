program ex7
  implicit none

  integer, parameter :: dp = selected_real_kind(12)
  integer, parameter :: nx = 256, ny = 256
  real(kind=dp), parameter :: dx = real(0.01,dp), dy=real(0.01,dp)

  type :: field
     integer :: nx
     integer :: ny
     real(kind=dp) :: dx
     real(kind=dp) :: dy
     real(kind=dp) :: dx2
     real(kind=dp) :: dy2
     real(kind=dp), dimension(:,:), allocatable :: data
  end type field

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

      allocate(fld % data(0:fld % nx+1,0:fld % ny+1), stat=allocstat)
      if (allocstat /= 0) then
         write (*,*) 'Error, memory allocation failed!'
         stop
      end if

      fld % data = real(0,dp)
      ! Left 
      fld % data(:,lbound(fld % data, 2))=real(30,dp)
      ! Right
      fld % data(:,ubound(fld % data, 2))=real(-10,dp)
      ! Upper
      fld % data(lbound(fld % data, 1),:)=real(15,dp)
      ! Lower 
      fld % data(ubound(fld % data, 1),:)=real(-25,dp)
    end subroutine initialize_field

    subroutine apply_fd_laplacian(curr, prev)
      implicit none
      type(field), intent(in) :: prev
      type(field) :: curr
      
      integer :: i, j, lny, uny, lnx, unx
      
      lny = lbound(prev % data,2)
      uny = ubound(prev % data,2)
      lnx = lbound(prev % data,1)
      unx = ubound(prev % data,1)

      do j=lny+1,uny-1
         do i=lnx+1,unx-1
            curr % data(i,j)=(prev % data(i-1,j) &
                             -2*prev % data(i,j) &
                             +prev % data(i+1,j))/prev % dx2+ &
                             (prev % data(i,j-1) &
                             -2*prev % data(i,j) &
                             +prev % data(i,j+1))/prev % dy2
         end do
      end do
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
