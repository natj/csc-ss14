program ex4
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

  type(field) :: fld

  call initialize_field_metadata(fld, nx, ny, dx, dy)
  call initialize_field(fld)
  call output(fld)

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
    integer, parameter :: boxsize = 20
    integer :: allocstat
    
    allocate(fld % data(0:fld % nx+1,0:fld % ny+1), stat=allocstat)
    if (allocstat /= 0) then
       write (*,*) 'Error, memory allocation failed!'
       stop
    end if
    
    fld % data = real(0,dp)
    ! Write a square red box inside the region
    fld % data((fld % nx/2)-boxsize:(fld % nx/2)+boxsize, &
               (fld % ny/2)-boxsize:(fld % ny/2)+boxsize) = real(256, dp)
  end subroutine initialize_field
 
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
         & 'ex4.png' // C_NULL_CHAR, 'F')
    if (stat == 0) write(*,*) 'Output written to ex4.png'
  end subroutine output

end program ex4
