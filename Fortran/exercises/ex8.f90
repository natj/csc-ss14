program ex8
  implicit none
  
  integer, parameter :: dp = selected_real_kind(12)
  real(kind=dp), parameter :: DX = real(0.01,dp), DY = real(0.01,dp)

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

  call read_input(fld, 'bottle.dat')
  call output(fld)

contains
    
  subroutine read_input(field0, filename)
    implicit none

    type(field), intent(out) :: field0
    character(len=*), intent(in) :: filename

    integer, parameter :: funit = 10
    integer :: nx, ny, i
    character(len=2) :: dummy

    ! reading of data from a file with a given name
    open(funit, file=filename, status='old')

    ! read column and row sizes
    read(funit, *), dummy, nx, ny
    write(*,*) 'nx=', nx, 'ny=',ny

    ! initialize field & remember ghost cells
    call initialize_field_metadata(field0, nx, ny)
    allocate(field0%data(0:nx+1, 0:ny+1))
    field0%data = 0.0

    ! read line by line into data
    do i = 1,nx
       !write(*,*) 'i=',i
       read(funit, *) field0%data(i, 1:ny)
       !write(*,*) ' '
       !write(*,*) field0%data(i, 1:ny)
    enddo
    close(funit)

    ! set boundaries
    field0%data(0, 1:ny) = field0%data(1, 1:ny) 
    field0%data(nx+1, 1:ny) = field0%data(nx, 1:ny) 
    field0%data(1:nx, 0) = field0%data(1:nx, 1)
    field0%data(1:nx, ny+1) = field0%data(1:nx, ny)

  end subroutine read_input
  
  subroutine initialize_field_metadata(field0, nx, ny)
    implicit none
    
    type(field), intent(out) :: field0
    integer, intent(in) :: nx, ny
    
    field0 % dx = DX
    field0 % dy = DY
    field0 % dx2 = DX**2
    field0 % dy2 = DY**2
    field0 % nx = nx
    field0 % ny = ny
    
    if (allocated(field0 % data)) deallocate(field0 % data)
  end subroutine initialize_field_metadata
 
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
         & 'ex8.png' // C_NULL_CHAR, 'F')
    if (stat == 0) write(*,*) 'Output written to ex8.png'
  end subroutine output

end program ex8
