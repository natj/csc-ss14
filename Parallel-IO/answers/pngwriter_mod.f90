module pngwriter
  implicit none
  integer, parameter :: dp = SELECTED_REAL_KIND(12)

contains
  function save_png(data, nx, ny, fname) result(stat)

    use, intrinsic :: ISO_C_BINDING
    implicit none

    real(kind=dp), dimension(:,:), intent(in) :: data
    integer, intent(in) :: nx, ny
    character(len=85), intent(in) :: fname
    integer :: stat

    ! Interface for save_png C-function
    interface
       ! The C-function definition is
       !   int save_png(double *data,
       !                const int nx, const int ny,
       !                const char *fname)
       function save_png_c(data, nx, ny, fname) &
            & bind(C,name="save_png") result(stat)
         use, intrinsic :: ISO_C_BINDING
         implicit none
         real(kind=C_DOUBLE) :: data(*)
         integer(kind=C_INT), value, intent(IN) :: nx, ny
         character(kind=C_CHAR), intent(IN) :: fname(*)
         integer(kind=C_INT) :: stat
       end function save_png_c
    end interface   

    real(kind=dp), dimension(:,:), allocatable, target :: tmp_data

    allocate(tmp_data(ny, nx))
    tmp_data(1:ny, 1:nx) = transpose(data)
    stat = save_png_c(tmp_data, nx, ny, TRIM(fname) // C_NULL_CHAR)
    deallocate(tmp_data)

  end function save_png

end module pngwriter
