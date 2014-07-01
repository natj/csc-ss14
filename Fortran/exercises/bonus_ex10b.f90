program bonus_ex10b
  implicit none

  character(len=*), parameter :: fname='inputfile.in'
  integer, parameter :: funit = 10

  character(len=80) :: line
  integer :: readstat, x, y, z
  character(len=50) :: chardata

  ! Open file for reading
  open(funit, file=fname, status='old', action='read')
  ! Read and print out lines
  do 
     read (funit, '(A)', iostat=readstat) line
     if (readstat /= 0) exit ! End of file!
     if (.not. iscommentline(line)) then
        write (*,*) trim(line)
     end if
  end do
  ! Close file
  close(funit)
  
  contains 

    function iscommentline(str) result(val)
      implicit none
      character(len=*), intent(in) :: str
      logical :: val
      integer :: i

      ! TODO: Implement comment line detection
    end function iscommentline

end program bonus_ex10b
