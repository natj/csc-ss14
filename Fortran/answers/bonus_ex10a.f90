program bonus_ex10a
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
     write (*,*) trim(line)
  end do
  ! Close file
  close(funit)
end program bonus_ex10a
