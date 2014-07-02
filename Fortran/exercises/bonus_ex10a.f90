program bonus_ex10a
  implicit none

  character(len=*), parameter :: fname='inputfile.in'
  integer, parameter :: funit = 10

  character(len=80) :: line
  integer :: readstat, x, y, z
  character(len=50) :: chardata

  ! TODO: Implement file reading and output file line by line
  open(funit, file=fname, status='old', iostat=readstat)

  do 
     ! terminate if the file ends
     if (readstat /= 0) then
        exit
     end if
     ! read line if not comment
     read(funit, '(A)', iostat=readstat), line
     if(.not. iscommentline(line)) then
        
        !write(*,*) chardata
        read(line, *) x, y, z, chardata

        write(*,*) 'x=',x, 'y=',y, 'z=',z, 'string=',chardata

     end if

  end do

  close(funit)

  contains
    function iscommentline(str) result(val)
      implicit none
      character(len=*), intent(in) :: str
      logical :: val
      integer :: i

      val = .false.
      do i = 1,len(str)
         if(str(i:i) == '#') then
            val=.true.
            exit
         elseif(str(i:i) /= ' ') then
            exit
         end if
      end do


    end function iscommentline

end program bonus_ex10a
