program bonus_ex11a
  use linkedlistmod
  implicit none
  
  integer, parameter :: n = 10

  integer :: i, value
  type(linkedlist_t) :: list
  logical :: found

  ! Add keys from 1 to n with values from 100 to 100+n to list
  write (*,*) 'INSERT'
  do i=1,n
     call insert(list, i, 100+i)
  end do
  
  ! Iterate through list and output values (in reverse order!)
  do while (.not. isempty(list))
     write (*,'("(",I0,",",I0,")",X)',ADVANCE='NO') list % first % key, & 
          list % first % value
     call deletefirst(list)
  end do
  write (*,*)

end program bonus_ex11a
