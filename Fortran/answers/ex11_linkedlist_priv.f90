program bonus_ex11b
  use linkedlistmod_priv
  implicit none
  
  integer, parameter :: n = 10

  integer :: i, value
  type(linkedlist_t) :: list
  type(listiterator_t) :: iter
  logical :: found

  ! Add keys from 1 to n with values from 100 to 100+n to list
  write (*,*) 'INSERT'
  do i=1,n
     call insert(list, i, 100+i)
  end do
  
  ! Iterate through list and output values (in reverse order!)
  iter = getiterator(list)
  do while (hasnext(iter))
     write (*,'("(",I0,",",I0,")",X)',ADVANCE='NO') nextkey(iter), next(iter)
  end do
  write (*,*)

  ! Delete every other value (even keys)
  write (*,*) 'DELETE'
  do i=10,1,-2
     call delete(list, i)
  end do
  
  ! Iterate through list and output values (in reverse order!)
  iter = getiterator(list)
  do while (hasnext(iter))
     write (*,'("(",I0,",",I0,")",X)',ADVANCE='NO') nextkey(iter), next(iter)
  end do
  write (*,*)

  ! Search values
  write (*,*) 'SEARCH'
  do i=10,1,-1
      found = search(list, i, value)
      if (found) then
         write (*,'("key=",I0,", value=",I0)') i, value
      else
         write (*,'("key=",I0,", not found")') i
      end if
  end do

  ! Cleanup
  do while (getsize(list)>0)
     call deletefirst(list)
  end do

end program bonus_ex11b
