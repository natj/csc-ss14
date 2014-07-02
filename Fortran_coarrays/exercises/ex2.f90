program ex2
  implicit none

  integer :: global[*], me

  me = this_image()

  if(me == 1) then
     write(*,*) 'Give value:'
     read(*,*) global
  end if
  sync all

  ! copy from image 1
  global = global[1]

  write(*,*) 'user gave =', global, ' in ', me

end program
