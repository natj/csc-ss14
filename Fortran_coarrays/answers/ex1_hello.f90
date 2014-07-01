program hello2
  integer :: me, i
  me = this_image()
  do i = 1, num_images()
     if (i == me) write(*,*) 'Hello! From Image', me
     sync all
  end do
end program hello2
