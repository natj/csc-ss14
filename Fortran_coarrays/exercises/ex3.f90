program ex3_global_max
  implicit none
  integer, parameter :: n_rand = 1000
  real, dimension(n_rand) :: x
  real :: local_max[*], global_max
  integer :: me, im, im_maxval, seed(2)

  me = this_image()
  seed = me
  call random_seed(put=seed)
  call random_number(x)
  local_max = maxval(x)
  
  ! now add the search for the global maximum
  ! across the images
  sync all

  global_max = 0.0
  if(me == 1) then
     do im=1,num_images()
        write(*,*) 'local max from ',im, ' is ', local_max[im]
        if(local_max[im] > global_max) then
           global_max = local_max[im]
        end if
     end do

     write(*,*) 'global max=', global_max
  end if

end program ex3_global_max
