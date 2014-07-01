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

end program ex3_global_max
