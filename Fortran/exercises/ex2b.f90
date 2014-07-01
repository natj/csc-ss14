program ex2b
  implicit none
  
  integer, parameter :: dp = selected_real_kind(12)
  integer, parameter :: m = 5
  integer, parameter :: n = 5

  real(kind=dp) :: array(m,n)
  
  array = 0.0 !default value
  array(1,:) = 30.0 !left
  array(m,:) = -10.0 !right
  array(:, 1) = 15.0 !top
  array(:, n) = -25.0 !bottom

  write (*,*) array
end program ex2b
