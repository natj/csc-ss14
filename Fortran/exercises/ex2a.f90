program ex2a
  implicit none
  
  integer, parameter :: dp = selected_real_kind(12)
  !TODO: Declare m and n to have m-by-n array

  real(kind=dp) :: array(m,n)
  integer :: i
  
  !TODO: Initialize array

  write (*,*) array
end program ex2a
