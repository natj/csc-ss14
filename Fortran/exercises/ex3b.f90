program ex3b
  implicit none

  integer, parameter :: n=6
  integer :: numbers(n), facts(n)
  
  integer :: i

  numbers(1:n) = (/ (i, i=1,n) /)

  ! TODO: Call factorial funtion or subroutine 
  ! for the array "numbers" and store the results
  ! in the array "facts"
  write (*,*) 'Factorial:'
  write (*,*) numbers
  write (*,*) facts

  contains
    
    ! TODO: Implement function or subroutine to compute the 
    ! factorial of a given array of integer numbers

end program ex3b
