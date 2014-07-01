program ex3a
  implicit none

  integer, parameter :: n=6
  integer :: numbers(n), facts(n)
  
  integer :: i

  numbers(1:n) = (/ (i, i=1,n) /)

  do i=1,n
     ! TODO: Call factorial funtion or subroutine 
     ! for all values in "numbers" and store the results
     ! in "facts"
  end do
  write (*,*) 'Factorial:'
  write (*,*) numbers
  write (*,*) facts

  contains 

    ! TODO: Implement function or subroutine to compute the 
    ! factorial of a given integer number

end program ex3a
