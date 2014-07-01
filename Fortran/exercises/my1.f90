program square_root_example

  IMPLICIT NONE
  REAL :: x, y
  intrinsic sqrt

  write (*,*) 'Give value for x:'
  read (*,*) x

  y = x**2+1

  write (*,*) 'given value x:', x
  write (*,*) 'computed value of x**2 +1:', y

  write (*,*) 'computed value of sqrt(x**2 + 1):', sqrt(y)

end program
