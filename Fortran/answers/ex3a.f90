program ex3a
  implicit none

  integer, parameter :: n=6
  integer :: numbers(n), facts(n)
  
  integer :: i

  numbers(1:n) = (/ (i, i=1,n) /)

  do i=1,n
     facts(i) = factorial(numbers(i))
  end do
  write (*,*) 'Factorial with a loop:'
  write (*,*) numbers
  write (*,*) facts

  do i=1,n
     facts(i) = recfactorial(numbers(i))
  end do
  write (*,*) 'Factorial with a recursion'
  write (*,*) numbers
  write (*,*) facts

  do i=1,n
     call subfactorial(numbers(i), facts(i))
  end do
  write (*,*) 'Factorial with a subroutine'
  write (*,*) numbers
  write (*,*) facts

  contains 

    function factorial(n) result(val)
      implicit none

      integer, intent(in) :: n
      integer :: i
      integer :: val

      val = 1
      do i=1,n
         val = val * i
      end do
    end function factorial

    recursive function recfactorial(n) result(val)
      implicit none

      integer, intent(in) :: n
      integer :: val

      if (n==1) then
         val = 1
      else
         val = n*recfactorial(n-1)
      end if
    end function recfactorial

    subroutine subfactorial(n, f)
      implicit none

      integer, intent(in) :: n
      integer, intent(out) :: f
      integer :: i, j

      f = 1
      do i=1,n
         f = f * i
      end do
    end subroutine subfactorial


end program ex3a
