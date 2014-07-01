program ex3b
  implicit none

  integer, parameter :: n=6
  integer :: numbers(n), facts(n)
  
  integer :: i

  numbers(1:n) = (/ (i, i=1,n) /)

  facts = factorial(numbers)
  write (*,*) 'Factorial with a loop:'
  write (*,*) numbers
  write (*,*) facts

  facts = recfactorial(numbers)
  write (*,*) 'Factorial with a recursion:'
  write (*,*) numbers
  write (*,*) facts

  call subfactorial(numbers, facts)
  write (*,*) 'Factorial with a subroutine'
  write (*,*) numbers
  write (*,*) facts

  contains

    function factorial(n) result(val)
      implicit none
      
      integer, intent(in) :: n(:)
      integer :: i, j
      integer :: val(size(n))

      val = 1
      do j=1,size(n)
         do i=1,n(j)
            val(j) = val(j) * i
         end do
      end do
    end function factorial
    
    recursive function recfactorial(n) result(val)
      implicit none
      
      integer, intent(in) :: n(:)
      integer :: i
      integer :: val(size(n))

      ! Warning! This is inefficient!
      where (n>1)
         val = n
      elsewhere
         val = 1
      end where
      if (any(n>1)) then
         val(:) = recfactorial(n-1)*val(:)
      end if
    end function recfactorial

    subroutine subfactorial(n, f)
      implicit none

      integer, intent(in) :: n(:)
      integer, intent(out) :: f(:)
      integer :: i, j

      f = 1
      do j=1,size(n)
         do i=1,n(j)
            f(j) = f(j) * i
         end do
      end do
    end subroutine subfactorial

end program ex3b
