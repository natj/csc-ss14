program ex3a
  implicit none

  integer, parameter :: n=6
  integer :: numbers(n), facts(n)
  integer :: efacts  

  integer :: i

  ! loop
  do i=1,n
     facts(i) = fact(i)
  end do

  ! element-wise
  numbers(1:n) = (/ (i, i=1,n) /)
  efacts = efact(numbers)

  write (*,*) 'Factorial:'
  write (*,*) numbers
  write (*,*) facts
  write (*,*) efacts

  contains 
    integer function fact(n)
      implicit none
      integer, intent(in) :: n

      integer :: i

      fact = 1
      do i = 1,n
         fact = fact*i
      end do

    end function fact

    integer function efact(numbers)
      implicit none
      integer, intent(in) :: numbers(:)
!      integer, intent(out) :: efact

      integer :: i, n

      n = size(numbers)
      efact = 1
      do i = 1,n
         efact = efact*numbers(i)
      end do

    end function efact


end program ex3a
