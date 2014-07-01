program ex2a
  implicit none
  
  integer, parameter :: dp = selected_real_kind(12)
  integer, parameter :: m = 5
  integer, parameter :: n = 5

  real(kind=dp) :: array(m,n)
  integer :: i, j

  logical :: left, right, top, bottom

  !Initialize arrays
  do i = 1,m
     do j = 1,n

        ! define logicals
        left = (i == 1)
        right = (i == m)
        top = (i > 1 .AND. i < m .AND. j == 1)
        bottom = (i > 1 .AND. i < m .AND. j == n)

        if (left) then
           array(i, j) = 30.0
        else if (right) then
           array(i, j) = -10.0
        else if (top) then
           array(i,j) = 15.0
        else if (bottom) then
           array(i,j) = -25.0
        else !default value for inner cells
           array(i,j) = 0.0
        end if
     end do
  end do

  write (*,*) array
end program ex2a
