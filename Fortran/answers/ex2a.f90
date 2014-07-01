program ex2a
  implicit none
  
  integer, parameter :: dp = selected_real_kind(12)
  integer, parameter :: m=258, n=258

  real(kind=dp) :: array(m,n)
  integer :: i
  
  array = real(0,dp)
  ! Left 
  do i=1,m
     array(i,1)=real(30,dp)
  end do
  ! Right
  do i=1,m
     array(i,n)=real(-10,dp)
  end do
  ! Upper
  do i=1,n
     array(1,i)=real(15,dp)
  end do
  ! Lower 
  do i=1,n
     array(m,i)=real(-25,dp)
  end do

  write (*,*) array
end program ex2a
