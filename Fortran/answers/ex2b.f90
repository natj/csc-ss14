program ex2b
  implicit none
  
  integer, parameter :: dp = selected_real_kind(12)
  integer, parameter :: m=258, n=258

  real(kind=dp) :: array(m,n)
  
  array = real(0,dp)
  ! Left 
  array(:,1)=real(30,dp)
  ! Right
  array(:,n)=real(-10,dp)
  ! Upper
  array(1,:)=real(15,dp)
  ! Lower 
  array(m,:)=real(-25,dp)

  write (*,*) array
end program ex2b
