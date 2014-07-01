program ex5a
  implicit none
  
  integer, parameter :: dp = selected_real_kind(12)
  integer, parameter :: m=258, n=258
  real(kind=dp), parameter :: dx=real(0.01,dp), dy=real(0.01,dp)

  ! TODO: Declare arrays
  integer :: i
  
  y=real(0,dp)

  call set_initial_values(x)
  call set_initial_values(y)
  call apply_fd_laplacian(x, y, dx, dy)
  
  if (check_result(x, y, dx, dy)) then
     write (*,*) 'The result seems ok'
  else
     write (*,*) 'The result does not seem ok'
  end if

  contains

    subroutine set_initial_values(array)
      implicit none
      real(kind=dp), intent(out) :: array(:,:)
      integer :: i, m, n
      
      m=size(array,1)
      n=size(array,2)

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
    end subroutine set_initial_values

    subroutine apply_fd_laplacian(x, y, dx, dy)
      implicit none
      real(kind=dp), intent(in) :: x(:,:)
      real(kind=dp), intent(out) :: y(:,:)
      real(kind=dp), intent(in) :: dx, dy
      
      ! TODO: Implement finite-difference Laplacian with loops
   
    end subroutine apply_fd_laplacian

    function check_result(x, y, dx, dy) result(iscorrect)
      implicit none
      real(kind=dp), intent(in) :: x(:,:), y(:,:)
      real(kind=dp), intent(in) :: dx, dy

      real(kind=dp) :: yc(size(x,1),size(x,2)) 
      integer :: lny, uny, lnx, unx
      real(kind=dp), parameter :: tol = 1e-15
      logical :: iscorrect 

      lny = lbound(x,2)+1
      uny = ubound(x,2)-1
      lnx = lbound(x,1)+1
      unx = ubound(x,1)-1

      yc=real(0,dp)

      yc(lnx:unx,lny:uny) = &
           (x(lnx-1:unx-1,lny:uny)-2*x(lnx:unx,lny:uny)+x(lnx+1:unx+1,lny:uny))/dx**2 + &
           (x(lnx:unx,lny-1:uny-1)-2*x(lnx:unx,lny:uny)+x(lnx:unx,lny+1:uny+1))/dy**2

      iscorrect = all(abs(yc(lnx:unx,lny:uny)-y(lnx:unx,lny:uny))<tol)
    end function check_result

end program ex5a
