program dotprod
  implicit none
  integer, parameter :: rk = kind(1d0)
  integer, parameter :: ik = selected_int_kind(9)
  integer, parameter :: nx = 102400

  real(kind=rk), dimension(nx) :: vecA, vecB
  real(kind=rk)    :: sum, psum
  integer(kind=ik) :: i

  ! Initialization of vectors
  do i = 1, nx
     vecA(i) = 1.0_rk/(real(nx - i + 1, kind=rk))
     vecB(i) = vecA(i)**2
  end do

  sum = 0.0_rk
  do i = 1, nx
     sum = sum + vecA(i) * vecB(i)
  end do
  write(*,*) 'Sum:', sum

end program dotprod
