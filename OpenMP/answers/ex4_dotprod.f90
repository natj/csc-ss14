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
  ! Sum with data race
  !$omp parallel do default(shared) private(i)
  do i = 1, nx
     sum = sum + vecA(i) * vecB(i)
  end do
  !$omp end parallel do
  write(*,*) 'Sum using critical section:                      ', sum

  sum = 0.0_rk
  ! Dot product using critical section = SERIAL CODE
  !$omp parallel do default(shared) private(i)
  do i = 1, nx
     !$omp critical
     sum = sum + vecA(i) * vecB(i)
     !$omp end critical
  end do
  !$omp end parallel do
  write(*,*) 'Sum using critical section:                      ', sum

  sum = 0.0_rk
  ! Dot product using reduction
  !$omp parallel do default(shared) private(i) reduction(+:sum)
  do i = 1, nx
     !$omp critical
     sum = sum + vecA(i) * vecB(i)
     !$omp end critical
  end do
  !$omp end parallel do
  write(*,*) 'Sum using reduction:                             ', sum

  sum = 0.0_rk
  ! Dot product using private variable and critical secion
  !$omp parallel default(shared) private(i, psum)
  psum = 0.0_rk
  !$omp do
  do i = 1, nx
     psum = psum + vecA(i) * vecB(i)
  end do
  !$omp end do
  !$omp critical
  sum = sum + psum
  !$omp end critical
  !$omp end parallel
  write(*,*) 'Sum using private variable and critical section: ', sum

end program dotprod
