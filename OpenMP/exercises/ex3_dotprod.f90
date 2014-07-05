program dotprod
  use omp_lib
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
  write(*,*) 'Sum (1-thread):', sum

  sum = 0.0_rk
  write(*,*) ' '
  write(*,*) 'Using reduction'

!$omp parallel do shared(vecA, vecB) private(i) reduction(+:sum)
  do i = 1, nx
     sum = sum + vecA(i) * vecB(i)
  end do
!$omp end parallel do

  write(*,*) 'Sum:', sum

  write(*,*) ' '
  write(*,*) 'Using critical'
  sum = 0.0_rk

!$omp parallel shared(vecA, vecB) private(i, psum)
  psum = 0.0_rk
!$omp do
  do i = 1, nx
     psum = psum + vecA(i) * vecB(i)
  end do
!$omp end do

!$omp critical(dosum)
  sum = sum + psum
!$omp end critical(dosum)
!$omp end parallel
  write(*,*) 'Sum:', sum

end program dotprod
