program vectorsum
  use omp_lib
  implicit none
  integer, parameter :: rk = kind(1d0)
  integer, parameter :: ik = selected_int_kind(9)
  integer, parameter :: nx = 102400

  real(kind=rk), dimension(nx) :: vecA, vecB, vecC
  real(kind=rk)    :: sum
  integer(kind=ik) :: i

  ! Initialization of vectors
  do i = 1, nx
     vecA(i) = 1.0_rk/(real(nx - i + 1, kind=rk))
     vecB(i) = vecA(i)**2
  end do


  !   Implement here the parallelized version of vector addition,
  !   vecC = vecA + vecB

!$omp parallel do shared(vecA, vecB, vecC)
  do i = 1,nx
     vecC(i) = vecA(i)*vecB(i)
  end do
!$omp end parallel do

  ! Compute the check value
  write(*,*) 'Reduction sum: ', sum(vecC)
  write(*,*) 'Reduction sum (1-thread)', sum(vecA*vecB)
  
end program vectorsum
