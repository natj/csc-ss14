program ex5
  use omp_lib
  implicit none

  integer :: nthreads, thread

!$omp parallel private(thread, nthreads)
  thread = omp_get_thread_num()
  nthreads = omp_get_num_threads()

!$omp critical(print)
  write(*,*) 'I am ',thread,' of ',nthreads
!$omp end critical(print)

!$omp end parallel

end program
