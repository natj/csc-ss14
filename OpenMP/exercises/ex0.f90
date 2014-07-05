program hello
  use omp_lib
  integer :: omp_rank

!$omp parallel private(omp_rank)
  omp_rank = omp_get_thread_num()
  print *, 'Hello world by', omp_rank
!$omp end parallel

end program hello
