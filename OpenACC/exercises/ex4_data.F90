program ex4_data
#ifdef _OPENACC
  use openacc
#endif
  implicit none

  integer, parameter :: sp = selected_real_kind(8)
  real(kind=sp) :: eps

  real(kind=sp), allocatable, dimension(:,:) :: u, unew
  real(kind=sp) :: norm, mlups
  integer :: maxiter,nx,ny,iter,maxth,nargs,ndef
  character(len=12) arg
  real(8) :: t, dt

  eps = real(0.5e-3, sp)
  ndef = 2400
  nargs = command_argument_count()

  if (nargs <= 0) then
     nx = ndef
     ny = nx
  elseif (nargs <= 1) then
     call get_command_argument(1, arg)
     read(arg,'(i12)') nx
     if (nx < 1) nx = ndef
     ny = nx
  else
     call get_command_argument(1, arg)
     read(arg,'(i12)') nx
     if (nx < 1) nx = ndef
     call get_command_argument(2, arg)
     read(arg,'(i12)') ny
     if (ny < 1) ny = ndef
  endif

  maxiter = int(real(1, sp) / eps)
  maxth = 1
  write(0,'(a,4(1x,i0),1x,g15.6))') &
       & 'Stencil: nx,ny,maxiter,maxth,eps=',&
       &  nx,ny,maxiter,maxth,eps

  allocate(u(0:nx+1,0:ny+1))
  allocate(unew(0:nx+1,0:ny+1))

  ! Initialize data region on device
!$acc data create(u, unew)
  call init(u)
  call init(unew)

  t = ftimer()

  norm = eps + 1
  iter = 0
  do while (iter <= maxiter .and. norm >= eps)
     call update(unew, u)
     call update(u, unew, norm)
     iter = iter + 2
     if (mod(iter,100) == 0 .or. norm < eps) then
        write(0,*) iter,': norm,eps=',norm,eps
     endif
  enddo
!$acc end data


  deallocate(u,unew)

  mlups = real(iter,sp) * real(nx,sp) * real(ny,sp) * real(1.0e-6, sp)
  dt = ftimer() - t
  write(0,'(a,g15.6,a,i0)') 'Stencil: norm =',norm,' with iter = ',iter
  write(0,'(2(a,f12.6))')   'Stencil: Time =',dt,' sec, MLups/s=',mlups/dt

contains

  subroutine init(new)
    implicit none
    real(kind=sp), intent(out) :: new(0:nx+1,0:ny+1)
    integer i,j
    ! TODO: Implement data initialization with OpenACC on device

!$acc parallel present(new)

!$acc loop &
!$acc collapse(2)
    do j=0,ny+1
       do i=0,nx+1
          new(i,j) = real(0, sp)
       enddo
    enddo
!$acc end loop

    ! Implement data initialization with OpenACC on device 
    new(:,ny+1) = real(1, sp)
    ! Implement data initialization with OpenACC on device 
    new(nx+1,:) = real(1, sp)
!$acc end parallel

  end subroutine init

  subroutine update(new, old, norm)
    implicit none
    real(kind=sp), intent(out) :: new(0:nx+1,0:ny+1)
    real(kind=sp), intent(in)  :: old(0:nx+1,0:ny+1)
    real(kind=sp), optional, intent(out) :: norm
    real(kind=sp), parameter :: factor = 0.25_sp
    integer i,j
    if (present(norm)) then
       norm = 0.0_sp
       ! Implement computation with OpenACC on device

!$acc parallel loop present(new) &
!$acc collapse(2)
       do j=1,ny
          do i=1,nx
             new(i,j) = factor*(old(i-1,j) + old(i+1,j) + old(i,j-1) + old(i,j+1))
             norm = max(norm,abs(new(i,j) - old(i,j)))
          enddo
       enddo
!$acc end parallel loop
    else
!$acc parallel loop present(new) &
!$acc collapse(2)
       ! Implement computation with OpenACC on device
       do j=1,ny
          do i=1,nx
             new(i,j) = factor*(old(i-1,j) + old(i+1,j) + old(i,j-1) + old(i,j+1))
          enddo
       enddo
!$acc end parallel loop
    endif
  end subroutine update

  function ftimer()
    implicit none
    real(kind=8) :: ftimer
    integer :: t, rate
    call system_clock(t,count_rate=rate)
    ftimer = real(t,kind(ftimer))/real(rate,kind(ftimer))
  end function ftimer

end program ex4_data
