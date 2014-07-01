program pario
  use mpi
  implicit none
  integer, parameter :: datasize = 100, writer_id = 0
  integer :: rc, my_id, ntasks
  integer, dimension(datasize) :: localvector
  integer, dimension(:), allocatable :: fullvector

  call mpi_init(rc)
  call mpi_comm_size(mpi_comm_world, ntasks, rc)
  call mpi_comm_rank(mpi_comm_world, my_id, rc)

  localvector = my_id

  ! ex 1a
  call single_writer()
  ! ex 1b
  call subset_of_writers()
  ! ex 1c
  call single_writer_mpi()
  ! ex 1d
  call subset_mpi()

  call mpi_finalize(rc)

contains

  subroutine single_writer()
    ! exercise 1a
    implicit none
    allocate (fullvector(ntasks*datasize))
    call mpi_gather(localvector, datasize, mpi_integer, fullvector, datasize, &
         mpi_integer, writer_id, mpi_comm_world, rc)
    if (my_id == writer_id) then
       open (10, file='ex1a.dat', status='replace', form='unformatted', &
            & access='stream')
       write (10, pos=1) fullvector
       close (10)
       write (*,*) 'Wrote' , size(fullvector), 'elements to file ex1a.dat'
    end if
    deallocate (fullvector)
  end subroutine single_writer

  subroutine subset_of_writers()
    ! exercise 1a
    implicit none
    integer :: color, iocomm, iocomm_id, iocomm_size
    character(len=9) :: filename
 
    if (my_id <= 3) then
       color = 1
    else
       color = 2
    end if
    call mpi_comm_split(mpi_comm_world, color, my_id, iocomm, rc)
    call mpi_comm_size(iocomm, iocomm_size, rc)
    call mpi_comm_rank(iocomm, iocomm_id, rc)

    allocate (fullvector(iocomm_size*datasize))
    call mpi_gather(localvector, datasize, mpi_integer, fullvector, datasize, &
         mpi_integer, writer_id, iocomm, rc)
    if (iocomm_id == writer_id) then
       write(filename,'(A4,I1,A4)') 'ex1b', color, '.dat'
       open (10, file=filename, form='unformatted', status='replace', &
            & access='stream')
       write (10, pos=1) fullvector
       close (10)
       write (*,*) my_id, 'wrote' , size(fullvector), 'elements to file ', filename
    end if
    deallocate (fullvector)
  end subroutine subset_of_writers

  subroutine single_writer_mpi()
    implicit none
    integer :: fileh
    integer(kind=mpi_offset_kind) :: displ
    integer :: intsize

    call mpi_file_open(mpi_comm_world, 'ex1c.dat', mpi_mode_create+mpi_mode_wronly, &
                       mpi_info_null, fileh, rc)
    call mpi_type_size(mpi_integer, intsize, rc)
    displ = my_id * datasize * intsize
    call mpi_file_write_at(fileh, displ, localvector, datasize, mpi_integer, &
                           mpi_status_ignore, rc)
    call mpi_file_close(fileh, rc)
    if (my_id == writer_id) then
       write (*,*) 'Wrote' , ntasks*datasize, 'elements to file ex1c.dat'
    end if

  end subroutine single_writer_mpi

  subroutine subset_mpi()
    ! exercise 1d
    implicit none
    integer, parameter ::  writer_id = 0
    integer :: color, iocomm, iocomm_id, iocomm_size
    character(len=9) :: filename
    integer(kind=mpi_offset_kind) :: displ
    integer :: fileh, intsize

    if (my_id <= 3) then
       color = 1
    else
       color = 2
    end if
    call mpi_comm_split(mpi_comm_world, color, my_id, iocomm, rc)
    call mpi_comm_size(iocomm, iocomm_size, rc)
    call mpi_comm_rank(iocomm, iocomm_id, rc)
    write(filename,'(A4,I1,A4)') 'ex1d', color, '.dat'
    call mpi_file_open(iocomm, filename, mpi_mode_create+mpi_mode_wronly, &
                       mpi_info_null, fileh, rc)
    call mpi_type_size(mpi_integer, intsize, rc)
    displ = iocomm_id * datasize * intsize
    call mpi_file_write_at(fileh, displ, localvector, datasize, mpi_integer, &
                           mpi_status_ignore, rc)
    call mpi_file_close(fileh, rc)
    if (iocomm_id == writer_id) then
       write (*,*) my_id, 'wrote' , iocomm_size*datasize, 'elements to file ', filename
    end if

  end subroutine subset_mpi


end program pario
