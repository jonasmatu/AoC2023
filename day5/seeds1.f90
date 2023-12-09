program name
  use iso_fortran_env
  use io
  use strings
  implicit none

  character(len=:), allocatable :: filename, line, str_seeds(:)
  integer(int64) :: file, i, j, k, nlines
  integer(int64), allocatable :: initseeds(:), translation(:,:), tmp_transl(:)

  call get_filename(filename)
  file = open(filename)
  call readline(file, line)
  rewind(file)
  nlines = num_lines(file)
  k = 1
  do i = 1, nlines
     call readline(file, line)
     if (i == 1) then
        call split(line(7:), str_seeds, delimiters=' ')
        initseeds = [(to_numeric(str_seeds(j)), j=1,size(str_seeds))]
        print *, "Initial seeds"
        print "(4(I10x))", initseeds(1),initseeds(2) ,initseeds(3), initseeds(4) 
     elseif (trim(line) /= '' .and. is_numeric(line(1:1))) then
        deallocate(str_seeds)
        call split(line, str_seeds, delimiters=' ')
        tmp_transl = [(to_numeric(str_seeds(j)), j=1,size(str_seeds))]
        if (k == 1) then
           if (allocated(translation)) deallocate(translation)
           allocate(translation(3,1))
           translation(:,1) = tmp_transl
        else
           call append_array(translation, tmp_transl)
        end if
        
        k = k + 1
     end if
     if (i > 3 .and. (trim(line) /= '' .and. .not. is_numeric(line(1:1)))) then
        call apply_map(translation, initseeds)
        print "(4(I3x))", initseeds(1),initseeds(2) ,initseeds(3), initseeds(4) 
        k = 1
     elseif (i == nlines) then
        call apply_map(translation, initseeds)
        print "(4(I3x))", initseeds(1),initseeds(2) ,initseeds(3), initseeds(4) 
        k = 1
     end if
     
  end do
  print *, "============"
  print *, "Result: ", minval(initseeds)


contains

  subroutine apply_map(translation, seeds)
    integer(int64), allocatable, intent(in) :: translation(:,:)
    integer(int64), allocatable, intent(inout) :: seeds(:)
    integer(int64), allocatable :: tmp_seeds(:)
    integer(int64) :: i, j

    tmp_seeds = seeds
    
    do i = 1, size(translation, 2)
       do j = 1, size(seeds)
          if (translation(2,i) <= seeds(j) .and. translation(2,i) + translation(3,i) > seeds(j)) then
             tmp_seeds(j) = translation(1,i) + seeds(j) - translation(2,i)
          end if
       end do
    end do

    seeds = tmp_seeds

  end subroutine apply_map

  subroutine append_array(array, newvals)
    integer(int64), allocatable, intent(inout) :: array(:,:)
    integer(int64), allocatable, intent(in) :: newvals(:)
    integer(int64), allocatable :: tmp_ar(:,:)
    integer(int64) :: n,m

    n = size(array, 1)
    m = size(array, 2)

    tmp_ar = array
    deallocate(array)
    allocate(array(n, m+1))
    array(:,:m) = tmp_ar
    array(:,m+1) = newvals

  end subroutine append_array

  subroutine get_filename(filename)
    character(len=:), allocatable, intent(inout) :: filename
    character(len=32) :: arg1
    
    call get_command_argument(1, arg1)
    filename = trim(arg1)
    if (filename == '') then
       stop "No file specified!"
    end if

  end subroutine get_filename

end program name
