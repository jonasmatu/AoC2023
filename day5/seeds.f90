program name
  use iso_fortran_env
  use io
  use strings
  implicit none

  type :: map
     integer(int64), allocatable :: translation(:,:)
  end type map

  character(len=:), allocatable :: filename, line, str_seeds(:)
  integer(int64) :: file, i, j, k, nlines, l, minloc, loc
  integer(int64), allocatable :: initseeds(:), translation(:,:), tmp_transl(:), &
       seeds(:)
  type(map) :: maps(7)
       
  call get_filename(filename)
  file = open(filename)
  call readline(file, line)
  rewind(file)
  nlines = num_lines(file)
  k = 1
  l = 1
  do i = 1, nlines
     call readline(file, line)
     if (i == 1) then
        call split(line(7:), str_seeds, delimiters=' ')
        initseeds = [(to_numeric(str_seeds(j)), j=1,size(str_seeds))]
        print *, "Initial seeds"
        print "(4(I10x))", initseeds(1), initseeds(2) , initseeds(3), initseeds(4) 
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
        maps(l)%translation = translation
        l = l + 1
        k = 1
     elseif (i == nlines) then
        maps(l)%translation = translation
        k = 1
     end if
  end do

  ! minloc = apply_map(maps, 79_int64)
  ! print *, minloc
  ! minloc = apply_map(maps, 14_int64)
  ! print *, minloc
  ! minloc = apply_map(maps, 55_int64)
  ! print *, minloc
  ! minloc = apply_map(maps, 13_int64)
  ! print *, minloc
  
  

  minloc = 999999999
  do i = 2, size(initseeds), 2
     do j = 1, initseeds(i)
        loc = apply_map(maps, initseeds(i-1) + j - 1)
        minloc = min(minloc, loc)
        ! print *, "Initseed:", initseeds(i-1) + j-1, "Location:", &
        !      loc
     end do
  end do
  
  print *, "============"
  print *, "Result: ", minloc


contains

  function apply_map(maps, seed) result(res)
    type(map), intent(in) :: maps(:)
    integer(int64), intent(in) :: seed
    integer(int64) :: tmp_seed, res
    integer(int64) :: j, k

    res = seed
    do k = 1, size(maps)
       res = apply_submap(maps(k)%translation, res)
    end do

  end function apply_map

  function apply_submap(translation, seed) result(res)
    integer(int64), allocatable, intent(in) :: translation(:,:)
    integer(int64), intent(in) :: seed
    integer(int64) :: tmp_seed, res
    integer(int64) :: i

    tmp_seed = seed
    
    do i = 1, size(translation, 2)
       if (translation(2,i) <= seed .and. translation(2,i) + translation(3,i) > seed) then
          tmp_seed = translation(1,i) + seed - translation(2,i)
       end if
    end do

    res = tmp_seed

  end function apply_submap

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
