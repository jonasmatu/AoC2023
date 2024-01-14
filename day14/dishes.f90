program dishes
  use iso_fortran_env
  use strings
  use io
  implicit none


  character(len=:), allocatable :: filename, lines(:)
  integer :: file

  call get_filename(filename)
  file = open(filename)
  lines = readlines(file)
  close(file)


  call part1(lines)
  call part2(lines)

contains

  function tilt_beam(col) result(beam)
    integer, intent(in) :: col(:)
    integer, allocatable :: beam(:)
    integer :: i, j
    integer :: rocks, last

    allocate(beam(size(col)))
    beam = 0
    
    rocks = 0
    last = 0
    do i = 1, size(col)
       if (col(i) == -1) then
          beam(i) = -1
          do j = 1, rocks
             beam(last + j) = 1
          end do
          rocks = 0
          last = i
       elseif (col(i) == 1) then
          rocks = rocks + 1
       end if
    end do

    do j = 1, rocks
       beam(last + j) = 1
    end do

  end function tilt_beam

  function spin_cycle(data) result(spun_data)
    integer, intent(in) :: data(:,:)
    integer, allocatable :: spun_data(:,:)
    integer :: i

    allocate(spun_data(size(data,1), size(data,2)))

    ! tilt north
    do i = 1, size(data, 2)
       spun_data(:,i) = tilt_beam(data(:,i))
    end do

    !tilt west
    do i = 1, size(data,1)
       spun_data(i,:) = tilt_beam(spun_data(i,:))
    end do

    !tilt south
    do i = 1, size(data, 2)
       spun_data(size(data,1):1:-1,i) = tilt_beam(spun_data(size(data,1):1:-1,i))
    end do

    !tilt east
    do i = 1, size(data, 1)
       spun_data(i,size(data,2):1:-1) = tilt_beam(spun_data(i,size(data,2):1:-1))
    end do
       
  end function spin_cycle

  function eval_config(data) result(res)
    integer, intent(in) :: data(:,:)
    integer(int64) :: res
    integer :: i

    do i = 1, size(data, 2)
       res = res + sum(data(:,i) * [(size(data,1) - i +1, i=1,size(data,1))], mask=data(:,i) > 0)
    end do

  end function eval_config

  subroutine part2(lines)
    character(len=:), allocatable :: lines(:)
    integer(int64) :: res
    integer, allocatable :: data(:,:)
    integer, allocatable :: cache_data(:,:,:), tmp_data(:,:,:)
    integer :: i, j, ncycle, start, end, lcycle

    res = 0
    ! Translate into numbers
    allocate(data(size(lines), len(lines(1))))
    allocate(cache_data(size(data,1), size(data,2),1))
    cache_data = 0
    data = 0
    do i = 1, size(lines)
       do j = 1, len(lines(1))
          if (lines(i)(j:j) == "#") data(i,j) = -1
          if (lines(i)(j:j) == "O") data(i,j) = +1
       end do
       ! print "(10(I1))", data(i,:)
    end do

    print *, ""
    cache_data(:,:,1) = spin_cycle(data)
    tmp_data = cache_data
    outer : do i = 2, 1000
       deallocate(tmp_data)
       tmp_data = cache_data
       deallocate(cache_data)
       allocate(cache_data(size(tmp_data,1),size(tmp_data,2),size(tmp_data,3)+1))
       cache_data(:,:,1:size(tmp_data,3)) = tmp_data
       cache_data(:,:,i) = spin_cycle(cache_data(:,:,i-1))
       do j = size(cache_data, 3)-1, 1, -1
          if (all(cache_data(:,:,i) == cache_data(:,:,j))) then
             print *, "Found cycle between", i, j
             start = j
             end = i
             exit outer
          end if
       end do
       ! print *, eval_config(cache_data(:,:,i))
       ! call append(cache_data, data)
    end do outer

    ! do i = 1, size(data,1)
    !    print "(10(I1))", data(i,:)
    ! end do

    lcycle = end-start
    ncycle = mod(1000000000 - start, lcycle)
    print *, "Ncycle = ", ncycle
    res = eval_config(cache_data(:,:,start + ncycle))
    
    print "(A,I10)", "Part2, result: ", res
    
  end subroutine part2

  subroutine append(data, arr)
    integer, allocatable, intent(inout) :: data(:,:,:)
    integer, intent(in) :: arr(:,:)
    integer, allocatable :: tmp_data(:,:,:)
    integer :: n,m,l
    integer :: i

    n = size(data,1)
    m = size(data,2)
    l = size(data,3)

    allocate(tmp_data(n,m,l+2))
    tmp_data(:,:,1:l) = data
    tmp_data(:,:,l+1) = arr
    ! deallocate(data)
    call move_alloc(tmp_data, data)
    

  end subroutine append

  subroutine part1(lines)
    character(len=:), allocatable :: lines(:)
    integer(int64) :: res
    integer :: i

    res = 0

    do i = 1, len(lines(1))
       ! print "(10(A1))", lines(:)(i:i) 
       res = res + prep_col(lines(:)(i:i))
    end do

    print *, "Part1, result: ", res
  end subroutine part1


  function prep_col(col) result(res)
    character(len=*), intent(in) :: col(:)
    integer(int64) :: res
    integer, allocatable :: beam(:)
    integer :: i, j
    integer :: rocks, last

    allocate(beam(size(col)))
    beam = 0
    
    rocks = 0
    last = 0
    do i = 1, size(col)
       if (col(i) == "#") then
          do j = 1, rocks
             beam(last + j) = 1
          end do
          rocks = 0
          last = i
       elseif (col(i) == "O") then
          rocks = rocks + 1
       end if
    end do

    do j = 1, rocks
       beam(last + j) = 1
    end do

    ! print "(10(I1))", beam 
    
    res = sum(beam * [(size(beam) - i +1, i=1,size(beam))])

  end function prep_col
  

  subroutine get_filename(filename)
    character(len=:), allocatable, intent(inout) :: filename
    character(len=32) :: arg1

    call get_command_argument(1, arg1)
    filename = trim(arg1)
    if (filename == '') then
       stop "No file specified!"
    end if

  end subroutine get_filename

end program dishes
