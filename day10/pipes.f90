program pipes
  use iso_fortran_env
  use io
  use strings
  implicit none


  character(len=:), allocatable :: filename, lines(:), substr(:)
  integer :: file, nlines
  integer :: i

  call get_filename(filename)
  file = open(filename)
  lines = readlines(file)
  close(file)

  call part1(lines)
  call part2(lines)


contains

  subroutine part2(lines)
    character(len=*), intent(inout) :: lines(:)
    integer :: i, j, n, m
    integer :: start(2), current(2), prev(2), next(2)
    integer, allocatable :: maze(:,:)
    logical :: inside
    integer :: tiles
    character(1) :: prev_sym

    ! Find starting point
    outer : do i = 1, size(lines)
       do j = 1, len(lines(i))
          if (lines(i)(j:j) == "S") then
             start = [i , j]
             exit outer
          end if
       end do
    end do outer

    ! Replace S by correct symbol
    n = start(1)
    m = start(2)
    if (index("|F7", lines(n-1)(m:m)) /= 0) then
       if (index("-7J", lines(n)(m+1:m+1)) /= 0) then
          lines(n)(m:m) = "L"
       elseif (index("|JL", lines(n+1)(m:m)) /= 0) then
          lines(n)(m:m) = "|"
       else 
          lines(n)(m:m) = "J"
       end if
    elseif (index("-J7", lines(n)(m+1:m+1)) /= 0) then
       if (index("|JL", lines(n+1)(m:m)) /= 0) then
          lines(n)(m:m) = "F"
       else 
          lines(n)(m:m) = "-"
       end if
    elseif (index("|J7", lines(n+1)(m:m)) /= 0) then
       if (index("-FL", lines(n)(m-1:m-1)) /= 0) then
          lines(n)(m:m) = "7"
       end if
    end if

    allocate(maze(size(lines),len(lines(1))))
    maze = -1
    ! -1 = left 
    ! Fix starting point
    
    current = start
    prev = current
    i = 0
    do while (.true.)
       if (all(current == start) .and. i /= 0) exit
       i = i + 1
       call search_connection(lines, current, next, prev)
       prev = current
       current = next
       maze(current(1),current(2)) = i
    end do

    tiles = 0
    do i = 1, size(lines)
       inside = .false.
       prev_sym = "."
       do j = 1, len(lines(i))
          if (maze(i,j) == -1 .and. inside) then
             tiles = tiles + 1
             lines(i)(j:j) = "I"
          elseif (lines(i)(j:j) == "|" .and. maze(i,j) /= -1) then
             inside = .not. inside
          elseif (lines(i)(j:j) == "F" .and. maze(i,j) /= -1) then
             prev_sym = "F"
          elseif (lines(i)(j:j) == "L" .and. maze(i,j) /= -1) then
             prev_sym = "L"
          elseif (lines(i)(j:j) == "J" .and. prev_sym == "F" &
                .and. maze(i,j) /= -1) then
             inside = .not. inside
          elseif (lines(i)(j:j) == "7" .and. prev_sym == "L" &
                .and. maze(i,j) /= -1) then
             inside = .not. inside
          end if
       end do
       print *, lines(i)
    end do

    print "(A)", "=================="
    print "(A,I4)", "Result: ", tiles

  end subroutine part2

  subroutine part1(lines)
    character(len=*), intent(inout) :: lines(:)
    integer :: i, j
    integer :: start(2), current(2), prev(2), next(2)

    ! Find starting point
    outer : do i = 1, size(lines)
       do j = 1, len(lines(i))
          if (lines(i)(j:j) == "S") then
             start = [i , j]
             exit outer
          end if
       end do
    end do outer

    current = start
    prev = current
    i = 0
    do while (.true.)
       if (all(current == start) .and. i /= 0) exit
       i = i + 1
       call search_connection(lines, current, next, prev)
       prev = current
       current = next
    end do

    print "(A)", "=================="
    print "(A,I4)", "Result: ", i/2

  end subroutine part1

  subroutine search_connection(maze, current, next, prev)
    character(len=*), intent(inout) :: maze(:)
    integer, intent(in) :: current(2), prev(2)
    integer, intent(out) :: next(2)
    integer :: i, j, n, m
    n = size(maze)
    m = len(maze(1))

    i = current(1)
    j = current(2)

    select case (maze(i)(j:j))
    case("|")
       if (i-1 /= prev(1)) then
          next = [i-1, j]
       else
          next = [i+1, j]
       end if
    case("-")
       if (j-1 /= prev(2)) then
          next = [i, j-1]
       else
          next = [i, j+1]
       end if
    case("L")
       if (i-1 /= prev(1)) then
          next = [i-1, j]
       else
          next = [i, j+1]
       end if
    case("J")
       if (i-1 /= prev(1)) then
          next = [i-1, j]
       else
          next = [i, j-1]
       end if
    case("7")
       if (i+1 /= prev(1)) then
          next = [i+1, j]
       else
          next = [i, j-1]
       end if
    case("F")
       if (i+1 /= prev(1)) then
          next = [i+1, j]
       else
          next = [i, j+1]
       end if
    case("S")
       if (index("|7F", maze(i-1)(j:j)) /= 0) then
          next = [i-1, j]
       elseif (index("-J7", maze(i)(j+1:j+1)) /= 0) then
          next = [i, j+1]
       else
          next = [i+1, j]
       end if
    end select

  end subroutine search_connection

  subroutine get_filename(filename)
    character(len=:), allocatable, intent(inout) :: filename
    character(len=32) :: arg1
    
    call get_command_argument(1, arg1)
    filename = trim(arg1)
    if (filename == '') then
       stop "No file specified!"
    end if

  end subroutine get_filename


end program pipes
