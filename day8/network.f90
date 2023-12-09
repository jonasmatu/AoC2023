program network
  use iso_fortran_env
  use strings
  use io
  use sorting
  implicit none

  character(len=:), allocatable :: filename
  character(len=:), allocatable :: path, last, current, vendpos(:)
  character(len=3), allocatable :: tlist(:), llist(:), rlist(:)
  integer(int64) :: ltree, lpath
  integer(int64) :: i, j, next, ipath
  integer(int64), allocatable :: snodes(:), length(:), endpos(:), cstart(:)
  integer(int64) :: res

  type node
     integer(int64) :: val, left, right
     logical :: goal
  end type node

  type(node), allocatable :: tree(:)

  call get_filename(filename)

  call construct_tree(tree, filename, path, tlist, llist, rlist)

  snodes = get_starting_nodes(tlist)
  allocate(length(size(snodes)))
  allocate(endpos(size(snodes)))
  allocate(cstart(size(snodes)))
  allocate(character(len=3) :: vendpos(size(snodes)))
  
  do i = 1, size(snodes)
  end do

  print "(A)", "snodes | length | endpos | vendpos"
  print "(A)", "----------------------------------"
  do i = 1, size(snodes)
     call analyze_path(tree, snodes(i), length(i), endpos(i), vendpos(i), path, tlist)
     print "(xAxx,A,I6,A,I6,A,xA)", tlist(snodes(i)), " | ", length(i),  " | ", endpos(i), &
          " | ", vendpos(i)
  end do

  res = kgV(length(1), length(2))
  do i = 3, size(snodes)
     res = kgV(res, length(i))
  end do

  print "(A)", "=================================="
  print *, "Result: ", res

contains

  function get_starting_nodes(tlist) result(snodes)
    character(len=*), intent(in) :: tlist(:)
    integer(int64), allocatable :: snodes(:)
    integer(int64) :: i, nstart

    nstart = 0
    do i = 1, size(tree)
       if (tlist(i)(3:3) == 'A') nstart = nstart + 1
    end do
    allocate(snodes(nstart))
    print *, "nstart = ", nstart
    nstart = 1
    do i = 1, size(tree)
       if (tlist(i)(3:3) == 'A') then
          snodes(nstart) = i
          nstart = nstart + 1
       end if
    end do

  end function get_starting_nodes

  function ggT(a, b) result(res)
    ! Greatest common devisor
    integer(int64), intent(in) :: a, b
    integer(int64) :: aa, bb, h
    integer(int64) :: res

    aa = a
    bb = b
    
    do while (bb /= 0)
       h = mod(aa, bb)
       aa = bb
       bb = h
    end do
    res = aa

  end function ggT

  function kgV(a, b) result(res)
    ! Smallest common multiple
    integer(int64), intent(in) :: a, b
    integer(int64) :: res

    res = abs(a) * abs(b) / ggT(a, b)
  end function kgV

  subroutine analyze_path(tree, start, length, endpos, vendpos, path, tlist)
    type(node), allocatable, intent(in) :: tree(:)
    character(len=*), intent(in) :: path, tlist(:)
    integer(int64), intent(in) :: start
    integer(int64), intent(out) ::  length, endpos
    character(len=*), intent(out) :: vendpos
    integer(int64) :: i, next, ipath, lpath, ltree, nendpos

    ltree = size(tree)
    lpath = len(path)

    next = start
    i = 1
    nendpos = 0
    do while (.true.)
       ipath = mod(i-1, lpath) + 1
       ! print "(Ax,Ax,Ax,A)", tlist(next), llist(next), rlist(next), path(ipath:ipath)
       if (path(ipath:ipath) == "L") then
          next = tree(next)%left
       else
          next = tree(next)%right
       end if
       if (tree(next)%goal .and. nendpos == 0) then
          nendpos = nendpos + 1
          endpos = i
          vendpos = tlist(next)
       elseif (tree(next)%goal) then
          length = i - endpos
          exit
       end if
       i = i + 1
    end do

  end subroutine analyze_path

  subroutine construct_tree(tree, filename, path, tlist, llist, rlist)
    type(node), allocatable, intent(out) :: tree(:)
    character(len=*), intent(in) :: filename
    character(len=3), allocatable, intent(out) :: tlist(:), llist(:), rlist(:)
    character(len=:), allocatable, intent(out) :: path
    integer(int64) :: res, nlines
    integer :: file
    character(len=:), allocatable :: line, substr(:)
    
    file = open(filename)
    nlines = num_lines(file)
    allocate(tlist(nlines-2))
    allocate(llist(nlines-2))
    allocate(rlist(nlines-2))
    allocate(tree(nlines-2))

    do i = 1, nlines
       call readline(file, line)
       if (i == 1) path = line
       if (i > 2) then
          call split(line, substr, delimiters="=")
          tlist(i-2) = trim(substr(1))
          llist(i-2) = trim(substr(2)(3:5))
          rlist(i-2) = trim(substr(2)(8:10))
       end if
    end do

    rewind(file)

    do i = 1, nlines
       call readline(file, line)
       if (i > 2) then
          call split(line, substr, delimiters="=")
          tree(i-2)%val = i-2
          tree(i-2)%left = findloc(tlist, llist(i-2), dim=1)
          tree(i-2)%right = findloc(tlist, rlist(i-2), dim=1)
          tree(i-2)%goal = .false.
          if (tlist(i-2)(3:3) == 'Z') tree(i-2)%goal = .true.
       end if
    end do
    
    close(file)

  end subroutine construct_tree

  subroutine get_filename(filename)
    character(len=:), allocatable, intent(inout) :: filename
    character(len=32) :: arg1
    
    call get_command_argument(1, arg1)
    filename = trim(arg1)
    if (filename == '') then
       stop "No file specified!"
    end if

  end subroutine get_filename

end program network
