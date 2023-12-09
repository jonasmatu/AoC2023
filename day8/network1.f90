program network
  use iso_fortran_env
  use strings
  use io
  use sorting
  implicit none

  character(len=:), allocatable :: filename
  character(len=:), allocatable :: path, last, current
  character(len=3), allocatable :: tlist(:), llist(:), rlist(:)
  integer :: ltree, lpath
  integer :: i, next, ipath, res, j

  type node
     integer :: val, left, right
     logical :: goal
  end type node

  type(node), allocatable :: tree(:)

  call get_filename(filename)

  call construct_tree(tree, filename, path, tlist, llist, rlist)
  ltree = size(tree)
  lpath = len(path)
  next = findloc(tlist, "AAA", dim=1)
  i = 1
  do while (.true.)
     ipath = mod(i-1, lpath) + 1
     print "(Ax,Ax,Ax,A)", tlist(next), llist(next), rlist(next), path(ipath:ipath)
     if (path(ipath:ipath) == "L") then
        next = tree(next)%left
     else
        next = tree(next)%right
     end if
     i = i + 1
     if (tree(next)%goal .and. i /= 1) exit
  end do
  res = i - 1

  print *, "=============="
  print *, "Result: ", res
  

contains

  subroutine construct_tree(tree, filename, path, tlist, llist, rlist)
    type(node), allocatable :: tree(:)
    character(len=*), intent(in) :: filename
    character(len=3), allocatable, intent(out) :: tlist(:), llist(:), rlist(:)
    character(len=:), allocatable, intent(out) :: path
    integer :: res, nlines, file
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
          if (tlist(i-2) == "ZZZ") tree(i-2)%goal = .true.
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
