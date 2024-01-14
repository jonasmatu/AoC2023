program galaxies
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

  subroutine part2(lines)
    character(len=*), intent(in) :: lines(:)
    integer, allocatable :: universe(:,:), posgal(:,:)
    integer, allocatable :: rows(:), cols(:)
    integer :: i, j, ngalax, ecols, erows
    integer :: x1, y1, x2, y2
    integer(int64) :: lpath, tpath
    
    call prep_large_universe(lines, universe, rows, cols, ngalax, posgal)

    lpath = 0
    ecols = 0
    erows = 0
    do i = 1, ngalax
       do j = i+1, ngalax
          tpath = 0
          x1 = min(posgal(1,i),posgal(1,j))
          x2 = max(posgal(1,i),posgal(1,j))
          y1 = min(posgal(2,i),posgal(2,j))
          y2 = max(posgal(2,i),posgal(2,j))
          tpath = abs(posgal(1,i)-posgal(1,j)) + abs(posgal(2,i)-posgal(2,j))
          ! count the number of empty rows and cols
          erows = count(rows(x1:x2) == 0)
          ecols = count(cols(y1:y2) == 0)
          ! print *, erows, ecols
          lpath = lpath + erows*999999 + ecols*999999
          ! tpath = tpath + erows + ecols
          ! print *, "Galaxy", i, j, "dist", tpath, erows, ecols
          lpath = lpath + tpath
       end do
    end do


    print *, "======================"
    print *, "Result part 2:", lpath
    
  end subroutine part2

  subroutine prep_large_universe(lines, universe, rows, cols, ngalax, posgalax)
    character(len=*), intent(in) :: lines(:)
    integer, allocatable, intent(out) :: universe(:,:), rows(:), cols(:), posgalax(:,:)
    integer, intent(out) :: ngalax
    integer :: i, j, crc_galax
    integer, allocatable :: tmp_pos(:,:)


    allocate(universe(size(lines),len(lines(1))))
    allocate(rows(size(lines)))
    allocate(cols(len(lines(1))))
    
    ngalax = 0
    universe = 0
    rows = 0
    cols = 0
    do i = 1, size(lines)
       do j = 1,len(lines(i))
          if (lines(i)(j:j) == '#') then
             ngalax = ngalax + 1
             universe(i,j) = ngalax
             rows(i) = rows(i) + 1
             cols(j) = cols(j) + 1
             ! append galaxy position
             if (allocated(tmp_pos)) deallocate(tmp_pos)
             allocate(tmp_pos(2,ngalax))
             tmp_pos(:,ngalax) = [i,j]
             if (allocated(posgalax)) then
                tmp_pos(:,:ngalax-1) = posgalax
                deallocate(posgalax)
                allocate(posgalax(2,ngalax))
             end if
             posgalax = tmp_pos
          end if
       end do
    end do

  end subroutine prep_large_universe

  subroutine part1(lines)
    character(len=*), intent(in) :: lines(:)
    integer, allocatable :: universe(:,:)
    integer :: i, j, n, m, ngalax, cgalax
    integer, allocatable :: galaxies(:,:)
    integer :: lpath

    universe = prepare_universe(lines)
    ngalax = count(universe /= 0)
    allocate(galaxies(2,ngalax))
    n = size(universe, 1)
    m = size(universe, 2)
    cgalax = 1
    do i = 1, n
       do j = 1, m
          if (universe(i,j) > 0) then
             galaxies(1,cgalax) = i
             galaxies(2,cgalax) = j
             cgalax = cgalax + 1
          end if
       end do
    end do


    lpath = 0
    do i = 1, ngalax
       do j = i+1, ngalax
          lpath = lpath + abs(galaxies(1,i)-galaxies(1,j)) + abs(galaxies(2,i)-galaxies(2,j))
          ! print *, "Distance", i, j, "=", abs(galaxies(1,i)-galaxies(1,j)) + abs(galaxies(2,i)-galaxies(2,j))
       end do
    end do
    
    ! do i = 1, size(universe, 1)
    !    print "(13(I1))", universe(i,:)
    ! end do
    print *, "===================="
    print *, "Result:", lpath
    
  end subroutine part1

  function prepare_universe(lines) result(universe)
    character(len=*), intent(in) :: lines(:)
    integer, allocatable :: universe(:,:)
    integer :: nlines, i, j, n, m, un, um
    integer :: cgalax
    logical :: galax

    n = size(lines)
    m = len(lines(1))
    un = n
    um = m
    do i = 1, n
       galax = .false.
       do j = 1, m
          if (lines(i)(j:j) == "#") then
             galax = .true.
             exit
          end if
       end do
       if (.not. galax) un = un + 1
    end do

    do j = 1, m
       galax = .false.
       do i = 1, n
          if (lines(i)(j:j) == "#") then
             galax = .true.
             exit
          end if
       end do
       if (.not. galax) um = um + 1
    end do
    
    allocate(universe(un, um))
    universe = 0
    un = 1
    cgalax = 1
    do i = 1, n
       if (index(lines(i), "#") == 0) then
          un = un + 2
          cycle
       end if
       um = 1
       do j = 1, m
          if (findloc(lines(:)(j:j), "#", dim=1) == 0) then
             um = um + 2
             cycle
          end if
          if (lines(i)(j:j) == "#") then
             universe(un,um) = cgalax
             cgalax = cgalax + 1
          end if
          um = um + 1
       end do
       un = un + 1
    end do
    
    
  end function prepare_universe

  subroutine get_filename(filename)
    character(len=:), allocatable, intent(inout) :: filename
    character(len=32) :: arg1
    
    call get_command_argument(1, arg1)
    filename = trim(arg1)
    if (filename == '') then
       stop "No file specified!"
    end if

  end subroutine get_filename
  

end program galaxies
