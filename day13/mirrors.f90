program mirrors
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


  ! deallocate(lines)
  ! allocate(character(len=5) :: lines(1))
  ! lines(1) = "12345"
  ! call append(lines, "67890")

  ! print *, size(lines)
  ! print *, lines(1)
  ! print *, lines(2)
  
  call part1(lines)
  call part2(lines)


  ! print *, len(trim(".     "))
  ! print *, len(trim("      "))

contains

  function prepare_mirror(mirror) result(nmir)
    character(len=*), intent(in) :: mirror(:)
    integer, allocatable :: nmir(:,:)
    integer :: i, j, nsmug

    allocate(nmir(size(mirror), len(mirror(1))))
    nmir = 0
    
    do i = 1,size(mirror)
       do j = 1,len(mirror(1))
          if (mirror(i)(j:j) == "#") nmir(i,j) = 1
       end do
    end do
  end function prepare_mirror

  function process_smuged_mirror(mirror) result(res)
    character(len=*), intent(in) :: mirror(:)
    integer(int64) :: res
    integer, allocatable :: nmir(:,:)
    integer :: i, j, jm, row, col, smugtol , xs, ys, nsmug

    ! print *, "Calculating numeric representation"
    nmir = prepare_mirror(mirror)
    ! do i = 1, size(nmir, 1)
    !    do j = 1, size(nmir, 2)
    !       write(*, "(I1)", advance='no') nmir(i,j)
    !    end do
    !    print *, ""
    ! end do
    ! print *, "Got numeric representation"
    res = 0
    smugtol = 1

    xs = 0
    ys = 0
    row = 0
    router : do i = 2, size(nmir, 1)
       nsmug = sum(abs(nmir(i,:) - nmir(i-1,:))) ! possible mirror axis?
       if (nsmug < 2) then
          row = i
          if (nsmug == 1) then
             xs = findloc(abs(nmir(i,:) - nmir(i-1,:)), 1, dim=1)
             ys = row
             print *, "Found smug, i = ", i
          end if
          do j = i+1, size(nmir, 1)
             ! print *, j
             jm = row - (j - row) - 1
             if (jm < 1) then
                if (nsmug == 1) exit router
                if (nsmug /= 1) exit
             end if
             if (sum(abs(nmir(j,:) - nmir(jm,:))) == 1) then
                xs = findloc(abs(nmir(j,:) - nmir(jm,:)), 1, dim=1)
                ys = j
                ! print *, "Found smug 2, j = ", j
             end if
             nsmug = nsmug + sum(abs(nmir(j,:) - nmir(jm,:)))
             if (nsmug > 2) then
                row = 0
                exit
             end if
             ! print *, "in func, nsmug = ", nsmug
             if (j == size(nmir,1)) then
                if (nsmug == 1) exit router
                if (nsmug /= 1) exit
             end if 
          end do
       end if
    end do router
    ! print *, "nsmug = ", nsmug
    if (nsmug /= 1) then
       row = 0
    else
       ! nmir(ys,xs) = mod(nmir(ys,xs)+1, 2)
       ! print *, "Wiping mirror"
    end if

    ! print *, "Done first part"
    

    if (row /= 0) smugtol = 0
    col = 0

    couter : do i = 2, size(nmir, 2)
       nsmug = sum(abs(nmir(:,i) - nmir(:,i-1))) ! possible mirror axis?
       if (nsmug < 2) then
          col = i
          if (nsmug == 1) then
             ys = findloc(abs(nmir(:,i) - nmir(:,i-1)), 1, dim=1)
             xs = col
             ! print *, "Found smug, i = ", i
          end if
          do j = i+1, size(nmir, 2)
             ! print *, j
             jm = col - (j - col) - 1
              if (jm < 1) then
                if (nsmug == 1) exit couter
                if (nsmug /= 1) exit
             end if
             if (sum(abs(nmir(:,j) - nmir(:,jm))) == 1) then
                xs = j
                ys = findloc(abs(nmir(:,j) - nmir(:,jm)), 1, dim=1)
                ! print *, "Found smug 2, j = ", j
             end if
             nsmug = nsmug + sum(abs(nmir(:,j) - nmir(:,jm)))
             if (nsmug > 2) then
                col = 0
                exit
             end if
            
             if (j == size(nmir, 2)) then
                if (nsmug == 1) exit couter
                if (nsmug /= 1) exit
             end if 
          end do
       end if
    end do couter
    if (nsmug /= 1) col = 0

    ! couter : do i = 2, size(nmir, 2)
    !    nsmug = sum(abs(nmir(:,i) - nmir(:,i-1)))
    !    if (nsmug <= smugtol) then
    !       col = i
    !       do j = i+1, size(nmir, 2)
    !          jm = col - (j - col) - 1
    !          if (jm < 0) then
    !             if nsmug ==

    !             if exit couter
    !          end if
    !          nsmug = nsmug + sum(abs(nmir(:,j)- nmir(:,jm)))
    !          if (nsmug > smugtol) then
    !             col = 0
    !             exit
    !          end if
    !          if (j == size(nmir, 2)) exit couter
    !       end do
    !    end if
    ! end do couter
    ! print *, "Nsmug = ", nsmug, "col = ", col

    if (row /= 0) res = res + 100 * (row-1)
    if (col /= 0) res = res + (col-1)

  end function process_smuged_mirror

  subroutine part2(lines)
    character(len=:), allocatable, intent(in) :: lines(:)
    character(len=:), allocatable :: mirror(:)
    integer :: i, j, mirr_len
    integer(int64) :: score, sc

    score = 0
    do i = 1, size(lines)
       ! print *, "Hello", i
       ! print *, lines(i), len(trim(lines(i))), ichar(trim(lines(i)))
       if (.not. allocated(mirror)) then
          mirr_len = 0
          do j = 1, len(lines(i))
             if (ichar(lines(i)(j:j)) == 0) exit
             mirr_len = mirr_len + 1
          end do
          allocate(character(len=mirr_len) :: mirror(1)) ! mirror(1) = lines(i)
          mirror(1) = lines(i)(1:mirr_len)
       elseif (ichar(trim(lines(i))) /= 0) then
          call append(mirror, lines(i)(1:mirr_len))
       end if
       if (ichar(trim(lines(i))) == 0 .or. i == size(lines)) then
          print *, "Start processing"
          sc = process_smuged_mirror(mirror)
          print *, "End processing"
          do j = 1, size(mirror)
             print *, mirror(j)
          end do
          print '(A,I5,/)', "Mirror score = ", sc
          score = score + sc
          deallocate(mirror)
       end if
    end do
    

    print *, "============"
    print *, "Result, part2:", score

  end subroutine part2

  function process_mirror(mirror) result(res)
    character(len=*), intent(in) :: mirror(:)
    integer(int64) :: res
    integer :: i, j, jm, row, col
    logical :: found 

    res = 0

    row = 0
    outer : do i = 2, size(mirror)
       if (mirror(i) == mirror(i-1)) then
          row = i
          do j = i+1, size(mirror)
             jm = row - (j - row) - 1
             if (jm < 1) exit outer
             if (mirror(j) /= mirror(jm)) then
                row = 0
                exit
             end if
             if (j == size(mirror)) exit outer
          end do
       end if
    end do outer

    col = 0
    outercol : do i = 2, len(mirror(1))
       if (all(mirror(:)(i:i) == mirror(:)(i-1:i-1))) then
          col = i
          do j = i+1, len(mirror(1))
             jm = col - (j - col) - 1
             if (jm < 1) exit outercol
             if (any(mirror(:)(j:j) /= mirror(:)(jm:jm))) then
                col = 0
                exit
             end if
             if (j == len(mirror(1))) exit outercol
          end do
       end if
    end do outercol

    if (row /= 0) res = res + (row-1)*100
    if (col /= 0) res = res + (col-1)

  end function process_mirror

  subroutine part1(lines)
    character(len=:), allocatable, intent(in) :: lines(:)
    character(len=:), allocatable :: mirror(:)
    integer :: i, j, mirr_len
    integer(int64) :: score, sc

    score = 0
    do i = 1, size(lines)
       ! print *, lines(i), len(trim(lines(i))), ichar(trim(lines(i)))
       if (.not. allocated(mirror)) then
          mirr_len = 0
          do j = 1, len(lines(i))
             if (ichar(lines(i)(j:j)) == 0) exit
             mirr_len = mirr_len + 1
          end do
          allocate(character(len=mirr_len) :: mirror(1)) ! mirror(1) = lines(i)
          mirror(1) = lines(i)(1:mirr_len)
       elseif (ichar(trim(lines(i))) /= 0) then
          call append(mirror, lines(i)(1:mirr_len))
       end if
       if (ichar(trim(lines(i))) == 0 .or. i == size(lines)) then
          sc = process_mirror(mirror)
          do j = 1, size(mirror)
             print *, mirror(j)
          end do
          print '(A,I5,/)', "Mirror score = ", sc
          score = score + sc
          deallocate(mirror)
       end if
    end do
    

    print *, "============"
    print *, "Result, part1:", score

  end subroutine part1

  !> Append a new line to the existing 
  subroutine append(array, vals)
    character(len=:), allocatable, intent(inout) :: array(:)
    character(len=*), intent(in) :: vals
    character(len=:), allocatable :: arr(:)

    allocate(character(len=len(array(1))) :: arr(size(array) + 1))
    arr(1:size(array)) = array
    arr(size(array) + 1) = vals
    deallocate(array)
    call move_alloc(arr, array)
    
  end subroutine append

  subroutine get_filename(filename)
    character(len=:), allocatable, intent(inout) :: filename
    character(len=32) :: arg1

    call get_command_argument(1, arg1)
    filename = trim(arg1)
    if (filename == '') then
       stop "No file specified!"
    end if

  end subroutine get_filename

end program mirrors
