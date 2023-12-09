program cards
  use iso_fortran_env
  use strings
  use io
  implicit none

  character(len=:), allocatable :: filename, line
  character(len=52) :: arg2
  integer, allocatable :: win_nums(:), nums(:)
  integer :: i, j, k, file, count, res, nwins, copy
  integer, allocatable :: copies(:), tmp_copies(:)

  res = 0
  call get_filename(filename)
  call get_command_argument(2, arg2)
  read (arg2, *) nwins

  copies = [(1, i=1,nwins)]
  tmp_copies = [(1, i=1,nwins)]
  
  file = open(filename)
  do i = 1, num_lines(file)
     call readline(file, line)
     call process_line(line, win_nums, nums)
     count = 0
     do j = 1, size(win_nums)
        
        do k = 1, size(nums)
           if (win_nums(j) == nums(k)) count = count + 1
        end do
     end do
     copy = copies(1)
     if (count > 0) then
        do k = 1, count
           tmp_copies(k) = tmp_copies(k) + 1*copy
        end do
     end if
     print *, "Cards = ", i, "copies = ", copy, " count =", count
     res = res + 1 * copy
     copies = tmp_copies
     tmp_copies(1:nwins-1) = tmp_copies(2:nwins)
     tmp_copies(nwins) = 1
  end do

  print *, "============="
  print *, "Result: ", res

contains

  subroutine process_line(line, win_nums, nums)
    character(len=*), intent(in) :: line
    integer, allocatable, intent(out) :: win_nums(:), nums(:)
    character(len=:), allocatable :: substr1(:), substr2(:), substr3(:)
    integer :: i

    call split(line, substr1, ':')
    call split(substr1(2), substr2, '|')
    call split(substr2(1), substr3, ' ')

    allocate(win_nums(size(substr3)))
    
    do i = 1, size(substr3)
       win_nums(i) = to_numeric(substr3(i))
    end do
    call split(substr2(2), substr3, ' ')
    allocate(nums(size(substr3)))
    do i = 1, size(substr3)
       nums(i) = to_numeric(substr3(i))
    end do

  end subroutine process_line

  subroutine get_filename(filename)
    character(len=:), allocatable, intent(inout) :: filename
    character(len=32) :: arg1
    
    call get_command_argument(1, arg1)
    filename = trim(arg1)
    if (filename == '') then
       stop "No file specified!"
    end if

  end subroutine get_filename
  

end program cards
