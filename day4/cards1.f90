program cards
  use iso_fortran_env
  use strings
  use io
  implicit none

  character(len=:), allocatable :: filename, line
  integer, allocatable :: win_nums(:), nums(:)
  integer :: i, j, k, file, count, res

  res = 0
  call get_filename(filename)
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
     if (count > 0) then
        res = res +  2**(count - 1)
        print *, count
     end if
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
