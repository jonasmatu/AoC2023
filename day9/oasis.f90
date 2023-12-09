program oasis
  use iso_fortran_env
  use io
  use strings
  implicit none

  character(len=:), allocatable :: filename, line, substr(:)
  integer :: file
  integer(int64), allocatable :: data(:)
  integer(int64) :: pval, bval, res1, res2
  integer :: i,j

  call get_filename(filename)

  file = open(filename)
  res1 = 0
  res2 = 0
  do i = 1, num_lines(file)
     call readline(file, line)
     call split(line, substr, " ")
     if (allocated(data)) deallocate(data)
     allocate(data(size(substr)))
     do j = 1, size(substr)
        read(substr(j), *) data(j)
     end do
     pval = predict_value(data)
     bval = predict_value_back(data)
     print "(I3,A,I3)", bval, line, pval
     res1 = res1 + pval
     res2 = res2 + bval
  end do
  close(file)

  print *, "==============="
  print *, "Result 1: ", res1
  print *, "Result 2: ", res2

contains

  recursive function predict_value(data) result(res)
    integer(int64), intent(in) :: data(:)
    integer(int64), allocatable :: diff_data(:)
    integer(int64) :: res, pval

    res = 0
    diff_data = data(2:size(data)) - data(1:size(data))
    if (.not. all(diff_data == 0)) then
       pval = predict_value(diff_data)
       res = data(size(data)) + pval
    else
       res = data(size(data))
    end if

  end function predict_value

  recursive function predict_value_back(data) result(res)
    integer(int64), intent(in) :: data(:)
    integer(int64), allocatable :: diff_data(:)
    integer(int64) :: res, pval

    res = 0
    diff_data = data(2:size(data)) - data(1:size(data))
    if (.not. all(diff_data == 0)) then
       pval = predict_value_back(diff_data)
       res = data(1) - pval
    else
       res = data(1)
    end if

  end function predict_value_back
  
  subroutine get_filename(filename)
    character(len=:), allocatable, intent(inout) :: filename
    character(len=32) :: arg1
    
    call get_command_argument(1, arg1)
    filename = trim(arg1)
    if (filename == '') then
       stop "No file specified!"
    end if

  end subroutine get_filename

end program oasis
