program race
  use iso_fortran_env
  use io
  use strings
  implicit none

  character(len=:), allocatable :: filename, line, substr(:), str
  real(real64), allocatable :: time, distance
  real(real64) :: tmin , tmax, res
  integer :: file, nlines, nraces
  integer :: i

  call get_filename(filename)
  file = open(filename)

  call readline(file, line)
  call split(line, substr, delimiters=' ')
  nraces = size(substr)-1
  str = ""
  do i = 2, size(substr)
     str = str // trim(substr(i))
  end do
  time = to_numeric(str)
  call readline(file, line)
  call split(line, substr, delimiters=' ')
  str = ""
  do i = 2, size(substr)
     str = str // trim(substr(i))
  end do
  distance = to_numeric(str)
  print *, time
  print *, distance
  
  tmin = ceiling((time - sqrt(time**2 - 4*distance))/2 + 1e-5)
  tmax = floor((time + sqrt(time**2 - 4*distance))/2 - 1e-5)
  res = tmax - tmin + 1
  print *, tmin
  print *, tmax

  print *, "============"
  print *, "Result", res

contains

  subroutine get_filename(filename)
    character(len=:), allocatable, intent(inout) :: filename
    character(len=32) :: arg1
    
    call get_command_argument(1, arg1)
    filename = trim(arg1)
    if (filename == '') then
       stop "No file specified!"
    end if

  end subroutine get_filename
  

end program race
