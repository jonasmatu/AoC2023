program race
  use iso_fortran_env
  use io
  use strings
  implicit none

  character(len=:), allocatable :: filename, line, substr(:)
  real, allocatable :: time(:), distance(:)
  real :: tmin , tmax, res
  integer :: file, nlines, nraces
  integer :: i

  call get_filename(filename)
  file = open(filename)

  call readline(file, line)
  call split(line, substr, delimiters=' ')
  nraces = size(substr)-1
  allocate(time(nraces))
  allocate(distance(nraces))
  do i = 2, size(substr)
     time(i-1) = to_numeric(substr(i))
  end do
  call readline(file, line)
  call split(line, substr, delimiters=' ')
  do i = 2, size(substr)
     distance(i-1) = to_numeric(substr(i))
  end do

  res = 1
  do i = 1, nraces
     tmin = ceiling((time(i) - sqrt(time(i)**2 - 4*distance(i)))/2 + 1e-5)
     tmax = floor((time(i) + sqrt(time(i)**2 - 4*distance(i)))/2 - 1e-5)
     res = res * (tmax - tmin + 1)
  end do

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
