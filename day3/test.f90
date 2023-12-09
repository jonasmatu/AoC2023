program test
  use io
  use iso_fortran_env
  implicit none

  character(len=:), allocatable :: filename, line
  integer :: i, file

  filename = "input.txt"
  file = open(filename)
  do i = 1, num_lines(file)
     call readline(file, line)
     print *, line
  end do

end program test
