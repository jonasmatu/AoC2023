program calibration
  use iso_fortran_env
  implicit none

  character(len=*), parameter :: filename = "input.txt"
  character(len=:), allocatable :: input(:)
  integer :: i, a, sum
  character(len=2) :: c

  sum = 0
  call read_file(filename, input, 1)

  do i = 1, size(input)
     print *, get_numbers(input(i)), input(i)
     sum = sum + get_numbers(input(i))
  end do

  print *, "==========================="
  print *, "Calibration sum: ", sum
  
contains

  integer function num_records(filename)
    ! Return the number of records (lines) of a text file.
    character(len=*), intent(in) :: filename
    integer :: fileunit
    open(newunit=fileunit, file=filename)
    num_records = 0
    do
      read(unit=fileunit, fmt=*, end=1)
      num_records = num_records + 1
    end do
    1 continue
    close(unit=fileunit)
  end function num_records

  subroutine read_file(filename, data, cols)
    character(len=*), intent(in) :: filename
    integer(int32), intent(in) :: cols
    character(len=:), allocatable, intent(inout) :: data(:)
    integer :: fileunit, n, nm
    nm = num_records(filename)

    if (allocated(data)) deallocate(data)
    allocate(character(len=100) :: data(nm))

    open(newunit=fileunit, file=filename)
    do n = 1, nm
       read(fileunit, fmt=*, end=1) data(n)
    end do
    1 close(fileunit)
    
  end subroutine read_file

  function len_string(string) result(res)
    character(len=*), intent(in) :: string
    integer(int32) :: res
    res = len(string)

  end function len_string

  function get_numbers(string) result(res)
    character(len=*), intent(in) :: string
    integer(int32) :: res
    character(len=1) :: c
    integer(int32) :: i, first, last, n
    n = len(string)

    first = -1
    last = -1

    do i = 1, n
       c = string(i:i)
       if ((ichar(c) <= 57) .and. (ichar(c) >= 48)) then
          if (first == -1) then
             ! first = 1
             read(c,'(I1)') first
          else
             ! last = 2
             read(c, '(I1)') last
          end if
       end if
    end do
    if (last == -1) last = first

    res = first*10 + last
    

  end function get_numbers

end program calibration
