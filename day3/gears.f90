program gears
  use iso_fortran_env
  implicit none

  character(len=:), allocatable :: filename
  character(len=52) :: line_length
  character(len=:), allocatable :: raw_data(:)
  integer, allocatable :: data(:,:)
  integer :: i, gear_num, line_len

  call get_filename(filename)


  call get_command_argument(2, line_length)
  read (line_length, *) line_len
  

  call read_file(filename, raw_data, 1, line_len)

  call get_data(raw_data, data)


  ! print '(a)', 
  ! do i = 1, size(raw_data)
  !    print '(a)', raw_data(i)
  ! end do


  ! do i = 1, size(data, 1)
  !    print "(12I2)", data(i,:)
  ! end do

  gear_num = get_gears(data)

  print *, "================="
  print *, "Result = ", gear_num

contains

  function get_gears(data) result(gears)
    integer, allocatable, intent(inout) :: data(:,:)
    integer :: gears
    integer :: i, j

    gears = 0
    
    do i = 2, size(data, 1) - 1
       do j = 2, size(data, 2) -1
          if (data(i,j) == -2) then
             gears = gears + get_sourrounding_numbers(i, j, data)
          end if
       end do
    end do
    

  end function get_gears

  function get_sourrounding_numbers(i,j, data) result(num)
    integer, intent(in) :: i, j
    integer, allocatable, intent(inout) :: data(:,:)
    integer, allocatable :: temp_data(:,:)
    integer :: n, num, numbs(2)

    allocate(temp_data(3,size(data,2)))

    temp_data(:,:) = data(i-1:i+1,:)
    
    numbs = [0, 0]
    num = 0
    n = 0
    if (temp_data(1,j-1) > -1) then
       n = n + 1
       if (n < 3) numbs(n) = get_number_line(1, j-1, temp_data)
    end if
    if (temp_data(1,j) > -1) then
       n = n + 1
       if (n < 3) numbs(n) = get_number_line(1, j, temp_data)
    end if
    if (temp_data(1,j+1) > -1) then
       n = n + 1
       if (n < 3) numbs(n) = get_number_line(1, j+1, temp_data)
    end if
    if (temp_data(2,j-1) > -1) then
       n = n + 1
       if (n < 3) numbs(n) = get_number_line(2, j-1, temp_data)
    end if
    if (temp_data(2,j+1) > -1) then
       n = n + 1
       if (n < 3) numbs(n) = get_number_line(2, j+1, temp_data)
    end if
    if (temp_data(3,j-1) > -1) then
       n = n + 1
       if (n < 3) numbs(n) = get_number_line(3, j-1, temp_data)
    end if
    if (temp_data(3,j) > -1) then
       n = n + 1
       if (n < 3) numbs(n) = get_number_line(3, j, temp_data)
    end if
    if (temp_data(3,j+1) > -1) then
       n = n + 1
       if (n < 3) numbs(n) = get_number_line(3, j+1, temp_data)
    end if

    if (n == 2) then
       num = numbs(1) * numbs(2)
       data(i-1:i+1,:) = temp_data
       print *, "Found adjacent numbers: ", numbs(1), ",", numbs(2)
    end if


  end function get_sourrounding_numbers

  function get_number_line(i, j, data) result(num)
    integer, intent(in) :: i, j
    integer, allocatable, intent(inout) :: data(:,:)
    integer :: num, n, lpos, rpos

    num = 0
    n = j
    lpos = 2
    do while (n > 1)
       n = n - 1
       if (data(i,n) < 0) then
          lpos = n + 1
          exit
       end if
    end do

    n = j
    rpos = size(data, 2) - 1
    do while (n < size(data, 2))
       n = n + 1
       if (data(i,n) < 0) then
          rpos = n - 1
          exit
       end if
    end do

    do n = 1, rpos - lpos + 1
       num = num + 10**((rpos-lpos)-n+1) * data(i,lpos+n-1)
       data(i,lpos+n-1) = -1
    end do

    ! print *, "Found number: ", num
    
  end function get_number_line

  subroutine get_data(raw_data, data)
    character(len=*), intent(in) :: raw_data(:)
    integer, allocatable, intent(inout) :: data(:,:)
    integer :: i, j

    if (allocated(data)) deallocate(data)
    allocate(data(size(raw_data)+2, len(raw_data(1))+2))

    data(:,:) = -1
    
    do i = 1, size(raw_data)
       do j = 1, len(raw_data(1))
          if (raw_data(i)(j:j) == '*') then
             data(i+1,j+1) = -2
          else if ((ichar(raw_data(i)(j:j)) <= 57) .and. (ichar(raw_data(i)(j:j)) >= 48)) then
             read (raw_data(i)(j:j), '(I1)') data(i+1,j+1)
          else
             data(i+1,j+1) = -1
          end if
       end do
    end do

  end subroutine get_data

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

  subroutine read_file(filename, data, cols, len)
    character(len=*), intent(in) :: filename
    integer(int32), intent(in) :: cols, len
    character(len=:), allocatable, intent(inout) :: data(:)
    integer :: fileunit, n, nm
    nm = num_records(filename)

    if (allocated(data)) deallocate(data)
    allocate(character(len=len) :: data(nm))

    open(newunit=fileunit, file=filename)
    do n = 1, nm
       read(fileunit, fmt="(a)", end=1) data(n)
    end do
    1 close(fileunit)
    
  end subroutine read_file

  subroutine get_filename(filename)
    character(len=:), allocatable, intent(inout) :: filename
    character(len=32) :: arg1
    
    call get_command_argument(1, arg1)
    filename = trim(arg1)

  end subroutine get_filename

end program gears
