program puzzle
  use iso_fortran_env
  implicit none

  character(len=:), allocatable :: filename
  character(len=:), allocatable :: data(:)
  character(len=:), allocatable :: substr(:)
  character(len=:), allocatable :: str
  integer :: i, res

  call get_filename(filename)

  call read_file(filename, data, 1)

  res = 0
  do i = 1, size(data)
     ! print *, data(i)
     res = res + get_puzzle_number(data(i))
  end do

  print *, "=========================="
  print *, "Result = ", res

  
contains


  function get_puzzle_number(line) result(res)
    character(len=*), intent(in) :: line
    integer :: res, id, i, j, k
    character(len=:), allocatable :: substr(:)
    character(len=:), allocatable :: games(:)
    character(len=:), allocatable :: draws(:)
    character(len=:), allocatable :: cubes(:)
    integer :: g, green, b, blue, r, red
    
    ! find Game ID
    call split_string(line, substr, ":")
    read (substr(1)(6:len(substr(1))), *) id

    res = id

    g = 0
    green = 0
    b = 0
    blue = 0
    r = 0
    red = 0

    ! process three draws
    call split_string(substr(2), games, ";")
    do i = 1, size(games)
       call split_string(games(i), draws, ",")
       do j = 1, size(draws)
          call split_string(draws(j), cubes, " ")
          if (string_contains(cubes(2), "green")) then
             read (cubes(1), *) g
             if (g > green) green = g
          end if
          if (string_contains(cubes(2), "blue")) then
             read (cubes(1), *) b
             if (b > blue) blue = b
          end if
          if(string_contains(cubes(2), "red")) then
             read (cubes(1), *) r
             if (r > red) red = r
          end if
       end do
    end do

    res = red * green * blue
    print *, "ID = ", id, "Power = ", res


  end function get_puzzle_number

  function string_contains(string, substr) result(res)
    character(len=*), intent(in) :: string, substr
    logical :: res

    res = index(string, substr) /= 0
    ! character(len=:), allocatable :: str
    
    ! integer :: i,n

    ! str = trim(string)
    
    ! n = len(substr)
    
    ! if (n > len(str)) then
    !    res = .false.
    ! else
    !    do i = 1, len(str) - n + 1
    !       if (str(i:i+n-1) == substr) res = .true.
    !    end do
    ! end if
    
  end function string_contains

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
    allocate(character(len=256) :: data(nm))

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
    
  
  subroutine split_string(string, substr, delimiter)
    character(len=*), intent(in) :: string
    character(len=:), allocatable, intent(inout) :: substr(:)
    character(len=1), intent(in) :: delimiter

    integer :: i, n, pos_prev, max_len

    n = 1 ! at least one substring (if no delimiter)
    pos_prev = 1
    max_len = 0
    do i = 1, len(string)
       if (string(i:i) == delimiter) then
          if ((i - 1 > pos_prev) .and. i /= len(string) ) n = n + 1 ! neigbouring delimieters
          if (i - pos_prev > max_len) max_len = i - pos_prev
          pos_prev = i
       end if
    end do
    if (len(string) - pos_prev > max_len) max_len = len(string) - pos_prev
    if (string(len(string):len(string)) == delimiter) n = n-1
    if (allocated(substr)) deallocate(substr)
    allocate(character(len=max_len) :: substr(n))
    n = 1
    pos_prev = 0
    if (string(1:1) == delimiter) pos_prev = 1
    do i = 1, len(string)
       if (string(i:i) == delimiter .and. i /= 1) then
          if (i - 1 > pos_prev) then
             substr(n) = string(pos_prev+1:i-1)
             ! print *, substr(n)
             n = n + 1
          end if
          pos_prev = i 
       end if
    end do
    if (string(len(string):len(string)) /= delimiter) substr(n) = string(pos_prev+1:len(string))
    
  end subroutine split_string

  ! subroutine split_string(string, substr, delimiter)
  !   character(len=*), intent(in) :: string
  !   character(len=:), allocatable, intent(inout) :: substr(:)
  !   character(len=1), intent(in) :: delimiter

  !   integer :: i, n, pos_prev, max_len

  !   n = 1 ! at least one substring (if no delimiter)
  !   pos_prev = 1
  !   max_len = 0
  !   do i = 1, len(string)
  !      if (string(i:i) == delimiter) then
  !         n = n + 1
  !         if (i - pos_prev > max_len) max_len = i - pos_prev
  !         pos_prev = i
  !      end if
  !   end do
  !   if (len(string) - pos_prev > max_len) max_len = len(string) - pos_prev
  !   if (allocated(substr)) deallocate(substr)
  !   allocate(character(len=max_len) :: substr(n))

  !   n = 1
  !   pos_prev = 1
  !   do i = 1, len(string)
  !      if (string(i:i) == delimiter) then
  !         substr(n) = string(pos_prev:i-1)
  !         pos_prev = i+1
  !         n = n + 1
  !      end if
  !   end do
  !   substr(n) = string(pos_prev:len(string))
    
  ! end subroutine split_string

end program puzzle
