program splistring
  use iso_fortran_env
  implicit none

  character(len=:), allocatable :: substr(:)
  integer :: i 

  call split_string("Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green", substr, ":")

  do i = 1, size(substr)
     print *, substr(i)
  end do
  
contains

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

end program splistring
