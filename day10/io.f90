module io
  use iso_fortran_env
  implicit none

  private 
  public :: open, num_lines, readline, readlines

contains

  function open(filename) result(u)
    !! Opens a file, returns the fileunit
    character(len=*), intent(in) :: filename
    integer :: u

    open(newunit=u, file=filename, access='stream', action='read', position='asis', &
         status='old', form="formatted")

  end function open

  function num_lines(fileunit) result(nlines)
    !! Returns the number of lines of opened file
    integer, intent(in) :: fileunit
    integer :: nlines, iostat

    rewind(fileunit)
    nlines = 0
    do
       read(fileunit, *, iostat=iostat)
       if (iostat /= 0) exit
       nlines = nlines + 1
    end do
    rewind(fileunit)
  end function num_lines

  function readlines(file) result(lines)
    !! Read all the lines from file and return array of lines
    integer, intent(in) :: file
    character(len=:), allocatable :: lines(:)
    character(len=:), allocatable :: line
    integer :: i, nlines, maxlen

    nlines = num_lines(file)
    maxlen = 0
    do i = 1, nlines
       call readline(file, line)
       maxlen = max(maxlen, len(line))
    end do
    rewind(file)
    allocate(character(len=maxlen) :: lines(nlines))
    do i = 1, nlines
       call readline(file, line)
       lines(i)(1:len(line)) = line
    end do

  end function readlines

  subroutine readline(fileunit, line)
    !! Read line from file
    integer, intent(in) :: fileunit
    character(len=:), allocatable, intent(out) :: line
    character(len=4096) :: buffer, msg
    integer :: chunk, stat
    logical :: opened

    line = ""
    if (fileunit /= -1) then
       inquire(unit=fileunit, opened=opened)
    else
       opened = .false.
    end if

    if (opened) then
       open(unit=fileunit, pad='yes' ,iostat=stat, iomsg=msg)
    end if
    do while (stat == 0)
       read(fileunit, '(a)', advance='no', iostat=stat, iomsg=msg, size=chunk) buffer
       if (stat > 0) exit
       line = line // buffer(:chunk)
    end do

  end subroutine readline

end module io
