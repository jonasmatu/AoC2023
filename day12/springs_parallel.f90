program springs_parallel
  use iso_fortran_env
  use io
  use strings
  implicit none

  character(len=:), allocatable :: filename, mylines(:)
  integer :: a[*]
  integer :: file, i, j

  

  if (num_images() /= 2) error stop 'Error: This program must be run on 2 images'
  a = 0


  call get_filename(filename)
  file = open(filename)
  mylines = readlines(file)
  close(file)
  

  if (this_image() == 1) then
     a = 1
     print *, 'Image ', this_image(), ' now has value', a
     print *, 'Image ', this_image(), ' sending new value to image 2.'
     a[2] = 2 * a 
  end if

  sync all

  if (this_image() == 2) then
     print *, 'Image ', this_image(), ' now has value ', a
     print *, 'Image ', this_image(), ' sending new value to image 1.'
     a[1] = 2 * a
  end if

  sync all

  if (this_image() == 2) print *, 'Image ', this_image(), &
       ' sees that image 1 has now value ', a[1]

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

end program springs_parallel
