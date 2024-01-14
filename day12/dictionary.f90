!! My sad attempt at implementing a dictionary in fortran, basically through a linked list
!! very inefficient compared to a solution using a hash map though

module dictionary
  use iso_fortran_env
  implicit none

  type dict
     type(dict_entry), pointer :: first => null()
     integer :: length = 0
  end type dict

  type dict_entry
     character(len=256) :: key = " "
     integer :: val = 0
     type(dict_entry), pointer :: next => null()
  end type dict_entry
  
contains

  function length(diction)
    type(dict), intent(in) :: diction
    integer :: length

    length = diction%length

  end function length

  function get(diction, key) result(val)
    type(dict), intent(in) :: diction
    character(len=*), intent(in) :: key
    integer :: val, i
    type(dict_entry), pointer :: ent

    ent => diction%first
    do i = 1, length(diction)
       if (trim(ent%key) == key) then
           val = ent%val
           exit
        end if
       ent => ent%next
    end do

  end function get

  subroutine add(diction, key, val)  
    type(dict), intent(inout) :: diction
    character(len=*), intent(in) :: key
    integer :: val, i
    type(dict_entry), pointer :: ent
    logical :: already_there

    already_there = .false.
    if (length(diction) == 0) then
       allocate(diction%first)
       diction%first%key = key
       diction%first%val = val
       diction%length = 1
    else
       ent => diction%first
       do i = 1, diction%length - 1
          if (ent%key == trim(key)) then ! checks if key present in dict
             already_there = .true.
             exit
          end if
          ent => ent%next
       end do
       if (.not. already_there) then
          allocate(ent%next)
          ent%key = key
          ent%val = val
          diction%length = diction%length + 1
       end if
    end if

  end subroutine add
  

end module dictionary
