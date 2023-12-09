module sorting
  use iso_fortran_env
  implicit none

  private
  public :: sort

  interface sort
     module procedure :: sort_int
  end interface sort

  interface merge_sort
     module procedure :: merge_sort_int
  end interface merge_sort

contains

  function sort_int(array) result(res)
    ! Merge sort algorithm stolen from wikipedia
    integer, intent(in) :: array(:)
    integer, allocatable :: res(:), tmp_res(:)
    integer :: i, width, n

    n = size(array)
    res = array
    tmp_res = res
    width = 1
    do while (width < n)
       i = 0
       do while (i < n)
          call merge_sort(res, tmp_res, i+1, min(i+1+width, n+1), min(i+1+2*width, n+1))
          i = i + 2*width
       end do
       res = tmp_res
       width = 2*width 
    end do

  end function sort_int

  subroutine merge_sort_int(A, B, ileft, iright, iend) 
    integer, intent(in) :: A(:)
    integer, intent(out) :: B(:)
    integer, intent(in) :: ileft, iright, iend
    integer :: i, j, k

    i = ileft
    j = iright
    do k = ileft, iend
       if (i < iright  .and. (j >= iend  .or.  A(i) <= A(j))) then
          B(k) = A(i)
          i = i + 1
       else
          B(k) = A(j)
          j = j + 1
       end if
    end do

  end subroutine merge_sort_int

  

end module sorting
