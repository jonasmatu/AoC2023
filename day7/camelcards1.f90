program camelcards
  use iso_fortran_env
  use strings
  use io
  implicit none

  character(len=:), allocatable :: filename, line, substr(:)
  character(len=:), allocatable :: cards
  character(len=5), allocatable :: hand(:), sortedhand(:)
  integer, allocatable :: bid(:), nums(:)
  integer :: n
  integer :: file, nlines
  integer :: i
  integer :: res(1)

  cards = "23456789TJQKA"

  call get_filename(filename)
  file = open(filename)
  nlines = num_lines(file)

  allocate(hand(nlines))
  allocate(bid(nlines))
  
  do i = 1, nlines
     call readline(file, line)
     call split(line, substr, delimiters=" ")
     hand(i) = substr(1)
     bid(i) = to_numeric(substr(2))
     print *, hand(i), bid(i), get_type(hand(i))
  end do

  sortedhand = sort_cards(hand)
  res(1) = 0
  do i = 1, nlines
     res = res + i* bid(findloc(hand, sortedhand(i)))
  end do
  print *, "============"
  print *, "Result:", res

contains

  function C1_larger_C2(C1, C2) result(res)
    character(len=*), intent(in) :: C1, C2
    logical :: res
    integer :: i

    res = .true.
    if (get_type(C1) == get_type(C2)) then
       do i = 1, len(C1)
          if (index(cards, C1(i:i)) > index(cards, C2(i:i))) then
             res = .true.
             exit
          elseif (index(cards, C1(i:i)) < index(cards, C2(i:i))) then
             res = .false.
             exit
          end if
       end do
    elseif (get_type(C1) > get_type(C2)) then
       res = .true.
    else
       res = .false.
    end if
    

  end function C1_larger_C2


  function sort_cards(array) result(res)
    ! Merge sort algorithm stolen from wikipedia
    character(len=5), intent(in) :: array(:)
    character(len=5), allocatable :: res(:), tmp_res(:)
    integer :: i, width, n

    n = size(array)
    res = array
    tmp_res = res
    width = 1
    do while (width < n)
       i = 0
       do while (i < n)
          call merge_sort_cards(res, tmp_res, i+1, min(i+1+width, n+1), min(i+1+2*width, n+1))
          i = i + 2*width
       end do
       res = tmp_res
       width = 2*width 
    end do

  end function sort_cards

  subroutine merge_sort_cards(A, B, ileft, iright, iend) 
    character(len=5), intent(in) :: A(:)
    character(len=5), intent(out) :: B(:)
    integer, intent(in) :: ileft, iright, iend
    integer :: i, j, k

    i = ileft
    j = iright
    do k = ileft, iend
       if (i < iright .and. (j >= iend  .or.  C1_larger_C2(A(j),A(i)))) then
          B(k) = A(i)
          i = i + 1
       else
          B(k) = A(j)
          j = j + 1
       end if
    end do
  end subroutine merge_sort_cards


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

  subroutine merge_sort(A, B, ileft, iright, iend) 
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

  end subroutine merge_sort

  function sort_hands(hands) result(ranks)
    character(len=5), intent(in) :: hands(:)
    integer, allocatable :: ranks(:), types(:)
    integer :: i

    allocate(ranks(size(hands)))
    allocate(ranks(size(types)))

    do i = 1, size(hands)
       types(i) = get_type(hands(i))
    end do
    
  end function sort_hands

  function get_type(hand) result(type)
    character(len=*), intent(in) :: hand
    integer :: type, i,j, kinds(5)

    kinds(:) = 1
    
    do i = 1, 5
       if (kinds(i) > 0) then
          do j = i+1, 5
             if (hand(i:i) == hand(j:j)) then
                kinds(i) = kinds(i) + 1
                kinds(j) = kinds(j) - 1
             end if
          end do
      end if
    end do
    if (maxval(kinds) == 5) then
       type = 6
    elseif (maxval(kinds) == 4) then
       type = 5
    elseif (any(kinds == 3) .and. any(kinds == 2)) then
       type = 4
    elseif (maxval(kinds) == 3) then
       type = 3
    elseif (maxval(kinds) == 1) then
       type = 0
    elseif (count(kinds == 2) == 2) then
       type = 2
    else
       type = 1
    end if
    
  end function get_type

  subroutine get_filename(filename)
    character(len=:), allocatable, intent(inout) :: filename
    character(len=32) :: arg1
    
    call get_command_argument(1, arg1)
    filename = trim(arg1)
    if (filename == '') then
       stop "No file specified!"
    end if

  end subroutine get_filename

end program camelcards
