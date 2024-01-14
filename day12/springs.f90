program strings
  use iso_fortran_env
  use io
  use strings
  use dictionary
  implicit none

  

  character(len=:), allocatable :: filename
  character(len=:), allocatable :: lines(:)
  integer :: file, j
  integer(int64) :: res, i

  call get_filename(filename)
  file = open(filename)
  lines = readlines(file)
  close(file)


  print *, arr_to_string([1,2,3])
 
  call part2(lines, res)
 

  

contains

  subroutine part2(lines, res)
    character(len=*), intent(in) :: lines(:)
    integer(int64), intent(out) :: res
    character(len=:), allocatable :: substr(:), ssubstr(:), springs, springs_unf
    integer, allocatable :: contg(:), unf_contg(:)
    integer :: i, j
    integer(int64) :: tres, ncontg
    type(dict) :: cache

    res = 0
    do i = 1, size(lines)
       call split(lines(i), substr, " ")
       springs = trim(substr(1))
       call split(substr(2), ssubstr, ",")
       ncontg = size(ssubstr)
       if (allocated(contg)) deallocate(contg)
       allocate(contg(ncontg))
       do j = 1, ncontg
          read (ssubstr(j),*) contg(j)
       end do
       if (allocated(unf_contg)) deallocate(unf_contg)
       allocate(unf_contg(ncontg*5))
       unf_contg(1:ncontg) = contg
       unf_contg(ncontg*1+1:ncontg*5) = contg
       unf_contg(ncontg*2+1:ncontg*5) = contg
       unf_contg(ncontg*3+1:ncontg*5) = contg
       unf_contg(ncontg*4+1:ncontg*5) = contg
       springs_unf = repeat(springs // "?" , 5)
       springs_unf = springs_unf(1:len(springs_unf)-1)
       ! print *, len(springs_unf), springs_unf
       ! print *, size(unf_contg), unf_contg
       tres = find_combis(springs_unf, unf_contg, cache)
       ! tres = find_combis(springs, contg)
       print *, "line", i, ":", tres
       res = res + tres
    end do

    print *, "==============="
    print *, "Result part 2:", res

  end subroutine part2

  ! stuff
  recursive function find_combis(springs, contg, cache) result(nconf)
    character(len=*), intent(in) :: springs
    integer, intent(in) :: contg(:)
    integer(int64) :: nconf
    integer :: i, j, nbr, c
    logical :: exact_match
    type(dict), intent(inout) :: cache

    nbr = count_char(springs, "#")
    nconf = 0
    c = 0
    ! print *, springs
    outer : do i = 1, len(springs) - (sum(contg+1) - 2)
       exact_match = .true.
       if (springs(i:i) == ".") cycle outer
       do j = i, i+contg(1)-1
          if (springs(j:j) == ".") then
             if (count_char(springs(i:j), "#") > 0) exit outer
             cycle outer
          end if
          if (springs(j:j) == "?") then
             exact_match = .false.
             ! print *, "Found exact match"
          end if
       end do
       if (i+contg(1) < len(springs)) then
          if (springs(i+contg(1):i+contg(1)) == "#" .and. &
               springs(i:i) == "#") exit outer
          if (springs(i+contg(1):i+contg(1)) == "#") cycle outer
       end if 
       if (size(contg) > 1) then
          c = get(cache, springs(i+contg(1)+1:) // arr_to_string(contg(2:)))
          if (c == 0) then
             nconf = nconf + find_combis(springs(i+contg(1)+1:len(springs)), contg(2:), cache)
             call add(cache, springs(i+contg(1)+1:) // arr_to_string(contg(2:)), c)
          else
             nconf = nconf + c
          end if 
       elseif (count_char(springs(i+contg(1):len(springs)), "#") == 0) then
          nconf = nconf + 1
          ! print *, "Found comb"
          if (nbr - contg(1) >= 0) exit outer
       end if
       if (exact_match) exit outer
       if (springs(i:i) == "#") exit outer
    end do outer
  end function find_combis

  function arr_to_string(arr) result(str)
    integer, intent(in) :: arr(:)
    integer :: i
    character(len=:), allocatable :: str
    character(len=8) :: tmp_str

    allocate(character(len=512) :: str)
    str = ""
    write (tmp_str, "(I2)") arr(1)
    str = str // trim(adjustl(tmp_str))
    do i = 2, size(arr)
       write (tmp_str, "(I2)") arr(i)
       str = str // "," // trim(adjustl(tmp_str))
    end do

    str = trim(str)

  end function arr_to_string

  subroutine part1(lines)
    character(len=*), intent(in) :: lines(:)
    character(len=:), allocatable :: substr(:), ssubstr(:), springs
    integer, allocatable :: contg(:)
    integer :: i, j, ncontg
    integer :: res, tres

    res = 0
    do i = 1, size(lines)
       call split(lines(i), substr, " ")
       springs = trim(substr(1))
       call split(substr(2), ssubstr, ",")
       ncontg = size(ssubstr)
       if (allocated(contg)) deallocate(contg)
       allocate(contg(ncontg))
       do j = 1, ncontg
          read (ssubstr(j),*) contg(j)
       end do
       tres = process_line(springs, contg)
       print *, tres
       res = res + tres
    end do


    print *, "==============="
    print *, "Result:", res

  end subroutine part1

  function process_line(springs, contg) result(nconf)
    character(len=*), intent(in) :: springs
    integer, intent(in) :: contg(:)
    integer :: nconf
    integer :: nqs, nbr, b
    integer :: i, j, k, m, n, l
    character(len=:), allocatable :: arr

    ! Convention: size of arrangement counts dots on the right #. = 2
    nqs = count_char(springs, '?')
    nbr = count_char(springs, '#')

    nconf = 0
    ! Only one possible config
    ! if (len(springs) == (sum(contg) + size(contg) - 1)) nconf = 1

    if (size(contg) == 2) then
       do i = 1, len(springs) - (sum(contg + 1) - 2)
          do j = i+contg(1)+1, len(springs) - contg(2)+1
             arr = ""
             arr = arr // repeat(".", i - 1) // repeat("#", contg(1)) // "." &
                  // repeat(".", j - (i+contg(1)+1)) // repeat("#", contg(2)) // &
                  repeat(".", len(springs) - (j+contg(2)-1))
             nconf = nconf + is_compat(springs, arr)
             if (len(springs) /= len(arr)) stop "Wrong length"
             ! print *, arr
          end do
       end do
    elseif (size(contg) == 3) then
       do i = 1, len(springs) - (sum(contg + 1) - 2)
          do j = i + contg(1)+1, len(springs) - (sum(contg(2:) + 1) - 2)
             do k = j+contg(2)+1, len(springs) - contg(3) + 1
                arr = ""
                arr = arr // repeat(".", i - 1) // repeat("#", contg(1)) // "." &
                     // repeat(".", j - (i+contg(1)+1)) // repeat("#", contg(2)) // "." &
                     // repeat(".", k - (j+contg(2)+1)) // repeat("#", contg(3)) // &
                     repeat(".", len(springs) - (k + contg(3)-1))
                ! print *, nconf
                nconf = nconf + is_compat(springs, arr)
                ! print *, nconf
                if (len(springs) /= len(arr)) stop "Wrong length"
                ! print *, springs
                ! print *, arr
             end do
          end do
       end do
    elseif (size(contg) == 4) then
       do i = 1, len(springs) - (sum(contg + 1) - 2)
          do j = i + contg(1)+1, len(springs) - (sum(contg(2:) + 1) - 2)
             do k = j + contg(2)+1, len(springs) - (sum(contg(3:) + 1) - 2)
                 do m = k + contg(3)+1, len(springs) - contg(4) +1 
                   arr = repeat(".", i-1) // repeat("#", contg(1)) // "." &
                        // repeat(".", j-(i+contg(1)+1)) // repeat("#", contg(2)) // "." &
                        // repeat(".", k-(j+contg(2)+1)) // repeat("#", contg(3)) // "." &
                        // repeat(".", m-(k+contg(3)+1)) // repeat("#", contg(4)) // &
                        repeat(".", len(springs) - (m + contg(4)-1))
                   ! print *, arr
                   nconf = nconf + is_compat(springs, arr)
                   ! print *, nconf
                   if (len(springs) /= len(arr)) stop "Wrong length"
                end do
             end do
          end do
       end do
    elseif (size(contg) == 5) then
       do i = 1, len(springs) - (sum(contg + 1) - 2)
          do j = i+contg(1)+1, len(springs) - (sum(contg(2:) + 1) - 2)
             do k = j+contg(2)+1, len(springs) - (sum(contg(3:) + 1) - 2)
                do m = k+contg(3)+1, len(springs) - (sum(contg(4:) + 1) - 2)
                   do n = m+contg(4)+1, len(springs) - contg(5)+1
                      arr = ""
                      arr = arr // repeat(".", i - 1) // repeat("#", contg(1)) // "." &
                           // repeat(".", j - (i+contg(1)+1)) // repeat("#", contg(2)) // "." &
                           // repeat(".", k - (j+contg(2)+1)) // repeat("#", contg(3)) // "." &
                           // repeat(".", m - (k+contg(3)+1)) // repeat("#", contg(4)) // "." &
                           // repeat(".", n - (m+contg(4)+1)) // repeat("#", contg(5)) // &
                           repeat(".", len(springs) - (n + contg(5)-1))
                      nconf = nconf + is_compat(springs, arr)
                      if (len(springs) /= len(arr)) stop "Wrong length"
                   end do
                end do
             end do
          end do
       end do
    elseif (size(contg) == 6) then
       do i = 1, len(springs) - (sum(contg + 1) - 2)
          do j = i+contg(1)+1, len(springs) - (sum(contg(2:) + 1) - 2)
             do k = j+contg(2)+1, len(springs) - (sum(contg(3:) + 1) - 2)
                do m = k+contg(3)+1, len(springs) - (sum(contg(4:) + 1) - 2)
                   do n = m+contg(4)+1, len(springs) - (sum(contg(5:) + 1) - 2)
                      do l = n+contg(5)+1, len(springs) - contg(6) + 1
                         arr = ""
                         arr = arr // repeat(".", i - 1) // repeat("#", contg(1)) // "." &
                              // repeat(".", j - (i+contg(1)+1)) // repeat("#", contg(2)) // "." &
                              // repeat(".", k - (j+contg(2)+1)) // repeat("#", contg(3)) // "." &
                              // repeat(".", m - (k+contg(3)+1)) // repeat("#", contg(4)) // "." &
                              // repeat(".", n - (m+contg(4)+1)) // repeat("#", contg(5)) // "." &
                              // repeat(".", l - (n+contg(5)+1)) // repeat("#", contg(6)) // &
                              repeat(".", len(springs) - (l + contg(6)-1))
                         nconf = nconf + is_compat(springs, arr)
                         ! print *, springs
                         ! print *, arr
                         if (len(springs) /= len(arr)) stop "Wrong length"
                      end do
                   end do
                end do
             end do
          end do
       end do
    end if

    ! if (nconf == 0) nconf = 1
    
  end function process_line

  function is_compat(springs, arr) result(res)
    ! ! Function checks if arr(angement) is compatible with given springs
    character(len=*), intent(in) :: springs, arr
    integer :: res
    integer :: i

    res = 1
    do i = 1, len(springs)
       if (springs(i:i) /= '?' .and. springs(i:i) /= arr(i:i)) then
          res = 0
          exit
       end if
    end do

  end function is_compat

  subroutine get_filename(filename)
    character(len=:), allocatable, intent(inout) :: filename
    character(len=32) :: arg1

    call get_command_argument(1, arg1)
    filename = trim(arg1)
    if (filename == '') then
       stop "No file specified!"
    end if

  end subroutine get_filename

end program
