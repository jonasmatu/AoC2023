module strings
  use iso_fortran_env
  implicit none

  private
  public :: is_numeric, to_numeric, split, count_char

contains

  function count_char(string, char) result(res)
    character(len=*), intent(in) :: string
    character(len=1), intent(in) :: char
    integer :: res, i

    res = 0
    do i = 1, len(string)
       if (string(i:i) == char) res = res + 1
    end do

  end function count_char

  function is_numeric(c) result(res)
    character, intent(in) :: c
    logical :: res
    integer :: i
    res = .true.
    do i = 1, len(c)
       if (.not. (c >= '0' .and. c <= '9')) res = .false.
    end do
    
  end function is_numeric

  function to_numeric(string) result(res)
    character(len=*), intent(in) :: string
    character(len=:), allocatable :: str
    integer(int64) :: res, n, i, val

    res = 0
    str = trim(string)
    n = len(str)
    do i = 1, n
       if (.not. is_numeric(str(i:i))) exit
       read (str(i:i), '(I1)') val
       res = res + 10**(n-(i)) * val
    end do
  end function to_numeric

  subroutine split(string, substr, delimiters, order,nulls)
    !! Stolen from fortran stdlib
    character(len=*),intent(in) :: string  !! input string to tokenize
    character(len=*),optional,intent(in)     :: delimiters  !! list of delimiter characters
    character(len=*),optional,intent(in)     :: order       !! order of output array sequential|[reverse|right]
    character(len=*),optional,intent(in)     :: nulls       !! return strings composed of delimiters or not ignore|return|ignoreend
    character(len=:),allocatable,intent(out) :: substr(:)    !! output array of tokens

    integer                       :: n                      ! max number of strings INPUT_LINE could split into if all delimiter
    integer,allocatable           :: ibegin(:)              ! positions in input string where tokens start
    integer,allocatable           :: iterm(:)               ! positions in input string where tokens end
    character(len=:),allocatable  :: dlim                   ! string containing delimiter characters
    character(len=:),allocatable  :: ordr                   ! string containing order keyword
    character(len=:),allocatable  :: nlls                   ! string containing nulls keyword
    integer                       :: ii,iiii                ! loop parameters used to control print order
    integer                       :: icount                 ! number of tokens found
    integer                       :: ilen                   ! length of input string with trailing spaces trimmed
    integer                       :: i10,i20,i30            ! loop counters
    integer                       :: icol                   ! pointer into input string as it is being parsed
    integer                       :: idlim                  ! number of delimiter characters
    integer                       :: ifound                 ! where next delimiter character is found in remaining input string data
    integer                       :: inotnull               ! count strings not composed of delimiters
    integer                       :: ireturn                ! number of tokens returned
    integer                       :: imax                   ! length of longest token

    ! decide on value for optional DELIMITERS parameter
    if (present(delimiters)) then                                     ! optional delimiter list was present
        if(delimiters/='')then                                       ! if DELIMITERS was specified and not null use it
            dlim=delimiters
        else                                                           ! DELIMITERS was specified on call as empty string
            dlim=' '//char(9)//char(10)//char(11)//char(12)//char(13)//char(0) ! use default delimiter when not specified
        endif
    else                                                              ! no delimiter value was specified
        dlim=' '//char(9)//char(10)//char(11)//char(12)//char(13)//char(0)    ! use default delimiter when not specified
    endif
    idlim=len(dlim)                                                   ! dlim a lot of blanks on some machines if dlim is a big string

    if(present(order))then; ordr=adjustl(order); else; ordr='sequential'; endif ! decide on value for optional ORDER parameter
    if(present(nulls))then; nlls=adjustl(nulls); else; nlls='ignore'    ; endif ! optional parameter

    n=len(string)+1                        ! max number of strings INPUT_LINE could split into if all delimiter
    allocate(ibegin(n))                        ! allocate enough space to hold starting location of tokens if string all tokens
    allocate(iterm(n))                         ! allocate enough space to hold ending location of tokens if string all tokens
    ibegin(:)=1
    iterm(:)=1

    ilen=len(string)                                           ! ILEN is the column position of the last non-blank character
    icount=0                                                       ! how many tokens found
    inotnull=0                                                     ! how many tokens found not composed of delimiters
    imax=0                                                         ! length of longest token found

    select case (ilen)

    case (0)                                                      ! command was totally blank

    case default                                                   ! there is at least one non-delimiter in INPUT_LINE if get here
        icol=1                                                      ! initialize pointer into input line
        INFINITE: do i30=1,ilen,1                                   ! store into each substr element
            ibegin(i30)=icol                                         ! assume start new token on the character
            if(index(dlim(1:idlim),string(icol:icol))==0)then  ! if current character is not a delimiter
            iterm(i30)=ilen                                       ! initially assume no more tokens
            do i10=1,idlim                                        ! search for next delimiter
                ifound=index(string(ibegin(i30):ilen),dlim(i10:i10))
                IF(ifound>0)then
                    iterm(i30)=min(iterm(i30),ifound+ibegin(i30)-2)
                endif
            enddo
            icol=iterm(i30)+2                                     ! next place to look as found end of this token
            inotnull=inotnull+1                                   ! increment count of number of tokens not composed of delimiters
            else                                                     ! character is a delimiter for a null string
            iterm(i30)=icol-1                                     ! record assumed end of string. Will be less than beginning
            icol=icol+1                                           ! advance pointer into input string
            endif
            imax=max(imax,iterm(i30)-ibegin(i30)+1)
            icount=i30                                               ! increment count of number of tokens found
            if(icol>ilen)then                                     ! no text left
            exit INFINITE
            endif
        enddo INFINITE

    end select

    select case (trim(adjustl(nlls)))
    case ('ignore','','ignoreend')
        ireturn=inotnull
    case default
        ireturn=icount
    end select
    allocate(character(len=imax) :: substr(ireturn))                ! allocate the substr to return
    !allocate(substr(ireturn))                                       ! allocate the substr to turn

    select case (trim(adjustl(ordr)))                              ! decide which order to store tokens
    case ('reverse','right') ; ii=ireturn ; iiii=-1                ! last to first
    case default             ; ii=1       ; iiii=1                 ! first to last
    end select

    do i20=1,icount                                                ! fill the substr with the tokens that were found
        if(iterm(i20)<ibegin(i20))then
            select case (trim(adjustl(nlls)))
            case ('ignore','','ignoreend')
            case default
            substr(ii)=' '
            ii=ii+iiii
            end select
        else
            substr(ii)=string(ibegin(i20):iterm(i20))
            ii=ii+iiii
        endif
    enddo
  end subroutine split

end module strings

