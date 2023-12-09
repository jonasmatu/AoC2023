program progressbar
  use iso_fortran_env
  implicit none

  type, public :: ProgressBar
     private
     logical :: init
     logical :: running
     logical :: done
     character(len=255) :: message
     character(len=30) :: progressString
     character(len=20) :: bar
     real :: progress
   contains
     private
     procedure,pass(this),public :: initialize
     procedure,pass(this),public :: reset
     procedure,pass(this),public :: run
     procedure,pass(this),private:: printbar
     procedure,pass(this),private:: updateBar
  end type ProgressBar

  

contains

  subroutine run(this,pct,Ix,msg)
    class(ProgressBar) :: this
    real::pct
    integer, intent(in), optional :: Ix
    character(len=*),intent(in),optional :: msg

    if (.not. this%init) call this%initialize(msg)
    if (.not. this%done) then
       this%running=.true.
       this%progress=pct
       call this%updateBar(Ix)
       call this%printbar()
       if (abs(pct-100.0)<1.0E-6) then
          this%done=.true.
          write(*,'(A6)') "] done"
       end if
    end if

  end subroutine run

  subroutine initialize(this, msg)
    class(ProgressBar) :: this
    character(len=*), intent(in), optional :: msg

  end subroutine initialize

  subroutine updateBar(this, Ix)
    class(ProgressBar) :: this
    
    write(*,trim(fm), advance='NO') achar(13), trim(this%message),trim(adjustl(this%progressString)),'%','[',trim(adjustl(this%bar))

  end subroutine updateBar

end program progressbar
