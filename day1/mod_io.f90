module io
    use iso_fortran_env
    implicit none

    private
    public :: read_csv, write_csv

    interface write_csv
       procedure :: write_csv_1d
       procedure :: write_csv_2d
    end interface write_csv

contains

    integer function num_records(filename)
        ! Return the number of records (lines) of a text file.
        character(len=*), intent(in) :: filename
        integer(int32) :: fileunit
        open(newunit=fileunit, file=filename)
        num_records = 0
        do
            read(unit = fileunit, fmt =*, end = 1)
            num_records = num_records + 1
        end do
        1 continue
        close(unit = fileunit)
    end function num_records

    subroutine read_csv(filename, data, cols)
      character(len=*), intent(in) :: filename
      integer(int32), intent(in) :: cols
      real(real64), allocatable, intent(out) :: data(:,:)
      integer(int32) :: fileunit, n, nm

      nm = num_records(filename)

      if (allocated(data)) deallocate(data)
      allocate(data(nm, cols))

      open(newunit=fileunit, file=filename)
      do n = 1, nm
         read(fileunit, fmt =*, end = 1) data(n,:)
      end do
      1 close(fileunit)

    end subroutine read_csv

    ! Write data into a comma separated file
    ! Parameters
    ! ----------
    !    filename : string
    ! ...
    subroutine write_csv_1d(filename, data)
      character(len=*), intent(in) :: filename
      real(real64), intent(in) :: data(:)
      integer :: fileunit, n
      open(newunit=fileunit, file=filename)
      do n = 1, size(data)
         write(fileunit, fmt=*) data(n)
      end do
      close(fileunit)
    end subroutine write_csv_1d
    
    subroutine write_csv_2d(filename, data)
      character(len=*), intent(in) :: filename
      real(real64), intent(in) :: data(:,:)
      integer :: fileunit, n
      open(newunit=fileunit, file=filename)
      do n = 1, size(data(:,0))
         write(fileunit, fmt=*) data(n,:)
      end do
      close(fileunit)
    end subroutine write_csv_2d

end module io
