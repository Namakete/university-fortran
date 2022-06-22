! Copyright (c) Namakete (Ilya Oberemok) <namakete.dev@gmail.com>.
! See the LICENCE file in the repository root for full licence text.  
module List_IO
    use Environment

    implicit none

    integer, parameter :: SURNAME_LEN = 15
    
    type node
        character(SURNAME_LEN, kind = CH_) :: Value
        type(node), allocatable         :: Next
    end type node

contains
    function Read_sorted_list(Input_File) result(List)
        type(node), allocatable     :: List
        character(*), intent(in):: Input_File
        integer  In

        open (file=Input_File, encoding=E_, newunit=In)
        call Read_sorted_value(In, List)
        close (In)
    end function Read_sorted_list

    recursive subroutine Read_sorted_value(In, list)
        type(node), allocatable     :: List
        integer, intent(in)     :: In
        integer  :: IO

        allocate (List)
        
        read (In, "(a)", iostat=IO) List%Value
        call Handle_IO_status(IO, "reading value from file")
        if (IO == 0) then
            call Read_sorted_value(In, List%next)
        else
            deallocate (List)
        end if
    end subroutine Read_sorted_value

    subroutine output_list(outputfile,position, code)
        character(*), intent(in):: outputfile, position
        type(node), allocatable, intent(in)  :: code
        integer  :: out

        open(file = outputfile, encoding = e_, newunit = out, position = position)
        write(out, '(/1x)')    
        call output_source_line(out, code)
        close (out)
    end subroutine output_list

    recursive subroutine output_source_line(out, code)
        integer, intent(in)     :: out
        type(node), allocatable,intent(in)  :: code
        integer  :: io

        if (allocated(code)) then
            write (out, "(a)", iostat=io) code%value
            call handle_io_status(io, 'writing surname')
            call output_source_line(out, code%next)
        end if
    end subroutine output_source_line
    
    subroutine output_list2(outputfile,position, code)
        character(*), intent(in):: outputfile, position
        type(node), allocatable, intent(in)  :: code
        integer  :: out

        open(file = outputfile, encoding = e_, newunit = out, position = position)
        write(out, '(/1x)')    
        call output_source_line(out, code)
        close (out)
    end subroutine output_list2

    recursive subroutine output_source_line2(out, code)
        integer, intent(in)     :: out
        type(node), allocatable,intent(in)  :: code
        integer  :: io

        if (allocated(code)) then
            write (out, "(a)", iostat=io) code%value
            call handle_io_status(io, 'writing surname')
            call output_source_line(out, code%next)
        end if
    end subroutine output_source_line2
end module List_IO
