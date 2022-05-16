! Copyright (c) Namakete (Ilya Oberemok) <namakete.dev@gmail.com>.
! See the LICENCE file in the repository root for full licence text.  
module List_IO
    use Environment

    implicit none

    type node
        character(:, CH_), allocatable  :: value
        type(node), pointer             :: next  => Null()
    end type node
    type(node), pointer                 :: head => Null()

contains
    subroutine Read_sorted_list(Input_File, List)
        type(node), pointer     :: List
        character(*), intent(in):: Input_File
        integer  In

        open (file=Input_File, encoding=E_, newunit=In)
        call Read_sorted_value(In, List)
        close (In)
    end subroutine Read_sorted_list

    recursive subroutine Read_sorted_value(In, list)
        type(node), pointer     :: List
        integer, intent(in)     :: In
        integer, parameter      :: max_len = 15
        character(max_len, CH_) :: string
        integer  :: IO

        allocate (List)
        
        read (In, "(a)", iostat=IO) string
        call Handle_IO_status(IO, "reading value from file")
        if (IO == 0) then
            List%value = Trim(string)
            call Read_sorted_value(In, List%next)
        else
            deallocate (List)
        end if
    end subroutine Read_sorted_value

    subroutine output_list(outputfile, code)
        character(*), intent(in):: outputfile
        type(node), intent(in)  :: code
        integer  :: out

        open (file=outputfile, encoding=e_, newunit=out)
        call output_source_line(out, code)
        close (out)
    end subroutine output_list

    recursive subroutine output_source_line(out, code)
        integer, intent(in)     :: out
        type(node), intent(in)  :: code
        integer  :: io

        write (out, "(a)", iostat=io) code%value
        call handle_io_status(io, "writing line to file")
        if (associated(code%next)) &
            call output_source_line(out, code%next)
    end subroutine output_source_line

    subroutine output_list2(outputfile, code)
        character(*), intent(in):: outputfile
        type(node), intent(in)  :: code
        integer  :: out

        open (file=outputfile, encoding=e_, newunit=out)
            call output_source_line2(out, code)
        close (out)
    end subroutine output_list2

    recursive subroutine output_source_line2(out, code)
        integer, intent(in)     :: out
        type(node), intent(in)  :: code
        integer                 :: io

        write (out, "(a)", iostat=io) code%value
        call handle_io_status(io, "writing line to file")
        if (associated(code%next)) &
            call output_source_line2(out, code%next)
    end subroutine output_source_line2
end module List_IO
