! Copyright (c) Namakete (Ilya Oberemok) <namakete.dev@gmail.com>.
! See the LICENCE file in the repository root for full licence text.
module Source_IO
    use Environment

    implicit none

    type SourceLine
        character(kind=CH_), allocatable   :: String(:)
        type(SourceLine), pointer        :: Next  => Null()
    end type SourceLine

contains
    function Read_Source_Code(InputFile) result (Code)
        type(SourceLine), pointer   :: Code
        character(*), intent(in)    :: InputFile
        integer  :: In

        open (file=InputFile, encoding=E_, newunit=In)
        Code => Read_Source_Line(in)
        close (In)
    end function Read_Source_Code


    recursive function Read_Source_Line(in) result(Code)
        type(SourceLine), pointer   :: Code
        integer, intent(in)         :: In
        integer, parameter          :: max_len = 15
        character(max_len, CH_)     :: string
        integer                     :: IO, i

        read (In, "(a)", iostat=IO) string
        call Handle_IO_Status(IO, "reading line from source code")
        if (IO == 0) then
            allocate (Code)
            Code%String = [(String(i:i),i = 1, len_Trim(string))]
            Code%Next => Read_Source_Line(In)
        else
            Code => Null()
        end if
    end function Read_Source_Line

    subroutine ReadCord(InputFile, N, N1, N2)
        character(*), intent(in):: InputFile
        integer  :: In
        integer, intent(out)    :: N, N1, N2
        character  :: LT

        open (file=InputFile, encoding=E_, newunit=In)
            read(In, '(I1,1X,I1,1X,I1,1X,A1)') N, N1, N2, LT
        close (In)
        if(LT == 'R') N = -N
    end subroutine ReadCord

    subroutine Output_Source_Code(OutputFile, Code, Title)
        character(*), intent(in)      :: OutputFile, Title
        type(SourceLine), intent(in)  :: Code
        integer  :: Out

        open (file=OutputFile, encoding=E_, newunit=Out)
        write (out, '(/a)') Title
        call Output_Source_Line(Out, Code)
        close (Out)
    end subroutine Output_Source_Code

    recursive subroutine Output_Source_Line(Out, Code)
        integer, intent(in)           :: Out
        type(SourceLine), intent(in)  :: Code
        integer  :: IO

        write (Out, "("//size(Code%string)//"a)", iostat=IO) Code%String
        call Handle_IO_Status(IO, "writing line to file")
        if (Associated(Code%next)) &
            call Output_Source_Line(Out, Code%next)
    end subroutine Output_Source_Line
end module Source_IO
