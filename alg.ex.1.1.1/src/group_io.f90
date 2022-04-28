! Copyright (c) Namakete (Ilya Oberemok) <namakete.dev@gmail.com>.
! See the LICENCE file in the repository root for full licence text.
module Group_IO
    use environment

    implicit none

    integer, parameter :: STUD_AMOUNT   = 5
    integer, parameter :: SURNAME_LEN   = 15
    integer, parameter :: INITIALS_LEN  = 5
    integer, parameter :: MARKS_AMOUNT  = 5

    type student
        character(SURNAME_LEN, kind=CH_)    :: Surname              = ""
        character(INITIALS_LEN, kind=CH_)   :: Initials             = ""
        character(kind=CH_)                 :: Sex                  = ""
        integer(I_)                         :: Marks(MARKS_AMOUNT)  = 0
        real(R_)                            :: Aver_mark            = 0
    end type student

    contains
    subroutine Create_data_file(Input_File, Data_File)
        character(*), intent(in)   :: Input_File, data_file

        type(student)              :: stud
        integer                    :: In, Out, IO, i, recl
        character(:), allocatable  :: format

        open (file=Input_File, encoding=E_, newunit=In)
        recl = (SURNAME_LEN + INITIALS_LEN + 1)*CH_ + MARKS_AMOUNT*I_ + R_
        open (file=Data_File, form='unformatted', newunit=Out, access='direct', recl=recl)
        format = '(3(a, 1x), ' // MARKS_AMOUNT // 'i1, f5.2)'
        do i = 1, STUD_AMOUNT
            read (In, format, iostat=IO) stud
            call Handle_IO_status(IO, "reading formatted class list, line " // i)
            write (Out, iostat=IO, rec=i) stud
            call Handle_IO_status(IO, "creating unformatted file with class list, record " // i)
        end do
        close (In)
        close (Out)
    end subroutine Create_data_file


end module Group_IO
