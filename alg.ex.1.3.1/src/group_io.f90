! Copyright (c) Namakete (Ilya Oberemok) <namakete.dev@gmail.com>.
! See the LICENCE file in the repository root for full licence text.
module Group_IO
    use Environment

    implicit none

    integer, parameter                      :: STUD_AMOUNT = 5, SURNAME_LEN = 15, INITIALS_LEN = 5, MARKS_AMOUNT = 5
    
    type student
        character(SURNAME_LEN, kind=CH_)    :: Surname              = ""
        character(INITIALS_LEN, kind=CH_)   :: Initials             = ""
        character(kind=CH_)                 :: Sex                  = ""
        integer(I_)                         :: Marks(MARKS_AMOUNT)  = 0
        real(R_)                            :: Aver_mark            = 0
    end type student
   
contains
    subroutine Create_Data_File(Input_File, Data_File)
        character(*), intent(in)   :: Input_File, data_file
      
        type(student)              :: Students
        integer                    :: In, Out, IO, i, recl
        character(:), allocatable  :: format
      
        open (file=Input_File, encoding=E_, newunit=In)
            recl = (SURNAME_LEN + INITIALS_LEN + 1)*CH_ + MARKS_AMOUNT*I_ + R_
            open (file=Data_File, form='unformatted', newunit=Out, access='direct', recl=recl)
                format = '(3(a, 1x), ' // MARKS_AMOUNT // 'i1, f5.2)'
                do i = 1, STUD_AMOUNT
                    read (In, format, iostat=IO) Students
                    call Handle_IO_status(IO, "reading formatted class list, line " // i)
                    write (Out, iostat=IO, rec=i) Students
                    call Handle_IO_status(IO, "creating unformatted file with class list, record " // i)
                end do
            close (In)
        close (Out)
    end subroutine Create_Data_File

    function Read_Class_List(File_Data) result(Group)
        type(student)                 Group(STUD_AMOUNT)
        character(*), intent(in)   :: File_Data

        integer In, IO, recl
      
        recl = ((SURNAME_LEN + INITIALS_LEN + 1)*CH_ + MARKS_AMOUNT*I_ + R_) * STUD_AMOUNT
        open (file=File_Data, form='unformatted', newunit=In, access='direct', recl=recl)
            read (In, iostat=IO, rec=1) Group
            call Handle_IO_status(IO, "reading unformatted class list")
        close (In)
    end function Read_Class_List
 
    subroutine Output_Class_List(Output_File, Group, Title, Position, Max_INDEX)
        character(*), intent(in)   :: Output_File, Position, Title
        type(student), intent(in)  :: Group(:)
        integer, intent(in)        :: Max_INDEX(:)
        integer                    :: Out, IO, i
        character(:), allocatable  :: format      
      
        open (file=Output_File, encoding=E_, position=Position, newunit=Out)
            write (out, '(/a)') Title 
            format = '(3(a, 1x), ' // MARKS_AMOUNT // 'i1, f5.2)'
            write (Out, format, iostat= IO ) (Group(Max_INDEX(i)), i = 1, size(Max_INDEX)) 
            call Handle_IO_status(IO, "writing " // Title)
        close (Out)
    end subroutine Output_Class_List

   subroutine Output_Average_List(Output_File, Aver_Mark, Ttile, Position)
        character(*), intent(in)   :: Output_File, Position, Ttile
        real(R_), intent(in)       :: Aver_Mark
        integer                    :: Out
      
        open (file=Output_File, encoding=E_, position=position, newunit=Out)
            write (Out, '(/a, f5.2)')Ttile, Aver_Mark       
        close (Out)
   end subroutine Output_Average_List
end module Group_IO 
