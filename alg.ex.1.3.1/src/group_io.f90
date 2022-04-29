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

    function Read_class_list(File_Data) result(Group)
        type(student)                 Group(STUD_AMOUNT)
        character(*), intent(in)   :: File_Data

        integer In, IO, recl
      
        recl = ((SURNAME_LEN + INITIALS_LEN + 1)*CH_ + MARKS_AMOUNT*I_ + R_) * STUD_AMOUNT
        open (file=File_Data, form='unformatted', newunit=In, access='direct', recl=recl)
            read (In, iostat=IO, rec=1) Group
            call Handle_IO_status(IO, "reading unformatted class list")
        close (In)
    end function Read_class_list
 
    subroutine Output_class_list(Output_File, Group, List_name, Position, Max_INDEX)
        character(*), intent(in)   :: Output_File, Position, List_name
        type(student), intent(in)  :: Group(:)
        integer, intent(in)        :: Max_INDEX(:)
        integer                    :: Out, IO, i
        character(:), allocatable  :: format      
      
        open (file=Output_File, encoding=E_, position=Position, newunit=Out)
            write (out, '(/a)') List_name 
            format = '(3(a, 1x), ' // MARKS_AMOUNT // 'i1, f5.2)'
            write (Out, format,iostat= IO ) (Group(Max_INDEX(i)), i = 1, size(Max_INDEX)) 
            call Handle_IO_status(IO, "writing " // List_name)
        close (Out)
    end subroutine Output_class_list

   subroutine Output_aver_list(Output_File, Aver_Mark, List_name, Position)
        character(*), intent(in)   :: Output_File, Position, List_name
        real(R_), intent(in)       :: Aver_Mark
        integer                    :: Out
      
        open (file=Output_File, encoding=E_, position=position, newunit=Out)
            write (out, '(/a)') List_name
            write (Out, '(f5.2)') Aver_Mark       
        close (Out)
   end subroutine Output_aver_list
end module Group_IO 
