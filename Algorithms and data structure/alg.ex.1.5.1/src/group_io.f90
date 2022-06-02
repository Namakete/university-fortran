! Copyright (c) Namakete (Ilya Oberemok) <namakete.dev@gmail.com>.
! See the LICENCE file in the repository root for full licence text.
module Group_IO
    use Environment

    implicit none
    integer, parameter :: SURNAME_LEN   = 15
    integer, parameter :: INITIALS_LEN  = 5
    integer, parameter :: MARKS_AMOUNT  = 5

    type student
        character(SURNAME_LEN, kind=CH_)    :: Surname              = ""
        character(INITIALS_LEN, kind=CH_)   :: Initials             = ""
        character(kind=CH_)                 :: Sex                  = ""
        integer                             :: Marks(MARKS_AMOUNT)  = 0
        real(R_)                            :: Aver_Mark            = 0
        type(student), pointer              :: next                 => Null()
    end type student

    character(kind=CH_), parameter   :: MALE = Char(1052, CH_), FEMALE = Char(1046, CH_)

contains
    function Read_class_list(Input_File) result(Class_List)
        type(student), pointer     :: Class_List
        character(*), intent(in)   :: Input_File
        integer  In

        open (file=Input_File, encoding=E_, newunit=In)
            Class_List => Read_student(In)        
        close (In)
    end function Read_class_list

    recursive function Read_student(In) result(Stud)
        type(student), pointer  :: Stud
        integer, intent(in)     :: In
        integer  IO
        character(:), allocatable  :: format

        allocate (Stud)
        
        format = '(3(a, 1x), ' // MARKS_AMOUNT // 'i1, f5.2)'
        read (In, format, iostat=IO) stud%Surname, stud%Initials, stud%Sex, stud%Marks, stud%Aver_Mark
        call Handle_IO_status(IO, "reading line from file")
        if (IO == 0) then
            Stud%next => Read_student(In)
        else
            deallocate (Stud)
            nullify (Stud)
        end if
    end function Read_student

    subroutine Output_class_list(Output_File, Class_List, Threshold, Do_Boys, Do_Girls, List_Name, Position)
        character(*), intent(in)   :: Output_File, Position, List_Name
        type(student), intent(in), pointer  :: Class_List
        real(R_), intent(in)       :: Threshold
        logical, intent(in)        :: Do_Boys, Do_Girls
        integer  :: Out

        open (file=Output_File, encoding=E_, position=Position, newunit=Out)
            write (out, '(/a)') List_Name
            call Output_student(Out, Class_List, Threshold, Do_Boys, Do_Girls)
        close (Out)
    end subroutine Output_class_list

    recursive subroutine Output_student(Out, Stud, Threshold, Do_Boys, Do_Girls)
        integer, intent(in)        :: Out
        type(student), intent(in), pointer  :: Stud
        real(R_), intent(in)       :: Threshold
        logical, intent(in)        :: Do_Boys, Do_Girls

        integer  :: IO
        character(:), allocatable  :: format
        
        !Вывод студента из головы списка (в случае, если его успеваемость не менее 
        !заданной и пол соответствует заданному критерию) и затем переходит к следующему студенту
        if (Associated (Stud)) then
            if ((Stud%Aver_Mark >= Threshold) .AND. ((Do_Boys .AND. Stud%Sex == MALE) &
                .OR. (Do_Girls .AND. Stud%Sex == FEMALE))) then
                format = '(3(a, 1x), ' // MARKS_AMOUNT // 'i1, f5.2)'
                write (Out, format, iostat=IO) Stud%Surname, Stud%Initials, Stud%Sex, Stud%Marks, Stud%Aver_Mark
                call Handle_IO_status(IO, "writing student")
            end if
            call Output_student(Out, Stud%next, Threshold, Do_Boys, Do_Girls)
        end if
    end subroutine Output_student

    subroutine Output_Aver_Mark(Output_File, Aver_mark, List_Name, Position)
        character(*), intent(in)   :: Output_File, Position, List_Name
        real(R_), intent(in)  :: Aver_Mark
        integer  :: Out

        open (file=Output_File, encoding=E_, position=Position, newunit=Out)
            write (out, '(/a)') List_Name
            write(out, '(f5.2)') Aver_Mark
        close (Out)
    end subroutine Output_Aver_Mark
end module Group_IO
