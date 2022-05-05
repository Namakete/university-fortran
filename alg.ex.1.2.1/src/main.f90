! Copyright (c) Namakete (Ilya Oberemok) <namakete.dev@gmail.com>.
! See the LICENCE file in the repository root for full licence text.
program reference_lab_1_1
    use Environment

    implicit none
   
    character(:), allocatable           :: input_file, output_file
    
    integer, parameter                  :: STUD_AMOUNT = 5, SURNAME_LEN = 15, INITIALS_LEN = 5, MARKS_AMOUNT = 5
    character(kind=CH_), parameter      :: MALE = Char(1052, CH_)
    character(kind=CH_),parameter       :: FEMALE = Char(1046, CH_)

    character(kind=CH_)                 :: Surnames(SURNAME_LEN, STUD_AMOUNT)  = ""
    character(kind=CH_)                 :: Initials(INITIALS_LEN, STUD_AMOUNT) = ""                    
    character(kind=CH_)                 :: Genders(STUD_AMOUNT) = ""

    character(kind=CH_), allocatable    :: Boys_Surnames(:, :), Girls_Surnames(:, :)
    character(kind=CH_), allocatable    :: Boys_Initials(:, :), Girls_Initials(:, :)

    integer                             :: Marks(MARKS_AMOUNT, STUD_AMOUNT) = 0, i = 0
    integer, allocatable                :: Boys_Marks(:, :), Girls_Marks(:, :)

    real(R_)                            :: Aver_Marks(STUD_AMOUNT) = 0

    real(R_),allocatable                :: Boys_Aver_Marks(:), Girls_Aver_Marks(:)
   
    integer, allocatable                :: Max_Value_Boys(:), Max_Value_Girls(:)
    real(R_)                            :: Boys_Aver_Mark, Girls_Aver_Mark

    integer, parameter                  :: INDEXES(*) = [(i, i = 1, STUD_AMOUNT)]

    input_file  = "../data/class.txt"
    output_file = "output.txt"

    call Read_class_list(input_file, Surnames, Initials, Genders, Marks, Aver_Marks)

    call Output_class_list(output_file, Surnames, Initials, Genders, Marks, Aver_Marks, &
      "Исходный список:", "rewind", INDEXES)
  
    call Get_list_by_gender(Surnames, Initials, Genders, Marks, Boys_Surnames, &
      Boys_Initials, Boys_Marks, Boys_Aver_Marks, MALE)
    call Get_list_by_gender(Surnames, Initials, Genders, Marks, Girls_Surnames, &
      Girls_Initials, Girls_Marks, Girls_Aver_Marks, FEMALE)

    Max_Value_Boys = Get_Max_Value(Boys_Aver_Marks)
    Max_Value_Girls = Get_Max_Value(Girls_Aver_Marks)

    call Output_class_list(output_file, Boys_Surnames, Boys_Initials, [(MALE, i = 1, Size(Boys_Aver_Marks))], &
    Boys_Marks, Boys_Aver_Marks, "Лучшая успеваемость среди юношей:", "append", Max_Value_Boys)
    call Output_class_list(output_file, Girls_Surnames, Girls_Initials, [(FEMALE, i = 1, Size(Girls_Aver_Marks))], &
    Girls_Marks, Girls_Aver_Marks, "Лучшая успеваемость среди девочек:", "append", Max_Value_Girls)

    call Get_Average_Mark(Boys_Aver_Marks, Boys_Aver_Mark)
    call Get_Average_Mark(Girls_Aver_Marks, Girls_Aver_Mark)

    call Output_Average_Mark(Output_file, Boys_Aver_Mark, "Средний балл среди юношей:", "append")
    call Output_Average_Mark(Output_file, Girls_Aver_Mark, "Средний балл среди девочек: ", "append")

contains
    subroutine Read_class_list(Input_File, Surnames, Initials, Genders, Marks, Aver_Marks)
        character(*)         Input_File
        character(kind=CH_)  Surnames(:, :), Initials(:, :), Genders(:)
        integer              Marks(:, :)
        real(R_)             Aver_Marks(:)
        intent (in)          Input_File
        intent (out)         Surnames, Initials, Genders, Marks, Aver_Marks

        integer In, IO, i
        character(:), allocatable  :: format
      
        open (file=Input_File, encoding=E_, newunit=In)
            format = '(' // SURNAME_LEN // 'a1, 1x, ' // INITIALS_LEN // 'a1, 1x, a, 1x, ' // &
            MARKS_AMOUNT // 'i1, f5.2)'
            read (In, format, iostat=IO) (Surnames(:, i), Initials(:, i), Genders(i), Marks(:, i), Aver_Marks(i), &
            i = 1, STUD_AMOUNT)
            call Handle_IO_status(IO, "reading class list")
        close (In)
    end subroutine Read_class_list

    subroutine Output_class_list(Output_File, Surnames, Initials, Genders, Marks, Aver_Marks, List_name, Position, Elem)
      character(*)         Output_File, Position, List_name
      character(kind=CH_)  Surnames(:, :), Initials(:, :), Genders(:)
      integer              Marks(:, :)
      real(R_)             Aver_Marks(:)
      integer              Elem(:)
      intent (in)    Output_File, Surnames, Initials, Genders, Marks, Aver_Marks, List_name, Position, Elem

      integer                       :: Out, i, IO
      character(:), allocatable     :: format
   
      open (file=output_file, encoding=E_, position=position, newunit=Out)
         write (out, '(/a)') List_name
         format = '(' // SURNAME_LEN // 'a1, 1x, ' // INITIALS_LEN // 'a1, 1x, a, 1x, ' // &
            MARKS_AMOUNT // 'i1, f5.2)'
         write (Out, format, iostat=IO) &
            (Surnames(:, Elem(i)), Initials(:, Elem(i)), Genders(Elem(i)), Marks(:, Elem(i)), &
            Aver_Marks(Elem(i)), i = 1, Size(Elem))
         call Handle_IO_status(IO, "writing " // List_name)
      close (Out)
    end subroutine Output_class_list

    pure subroutine Get_list_by_gender(Surnames, Initials, Genders, Marks, &
        Gender_Surnames, Gender_Initials, Gender_Marks, Gender_Aver_Marks, Gender)

        character(kind=CH_)  Surnames(:, :), Initials(:, :), Genders(:)
        integer              Marks(:, :)
        character(kind=CH_)  Gender_Surnames(:, :), Gender_Initials(:, :)
        integer              Gender_Marks(:, :)
        real(R_)             Gender_Aver_Marks(:)
        character(kind=CH_)  Gender
        intent(in)           Surnames, Initials, Genders, Marks, Gender
        intent(out)          Gender_Surnames, Gender_Initials, Gender_Marks, Gender_Aver_Marks
     
        allocatable          Gender_Surnames, Gender_Initials, Gender_Marks, Gender_Aver_Marks

        logical, allocatable        :: Is_A_Gender(:)
        integer, allocatable        :: Gender_Pos(:)
        integer                     :: Gender_Amount, i
        integer, parameter          :: INDEXES(*) = [(i, i = 1, STUD_AMOUNT)]

        Is_A_Gender    = Genders == Gender
        Gender_Amount  = Count(Is_A_Gender)

        Gender_Pos  = Pack(INDEXES, Is_A_Gender)
        allocate (Gender_Surnames(Gender_Amount, SURNAME_LEN), &
        Gender_Initials(Gender_Amount, INITIALS_LEN), Gender_Marks(Gender_Amount, MARKS_AMOUNT))
      
        do concurrent (i = 1:Gender_Amount)
            Gender_Surnames(i, :)  = Surnames(Gender_Pos(i), :)
            Gender_Initials(i, :)  = Initials(Gender_Pos(i), :)
            Gender_Marks(i, :)  = Marks(Gender_Pos(i), :)
        end do

        Gender_Aver_Marks = Real(Sum(Gender_Marks, dim=2), R_) / MARKS_AMOUNT
    end subroutine Get_list_by_gender

    pure function Get_Max_Value(Gender_Aver_Marks) result(Max_Gender_Number)
        real(R_), intent(in)        :: Gender_Aver_Marks(:)
        integer, allocatable        :: Max_Gender_Number(:)
        real(R_)                    :: Max_Element  

        Max_Element = MaxVal(Real(Gender_Aver_Marks, R_))
        Max_Gender_Number = Pack([(i, i = 1, size(Gender_Aver_Marks))], [Gender_Aver_Marks == Max_Element])      
    end function Get_Max_Value
   
    pure subroutine Get_Average_Mark(Gender_Average_Marks, Average_Mark)
        real(R_), intent(in)        :: Gender_Average_Marks(:)
        real(R_), intent(inout)     :: Average_Mark

        Average_Mark = Sum(Real(Gender_Average_Marks,R_))/Size(Gender_Average_Marks)
    end subroutine Get_Average_Mark

    subroutine Output_Average_Mark(Output_File, Average_Mark, List_name, Position)
        character(*), intent(in)    :: Output_File, Position, List_name
        real(R_), intent(in)        :: Average_Mark
        integer                     :: Out
        
        open(file=output_file, encoding=E_, position=position, newunit=Out)
            write (out, '(/a)') List_name
            write (Out, '(f5.2)') Average_Mark
        close (Out)
    end subroutine Output_Average_Mark
end program reference_lab_1_1
