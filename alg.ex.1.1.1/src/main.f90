! Copyright (c) Namakete (Ilya Oberemok) <namakete.dev@gmail.com>.
! See the LICENCE file in the repository root for full licence text.
program reference_lab_1_1
   use Environment

    implicit none
    
    character(:), allocatable                       :: input_file, output_file, format
    
    integer, parameter                              :: STUD_AMOUNT = 5, SURNAME_LEN = 15, INITIALS_LEN = 5, MARKS_AMOUNT = 5
    character(kind=CH_), parameter                  :: MALE = Char(1052, CH_) 
    character(kind=CH_), parameter                  :: FEMALE = Char(1046, CH_) 
    
    integer                                         :: In, Out, IO, i
    character(SURNAME_LEN, kind=CH_)                :: Surnames(STUD_AMOUNT) = ""
    character(SURNAME_LEN, kind=CH_), allocatable   :: Boys_Surnames(:), Girls_Surnames(:)
   
    character(INITIALS_LEN, kind=CH_)               :: Initials(STUD_AMOUNT) = ""
    character(INITIALS_LEN, kind=CH_), allocatable  :: Boys_Initials(:), Girls_Initials(:)
   
    character(kind=CH_)                             :: Gender(STUD_AMOUNT) = ""
   
    integer                                         :: Marks(STUD_AMOUNT, MARKS_AMOUNT) = 0
    integer, allocatable                            :: Boys_Marks(:, :), Girls_Marks(:, :), Boys_Pos(:), Girls_Pos(:)
   
    real(R_)                                        :: Aver_Marks(STUD_AMOUNT) = 0
    real(R_), allocatable                           :: Boys_Aver_Marks(:), Girls_Aver_Marks(:)

    logical, allocatable                            :: Is_A_Boy(:), Is_A_Girl(:)
    integer                                         :: Boys_Amount = 0, Girls_Amount = 0

    real(R_)                                        :: max_Female, max_Male 
    real(R_)                                        :: All_Marks_Male, All_Marks_Female
    logical, allocatable                            :: mask_Female(:), mask_Male(:)
    integer, allocatable                            :: A_M(:), A_F(:)
    
    integer, parameter                              :: INDEXES(*) = [(i, i = 1, STUD_AMOUNT)]

    input_file = "../data/class.txt"
    output_file = "output.txt"
 
    open (file=input_file, encoding=E_, newunit=In)
        format = '(3(a, 1x), ' // MARKS_AMOUNT // 'i1, f5.2)'
        read (In, format, iostat=IO) (Surnames(i), Initials(i), Gender(i), Marks(i, :), Aver_Marks(i), i = 1, STUD_AMOUNT)
    close (In)

    open (file=output_file, encoding=E_, newunit=Out)
        write (out, '(a)') "Исходный список:"
        write (Out, format, iostat=IO) (Surnames(i), Initials(i), Gender(i), Marks(i, :), Aver_Marks(i), i = 1, STUD_AMOUNT)
    close (Out)

    Is_A_Boy = Gender == MALE
    Is_A_Girl = Gender == FEMALE
    
    Boys_Amount = Count(Is_A_Boy)
    Girls_Amount = Count(Is_A_Girl)
   
    Boys_Pos = Pack(INDEXES, Is_A_Boy)
    Girls_Pos   = Pack(INDEXES, Is_A_Girl)
    
    allocate (Boys_Surnames(Boys_Amount), Boys_Initials(Boys_Amount), Boys_Marks(Boys_Amount, MARKS_AMOUNT))
    allocate (Girls_Surnames(Girls_Amount), Girls_Initials(Girls_Amount), Girls_Marks(Girls_Amount, MARKS_AMOUNT))
   
    do concurrent (i = 1:Boys_Amount)
        Boys_Surnames(i)  = Surnames(Boys_Pos(i))
        Boys_Initials(i)  = Initials(Boys_Pos(i))
        Boys_Marks(i, :)  = Marks(Boys_Pos(i), :)
    end do
   
    do concurrent (i = 1:Girls_Amount)
        Girls_Surnames(i)  = Surnames(Girls_Pos(i))
        Girls_Initials(i)  = Initials(Girls_Pos(i))
        Girls_Marks(i, :)  = Marks(Girls_Pos(i), :)
    end do
      
    Boys_Aver_Marks = Real(Sum(Boys_Marks, dim=2), R_) / MARKS_AMOUNT
    Girls_Aver_Marks = Real(Sum(Girls_Marks, dim=2), R_) / MARKS_AMOUNT
    
    max_Male = MAxVal(Boys_Aver_Marks)
    max_Female = MaxVal(Girls_Aver_Marks)

    mask_Male = [Boys_Aver_Marks == max_Male]
    mask_Female = [Girls_Aver_Marks == max_Female]

    A_M = Pack([(i, i=1, Boys_Amount)], mask_Male)
    A_F = Pack([(i, i=1, Girls_Amount)], mask_Female)

    All_Marks_Male = Sum(Boys_Aver_Marks)/Boys_Amount
    All_Marks_Female = Sum(Girls_Aver_Marks)/Girls_Amount
 
    open (file=output_file, encoding=E_, position='append', newunit=Out)
        write (out, '(/a)') "Лучшая успеваемость среди юношей:"
        write (Out, format, iostat=IO) &
        (Boys_Surnames(A_M(i)), Boys_Initials(A_M(i)), "М", Boys_Marks(A_M(i), :), Boys_Aver_Marks(A_M(i)), i = 1,size(A_M))
        write (out, '(/a)') "Лучшая успеваемость среди девочек:"
        write (Out, format, iostat=IO) &
        (Girls_Surnames(A_F(i)), Girls_Initials(A_F(i)), "Ж", Girls_Marks(A_F(i), :), Girls_Aver_Marks(A_F(i)), i = 1,size(A_F))
    close (Out)
   
    open (file=output_file, encoding=E_, position ='append', newunit=Out)
        write (out, '(/a, f5.2)') "Средний балл среди юношей: ", All_Marks_Male
        write (out, '(a, f5.2)') "Средний балл среди девочек: ", All_Marks_Female
    close (Out)
end program reference_lab_1_1
