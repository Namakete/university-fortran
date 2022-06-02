! Copyright (c) Namakete (Ilya Oberemok) <namakete.dev@gmail.com>.
! See the LICENCE file in the repository root for full licence text.
program main
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
    real(R_), allocatable                           :: Boys_Average_Marks(:), Girls_Average_Marks(:)

    logical, allocatable                            :: Is_A_Boy(:), Is_A_Girl(:)
    integer                                         :: Boys_Amount = 0, Girls_Amount = 0

    real(R_)                                        :: Max_Girls, Max_Boys 
    real(R_)                                        :: All_Marks_Boys, All_Marks_Girls
    logical, allocatable                            :: Mask_Girls(:), Mask_Boys(:)
    integer, allocatable                            :: A_B(:), A_G(:)
    
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
      
    Boys_Average_Marks = Real(Sum(Boys_Marks, dim=2), R_)/MARKS_AMOUNT
    Girls_Average_Marks = Real(Sum(Girls_Marks, dim=2), R_)/MARKS_AMOUNT
    
    Max_Boys = MaxVal(Boys_Average_Marks)
    Max_Girls = MaxVal(Girls_Average_Marks)

    Mask_Boys = [Boys_Average_Marks == Max_Boys]
    Mask_Girls = [Girls_Average_Marks == Max_Girls]

    A_B = Pack([(i, i=1, Boys_Amount)], Mask_Boys)
    A_G = Pack([(i, i=1, Girls_Amount)], Mask_Girls)

    All_Marks_Boys = Sum(Boys_Average_Marks)/Boys_Amount
    All_Marks_Girls = Sum(Girls_Average_Marks)/Girls_Amount
 
    open (file=output_file, encoding=E_, position='append', newunit=Out)
        write (out, '(/a)') "Лучшая успеваемость среди юношей:"
        write (Out, format, iostat=IO) &
        (Boys_Surnames(A_B(i)), Boys_Initials(A_B(i)), "М", Boys_Marks(A_B(i), :), Boys_Average_Marks(A_B(i)), i = 1,size(A_B))
        write (out, '(/a)') "Лучшая успеваемость среди девочек:"
        write (Out, format, iostat=IO) &
        (Girls_Surnames(A_G(i)), Girls_Initials(A_G(i)), "Ж", Girls_Marks(A_G(i), :), Girls_Average_Marks(A_G(i)), i = 1,size(A_G))
    close (Out)
   
    open (file=output_file, encoding=E_, position ='append', newunit=Out)
        write (out, '(/a, f5.2)') "Средний балл среди юношей: ", All_Marks_Boys
        write (out, '(a, f5.2)') "Средний балл среди девочек: ", All_Marks_Girls
    close (Out)
end program main
