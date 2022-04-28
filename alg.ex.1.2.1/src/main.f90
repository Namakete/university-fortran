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
 
end program reference_lab_1_1
