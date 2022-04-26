! Copyright (c) Namakete (Ilya Oberemok) <namakete.dev@gmail.com>.
! See the LICENCE file in the repository root for full licence text.
program main
    use environment
    
    implicit none

    character(*), parameter                        :: input_file = "../data/input.txt", output_file = "output.txt"
    integer, parameter                             :: STUD_AMOUNT = 5, SURNAME_LEN = 15, INITIALS_LEN = 5, MARKS_AMOUNT = 5
    character(kind=CH_), parameter                 :: MALE = Char(1052, CH_)
    
    integer                                        :: Out = 0, In = 0, IO, i, j
    real(R_)                                       :: max_F, max_M, all_marks_F, all_marks_M
    integer                                        :: A_F(:), A_M(:)
    logical                                        :: mask_F(:), mask_M(:)
    
    
   character(SURNAME_LEN, kind=CH_)                :: tmpSurname = "", Surnames(STUD_AMOUNT) = ""
   character(SURNAME_LEN, kind=CH_), allocatable   :: Boys_Surnames(:), Girls_Surnames(:)
   
   character(INITIALS_LEN, kind=CH_)               :: tmpInitials = "", Initials(STUD_AMOUNT) = ""
   character(INITIALS_LEN, kind=CH_), allocatable  :: Boys_Initials(:), Girls_Initials(:)
   
   character(kind=CH_)                             :: Gender(STUD_AMOUNT) = ""
   
   integer                                         :: tmpMarks(MARKS_AMOUNT) = 0, Marks(STUD_AMOUNT, MARKS_AMOUNT) = 0
   integer, allocatable                            :: Boys_Marks(:, :), Girls_Marks(:, :), Boys_Pos(:), Girls_Pos(:)
   
   real(R_)                                        :: tmpAverMark = 0, Aver_Marks(STUD_AMOUNT) = 0
   real(R_), allocatable                           :: Boys_Aver_Marks(:), Girls_Aver_Marks(:)

   logical, allocatable                            :: Is_A_Boy(:), Is_A_Girl(:)
   integer                                         :: Boys_Amount = 0, Girls_Amount = 0

   integer, parameter                              :: INDEXES(*) = [(i, i = 1, STUD_AMOUNT)]
   logical                                         :: Swap

end program main
