! Copyright (c) Namakete (Ilya Oberemok) <namakete.dev@gmail.com>.
! See the LICENCE file in the repository root for full licence text.  
program main
    use Environment
    use Group_Process
    use Group_IO

    implicit none
    
    character(:), allocatable       :: input_file, output_file, data_file

    character(kind=CH_), parameter  :: MALE = Char(1052, CH_) 
    character(kind=CH_), parameter  :: FEMALE = Char(1046, CH_)  
    
    integer                         :: i = 0
    
    type(student)                   :: Group(STUD_AMOUNT)
    type(student), allocatable      :: Boys(:), Girls(:)
    
    integer, allocatable            :: b_ind(:), g_ind(:)
    real(R_)                        :: Boys_Aver_Mark, Girls_Aver_Mark
    
    integer, parameter              :: INDEXIS(*) = [(i, i = 1, STUD_AMOUNT)]

    input_file  = "../data/class.txt"
    output_file = "output.txt"
    data_file   = "class.dat"
   
    call Create_data_file(input_file, data_file)
   
    Group = Read_class_list(data_file)
    call Output_class_list(output_file, Group, "Исходный список:", "rewind", INDEXIS)

    Boys  = Pack(Group, Group%Sex == MALE)
    Girls = Pack(Group, Group%Sex == FEMALE)
  
    do concurrent (i = 1:Size(Boys))
        Boys(i)%Aver_mark = Real(Sum(Boys(i)%Marks), R_) / MARKS_AMOUNT
    end do
   
    do concurrent (i = 1:Size(Girls))
        Girls(i)%Aver_mark = Real(Sum(Girls(i)%Marks), R_) / MARKS_AMOUNT
    end do

    b_ind = Sort_class_list(Boys)
    g_ind = Sort_class_list(Girls)

    call Output_class_list(output_file, Boys, "Лучшая успеваемость среди юношей:", "append", b_ind)
    call Output_class_list(output_file, Girls, "Лучшая успеваемость среди девочек:", "append", g_ind)

    call sub_aver_mark(Boys, Boys_Aver_Mark)
    call sub_aver_mark(Girls, Girls_Aver_Mark)

    call Output_aver_list(Output_file, Boys_Aver_Mark, "Средний балл среди юношей: ", "append")
    call Output_aver_list(Output_file, Girls_Aver_Mark, "Средний балл среди девочек: ", "append")
end program main
