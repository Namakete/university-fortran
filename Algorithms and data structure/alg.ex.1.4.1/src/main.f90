! Copyright (c) Namakete (Ilya Oberemok) <namakete.dev@gmail.com>.
! See the LICENCE file in the repository root for full licence text.  
program main
    use Environment
    use Group_Process
    use Group_IO

    implicit none
    
    character(:), allocatable   :: input_file, output_file, data_file
   
    integer                     :: i = 0 
    
    type(student), allocatable  :: Boys_Best_Average(:), Girls_Best_Average(:)
    type(student)               :: Group(STUD_AMOUNT)

    real(R_)                    :: Boys_Max_Value = 0.0, &
                                   Girls_Max_Value = 0.0, &
                                   Sum_Boys_Amount = 0.0, &
                                   Sum_Girls_Amount = 0.0

    integer                     :: Number_of_Boys = 0, Number_of_Girls = 0
    real(R_)                    :: Boys_Average_Marks = 0.0, Girls_Average_Marks = 0.0
 
    input_file  = "../data/class.txt"
    output_file = "output.txt"
    data_file   = "class.dat"
   
    call Create_data_file(input_file, data_file)
   
    Group = Read_class_list(data_file)

    call Output_class_list(output_file, Group, "Исходный список:", "rewind")

    do concurrent (i = 1:Size(Group))
        Group(i)%Aver_mark = Real(Sum(Group(i)%Marks), R_) / MARKS_AMOUNT
    end do

    call Max_and_sum (Group, 1, Boys_Max_Value, Girls_Max_Value, &
        Sum_Boys_Amount, Sum_Girls_Amount, Number_of_Boys, Number_of_Girls)
    
    Boys_Average_Marks = Sum_Boys_Amount / Number_of_Boys
    Girls_Average_Marks = Sum_Girls_Amount / Number_of_Girls

    Boys_Best_Average = Pack(Group, Group%Sex==MALE .and. Group%Aver_mark>=(Boys_Max_Value))
    Girls_Best_Average = Pack(Group, Group%Sex==FEMALE .and. Group%Aver_mark>=(Girls_Max_Value))

    call Output_class_list(output_file, Boys_Best_Average, &
        "Лучшая успеваемость среди юношей:", "append")
    call Output_class_list(output_file,  Girls_Best_Average, &
        "Лучшая успеваемость среди девочек:", "append")

    call output_Aver_Mark(Output_file, Boys_Average_Marks, &
        "Средний балл среди юношей: ", "append")
    call output_Aver_Mark(Output_file, Girls_Average_Marks, &
        "Средний балл среди девочек: ", "append")
end program main
