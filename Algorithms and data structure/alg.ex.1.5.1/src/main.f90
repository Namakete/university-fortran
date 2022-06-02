! Copyright (c) Namakete (Ilya Oberemok) <namakete.dev@gmail.com>.
! See the LICENCE file in the repository root for full licence text.
program main
    use Environment
    use Group_Process
    use Group_IO

    implicit none
    character(:), allocatable   :: input_file, output_file
    type(student), pointer      :: Group_List => Null()
    real(R_)                    :: Boys_Max_Value = 0.0, &
                                   Girls_Max_Value = 0.0, &
                                   Sum_Boys_Amount = 0.0, &
                                   Sum_Girls_Amount = 0.0

    integer                     :: Number_of_Boys = 0, Number_of_Girls = 0
    real(R_)                    :: Boys_Average_Marks = 0.0, Girls_Average_Marks = 0.0

    input_file  = "../data/class.txt"
    output_file = "output.txt"

    Group_List => Read_class_list(input_file)

    if (Associated(Group_List)) then
        call Output_class_list(output_file, Group_List, 0.0, .TRUE., .TRUE., &
            "Исходный список:", "rewind")

        call Max_and_sum (Group_List, Boys_Max_Value, Girls_Max_Value, &
            Sum_Boys_Amount, Sum_Girls_Amount, Number_of_Boys, Number_of_Girls)

        Boys_Average_Marks = Sum_Boys_Amount / Number_of_Boys
        Girls_Average_Marks = Sum_Girls_Amount / Number_of_Girls

        call Output_class_list(output_file, Group_List, Boys_Max_Value, .TRUE., .FALSE., &
            "Лучшая успеваемость среди юношей:", "append")
        call Output_class_list(output_file,  Group_List, Girls_Max_Value, .FALSE., .TRUE., &
            "Лучшая успеваемость среди девочек:", "append")

        call output_Aver_Mark(Output_file, Boys_Average_Marks, &
            "Средний балл среди юношей: ", "append")
        call output_Aver_Mark(Output_file, Girls_Average_Marks, &
            "Средний балл среди девочек: ", "append")
    end if
end program main
