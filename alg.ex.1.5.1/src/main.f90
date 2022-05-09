! Copyright (c) Namakete (Ilya Oberemok) <namakete.dev@gmail.com>.
! See the LICENCE file in the repository root for full licence text.  
program main
    use Environment
    use Group_Process
    use Group_IO

    implicit none
   
    character(:), allocatable       :: input_file, output_file
   
    character(kind=CH_), parameter  :: MALE = Char(1052, CH_) 
    character(kind=CH_), parameter  :: FEMALE = Char(1046, CH_)
   
    type(student), pointer          :: Group_List => Null(), &
                                       Boys_list => Null(), &
                                       Girls_list => Null(), &
                                       Boys_a_list => Null(), &
                                       Girls_a_list => Null()
   
    integer(I_)                     :: Boys_Amount = 0, Girls_Amount = 0
    real(R_)                        :: Boys_Average_Mark, Girls_Average_Mark
   
    input_file  = "../data/class.txt"
    output_file = "output.txt"
   
    Group_List => Read_class_list(input_file)

    if (Associated(Group_List)) then
        call Output_class_list(output_file, Group_List, "Исходный список:", "rewind")

        call Get_list_by_gender(Group_List, Boys_list, Boys_Amount, MALE)
        call Get_list_by_gender(Group_List, Girls_list, Girls_Amount, FEMALE)
      
        call Sort_class_list(Boys_list, Boys_Amount, Boys_Average_Mark)
        call Sort_class_list(Girls_list, Girls_Amount, Girls_Average_Mark)

        call Get_list_by_Average(Boys_list, Boys_a_list,  Boys_Average_Mark)
        call Get_list_by_Average(Girls_list, Girls_a_list, Girls_Average_Mark)

        if (Associated(Boys_list)) &
            call Output_class_list(output_file, Boys_a_list, &
            "Лучшая успеваемость среди юношей:", "append")
        if (Associated(Girls_list)) &
            call Output_class_list(output_file, Girls_a_list, &
            "Лучшая успеваемость среди девочек:", "append")
  
        call Sub_average_mark(Boys_list, Boys_Amount, Boys_Average_Mark)  
        call Sub_average_mark(Girls_list, Girls_Amount, Girls_Average_Mark)  

        call Output_Aver_Mark(Output_file, Boys_Average_Mark, &
            "Cредний балл среди юношей: ", "append")
        call Output_Aver_Mark(Output_file, Girls_Average_Mark, &
            "Средний балл среди девочек: ", "append")
    end if
end program main
