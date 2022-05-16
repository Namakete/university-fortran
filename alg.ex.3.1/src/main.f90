! Copyright (c) Namakete (Ilya Oberemok) <namakete.dev@gmail.com>.
! See the LICENCE file in the repository root for full licence text.  
program main
    use Environment
    use List_Process
    use List_IO

    implicit none
    
    character(:), allocatable   :: input_file, output_file,output_file2
    type(node), pointer         :: List  => Null()

    input_file  = "../data/list.txt"
    output_file = "output.txt"
    output_file2 = "output2.txt"

    call Read_sorted_list(input_file, List)
    call Selection_Sort(List)
    call Output_list(output_file, List)
    call Delete(List)
    
    if(associated(list)) &
        call Output_list2(output_file2, List)
end program main
