! Copyright (c) Namakete (Ilya Oberemok) <namakete.dev@gmail.com>.
! See the LICENCE file in the repository root for full licence text.  
program main
    use Environment
    use List_Process
    use List_IO

    implicit none
    
    character(:), allocatable   :: input_file, output_file,output_file2
    type(node), allocatable         :: List
    
    input_file  = "../data/list.txt"
    output_file = "output.txt"
    output_file2 = "output2.txt"

    List = Read_sorted_list(input_file)
    call output_list(output_file, 'rewind', List)
    call Selection_Sort(List)
    call output_list(output_file, 'append', List)
    DEALLOCATE(list)

    if(ALLOCATED(list)) &
        call Output_list2(output_file2, 'append' ,List)
end program main
