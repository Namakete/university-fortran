program main
    use environment
    use FindMaxMinElements
    use Matrix_IO
    
    implicit none

    character(*), parameter         :: output_file = "output.txt", input_file_A = "../data/A.txt", input_file_C = "../data/C.txt"
    integer                         :: Max = 0, Min = 0
    integer, allocatable, target    :: A(:,:), C(:,:)
    integer, contiguous, pointer    :: MPA(:), MPC(:)
    
    A = ReadMatrix(input_file_A)
    call OutputMatrix(output_file, A)
    MPA(1:Size(A)) => A
    call fina_max_min_elements(MPA, Max, Min)
    call OutputMaxMinElements(output_file, Max, Min)

    C = ReadMatrix(input_file_C)
    call OutputMatrix(output_file, C)
    MPC(1:Size(C)) => C
    call fina_max_min_elements(MPC, Max, Min)
    call OutputMaxMinElements(output_file, Max, Min)
   
end program main





