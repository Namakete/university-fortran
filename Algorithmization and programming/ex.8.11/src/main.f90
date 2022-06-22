! Copyright (c) Namakete (Ilya Oberemok) <namakete.dev@gmail.com>.
! See the LICENCE file in the repository root for full licence text.
program main
    use environment
    use FindMaxMinElements
    use Matrix_IO
    
    implicit none

    character(:), allocatable       :: input_file_A, input_file_C, output_file
    integer                         :: Max = 0, Min = 0
    integer, allocatable, target    :: A(:,:), C(:,:)
    integer, contiguous, pointer    :: MPA(:), MPC(:)
    
    input_file_A = "../data/A.txt"
    input_file_C = "../data/C.txt"
    output_file = "output.txt"
    
    A = Read_Matrix(input_file_A)
    call Output_Matrix(output_file, A)
    MPA(1:Size(A)) => A
    call Fina_Max_Min_Elements(MPA, Max, Min)
    call Output_Max_Min_Elements(output_file, Max, Min)

    C = Read_Matrix(input_file_C)
    call Output_Matrix(output_file, C)
    MPC(1:Size(C)) => C
    call Fina_Max_Min_Elements(MPC, Max, Min)
    call Output_Max_Min_Elements(output_file, Max, Min)
end program main





