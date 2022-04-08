program main
    use environment

    implicit none

    character(*), parameter         :: input_file = "../data/input.txt", output_file = "output.txt"
    integer                         :: In = 0, Out = 1, N = 0, i, j, m, loc (2)
    integer, allocatable            :: A(:,:), S(:,:), T(:,:)
 
    open (file=input_file, newunit=In)
        read (In, *) N
        allocate (A(N, N))
        read (In, *) (A(i, :), i = 1, N)
    close(In)
end program main
