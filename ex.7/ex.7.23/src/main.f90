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

    open (file=output_file, encoding=E_, newunit=Out)
        write(*, *) 'Input array'
        write (*, '('//N//'i4)') (A(i,:), i = 1,N)
    close(Out)

    allocate (S (N, N-1))
    allocate (T (N-1, N-1))

    S(:,1:N-1) = A(:,1:N-1)+A(:,2:N)
    T(1:N-1,:) = S(1:N-1,:)+S(2:N,:)

end program main
