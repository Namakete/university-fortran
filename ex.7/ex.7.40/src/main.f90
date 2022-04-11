program main
    use environment

    implicit none

    character(*), parameter         :: input_file = "../data/input.txt", output_file = "output.txt"
    integer                         :: In = 0, Out = 1, N = 0, i = 0
    integer, allocatable            :: C(:,:), B(:)

    open (file=input_file, newunit=In)
        read (In, *) N
        allocate (C(N, N))
        read (In, *) (C(i, :), i = 1, N)
    close(In)

    open (file=output_file, encoding=E_, newunit=Out)
        write (Out, '('//N//'i4)') (C(i, :), i = 1, N)
    close (Out)

    B = [(Pack(C(1:N:2,i), C(1:N:2,i)<0), Pack(C(2:N:2,i+1),C(2:N:2,i+1)<0), i=1,N,2)]

    open (file=output_file, encoding=E_, newunit=Out, position='append')
        write(Out, *) 'Elements'
        write (Out, '('//N//'i4)') B
    close (Out)

end program main
