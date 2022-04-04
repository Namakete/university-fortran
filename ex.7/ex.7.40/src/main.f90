program main
    use environment

    implicit none

    character(*), parameter         :: input_file = "../data/input.txt", output_file = "output.txt"
    integer                         :: In = 0, Out = 1, N = 0, i = 0, j = 0
    integer, allocatable            :: C(:,:), B(:), MTElements(:)
    logical, allocatable            :: Mask(:)

    open (file=input_file, newunit=In)
        read (In, *) N
        allocate (C(N, N))
        read (In, *) (C(i, :), i = 1, N)
    close(In)

    open (file=output_file, encoding=E_, newunit=Out)
        write (Out, '('//N//'i4)') (C(i, :), i = 1, N)
    close (Out)

    B = [((C(i,j)*mod(i+j,2),i=1,N),j=1,N)]
    !print *, B
    Mask = B < 0
    MTElements = Pack(B, Mask)

    open (file=output_file, encoding=E_, newunit=Out, position='append')
        write(Out, *) 'Elements'
        write (Out, '('//N//'i4)') MTElements
    close (Out)

end program main
