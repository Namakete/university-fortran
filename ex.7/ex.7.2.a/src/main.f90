program main
    use Environment

    implicit none
    character(*), parameter :: input_file = "../data/input.txt", output_file = "output.txt"
    integer                 :: In = 0, Out = 0, i, Size = 0, Negatives = 0, MinPos = 0
    integer, allocatable   :: A(:)
    logical, allocatable    :: Mask(:)

    open (file=input_file, newunit=In)
        read (In, *) Size
        allocate(A(Size))
        read (In, *) A
    close (In)

    open (file=output_file, newunit=Out)
        write (Out, *) 'Array'
        write (Out, '('//Size//'i4)') A(:)
    close (Out)

    Mask = A <= 0

    Negatives = count(mask)

    A=[Pack(A, .not. Mask), Pack(A, Mask)]
   
    do i = 1, negatives
        MinPos = minloc(A(i:negatives), dim = 1)
        if (MinPos /= 1) then
            A(i:negatives) = cshift(A(i:negatives), MinPos - 1)
        endif
    enddo

    open (file=output_file, encoding=E_, newunit=Out, position='append')
        write (Out, *) 'Sort array'
        write (Out, '('//Size//'i4)') A(:)
    close (Out)

end program main












