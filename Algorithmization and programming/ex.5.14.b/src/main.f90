! Copyright (c) Namakete (Ilya Oberemok) <namakete.dev@gmail.com>.
! See the LICENCE file in the repository root for full licence text.
program main
    use environment

    implicit none

    character(:), allocatable   :: input_file, output_file
    integer                 :: In = 0, Out = 0, Size = 0, i = 0
    integer, allocatable    :: array(:), get_indexes(:), negative_indexes(:), negative_array(:)
    logical, allocatable    :: mask(:)

    input_file = "../data/input.txt"
    output_file = "output.txt"
    
    open (file=input_file, newunit=In)
        read(In, *) Size
        allocate(array(Size))
        read(In, *) array(:)
    close (In)

    get_indexes = [(i, i = 1, Size)]

    open (file=output_file, encoding=E_, newunit=Out)
        write(Out,*) "Output array indexes"
        write (Out, '('//Size//'i4)') get_indexes
        write(Out,*) "Outputting array elements"
        write (Out, '('//Size//'i4)') array
    close (Out)

    mask = (array < 0)

    negative_indexes = Pack(get_indexes, mask)
    negative_array =  Pack(array, mask)

    open (file=output_file, encoding=E_, newunit=Out, position='append')
        write(Out, *) "Print of negative indexes"
        write(Out, '('//Size//'i4)') negative_indexes
        write(Out, *) "Print of negative array elements"
        write(Out, '('//Size//'i4)') negative_array
    close (Out)
end program main


