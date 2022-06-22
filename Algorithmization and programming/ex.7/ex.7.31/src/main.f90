! Copyright (c) Namakete (Ilya Oberemok) <namakete.dev@gmail.com>.
! See the LICENCE file in the repository root for full licence text.
program main
    use environment

    implicit none

    character(:), allocatable   :: input_file, output_file
    integer                     :: In = 0, Out = 1, row = 0, column = 0, i = 0, max_number = 0
    integer, allocatable        :: array(:,:), get_indexes(:)

    input_file = "../data/input.txt"
    output_file = "output.txt"
    
    open (file=input_file, newunit=In)
        read(In, *) row, column
        allocate(array(column,row))
        allocate(get_indexes(column))
        read(In, *) (array(i,:), i = 1, column)
    close (In)

    open (file=output_file, encoding=E_, newunit=Out)
        write(Out, *) 'Array'
        write (Out, '('//row//'i4)') (array(i, :), i = 1, column)
    close (Out)

    get_indexes = [(i, i = 1, row)]

    do i = 1, row
        max_number = maxloc(array(1,i:row),dim=1)+i-1
        get_indexes([i,max_number]) = get_indexes([max_number,i])
        array(:,[i, max_number]) = array(:,[max_number,i])
    end do

    open (file=output_file, encoding=E_, newunit=Out, position='append')
        write(Out, *) "Index of maximum element in bounds from and to rows"
        write (Out, '('//column//'i4)') (get_indexes(i), i = 1, row )
        write(Out, *) "Sorted array"
        write (Out, '('//row//'i4)') (array(i, :), i = 1, column)
    close (Out)
end program main



