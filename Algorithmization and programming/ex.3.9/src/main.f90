! Copyright (c) Namakete (Ilya Oberemok) <namakete.dev@gmail.com>.
! See the LICENCE file in the repository root for full licence text.
program main
    use environment

    implicit none

    character(:), allocatable   :: input_file, output_file
    integer                 :: In = 0, Out = 1, row = 0, column = 0, i = 0
    integer, allocatable   :: array(:, :), sum_row(:), sum_column(:)

    input_file = "../data/input.txt"
    output_file = "output.txt"
    
    open (file=input_file, newunit=In)
        read (In, *) row, column
        allocate (array(row, column))
        read (In, *) (array(i, :), i = 1, row)
    close (In)

    open (file=output_file, encoding=E_, newunit=Out)
        write (Out, *) "Input Array:"
        write (Out, '('//column//'i4)') (array(i, :), i = 1, row)
    close (Out)
    
    sum_row = Sum(array, 2)
    sum_column = Sum(array, 1)

    open (file=output_file, encoding=E_, newunit=Out, position='append')
        write(Out, *) "Sum of Rows:"
        write(Out, '('//row//'i4)') sum_row
        write(Out, *) "Sum of Columns:"
        write(Out, '('//row//'i4)') sum_column 
    close (Out)
end program main

