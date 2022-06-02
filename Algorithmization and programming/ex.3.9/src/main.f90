! Copyright (c) Namakete (Ilya Oberemok) <namakete.dev@gmail.com>.
! See the LICENCE file in the repository root for full licence text.
program main
    use environment

    implicit none

    character(*), parameter :: input_file = "../data/input.txt", output_file = "output.txt"
    integer                 :: In = 0, Out = 0, rows= 0, columns = 0, i = 0
    integer, allocatable   :: A(:, :), columns_sum(:), rows_sum(:)

    open (file=input_file, newunit=In)
        read (In, *) rows, columns
        allocate (A(rows, columns))
        read (In, *) (A(i, :), i = 1, rows)
    close (In)

    open (file=output_file, encoding=E_, newunit=Out)
        write (Out, *) 'Input array'
        write (Out, '('//columns//'i4)') (A(i, :), i = 1, rows)
    close (Out)
    
    columns_sum = sum(A,dim=1)
    rows_sum = sum(A,dim=2)

    open (file=output_file, encoding=E_, newunit=Out, position='append')
        write(Out, *) 'Sum rows'
        write(Out, '('//rows//'i4)') rows_sum
        write(Out, *) 'Sum columns'
        write(Out, '('//rows//'i4)') columns_sum 
    close (Out) 
end program main

