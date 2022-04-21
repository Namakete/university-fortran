! Copyright (c) Namakete (Ilya Oberemok) <namakete.dev@gmail.com>.
! See the LICENCE file in the repository root for full licence text.
program main
    use environment

    implicit none

    character(*), parameter    :: input_file = "../data/input.txt", output_file = "output.txt"
    integer                    :: In = 0, Out = 1, rows = 0, columns = 0, i = 0, max_num = 0
    integer, allocatable       :: A(:,:), Indexes(:)

    open (file=input_file, newunit=In)
        read(In, *) rows, columns
        allocate(A(columns,rows))
        allocate(Indexes(columns))
        read(In, *) (A(i,:), i = 1, columns)
    close (In)

    open (file=output_file, encoding=E_, newunit=Out)
        write(Out, *) 'Array'
        write (Out, '('//rows//'i4)') (A(i, :), i = 1, columns)
    close (Out)

    Indexes = [(i, i = 1, rows)]

    do i = 1, rows
        max_num = maxloc(A(1,i:rows),dim=1)+i-1
        Indexes([i,max_num]) = Indexes([max_num,i])
        A(:,[i, max_num]) = A(:,[max_num,i])
    end do

    open (file=output_file, encoding=E_, newunit=Out, position='append')
        write(Out, *) 'index of maximum element in bounds from and to rows'
        write (Out, '('//columns//'i4)') (Indexes(i), i = 1, rows )
        write(Out, *) 'Sorted array'
        write (Out, '('//rows//'i4)') (A(i, :), i = 1, columns)
    close (Out)
end program main



