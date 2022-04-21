! Copyright (c) Namakete (Ilya Oberemok) <namakete.dev@gmail.com>.
! See the LICENCE file in the repository root for full licence text.
module Matrix_IO
    use Environment

    implicit none
contains
    function ReadMatrix(input_file) result(Array)
        character(*), intent(in)   :: input_file
        integer, allocatable      :: Array(:, :)

        integer :: In = 0, i = 0, rows = 0, columns = 0

        open (file=input_file, newunit=In)
            read (In, *) rows, columns
            allocate (Array(rows, columns))
            read (In, *) (Array(i, :), i = 1, rows)
        close (In)
    end function ReadMatrix

    subroutine OutputMatrix(output_file, Array)
        character(*), intent(in)   :: output_file
        integer, intent(in)       :: Array(:, :)

        integer :: Out = 0, i = 0

        open (file=output_file, encoding=E_, newunit=Out, position='append')
            write (Out, '('//ubound(Array, 2)//'i4)') (Array(i, :), i = 1, ubound(Array, 1))
        close (Out)
    end subroutine OutputMatrix

    subroutine OutputMaxMinElements(output_file, Max, Min)
        character(*), intent(in)  :: output_file
        integer                   :: Max, Min

        integer :: Out = 0

        Open (file=output_file, encoding=E_, newunit=Out, position='append')
            write(Out, *) "Max elemets: ", Max
            write(Out, *) "Min elemets: ", Min
        close (Out)
    end subroutine OutputMaxMinElements
end module Matrix_IO
