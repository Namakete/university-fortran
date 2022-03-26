program main
    use environment
    use FindMaxMinElements

    implicit none

    character(*), parameter         :: output_file = "output.txt"
    character(*), parameter         :: input_file = "../data/input.txt"
    integer                         :: In = 0, Out = 0, rows = 0, columns = 0, i = 0, Max = 0, Min = 0
    integer, allocatable, target    :: A(:,:)
    integer, contiguous, pointer    :: B(:), C(:), D(:)

    Open (file=input_file, newunit=In)
        read (In, *) rows, columns
        allocate (A(rows, columns))
        read (In, *) (A(i, :), i = 1, rows)
    close (In)

    open (file=output_file, encoding=E_, newunit=Out)
        write (Out, '('//columns//'i4)') (A(i, :), i = 1, rows)
    close (Out)

    B(1:Size(A)) => A
    C(1:Size(A)) => A

    call fina_max_min_elements(B, Max, Min)

    open (file=output_file, encoding=E_, newunit=Out, position='append')
        write(Out, *) "Max elemets: ", Max
        write(Out, *) "Min elemets: ", Min
    close (Out)

end program main





