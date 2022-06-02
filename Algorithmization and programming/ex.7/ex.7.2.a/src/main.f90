! Copyright (c) Namakete (Ilya Oberemok) <namakete.dev@gmail.com>.
! See the LICENCE file in the repository root for full licence text.
program main
    use Environment

    implicit none

    character(*), parameter :: input_file = "../data/input.txt", output_file = "output.txt"
    integer                 :: In = 0, Out = 0, Size = 0, Negatives = 0
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

    A = [Pack(A, Mask), Pack(A, .not. Mask)]

    call Sort(Negatives, A)

    open (file=output_file, encoding=E_, newunit=Out, position='append')
        write (Out, *) 'Sorted array'
        write (Out, '('//Size//'i4)') A(:)
    close (Out)
contains
    pure subroutine Sort(Negatives, A)
        integer Negatives, A(:)
        intent(in) Negatives
        intent(out) A
        integer i, MinPos

        do i = 1, Negatives
            MinPos = minloc(A(i:Negatives), dim = 1)
            if (MinPos /= 1) then
                A(i:Negatives) = cshift(A(i:Negatives), MinPos - 1, dim = 1)
            end if
        end do
    end subroutine Sort
end program main













