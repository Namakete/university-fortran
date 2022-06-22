! Copyright (c) Namakete (Ilya Oberemok) <namakete.dev@gmail.com>.
! See the LICENCE file in the repository root for full licence text.
program main
    use environment

    implicit none

    character(:), allocatable   :: input_file, output_file
    integer                     :: In = 0, Out = 1, N = 0, i = 0, max_pos = 0, number_max_position = 0
    integer, allocatable        :: A(:,:), S(:,:)
    integer, allocatable        :: indexes(:, :), index_max_position(:, :)
    logical, allocatable        :: mask(:)

    input_file = "../data/input.txt"
    output_file = "output.txt"

    open (file=input_file, newunit=In)
        read (In, *) N
        allocate (A(N, N))
        read (In, *) (A(i, :), i = 1, N)
    close(In)

    open (file=output_file, encoding=E_, newunit=Out)
        write(*, *) "Basic of array"
        write (*, '('//N//'i4)') (A(i,:), i = 1,N)
    close(Out)

    allocate (S (N, N-1))
    allocate (indexes((N-1)**2, 2))
    allocate (mask((N-1)**2), source=.false.)

    S(:,1:N-1) = A(:,1:N-1)+A(:,2:N)
    S(1:N-1,:) = S(1:N-1,:)+S(2:N,:)

    call Find_Max_Position(S(1:N-1,1:N-1), max_pos, mask, indexes, index_max_position, number_max_position)

    open (file=output_file, encoding=E_, newunit=Out, position='append')
        write(Out, *) "Output array"
        write (Out, '('//N-1//'i4)') (S(i,:), i=1,N-1)
        write(Out, *) "Coordinates of the maximum element of the matrix"
        write (Out, '(2i4)') (index_max_position(i,:), i = 1,number_max_position)
    close (Out)

contains

    pure subroutine Find_Max_Position(array, max_pos, mask, indexes, index_max_position, number_max_position)
        integer, allocatable, intent(out)   ::  index_max_position(:, :)
        integer, intent(in)                 :: array(:, :)
        integer, intent(out)                :: max_pos
        integer, intent(out)                :: indexes(:, :)
        logical, intent(out)                :: mask(:)
        integer, intent(out)                :: number_max_position
        integer i, j, N, M

        N = size(array,1)
        M = size(array,2)

        indexes(:, 1) = [((i, i = 1, N), j = 1, M)]
        indexes(:, 2) = [((j, i = 1, N), j = 1, M)]

        max_pos = MaxVal(array)

        mask        = [array == max_pos]
        number_max_position   = Count(mask)

        allocate(index_max_position(number_max_position, 2))

        index_max_position(:, 1) = Pack(indexes(:, 1), mask)
        index_max_position(:, 2) = Pack(indexes(:, 2), mask)
    end subroutine Find_Max_Position

end program main
