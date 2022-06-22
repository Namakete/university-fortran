! Copyright (c) Namakete (Ilya Oberemok) <namakete.dev@gmail.com>.
! See the LICENCE file in the repository root for full licence text.
program main
    use Environment

    implicit none

    character(:), allocatable   :: input_file, output_file
    integer                     :: In = 0, Out = 1, Size = 0, negative_elements = 0
    integer, allocatable        :: array(:)
    logical, allocatable        :: mask(:)

    input_file = "../data/input.txt"
    output_file = "output.txt"
    
    open (file=input_file, newunit=In)
        read (In, *) Size
        allocate(array(Size))
        read (In, *) array
    close (In)

    open (file=output_file, newunit=Out)
        write (Out, *) "Basic of array"
        write (Out, '('//Size//'i4)') array(:)
    close (Out)

    mask = (array <= 0)

    negative_elements = count(mask)

    array = [Pack(array, mask), Pack(array, .not. mask)]

    call Sort(negative_elements, array)

    open (file=output_file, encoding=E_, newunit=Out, position='append')
        write (Out, *) "Sorted of array"
        write (Out, '('//Size//'i4)') array(:)
    close (Out)

contains

    pure subroutine Sort(negative_array, array)
        integer negative_array, array(:)
        intent(in) negative_array
        intent(out) array
        integer i, MinPos

        do i = 1, negative_array
            MinPos = Minloc(array(i:negative_array), 1)
            if (MinPos /= 1) then
                array(i:negative_array) = Cshift(array(i:negative_array), MinPos - 1, 1)
            end if
        end do
    end subroutine Sort

end program main













