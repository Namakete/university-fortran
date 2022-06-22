! Copyright (c) Namakete (Ilya Oberemok) <namakete.dev@gmail.com>.
! See the LICENCE file in the repository root for full licence text.
program main
    use Environment

    implicit none

    character(:), allocatable   :: input_file, output_file
    integer                     :: In = 0, Out = 1, row = 0, column = 0, i = 0
    real(R_)                    :: result = 0
    real(R_), allocatable       :: array(:,:)

    input_file = "../data/input.txt"
    output_file = "output.txt"
    
    open (file=input_file, newunit=In)
        read(In, *) row, column
        allocate(array(row, column))
        read(In, *) (array(i,:), i = 1, row)
    close (In)

    open (file=output_file, encoding=E_, newunit=out)
        write (out,*) "Basic of array"
        write (out, "("//column//"f6.1)") (array(i,:), i = 1, row)
    close (out)
    
    call Min_Max_Element(array, result)
    
    open (file=output_file, encoding=e_, newunit=out, position="append")
        write (Out, * ) "Min element value by columns"
        write (Out,"(f6.1)") result
    close (out)

contains

    pure subroutine Min_Max_Element(array, result)
        real(R_) array(:,:)
        real(R_) result
        intent(in) array
        intent(out) result

        result = Minval(Maxval(array, 1))
    end subroutine Min_Max_Element

end program main




