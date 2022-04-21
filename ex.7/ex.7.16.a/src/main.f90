! Copyright (c) Namakete (Ilya Oberemok) <namakete.dev@gmail.com>.
! See the LICENCE file in the repository root for full licence text.
program main
    use Environment

    implicit none

    character(*), parameter     :: input_file = "../data/input.txt" , output_file = "output.txt"
    integer                     :: In = 0, Out = 0, rows = 0, columns = 0, i = 0
    real(R_)                    :: Z = 0
    real(R_), allocatable       :: A(:,:)

    open (file=input_file, newunit=In)
        read(In, *) rows, columns
        allocate(A(rows, columns))
        read(In, *) (A(i,:), i = 1, rows)
    close (In)

    open (file=output_file, encoding=e_, newunit=out)
        write (out,*)"Array"
        write (out, "("//columns//"f6.1)") (A(i,:), i = 1, rows)
    close (out)
    
    call MinMaxElement(A,Z)
    
    open (file=output_file, encoding=e_, newunit=out, position="append")
        write (Out, * ) 'Min element value by columns'
        write (Out,"(f6.1)") Z
    close (out)
contains
    pure subroutine MinMaxElement(A,Z)
        real(R_) A(:,:)
        real(R_) Z
        intent(in) A
        intent(out) Z

        Z = Minval(Maxval(A, dim = 1))
    end subroutine MinMaxElement
end program main




