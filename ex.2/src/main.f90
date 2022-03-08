program main
    use environment

    implicit none

    character(*), parameter    :: input_file = "../data/input.txt", output_file = "output.txt"
    character(:), allocatable  :: fmt
    integer                    :: In = 0, Out = 1
    real(R_)                   :: a, b, c, d, x, w, y1, y2

    open (file=input_file, newunit=In)
        read (In, *) a, b, c, d, x, w
    close(In)

    
    if ((a <= x .and. x <= b) .and. (c <= w .and. w <= d)) then
        if(x < shorthand_multiplication(a, b)) then
            y1 = (x**2 + w**2)
        else if (w <= shorthand_multiplication(c, d)) then
            y1 = (x**2 - w**2)
        else 
            write (Out, fmt) "Case not provided"
        end if
        else
            write (Out, fmt) "Case not provided"
    end if
    
    open (file=output_file, encoding=E_, newunit=Out)
        fmt = "(a, T7, f6.2)"
        write(Out, fmt) 'y1', y1
        write(Out, fmt) 'y2', y2
    close (Out)

contains

    real function shorthand_multiplication(num1, num2) result(r)
        real :: num1, num2
        r = (num1 + num2) / 2
    end function shorthand_multiplication

end program main







