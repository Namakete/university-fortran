program main
    use environment
    use IEEE_Arithmetic

    implicit none

    character(*), parameter    :: input_file = "../data/input.txt", output_file = "output.txt"
    integer                    :: In = 0, Out = 0
    real(R_)                   :: a = 0, b = 0, c = 0, d = 0, x = 0, w = 0, func = 0

    open (file=input_file, newunit=In)
        read (In, *) a, b, c, d, x, w
    close(In)

    open (file=output_file, encoding=E_, newunit=Out)
      write (Out, "(4(a, f0.2/))") "a = ", a, "b = ", b, "c = ", c, "d = ", d, "x = ", x, "w = ", w
   close (Out)

    if ((a <= x .and. x <= b) .and. (c <= w .and. w <= d)) then
        func = F(a, b, x, w)
    else 
        write(Out, "('f = ', f0.2)") "Error"
    end if
    
    open (file=output_file, encoding=E_, newunit=Out, position='append')
        write(Out, "(1(a, f0.2/))") "f1 = ", func
    close (Out)

contains

    pure function F(a, b, x, w)
        real(R_) F, a, b, x, w
        intent(in) a, b, x, w

        if(x < ((a + b)/2)) then
            F = (x**2 + w**2)
        else if (w <= ((a + b)/2)) then
            F = (x**2 - w**2)
        else 
            F = IEEE_Value(x, IEEE_Quiet_NaN)
        end if
        
    end function F

end program main







