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
      write (Out, "(6(a, f0.2/))") "a = ", a, "b = ", b, "c = ", c, "d = ", d, "x = ", x, "w = ", w
   close (Out)

   func = F(a, b, c, d, x, w)
  
contains

    function F(a, b, c, d, x, w)
        real(R_) F, a, b, c, d, x, w
        intent(in) a, b, c, d, x, w
 
        open (file=output_file, encoding=E_, newunit=Out, position='append')
        eval: block
            if ((a <= x .and. x <= b) .and. (c <= w .and. w <= d)) then
                if(x < ((a + b)/2)) then
                    F = (x**2 + w**2)
                else if (w <= ((a + b)/2)) then
                    F = (x**2 - w**2)
                else 
                    F = IEEE_Value(x, IEEE_Quiet_NaN)
                end if
                write (Out, "('y = ', f0.2)") F
            else
                write (Out, "('Case not provided')")
            end if
        end block eval
        close (Out)
    end function F
    
end program main







