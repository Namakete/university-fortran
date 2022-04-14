! Copyright (c) Namakete (Ilya Oberemok) <namakete.dev@gmail.com>.
! See the LICENCE file in the repository root for full licence text.
program main
    use environment
    use IEEE_Arithmetic

    implicit none

    character(*), parameter    :: input_file = "../data/input.txt", output_file = "output.txt", errorType = "Case not provided"
    integer                    :: In = 0, Out = 0
    real(R_)                   :: a = 0, b = 0, c = 0, d = 0, x = 0, w = 0, func = 0, dig = 0

    open (file=input_file, newunit=In)
        read (In, *) a, b, c, d, x, w
    close(In)

    dig = ((a + b) / 2)

    open (file=output_file, encoding=E_, newunit=Out)
      write (Out, "(4(a, f0.2/))") "a = ", a, "b = ", b, "c = ", c, "d = ", d
      write (Out, "(2(a, f0.2/))") "x = ", x, "w = ", w
      write (Out, "(1(a, f0.2/))") "dig = ", dig
   close (Out)

   if ((a <= x .and. x <= b) .and. (c <= w .and. w <= d)) then
        func = F(x, w, dig)
   else 
        write(*, *) "Error massage: ", errorType
   end if

   open (file=output_file, encoding=E_, newunit=Out, position='append')
        write (Out, "('y(x,w) = ', f0.2)") func
   close (Out)
contains

    pure function F(x, w, dig)
        real(R_) F, x, w, dig
        intent(in) x, w, dig
 
        if(x < dig) then
            F = (x**2 + w**2)
        else if (w <= dig) then
            F = (x**2 - w**2)
        else 
            F = IEEE_Value(x, IEEE_Quiet_NaN)
        end if
    end function F
    
end program main







