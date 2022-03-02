program main
   use func
   use environment

   implicit none

   character(*), parameter    :: input_file = "../data/input.txt", output_file = "output.txt"
   character(:), allocatable  :: fmt
   integer                    :: In = 0, Out = 1
   real(R_)                   :: a, b, c, d, x, w, y1!, y2
   character                  :: plus = '+'!, minus = '-'

   open (file=input_file, newunit=In)
      read (In, *) a, b, c, d, x, w
   close(In)

   !if(digit_conditions(a, b, x) < shorthand_multiplication(a, b)) then
      y1 = calc(x,w, plus)
   !end if



   open (file=output_file, encoding=E_, newunit=Out)
      fmt = "(a, T7, '= ', f6.2)"
      write (Out, fmt) "y1", y1
      !write (Out, fmt) "y2"
    close (Out)        

end program main






