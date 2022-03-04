program main
  use func
  use environment

  implicit none

  character(*), parameter    :: input_file = "../data/input.txt", output_file = "output.txt"
  character(:), allocatable  :: fmt
  integer                    :: In = 0, Out = 1
  real(R_)                   :: a, b, c, d, x, w
  character                  :: plus = '+', minus = '-'

  open (file=input_file, newunit=In)
  read (In, *) a, b, c, d, x, w
  close(In)

  open (file=output_file, encoding=E_, newunit=Out)
  fmt = "(a, T7, f6.2)"

  if ((a <= x .and. x <= b) .and. (c <= w .and. w <= d)) then
    if((x < shorthand_multiplication(a, b)) .and. (w <= shorthand_multiplication(c, d))) then
      write (Out, fmt) "y1=", calculation(x, w, plus)
      write (Out, fmt) "y2=", calculation(x, w, minus)
    else 
      write (Out, fmt) "error1"
    end if
  else
    write (Out, fmt) "error2"
  end if

  close (Out)

end program main






