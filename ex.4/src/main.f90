program main
  use environment

  implicit none
  
character(*), parameter    :: input_file = "../data/input.txt", output_file = "output.txt"
  integer                    :: In = 0, Out = 0
  real(R_)                   :: a = 0, b = 0, n = 100, h = 0, dx = 0

  open (file=input_file, newunit=In)
    read (In, *) a, b, h
  close(In)
  
  dx = (0.8 * x *)

  h = trap_f(a, b, n)








  open (file = output_file, encoding = E_, newunit = Out)
    write (Out, *) "Trapez: "
  close (Out)

contains 

  !Формлу трапеций, которая вычисляет длину каждого
  !маленького отрезка или шага
  pure function trap_f(a, b, n)
    real(R_) trap_f, a, b, n
    intent(in) a, b, n

    trap_f = ((b - a) / n)

  end function trap_f

end program main






