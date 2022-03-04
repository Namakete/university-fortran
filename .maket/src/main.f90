program main
  use environment

  implicit none
  
character(*), parameter    :: input_file = "../data/input.txt", output_file = "output.txt"
  character(:), allocatable  :: fmt
  integer                    :: In = 0, Out = 1
  real(R_)                   :: a, b, z

  open (file=input_file, newunit=In)
  read (In, *) a, b
  close(In)

  z = a + b

  open (file = output_file, encoding = E_, newunit = Out)
  fmt = "(a, T7, '= ', f6.2)"
  write (Out, fmt) "z=", z
  close (Out)
  
end program main






