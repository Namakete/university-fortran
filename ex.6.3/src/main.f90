program main
  use environment

  implicit none
  
  character(*), parameter    :: input_file = "../data/input.txt", output_file = "output.txt"
  integer                    :: In = 0, Out = 1
  real(R_)                   :: x = 0

  open (file=input_file, newunit=In)
    read (In, *) x
  close(In)


  open (file = output_file, encoding = E_, newunit = Out)
    write(Out, *)
  close (Out)
  
end program main






