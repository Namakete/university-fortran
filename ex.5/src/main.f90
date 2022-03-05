program main
  use environment

  implicit none
  
character(*), parameter      :: input_file = "../data/input.txt", output_file = "output.txt"
  integer                    :: In = 0, Out = 0, M(25)
  real(R_), allocatable      :: X(:)



  open (file = output_file, encoding = E_, newunit = Out)

  close (Out)
  
end program main






