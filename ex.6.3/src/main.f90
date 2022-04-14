! Copyright (c) Namakete (Ilya Oberemok) <namakete.dev@gmail.com>.
! See the LICENCE file in the repository root for full licence text.
program main
  use environment

  implicit none
  
  character(*), parameter    :: input_file = "../data/input.txt", output_file = "output.txt"
  integer                    :: In = 0, Out = 1
  real(R_)                   :: a, epsilon, y, y1

  open (file=input_file, newunit=In)
    read (In, *) a, epsilon
  close(In)

  if (a <= epsilon) then
    write (*, *) "Parameter 'A' is too low"
    stop
  end if

  y = a
  do
    y1 = y - (y*y-a)/(2*y)
    if (abs(y1-y) < epsilon) exit
    y = y1
  end do
 
  open (file = output_file, encoding = E_, newunit = Out)
    write (Out, '(1(a, T4, "= ", f0.4/))') "y", y1
  close (Out)
  
end program main






