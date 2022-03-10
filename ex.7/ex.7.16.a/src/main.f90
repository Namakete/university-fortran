program main
  use environment

  implicit none
  
character(*), parameter    :: input_file = "../data/input.txt", output_file = "output.txt"
  integer                  :: In = 0, Out = 0, size
  integer, allocatable    :: A(:,:)

  open (file=input_file, newunit=In)
    read (In, *) size
    allocate(A(size, size))
    read (In, *) A
  close(In)

  open (file = output_file, encoding = E_, newunit = Out)
    write (Out, *) "MinVal", MinVal((A))
    write (Out, *) "MaxVal", MaxVal((A))
    write (Out, *) "MinLoc", MinLoc((A))
    write (Out, *) "MaxLoc", MaxLoc((A))
    write (Out, *) "Sum", Sum((A))

  close (Out)
  
end program main






