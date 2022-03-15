program main
  use Environment

  implicit none
  character(*), parameter :: input_file = "../data/input.txt", output_file = "output.txt"
  integer                 :: In = 0, Out = 0, i = 0, H = 0
  real(R_)                :: a = 0, b = 0             
  real(R_)                :: n                          
  real(R_),allocatable    :: X(:)                    
  real(R_)                :: Integral

  open(newunit=In, file=input_file)
     read(In, *) a, b, n
  close(In)

  open (file=output_file, encoding=E_, newunit=Out)
      write (Out, '(3(a, T4, "= ", f0.4/))') "a", a, "b", b, "h", n
   close (Out)

  H = Int((b-a)/n)
  
  allocate(X(H))

  X = [((a+n*(i+1)), i = 1, H)]

  Integral = n*Sum(0.8*(X)*(-exp(X**2+.5_R_)))

  open (file=output_file, encoding=E_, newunit=Out, position='append')
     write(Out,*) "The integral is", Integral
  close(Out)

end program main





