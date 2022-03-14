program lab4_5a
  use Environment
  use ieee_arithmetic

  implicit none
  character(*), parameter :: input_file = "../data/input.txt", output_file = "output.txt"
  integer                 :: In=0, Out=1, i = 0, H = 0
  real(R_)                :: a = 0, b = 0             !границы интегрирования
  real(R_)                :: n                        !шаг  
  real(R_),allocatable    :: X(:)                     !Массив точек вектора
  real(R_)                :: Integral

  open(newunit=In, file=input_file)
     read(In, *) a, b, n
  close(In)

  open (file=output_file, encoding=E_, newunit=Out)
      write (Out, '(3(a, T4, "= ", f0.4/))') "a", a, "b", b, "h", n
   close (Out)

  H = Int((b-a)/n)
  
  allocate(X(H))

  X = [((a+n*(i+1)/2), i = 1, H)]
  
  Integral = n*Sum(0.8*(X)*(-exp(X**2+.5_R_)))

  open (file=output_file, encoding=E_, newunit=Out, position='append')
     write(Out,*) "The integral is", Integral
  close(Out)

end program lab4_5a 





