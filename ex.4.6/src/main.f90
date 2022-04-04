program main
  use Environment

  implicit none
  character(*), parameter :: input_file = "../data/input.txt", output_file = "output.txt"
  integer                 :: In = 0, Out = 0, n = 0, i
  real(R_)                :: h = 0, s = 0
  real(R_), allocatable   :: X(:)                  

  open(newunit=In, file=input_file)
     read(In, *)  h
  close(In)

  open (file=output_file, encoding=E_, newunit=Out)
      write (Out, '(1(a, T4, "= ", f0.4/))') "h", h
   close (Out)

   n = NInt(1._R_/h)

   X = [(f(h*i), i=0,n)]
   X(1) = X (1)*.5_R_
   X(N+1) = X (N+1)*.5_R_
  !print *, X 
   s = h * sum(X)

  open (file=output_file, encoding=E_, newunit=Out, position='append')
     write(Out,*) "The integral is ", s
  close(Out)

contains

pure function f (x) result(r)
    real(R_), intent(in) :: x
    real(R_) :: r

    r = .8_R_*x*exp(-(x*x+.5_R_))
end function f 

end program main





