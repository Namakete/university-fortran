program main
  use Environment

  implicit none
  character(*), parameter :: input_file = "../data/input.txt", output_file = "output.txt"
  integer                 :: In = 0, Out = 0, N = 0
   real(R_)                :: a = 0, b = 0, h = 0, I = 0
   real(R_), allocatable   :: X(:)                  

  open(newunit=In, file=input_file)
     read(In, *) a, b, h
  close(In)

  open (file=output_file, encoding=E_, newunit=Out)
      write (Out, '(3(a, T4, "= ", f0.4/))') "a", a, "b", b, "h", h
   close (Out)

  N = Int(((b-a)/h+.5_R_)/2)

  allocate(X(N))
  call Integral(a, h, X, I)

  open (file=output_file, encoding=E_, newunit=Out, position='append')
     write(Out,*) "The integral is ", I
  close(Out)

contains

   pure subroutine Integral(a, h, X, I)
      real(R_) a, h, X(:), I
      intent(in) a, h
      intent(out) X, I
      integer j

      X = [((a/2)+2*(a+h*(j-1)), j = 1, Size(X))]
      X = .8_R_* X *(-exp(X**2+.5_R_))
      I = Sum(X) * h
   end subroutine Integral

end program main





