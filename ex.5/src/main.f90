program main
  use environment

  implicit none
  
character(*), parameter      :: input_file = "../data/input.txt", output_file = "output.txt"
  integer                    :: In = 0, Out = 0, N = 0, M = 0
  logical                    :: mask = .true.
  integer, allocatable       :: X(:), Mat(:), Slice(:)
  logical, allocatable       :: Neg(:)

  open (file=input_file, newunit = In)
    read (In, *) N
    allocate(X(N))
    read(in,*) X
  close(In)

  Mat = X
  Slice = pack(Mat, Mask)

  allocate(Neg(N))
  call Negative(Slice, Neg, M)

  open (file=output_file, encoding=E_, newunit=Out)
  write (Out, "("//N//"(i0, 1x))") X
  write (Out, *) Neg
  write (Out, '(/1(a, T11, ": ", i0))') 'Array Size', N
  write (Out, '(1(a, T21, ": ", i0/))') 'Value negative items', M
  close (Out)

contains

  pure subroutine Negative(X, Neg, M)
    integer     X(:), M
    logical     Neg(:)
    intent(in)  X
    intent(Out) Neg, M

    Neg = X < 0
    M = Count(Neg)

 end subroutine Negative
 
end program main





