! Copyright (c) Namakete (Ilya Oberemok) <namakete.dev@gmail.com>.
! See the LICENCE file in the repository root for full licence text.
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

    N = NInt((b-a)/h)+1

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

        X = [((a+(j-1)*h), j = 1, Size(X))]
        X = .8_R_* X *(-exp(X**2+.5_R_))
        X(1) = X(1)*.5_R_
        X(Size(X)) = X (Size(X))*.5_R_

        I = Sum(X) * h
    end subroutine Integral

end program main


