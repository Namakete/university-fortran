! Copyright (c) Namakete (Ilya Oberemok) <namakete.dev@gmail.com>.
! See the LICENCE file in the repository root for full licence text.
program main
    use environment

    implicit none

    character(:), allocatable   :: input_file, output_file
    integer                     :: In = 0, Out = 1
    real(R_)                    :: a, b, c, d, e, f, m1, m2
    
    input_file = "../data/input.txt"
    output_file = "output.txt"

    open (file=input_file, newunit=In)
        read (In, *) a, b, c, d, e, f
    close(In)

    if (Denominator(a, b, d, e) == 0) then
        print *, 'System determinant is zero'
    else
        m1 = F1(b, c, e, f) / Denominator(a, b, d, e)
        m2 = F2(a, c, d, f) / Denominator(a, b, d, e)
    end if

    open (file = output_file, encoding = E_, newunit = Out)
        write (Out, *) "f1", m1
        write (Out, *) "f2", m2
    close (Out)

contains

    real function Denominator(a,b,d,e) result(answer)
        real :: a, b, d, e

        answer = (a * e - d * b)
    end function Denominator

    real function F1(b,c,e,f) result(answer)
        real :: b, c, e, f

        answer = (c * e - f * b)
    end function F1

    real function F2(a,c,d,f) result(answer)
        real :: a, c, d, f

        answer = (a * f - d * c)
    end function F2

end program main
