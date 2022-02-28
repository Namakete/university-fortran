module cramer

    implicit none

    public :: f1, f2

    contains

    integer function f1(a,b,c,d,e,f) result(answer)
        integer :: a,b,c,d,e,f
        answer = (c * e - f * b) / (a * e - d * b)
    end function f1

    integer function f2(a,b,c,d,e,f) result(answer)
        integer :: a,b,c,d,e,f
        answer = (a * f - d * c) / (a * e - d * b) 
    end function f2

end module cramer