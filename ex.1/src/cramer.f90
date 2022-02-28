module cramer

    implicit none

    public :: f1, f2

    contains

    integer function denominator(a,b,d,e) result(answer)
        integer :: a,b,d,e
        answer = (a * e - d * b)
    end function denominator    

    integer function f1(b,c,e,f) result(answer)
        integer :: b,c,e,f
        answer = (c * e - f * b)
    end function f1

    integer function f2(a,c,d,f) result(answer)
        integer :: a,c,d,f
        answer = (a * f - d * c) 
    end function f2

end module cramer