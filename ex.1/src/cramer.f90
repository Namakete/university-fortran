module cramer

    use environment

    implicit none

    public :: f1, f2

    contains

    real function denominator(a,b,d,e) result(answer)
        real :: a,b,d,e
        answer = (a * e - d * b)
    end function denominator    

    real function f1(b,c,e,f) result(answer)
        real :: b,c,e,f
        answer = (c * e - f * b)
    end function f1

    real function f2(a,c,d,f) result(answer)
        real :: a,c,d,f
        answer = (a * f - d * c) 
    end function f2

end module cramer
