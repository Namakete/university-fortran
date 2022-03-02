module func

    use environment

    implicit none

    public :: f

    contains

    real function f(a, b) result(answer)
        real :: a, b
        answer = (a + b) / 2
     end function f

end module func