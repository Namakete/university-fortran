module func

private :: f
public :: calc

contains

integer function f(a, b) result(answer)
    integer :: a, b
        answer = (a + b) / 2
    end function f


end module func