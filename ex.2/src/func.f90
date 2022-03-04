module func
  use environment

  implicit none

  public :: shorthand_multiplication, calculation

contains

  real function calculation(x, w, op) result(r)
    real :: x, w
    character :: op

    select case (op)
    case('+')
      r = x**2 + w**2
    case('-')
      r = x**2 - w**2
    end select
  end function calculation

  real function shorthand_multiplication(num1, num2) result(r)
    real :: num1, num2
    r = (num1 + num2) / 2
  end function shorthand_multiplication

end module func
