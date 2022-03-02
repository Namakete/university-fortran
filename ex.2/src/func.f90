module func
    use environment

    implicit none

    public :: shorthand_multiplication, digit_conditions, calculation

    contains

    real function calculation(x, w, op) result(r)
        real :: x, w
        character :: op
        
        select case (op)
        case('+')
            r = x**2 + w**2
        case('-')
            r = x**2 - w**w
        end select
    end function calculation

    real function digit_conditions(num_firts, num_secont, digit) result(r)
        real :: num_firts, num_secont, digit
        if(digit >= num_firts .and. digit <= num_secont) then
            r = digit
        else 
            write(*,*) "'digit' could be >= or <= " 
        end if 
    end function digit_conditions

    real function shorthand_multiplication(a, b) result(r)
        real :: a, b
        r = (a + b) / 2
    end function shorthand_multiplication

end module func