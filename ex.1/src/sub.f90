module sub
  implicit none
  private
  public :: plus
contains
  integer function plus(a, b) result(answer)
    integer :: a, b
    answer = a + b
  end function plus
end module sub
