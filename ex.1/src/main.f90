program main

  use cramer

  implicit none

  integer :: a = 2, b = 4, c = 6, d = 8, e = 10, f = 12
  integer :: m1, m2 = 2

  if (denominator(a,b,d,e) == 0) then
    print *, 'System determinant is zero'
  else
    m1 = f1(b,c,e,f) / denominator(a,b,d,e)
    m2 = f2(a,c,d,f) / denominator(a,b,d,e)

    print *, 'f1 = ',m1
    print *, 'f2 = ',m2
  end if

end program main

