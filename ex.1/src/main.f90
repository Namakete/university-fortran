program main

  use iso_fortran_env

  implicit none

  real(8) :: a = 2, b = 4, c = 6, d = 8, e = 10, f = 12

  real(8) :: f1, f2

  f1 = (c * e - f * b) / (a * e - d * b)
  f2 = (a * f - d * c) / (a * e - d * b) 

  print *, f1
  print *, f2



end program main

