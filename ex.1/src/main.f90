program main
  implicit none
  real(8) :: a,b,c,qwe
  real(8),external :: matrixDeterminanta
  a = 2
  b = 4
  c = 6
  qwe = matrixDeterminanta(a,b,c)

  print *, qwe

end program main

real(8) function matrixDeterminanta(num1, num2, num3) result(answer) 
  real(8) :: num1, num2, num3
  answer = num1 + num2 + num3
end function matrixDeterminanta








