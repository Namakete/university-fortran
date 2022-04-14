! Copyright (c) Namakete (Ilya Oberemok) <namakete.dev@gmail.com>.
! See the LICENCE file in the repository root for full licence text.
program main
    use environment
  
    implicit none
  
    character(*), parameter    :: input_file = "../data/input.txt", output_file = "output.txt"
    integer                    :: In = 0, Out = 1
    real(R_)                   :: a, b, c, d, e, f, m1, m2
  
    open (file=input_file, newunit=In)
      read (In, *) a, b, c, d, e, f
    close(In)
  
    if (denominator(a, b, d, e) == 0) then
      print *, 'System determinant is zero'
    else
      m1 = f1(b, c, e, f) / denominator(a, b, d, e)
      m2 = f2(a, c, d, f) / denominator(a, b, d, e)
    end if 
      
    open (file = output_file, encoding = E_, newunit = Out)
        write (Out, *) "f1", m1
        write (Out, *) "f2", m2
    close (Out)

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
end program main
