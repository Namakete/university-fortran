program main
  use environment
  use cramer

  implicit none

  character(*), parameter    :: input_file = "../data/input.txt", output_file = "output.txt"
  character(:), allocatable  :: fmt
  integer                    :: In = 0, Out = 1
  real(R_)                   :: a, b, c, d, e, f, m1, m2

  open (file = input_file, newunit = In)

    read (In, *) a, b, c, d, e, f
  close(In)

  if (denominator(a, b, d, e) == 0) then
    print *, 'System determinant is zero'
  else
    m1 = f1(b, c, e, f) / denominator(a, b, d, e)
    m2 = f2(a, c, d, f) / denominator(a, b, d, e)
    
    open (file = output_file, encoding = E_, newunit = Out)
      fmt = "(a, T7, '= ', f6.2)"
      write (Out, fmt) "f1", m1
      write (Out, fmt) "f2", m2
    close (Out)
  end if

end program main

