program main
  use environment
  use gen_array

  implicit none
  
character(*), parameter    :: input_file = "../data/input.txt", output_file = "output.txt"
  character(:), allocatable  :: fmt
  integer                    :: In = 0, Out = 1, arr_size = 10, n = 0
  real(R_), allocatable      :: Arr(:)

  call number_random_generation(arr_size)
  
  open (file=input_file, newunit=In)
    read (In, *) n
    allocate(Arr(n))
    read(In, *) Arr
  close(In)

  


  open (file = output_file, encoding = E_, newunit = Out)
    fmt = "(a, T7, '= ', f6.2)"
    write (Out, fmt) "z="
  close (Out)
  
end program main






