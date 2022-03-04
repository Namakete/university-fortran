program main
  use environment
  use gen_array

  implicit none
  
  character(*), parameter       :: input_file = "../data/input.txt", output_file = "output.txt"
  integer                       :: In = 0, Out = 1, size = 10, N = 0
  real(R_), allocatable      :: Arr(:,:)

  call number_random_generation(size)
  
  open (file=input_file, newunit=In)
    read(In, *) N
    allocate(Arr(N,N))
  close(In)



  

  open (file=output_file, encoding=E_, newunit=Out)
  close (Out)
  
end program main






