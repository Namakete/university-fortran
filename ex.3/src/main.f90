program main
  use environment
  use gen_array

  implicit none
  
  character(*), parameter       :: input_file = "../data/input.txt", output_file = "output.txt"
  integer                       :: In = 0, Out = 0, rows = 4, columns = 3
  real(R_), allocatable         :: Arr(:,:)
  real                          :: r_rows = 0, r_columns = 0
  !integer                       :: size = 10

  !call number_random_generation(size)
  
  open (file=input_file, newunit=In)
    read(In, *) rows, columns
    allocate(Arr(rows,columns))
  close(In)







  

  open (file=output_file, encoding=E_, newunit=Out)
    write (Out, "(f6.3)") r_rows
    write (Out, "(f6.3)") r_columns
  close (Out)
  
end program main






