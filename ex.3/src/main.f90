program main
  use environment

  implicit none
  
   character(*), parameter    :: input_file = "../data/input.txt", output_file = "output.txt"
   integer                    :: In = 0, Out = 0, rows = 0, columns = 0!, i = 0
   integer, allocatable       :: A(:,:)
   integer                    :: res_rows = 0, res_columns = 0 
         
   open (file=input_file, newunit=In)
    read(In, *) rows, columns
    allocate(A(rows, columns))
    read (In, *) A
   close (In)

   res_rows = sum(A(1:columns+1,1), dim=1)

   res_columns = sum(A(1:rows+1,1), dim=1)

   !outout data
   open (file=output_file, encoding=E_, newunit=Out, position='append')
      write(*,*)"rows:",res_rows
      write(*,*)"columns:",res_columns
   close (Out)

end program main








