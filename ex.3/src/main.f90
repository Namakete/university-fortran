program main
  use environment

  implicit none

  character(*), parameter    :: input_file = "../data/input.txt", output_file = "output.txt"
  integer                    :: In = 0, Out = 0,  rows = 0, columns = 0, i = 0
  integer, allocatable       :: A(:,:)

  open (file=input_file, newunit=In)
   read(In, *) rows, columns
   allocate(A(rows, columns))

   do i = 1, rows
      read(in,*) a(i,:)
   end do
  close (In)

  open (file=output_file, newunit=Out)
      print '(A,4(1X,I0))', 'row:', sum(a,dim=2)
      print '(A,4(1X,I0))', 'column:',  sum(a,dim=1)
  close (Out)

end program main








