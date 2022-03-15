program main
  use environment
  implicit none

  character(*), parameter :: input_file = "../data/input.txt", output_file = "output.txt"
  integer                 :: Size = 0, Out = 0, In = 0, i = 0 
  integer, allocatable    :: X(:), Indexes(:), NegativeIndexes(:), NegativeArray(:)
  logical, allocatable    :: mask(:)

  open (file=input_file, newunit=In)
      read(In, *) Size
      allocate(X(Size))
      read(In, *) X(:)
  close (In)

  Indexes = [(i, i = 1, Size)]
  
  open (file=output_file, encoding=E_, newunit=Out)
    write(Out,*) 'Indexes'
    write (Out, '('//Size//'i4)') Indexes
    write(Out,*) 'Array'
    write (Out, '('//Size//'i4)') X
  close (Out)

  mask = X < 0
  print *, mask

  NegativeIndexes = pack(Indexes, mask)
  NegativeArray =  pack(X, mask)


  open (file=output_file, encoding=E_, newunit=Out, position='append')
      write(Out, *) 'Negative indexes'
      write(Out, '('//Size//'i4)') NegativeIndexes
      write(Out, *) 'Negative array'
      write(Out, '('//Size//'i4)') NegativeArray
  close (Out)

end program main


