program main
  use environment

  implicit none
  
  character(*), parameter    :: input_file = "../data/input.txt", output_file = "output.txt"
  integer                    :: In = 0, Out = 1, rows = 0, columns = 0, i = 0, j = 0, max_num = 0, temp = 0 
  integer, allocatable       :: A(:,:), Indexes(:), TempA(:)


  open (file=input_file, newunit=In)
    read(In, *) rows, columns
    allocate(A(columns,rows))
    allocate(Indexes(columns))
    read(In, *) (A(i,:), i = 1, columns)
  close (In)

  open (file=output_file, encoding=E_, newunit=Out)
      write(Out, *) 'Array'
      write (Out, '('//rows//'i4)') (A(i, :), i = 1, columns)
  close (Out)

  Indexes = [(i, i = 1, rows)]

  do i = 1, rows
    max_num = maxloc(A(1,i:rows),dim=1)

    TempA = A(:,i) 
    A(:,i) = A(:, max_num+i-1)
    A(:, max_num+i-1) = TempA

    temp = Indexes(i)
    Indexes(i) = Indexes(max_num+i-1)
    Indexes(max_num+i-1) = temp
  end do

  open (file=output_file, encoding=E_, newunit=Out, position='append')
    write(Out, *) 'index of maximum element in bounds from and to rows'
    write (Out, '('//columns//'i4)') (Indexes(i), i = 1, rows )
    write(Out, *) 'Sorted array'
    write (Out, '('//rows//'i4)') ((A(i, j), j = 1, rows ), i = 1, columns)
  close (Out)

end program main



