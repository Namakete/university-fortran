program main
  use environment

  implicit none
  
  character(*), parameter         :: input_file = "../data/input.txt", output_file = "output.txt"
  integer                         :: In = 0, Out = 1, rows = 0, columns = 0, i = 0
  integer, allocatable, target    :: C(:,:)
  integer, contiguous, pointer    :: B(:)
  logical, allocatable            :: Mask(:,:)
  integer, allocatable            :: Indexes(:), MTIndexes(:), MTElements(:) 

  open (file=input_file, newunit=In)
    read (In, *) rows, columns
    allocate (C(rows, columns))
    read (In, *) (C(i, :), i = 1, rows)
  close(In)

  open (file=output_file, encoding=E_, newunit=Out)
      write (Out, '('//columns//'i4)') (C(i, :), i = 1, rows)
  close (Out)
    
    allocate(Mask(rows, columns), source = .false.)

  B(1:Size(C)) => C

  Indexes = [(i, i = 1, Size(B))] 
!
  Mask = ((Mod(Indexes, 2) == 0) .and. (B < 1))

  MTIndexes = pack(Indexes, mask)
  MTElements = pack(B, mask)

  open (file=output_file, encoding=E_, newunit=Out, position='append')
    write(Out, *) 'Indexes'
    write (Out, '('//columns//'i4)') MTIndexes
    write(Out, *) 'Elements'
    write (Out, '('//columns//'i4)') MTElements
  close (Out)
  
end program main















