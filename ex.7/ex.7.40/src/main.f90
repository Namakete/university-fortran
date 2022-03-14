program main
  use environment

  implicit none
  
character(*), parameter           :: input_file = "../data/input.txt", output_file = "output.txt"
  integer                         :: In = 0, Out = 1, rows = 0, columns = 0, i = 0
  integer, allocatable, target    :: A(:,:)
  integer, contiguous, pointer    :: B(:)
  logical, allocatable            :: Mask(:)
  integer, allocatable            :: Indexes(:), MTIndexes(:), Elements(:)
  

  open (file=input_file, newunit=In)
    read (In, *) rows, columns
    allocate (A(rows, columns))
    read (In, *) (A(i, :), i = 1, rows)
  close(In)

  open (file=output_file, encoding=E_, newunit=Out)
      write (Out, '('//columns//'i4)') (A(i, :), i = 1, rows)
  close (Out)

  B(1:Size(A)) => A

  Indexes = [(i, i = 1, Size(B))] 

  Mask = (Mod(Indexes, 2) == 0)

  MTIndexes = pack(B, mask)

  print *, MTIndexes

  do i = 1, Size(MTIndexes)
    if (MTIndexes(i) < 1) Elements = MTIndexes(i)
  end do

  print *, Elements

  open (file=output_file, encoding=E_, newunit=Out, position='append')
    write (Out, *)
  close (Out)
  
end program main






