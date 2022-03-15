program main
  use environment
  use MaxEleFind

  implicit none
  
  character(*), parameter           :: input_file = "../data/input.txt", output_file = "output.txt"
  integer                         :: In = 0, Out = 0, rows = 0, columns = 0, i = 0, Res = 0
  integer, allocatable, target    :: A(:,:)
  integer, contiguous, pointer    :: B(:)

  Open (file=input_file, newunit=In)
    read (In, *) rows, columns
    allocate (A(rows, columns))
    read (In, *) (A(i, :), i = 1, rows)
  close (In)

  open (file=output_file, encoding=E_, newunit=Out)
      write (Out, '('//columns//'i4)') (A(i, :), i = 1, rows)
  close (Out)

  B(1:Size(A)) => A

  call max_ele_find(B, Res)

  open (file=output_file, encoding=E_, newunit=Out, position='append')
    write(Out, *) "Max elemets: ", Res
  close (Out)
  
end program main





