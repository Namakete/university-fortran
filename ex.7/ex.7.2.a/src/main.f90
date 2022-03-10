program main
  use Environment

   implicit none
   character(*), parameter :: input_file = "../data/input.txt", output_file = "output.txt"
   integer                 :: In = 0, Out = 0, i, N = 0
   real(R_), allocatable   :: A(:)
   logical, allocatable    :: Mask(:)       

   open (file=input_file, newunit=In)
      read (In, *) N
      allocate(A(N))
      read (In, *) A
   close (In)

   open (file=output_file, newunit=Out)
      write(Out, fmt = '(10F10.2)') A(:)
   close (Out)

   Mask = A > 0

   A=[Pack(A, .not. Mask), Pack(A, Mask)]

   do i = 1, N
      if (A(i) <= 0) then
        call sort(A,i,N)
        exit
      end if
   end do

   open (file=output_file, encoding=E_, newunit=Out, position='append')
        write(Out, fmt = '(10F10.2)') A(:)
    close (Out)

   contains
   
recursive subroutine sort(a, first, last)
   implicit none
   real(R_)  a(*), x, t
   integer first, last
   integer i, j

   x = a( (first+last) / 2 )
   i = first
   j = last
   do
      do while (a(i) < x)
         i=i+1
      end do
      do while (x < a(j))
         j=j-1
      end do
      if (i >= j) exit
      t = a(i);  a(i) = a(j);  a(j) = t
      i=i+1
      j=j-1
   end do
   if (first < i-1) call sort(a, first, i-1)
   if (j+1 < last)  call sort(a, j+1, last)
   end subroutine sort

end program main












