! Copyright (c) Namakete (Ilya Oberemok) <namakete.dev@gmail.com>.
! See the LICENCE file in the repository root for full licence text.
program main
    use environment

    implicit none

    character(*), parameter         :: input_file = "../data/input.txt", output_file = "output.txt"
    integer                         :: In = 0, Out = 1, N = 0, i, max_pos, N_max_pos
    integer, allocatable            :: A(:,:), S(:,:)
    integer, allocatable            :: Indexes(:, :), Ind_max_pos(:, :)
    logical, allocatable            :: Mask(:)

    open (file=input_file, newunit=In)
        read (In, *) N
        allocate (A(N, N))
        read (In, *) (A(i, :), i = 1, N)
    close(In)

    open (file=output_file, encoding=E_, newunit=Out)
        write(*, *) 'Input array'
        write (*, '('//N//'i4)') (A(i,:), i = 1,N)
    close(Out)

    allocate (S (N, N-1))
    allocate (Indexes((N-1)**2, 2))
    allocate (Mask((N-1)**2), source=.false.)

    S(:,1:N-1) = A(:,1:N-1)+A(:,2:N)
    S(1:N-1,:) = S(1:N-1,:)+S(2:N,:)

    call MaxPos(S(1:N-1,1:N-1), max_pos, Mask, Indexes, Ind_max_pos, N_max_pos)

    open (file=output_file, encoding=E_, newunit=Out, position='append')
        write(Out, *) 'Output array'
        write (Out, '('//N-1//'i4)') (S(i,:), i=1,N-1)
        write(Out, *) 'Coordinates of the maximum element of the matrix'
        write (Out, '(2i4/)') (Ind_max_pos(i,:), i = 1,N_max_pos)
    close (Out)

contains
   pure subroutine MaxPos(C, max_pos, Mask, Indexes, Ind_max_pos, N_max_pos)
      integer, intent(in)    :: C(:, :)
      integer, intent(out)   :: max_pos
      integer, intent(out)    :: Indexes(:, :)
      integer, allocatable, intent(out) ::  Ind_max_pos(:, :)
      logical, intent(out)    :: Mask(:)
      integer, intent(out)    :: N_max_pos
      integer i, j, N, M

      N = size(C,1)
      M = size(C,2)

      Indexes(:, 1) = [((i, i = 1, N), j = 1, M)]
      Indexes(:, 2) = [((j, i = 1, N), j = 1, M)]

      max_pos = MaxVal(C)

      Mask        = [C == max_pos]
      N_max_pos   = Count(Mask)

      allocate(Ind_max_pos(N_max_pos, 2))

      Ind_max_pos(:, 1) = Pack(Indexes(:, 1), Mask)
      Ind_max_pos(:, 2) = Pack(Indexes(:, 2), Mask)
   end subroutine MaxPos
end program main
