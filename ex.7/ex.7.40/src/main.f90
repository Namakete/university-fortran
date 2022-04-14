! Copyright (c) Namakete (Ilya Oberemok) <namakete.dev@gmail.com>.
! See the LICENCE file in the repository root for full licence text.
program main
    use environment

    implicit none

    character(*), parameter         :: input_file = "../data/input.txt", output_file = "output.txt"
    integer                         :: In = 0, Out = 1, N = 0, i = 0, numbers
    integer, allocatable, target    :: C(:,:)
    integer, contiguous, pointer    :: B(:)
    integer, allocatable            :: Elements(:)

    open (file=input_file, newunit=In)
        read (In, *) N
        allocate (C(N, N))
        read (In, *) (C(i, :), i = 1, N)
    close(In)

    open (file=output_file, encoding=E_, newunit=Out)
        write (Out, '('//N//'i4)') (C(i, :), i = 1, N)
    close (Out)

    numbers = Size(C)

    if (.not.(mod(numbers, 2) == 0)) then
        B(1:Size(C)) => C
        Elements = [(Pack(B(::2), B(::2)<0))]
    else
        Elements = [(Pack(C(1:N:2,i), C(1:N:2,i)<0), Pack(C(2:N:2,i+1),C(2:N:2,i+1)<0), i=1,N,2)]
    end if

    open (file=output_file, encoding=E_, newunit=Out, position='append')
        write(Out, *) 'Elements'
        write (Out, '('//N//'i4)') Elements
    close (Out)

end program main
