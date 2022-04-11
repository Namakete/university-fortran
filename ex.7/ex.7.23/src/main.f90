program main
    use environment

    implicit none

    character(*), parameter         :: input_file = "../data/input.txt", output_file = "output.txt"
    integer                         :: In = 0, Out = 1, N = 0, i = 0, m, k
    integer, allocatable            :: A(:,:), S(:,:), XY(:,:)
 
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
    allocate (XY((N-1)**2, 2))

    S(:,1:N-1) = A(:,1:N-1)+A(:,2:N)
    S(1:N-1,:) = S(1:N-1,:)+S(2:N,:)
    
    k = 0
    do
      m = maxval (S)
      k = k+1
      XY(k,:) = maxloc(S)
      S (XY(k,1), XY(k,2)) = m-1  
      if (maxval (S) < m) exit
    end do

    open (file=output_file, encoding=E_, newunit=Out, position='append')
        write(Out, *) 'Output array'
        write (Out, '('//N-1//'i4)') (S(i,:), i=1,N-1)
        write(Out, *) 'Coordinates of the maximum element of the matrix'
        write (Out, '(2i4/)') (XY(i,:), i=1,k)
    close (Out)
end program main
