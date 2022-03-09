program main
    use environment

    implicit none

    character(*), parameter    :: input_file = "../data/input.txt", output_file = "output.txt"
    integer                    :: In = 0, Out = 0,  rows = 0, columns = 0, i = 0
    integer, allocatable       :: A(:,:)
    integer, dimension(3)      :: rows_sum
    integer, dimension(4)      :: columns_sum

    open (file=input_file, newunit=In)
        read(In, *) rows, columns
        allocate(A(rows, columns))
        do i = 1, rows
            read(in,*) A(i,:)
        end do
    close (In)

    open (file=output_file, encoding=E_, newunit=Out)
        write(Out, '(/(4(4X,I0)/))') A
    close (Out)
    
    columns_sum = sum(a,dim=2)
    rows_sum = sum(a,dim=1)

    open (file=output_file, encoding=E_, newunit=Out, position='append')
        write(Out, '(A,4(1X,I0))') 'sum columns:', columns_sum
        write(Out, '(A,4(1X,I0))') 'sum rows:',  rows_sum 
    close (Out) 

end program main








