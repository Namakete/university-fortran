module gen_array
    use environment

    implicit none

    public :: number_random_generation

contains

subroutine number_random_generation(n)
    integer :: n, Out = 0, i
    character(*), parameter    :: input_file = "../data/input.txt"
    real(4) :: random

    open(file=input_file, encoding=E_, newunit=Out)
    write(Out, "(i10)") n
    do i = 1, n
        call random_seed()
        call random_number(random)
        write (Out, "(f6.3)") random
    end do
    close(Out)
end subroutine number_random_generation

end module gen_array