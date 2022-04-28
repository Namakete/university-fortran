! Copyright (c) Namakete (Ilya Oberemok) <namakete.dev@gmail.com>.
! See the LICENCE file in the repository root for full licence text.
program main
    use environment
    use Group_IO

    implicit none

    character(kind=CH_), parameter  :: MALE = Char(1052, CH_), FEMALE = Char(1046, CH_)
    character(:), allocatable       :: input_file, output_file, data_file

    input_file  = "../data/input.txt"
    output_file = "output.txt"
    data_file   = "input.dat"

    call Create_data_file(input_file, data_file)

end program main
