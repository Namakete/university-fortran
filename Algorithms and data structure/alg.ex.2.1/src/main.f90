! Copyright (c) Namakete (Ilya Oberemok) <namakete.dev@gmail.com>.
! See the LICENCE file in the repository root for full licence text.
program main
    use Environment
    use Source_Process
    use Source_IO

    implicit none

    character(:), allocatable :: File1, File2, File3

    type(SourceLine), pointer :: InitialCode  => Null()
    type(SourceLine), pointer :: DiffCode     => Null()

    integer(I_)               :: N = 0, N1 = 0, N2 = 0, k = 1

    File1 = "../data/source.f90"
    File2 = "../data/mod_source.f90"
    File3 = "source.f90.diff"

    InitialCode => Read_Source_Code(File1)

    call ReadCord(File2, N, N1, N2)

    if (Associated(InitialCode)) then
        DiffCode => Diff_Codes(InitialCode, k, N, N1, N2)
        if (Associated(DiffCode)) &
            call Output_Source_Code(File3, DiffCode, "Обрезанная пирамида:")
    end if
end program main
