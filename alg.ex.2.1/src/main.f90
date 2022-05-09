! Copyright (c) Namakete (Ilya Oberemok) <namakete.dev@gmail.com>.
! See the LICENCE file in the repository root for full licence text.  
program main
    use Environment 
    use Source_Process
    use Source_IO

    implicit none
   
    character(:), allocatable :: File1, File2, File3

    type(SourceLine), pointer :: InitialCode  => Null()
    type(SourceLine), pointer :: ModdedCode   => Null()
    type(SourceLine), pointer :: DiffCode     => Null()
    
    integer(I_)               :: N = 0, N1 = 0, N2 = 0, C = 1
    character(CH_)            :: LR

    File1 = "../data/source.f90"
    File2 = "../data/mod_source.f90"
    File3 = "source.f90.diff"
   
    InitialCode => Read_Source_Code(File1)
    ModdedCode  => Read_Source_Code(File2)
    
    if (Associated(ModdedCode)) &
        call Array_slice(ModdedCode, N, N1, N2, LR)

    if (Associated(InitialCode)) then
        DiffCode => Diff_Codes(InitialCode, C, N, N1, N2)
        if (Associated(DiffCode)) &
            call Output_Source_Code(File3, DiffCode)
    end if
end program main
