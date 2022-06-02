! Copyright (c) Namakete (Ilya Oberemok) <namakete.dev@gmail.com>.
! See the LICENCE file in the repository root for full licence text.  
module Source_Process
   use Environment
   use Source_IO

   implicit none

contains
    recursive function Diff_Codes(InitialCode,  k, N, N1, N2) result(DiffCode)
        type(SourceLine), pointer       :: DiffCode
        type(SourceLine), intent(in)    :: InitialCode
        
        integer(I_), intent(in)         :: N, N1, N2, k
        
        allocate (DiffCode)
        if ((k >= N1) .and. (k<= N2)) then 
            DiffCode%String = EOSHIFT(InitialCode%String, N)
        else
            DiffCode%String = InitialCode%String
        end if

        if (Associated(InitialCode%next)) &
            DiffCode%next => Diff_Codes(InitialCode%next, k+1, N, N1, N2)
     end function Diff_Codes
end module Source_process
