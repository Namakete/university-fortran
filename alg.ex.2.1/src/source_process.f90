! Copyright (c) Namakete (Ilya Oberemok) <namakete.dev@gmail.com>.
! See the LICENCE file in the repository root for full licence text.  
module Source_Process
   use Environment
   use Source_IO

   implicit none

contains
    pure subroutine Array_slice(InitCode, N, N1, N2, LR) 
        type(SourceLine), intent(in):: InitCode
        integer(I_), intent(inout)  :: N, N1, N2
        character, intent(inout)    :: LR
   
        read(InitCode%String(1:1),'(I1)') N
        read(InitCode%String(3:3),'(I1)') N1
        read(InitCode%String(5:5),'(I1)') N2
        
        LR = InitCode%String(7:7)
        
        if(LR == 'R') then 
            N = -N
        end if
    end subroutine Array_slice

    recursive function Diff_Codes(InitialCode,  k, N, N1, N2) result(DiffCode)
        type(SourceLine), pointer     :: DiffCode
        type(SourceLine), intent(in)  :: InitialCode
        
        integer(I_), intent(in)       :: N, N1, N2, k
        integer(I_)                   :: i
        
        character(CH_), allocatable   :: temp(:)
        
        allocate (DiffCode)

        if ((k >= N1) .and. (k<= N2) ) then 
            allocate (temp(len_trim(InitialCode%String)))

            do i = 1, len_trim(InitialCode%String)
                temp(i) =InitialCode%String(i:i)
            end do
         
            temp = EOSHIFT(temp, N,dim = 1)
         
            DiffCode%String =InitialCode%String
            
            do i = 1, size(temp)
                DiffCode%String(i:i) = temp(i)
            end do
        else
            DiffCode%String =Trim(InitialCode%String)
        end if
  
        if (Associated(InitialCode%next) ) &
            DiffCode%next => Diff_Codes(InitialCode%next, k+1, N, N1, N2)
     end function Diff_Codes
end module Source_process
