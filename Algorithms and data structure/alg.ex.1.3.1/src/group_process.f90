! Copyright (c) Namakete (Ilya Oberemok) <namakete.dev@gmail.com>.
! See the LICENCE file in the repository root for full licence text.
module Group_Process
   use Environment
   use Group_IO

   implicit none
   
contains
    pure function Find_Amount(Group) result (Max_Amount_Position)
        type(student), intent(in)   :: Group(:)
        real(R_)                    :: Max_Amount
        integer, allocatable        :: Max_Amount_Position(:)
        integer                     :: i
    
        Max_Amount = MaxVal(Real(Group%Aver_Mark, R_))
        Max_Amount_Position = Pack([(i, i = 1, Size(Group))],[Group%Aver_mark==Max_Amount])
    end function Find_Amount
 
    pure subroutine Sub_average_mark(Group,Aver_Mark)
        type(student), intent(in)   :: Group(:)
        real(R_), intent(inout)     :: Aver_Mark

        Aver_Mark = Sum(Group%Aver_Mark)/Size(Group)
    end subroutine Sub_average_mark
end module group_process
