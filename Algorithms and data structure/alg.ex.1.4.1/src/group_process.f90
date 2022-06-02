! Copyright (c) Namakete (Ilya Oberemok) <namakete.dev@gmail.com>.
! See the LICENCE file in the repository root for full licence text.  
module Group_Process
   use Environment
   use Group_IO

   implicit none

   character(kind=CH_), parameter   :: MALE = Char(1052, CH_), FEMALE = Char(1046, CH_)

contains
   pure recursive subroutine Max_and_sum (Group, i, Boys_Max_Value, Girls_Max_Value, &
           Sum_Boys_Amount, Sum_Girls_Amount, Number_of_Boys, Number_of_Girls)
     type(student), intent(inout) :: Group(:)
     integer, intent(in) :: i
     real(R_), intent(inout) :: Boys_Max_Value, Girls_Max_Value, Sum_Boys_Amount, Sum_Girls_Amount
     integer, intent(inout) :: Number_of_Boys, Number_of_Girls

     if (i <= size(Group)) then
       if (Group(i)%Sex == MALE) then
         Sum_Boys_Amount = Sum_Boys_Amount + Group(i)%Aver_mark
         Number_of_Boys = Number_of_Boys + 1
         if (Group(i)%Aver_mark > Boys_Max_Value) &
           Boys_Max_Value = Group(i)%Aver_mark
       end if
       if (Group(i)%Sex == FEMALE) then
         Sum_Girls_Amount = Sum_Girls_Amount + Group(i)%Aver_mark
         Number_of_Girls = Number_of_Girls + 1         
         if (Group(i)%Aver_mark > Girls_Max_Value) &
           Girls_Max_Value = Group(i)%Aver_mark
       end if
       call Max_and_sum (Group, i+1, Boys_Max_Value, Girls_Max_Value, &
           Sum_Boys_Amount, Sum_Girls_Amount, Number_of_Boys, Number_of_Girls)
     end if 
   end subroutine Max_and_sum
end module group_process
