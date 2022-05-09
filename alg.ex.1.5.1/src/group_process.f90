! Copyright (c) Namakete (Ilya Oberemok) <namakete.dev@gmail.com>.
! See the LICENCE file in the repository root for full licence text.  
module Group_Process
    use Environment
    use Group_IO

    implicit none

contains
    pure recursive subroutine Get_list_by_gender(Stud, List, Amount, Gender)
        type(student), intent(in)        :: Stud
        type(student), pointer           :: List
        integer(I_), intent(inout)       :: Amount
        character(kind=CH_), intent(in)  :: Gender

        if (Stud%Sex == Gender) then
            allocate (List, source=Stud)
            
            Amount = Amount + 1
            
            List%Aver_Mark = Real(Sum(List%Marks), R_) / MARKS_AMOUNT
            List%next => Null()
            
            if (Associated(Stud%next)) &
                call Get_list_by_gender(Stud%next, List%next, Amount, Gender)
        else if (Associated(Stud%next)) then
            call Get_list_by_gender(Stud%next, List, Amount, Gender)
        end if
    end subroutine Get_list_by_gender

    pure recursive subroutine Get_list_by_Average(Stud, List, Max_average_mark)
        type(student), intent(in)        :: Stud
        type(student), pointer           :: List
        real(R_), intent(in)             :: Max_average_mark

        if (Real(Stud%Aver_Mark,R_) == Max_average_mark) then
            allocate (List, source=Stud)
            
            List%next => Null()
            
            if (Associated(Stud%next)) &
                call Get_list_by_Average(Stud%next, List%next, Max_average_mark)
        else if (Associated(Stud%next)) then
            call Get_list_by_Average(Stud%next, List, Max_average_mark)
        end if
    end subroutine Get_list_by_Average

    pure subroutine Sort_class_list(Group, Amount, Find_max_amount)
        type(student),pointer           :: Group
        integer(I_), intent(inout)      :: Amount

        real(R_) , intent(inout)        :: Find_max_amount
        Find_max_amount = 0

        call find_max(Group,Amount, 1, Find_max_amount)
    end subroutine Sort_class_list

    pure recursive subroutine Find_max(Current, N, j, Find_max_value)
        type(student), pointer    :: Current
        integer, intent(in)       :: N, j
        real(R_), intent(inout)   :: Find_max_value

        if (Find_max_value < Real(Current%Aver_Mark,R_)) then
            Find_max_value =  Real(Current%Aver_Mark,R_)
        end if
        if (j < N) &
            call find_max(Current%next, N, j+1, Find_max_value)
    end subroutine Find_max

    pure subroutine Sub_average_mark(Group, Amount, Average_Mark)
        type(student), pointer  :: Group
        integer(I_), intent(in) :: Amount
        real(R_), intent(inout) :: Average_Mark
        real(R_)                :: Sum_Amount
        integer                 :: i
        i = 1
        Sum_Amount = 0
        
        call find_sum(Group, Amount, i, Sum_Amount)

        Average_Mark = Sum_Amount/Amount
    end subroutine Sub_average_mark

    pure recursive subroutine Find_sum(Current, N, j, Find_max_value)
        type(student), pointer    :: Current
        integer, intent(in)       :: N, j
        real(R_), intent(inout)   :: Find_max_value

        Find_max_value =  Find_max_value + Real(Current%Aver_Mark,R_)
        
        if (j < N) &
            call find_sum(Current%next, N, j+1, Find_max_value)
    end subroutine Find_sum
end module Group_process
