! Copyright (c) Namakete (Ilya Oberemok) <namakete.dev@gmail.com>.
! See the LICENCE file in the repository root for full licence text.
module Group_Process
    use Environment
    use Group_IO

    implicit none

contains
    pure recursive subroutine Max_and_sum (List, Boys_Max_Value, Girls_Max_Value, &
            Sum_Boys_Amount, Sum_Girls_Amount, Number_of_Boys, Number_of_Girls)
        type(student), pointer :: List
        real(R_), intent(inout) :: Boys_Max_Value, Girls_Max_Value, Sum_Boys_Amount, Sum_Girls_Amount
        integer, intent(inout) :: Number_of_Boys, Number_of_Girls

        if (Associated(List)) then
            ! Получаем средний былл кажого мальчика и девочки
            List%Aver_mark = Real(Sum(List%Marks), R_) / MARKS_AMOUNT
            !Получаем мальчиков
            if (List%Sex == MALE) then
                ! Получаем общюю сумму всех оценок мальчиков
                Sum_Boys_Amount = Sum_Boys_Amount + List%Aver_mark
                ! Получаем общее колличество всех оценок мальчиков
                Number_of_Boys = Number_of_Boys + 1
                ! Получаем максимальную успеваемость среди мальчиков
                if (List%Aver_mark > Boys_Max_Value) &
                    Boys_Max_Value = List%Aver_mark
            end if
            !Получаем девочек
            if (List%Sex == FEMALE) then
                ! Получаем общюю сумму всех оценок девочек
                Sum_Girls_Amount = Sum_Girls_Amount + List%Aver_mark
                ! Получаем общее колличество всех оценок девочек
                Number_of_Girls = Number_of_Girls + 1
                ! Получаем максимальную успеваемость среди девочек
                if (List%Aver_mark > Girls_Max_Value) &
                    Girls_Max_Value = List%Aver_mark
            end if
            call Max_and_sum (List%next, Boys_Max_Value, Girls_Max_Value, &
                Sum_Boys_Amount, Sum_Girls_Amount, Number_of_Boys, Number_of_Girls)
        end if
    end subroutine Max_and_sum
end module Group_process
