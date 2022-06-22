! Copyright (c) Namakete (Ilya Oberemok) <namakete.dev@gmail.com>.
! See the LICENCE file in the repository root for full licence text.
module List_Process
    use Environment
    use List_IO

    implicit none

contains
    
    pure recursive subroutine Selection_Sort(list)
        type(node),allocatable, intent(inout) ::list
        
        if(ALLOCATED(list)) then
            call Choose_And_Paste(list, list, list%next)
            call Selection_Sort(list%next)
        end if
    end subroutine Selection_Sort

    pure recursive subroutine Choose_And_Paste(list, minimum, current)
        type(node), allocatable, intent(inout) :: list
        type(node), allocatable, intent(inout) :: minimum, current
        type(node),  allocatable:: tmp, tmpNext

        if(ALLOCATED(current)) then
            if(current%value < minimum%value) then
                call Choose_And_Paste(list, current, current%next)
            else
                call Choose_And_Paste(list, minimum, current%next)
            end if
        else if (list%Value /= minimum%Value) then
            call move_alloc(minimum, tmp)
            call move_alloc(tmp%Next, tmpNext)

            call move_alloc(list%Next, tmp%Next)
            call move_alloc(tmpNext, list%Next)

            call move_alloc(list, minimum)
            call move_alloc(tmp, list)
        end if
    end subroutine Choose_And_Paste

end module List_process
