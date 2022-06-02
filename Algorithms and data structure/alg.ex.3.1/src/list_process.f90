! Copyright (c) Namakete (Ilya Oberemok) <namakete.dev@gmail.com>.
! See the LICENCE file in the repository root for full licence text.  
module List_Process
    use Environment
    use List_IO

    implicit none

contains
    recursive subroutine Selection_Sort(list)
        type(node), pointer ::list
        if(Associated(list)) then
            call Choose_And_Paste(list, list, list%next)
            call Selection_Sort(list%next)           
        end if
    end subroutine Selection_Sort

    recursive subroutine Choose_And_Paste(list, maximum, current)
        type(node), pointer :: list
        type(node), pointer :: maximum
        type(node), pointer :: current
        type(node), pointer :: tmp

        if(Associated(current))then
            if(current%value < maximum%value) then
                call Choose_And_Paste(list, current, current%next)
            else
                call Choose_And_Paste(list, maximum, current%next)
            end if
        else
            if(.not. associated(list, maximum)) then
                tmp         => maximum
                maximum     => current
                tmp%next    => list
                list        => tmp
            end if
        end if
    end subroutine Choose_And_Paste

    pure recursive subroutine Delete(current)
        type(node), pointer, intent(inout) :: current

        if (Associated(current)) then
            deallocate(current)
            call Delete(current)
        end if
    end subroutine Delete
end module List_process
