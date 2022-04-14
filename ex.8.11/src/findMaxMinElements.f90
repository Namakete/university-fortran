! Copyright (c) Namakete (Ilya Oberemok) <namakete.dev@gmail.com>.
! See the LICENCE file in the repository root for full licence text.
module FindMaxMinElements
    use Environment
    implicit none

contains
    pure subroutine fina_max_min_elements(Array, Max, Min)
        integer, intent(in)             :: Array(:)
        integer, intent(inout)          :: Max, Min
        integer :: i

        Max = -(huge(Array)-1)
        Min = huge(Array)

        do i = 1, Size(Array, dim = 1)
            if (Array(i) > Max) Max = Array(i) 
        end do
        
        do i = 1, Size(Array, dim = 1)
            if (Array(i) < Min) Min = Array(i)
        end do

    end subroutine fina_max_min_elements 

end module FindMaxMinElements
