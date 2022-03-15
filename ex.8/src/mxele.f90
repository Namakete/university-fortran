module MaxMinEleFind
    use Environment
    implicit none

contains
    pure subroutine max_ele_find(B, Max)
        integer, intent(in)  :: B(:)
        integer, intent(inout) :: Max
        
        Max = MaxVal(B, dim = 1)
    end subroutine max_ele_find

    pure subroutine min_ele_find(B, Min)
        integer, intent(in)  :: B(:)
        integer, intent(inout) :: Min
        
        Min = MinVal(B, dim = 1)
    end subroutine min_ele_find
end module MaxMinEleFind
