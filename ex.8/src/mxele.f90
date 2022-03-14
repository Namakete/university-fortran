module MaxEleFind
    use Environment
    implicit none

contains
    pure subroutine max_ele_find(B, Res)
        integer, intent(in)  :: B(:)
        integer, intent(inout) :: Res
        integer  :: i
        
        do i = 1, Size(B, dim = 1)
            if (B(i) > Res) Res = B(i)
        end do
    end subroutine max_ele_find
end module MaxEleFind