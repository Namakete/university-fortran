program exercise_1_3
   use Environment
   
   implicit none
   character(*), parameter    :: input_file = "../data/input.txt", output_file = "output.txt"
   character(:), allocatable  :: fmt
   integer                    :: In = 0, Out = 0, i = 0
   real(R_)                   :: x = 0, ln_x
   real(R_)                   :: Items(4) = 0

   open (file=input_file, newunit=In)
      read (In, *) x
   close (In)

   open (file=output_file, encoding=E_, newunit=Out)
      fmt = "(a, T7, '= ', f6.2)"
      write (Out, fmt) "x", x
   close (Out)

   Items(1) = x-1
   
   do i = 2, 4
      Items(i) = Items(i-1) * Items(1)
   end do
   
   Items = Items / [1, -2, 3, -4] ! ВЕКТОРИЗАЦИЯ. SSE, AVX
   !    Items(1:4) = Items(1:4) / [1, -2, 3, -4] 
   
   ! Используется этот код с 4-мя операциями,
   ! а не следующий, при котором векторизация невозможна:
   ! Items(2:4) = Items(2:4) / [-2, 3, -4]

   ln_x = Sum(Items)

   open (file=output_file, encoding=E_, newunit=Out, position='append')
      write (Out, fmt) "ln(x)", ln_x
      ! Проверка:
      write (Out, fmt) "error", log(x) - ln_x
   close (Out)
end program exercise_1_3
