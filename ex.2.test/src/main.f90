program exercise_2
   use Environment
   use IEEE_Arithmetic
   
   implicit none
   character(*), parameter :: input_file = "../data/input.txt", output_file = "output.txt"
   integer                 :: In = 0, Out = 0
   real(R_)                :: x = 0, y = 0, z = 0, w = 0, fval = 0

   open (file=input_file, newunit=In)
      read (In, *) x, y, z, w
   close (In)
   
   open (file=output_file, encoding=E_, newunit=Out)
      write (Out, "(4(a, f0.2/))") "x = ", x, "y = ", y, "z = ", z, "w = ", w
   close (Out)
   
   fval = F(x, y, z, w)

   open (file=output_file, encoding=E_, newunit=Out, position='append')
      write (Out, "('f = ', f0.2)") fval
   close (Out)

   !fval = FImpure(x, y, z, w)

contains
   ! Чистая функция.
   pure function F(x, y, z, w)
      real(R_) F, x, y, z, w
      intent(in)  x, y, z, w
 
      if (x>0 .and. z<5) then
         F = x*x + w
      else if (x>0 .and. z>=5) then
         F = cos(z) + x*y
      else if (x<0 .and. z>5) then
         F = w + cos(y)*x
      else
         ! Присвоение не числа -- NaN.
         F = IEEE_Value(x, IEEE_Quiet_NaN)
      end if
   end function F
   
   ! Нечистая функция с вычислениями и с вводом/выводом одновременно.
   ! Так реализовывать не нужно.
   ! Демонстирует применение блока БЕЗ использования меток.
   function FImpure(x, y, z, w) result(F)
      real(R_) F, x, y, z, w
      intent(in)  x, y, z, w
 
      open (file=output_file, encoding=E_, newunit=Out, position='append')
         eval: block
            if (x>0 .and. z<5) then
               F = x*x + w
            else if (x>0 .and. z>=5) then
               F = cos(z) + x*y
            else if (x<0 .and. z>5) then
               F = w + cos(y)*x
            else
               write (Out, "('f is indetermined')")
               exit eval
            end if
            write (Out, "('f = ', f0.2)") F
         end block eval
      close (Out)
   end function FImpure
end program exercise_2
