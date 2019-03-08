-------------------------------------------------------------------------------
--  LSE -- L-System Editor
--  Author: Heziode
--
--  License:
--  MIT License
--
--  Copyright (c) 2018 Quentin Dauprat (Heziode) <Heziode@protonmail.com>
--
--  Permission is hereby granted, free of charge, to any person obtaining a
--  copy of this software and associated documentation files (the "Software"),
--  to deal in the Software without restriction, including without limitation
--  the rights to use, copy, modify, merge, publish, distribute, sublicense,
--  and/or sell copies of the Software, and to permit persons to whom the
--  Software is furnished to do so, subject to the following conditions:
--
--  The above copyright notice and this permission notice shall be included in
--  all copies or substantial portions of the Software.
--
--  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
--  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
--  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
--  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
--  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
--  FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
--  DEALINGS IN THE SOFTWARE.
-------------------------------------------------------------------------------

package body LSE.Utils.Angle is

   function Is_Angle (Value : String) return Boolean
   is
      Tmp : Angle;
      pragma Unreferenced (Tmp);
   begin
      Tmp := Angle'Value (Value);
      return True;
   exception
      when others =>
         return False;
   end Is_Angle;

   function To_Angle (Value : Float) return Angle
   is
      Tmp : Float := Value;
   begin
      while Tmp not in Angle'Range loop
         if Tmp < Angle'First then
            Tmp := Tmp + Degrees_Cycle;
         else
            Tmp := Tmp - Degrees_Cycle;
         end if;
      end loop;
      return Angle ((if Tmp = Degrees_Cycle then Angle'First else Tmp));
   end To_Angle;

end LSE.Utils.Angle;
