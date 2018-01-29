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

with Ada.Text_IO;
with LSE.Model.IO.Turtle;
with LSE.Model.L_System.L_System;
with LSE.Model.L_System.Concrete_Builder;

use Ada.Text_IO;
use LSE.Model.IO.Turtle;
use LSE.Model.L_System.L_System;
use LSE.Model.L_System.Concrete_Builder;

--  @description
--  Entry point of the app
--
procedure Main is
   LS : constant String := "60.00 -F++F++F F   F-F++F-F";

   T : LSE.Model.IO.Turtle.Instance;
   B : LSE.Model.L_System.Concrete_Builder.Instance;
   L : LSE.Model.L_System.L_System.Instance;
begin
   Initialize (T);

   T.Set_Background_Color ("#FF0000");
   T.Set_Forground_Color ("#0000FF");
   Put (T);

   Put_Line (ASCII.LF & "##########" & ASCII.LF);

   Put_Line ("L-System (constant):");
   Put_Line (LS);

   Put_Line (ASCII.LF & "L-System (object):");

   Initialize (B);
   if B.Make (LS) then
      L := B.Get_Product;
      Put_Line (L.Get_LSystem);
   else
      Put_Line ("L-System creation error:");
      Put_Line (B.Get_Error);
   end if;
end Main;