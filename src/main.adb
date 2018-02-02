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

with Ada.Characters.Latin_1;
with Ada.Text_IO;
with LSE.Model.IO.Text_File;
with LSE.Model.IO.Turtle_Utils;
with LSE.Model.IO.Turtle;
with LSE.Model.IO.Turtle.PostScript;
with LSE.Model.L_System.L_System;
with LSE.Model.L_System.Concrete_Builder;

use Ada.Text_IO;
use LSE.Model.IO.Text_File;
use LSE.Model.IO.Turtle_Utils;
use LSE.Model.IO.Turtle;
use LSE.Model.IO.Turtle.PostScript;
use LSE.Model.L_System.L_System;
use LSE.Model.L_System.Concrete_Builder;

--  @description
--  Entry point of the app
--
procedure Main is

   package LASCII renames Ada.Characters.Latin_1;

   --   LS : constant String := "60.0 -F++F++F F F-F++F-F ";
--   LS : constant String := "22.5 F F FF+[+F-F-F]-[-F+F+F]";
--   LS : constant String := "30.0 F F FF+[+F[+F]-F+F]-[-F-[F]+F]";
   LS : constant String := "45 F[++FF]--F F F";
--   LS : constant String := "25 X X F-[X+X]+F[+FX]-X F FF";

   Source : constant String := "data/test.ls";
   Dest   : constant String := "data/test-save.ls";
   PS     : constant String := "data/test.ps";

   T : Holder;
   B : LSE.Model.L_System.Concrete_Builder.Instance;
   L : LSE.Model.L_System.L_System.Instance;
   F : File_Type;
begin
   T := To_Holder (Initialize (PS));


   T.Reference.Set_Background_Color ("#FF0000");
   T.Reference.Set_Forground_Color ("#0000FF");
   T.Element.Put;

   Put_Line (LASCII.LF & "##########" & LASCII.LF);

   Put_Line ("L-System (constant):");
   Put_Line (LS);

   Put_Line (LASCII.LF & "L-System (object):");

   Initialize (B);
   if B.Make (LS) then
      L := B.Get_Product;
      Put_Line (L.Get_LSystem);

      L.Set_State (3);
      L.Develop;
      L.Interpret (T);
   else
      Put_Line ("L-System creation error:");
      Put_Line (B.Get_Error);
   end if;

   Put_Line (LASCII.LF & "##########" & LASCII.LF);

   Open_File (F, In_File, Source, False);
   if Read_LSystem (F, B, L) then
      Put_Line ("L-System (from file):");
      Put_Line (L.Get_LSystem);

      for i in 0 .. 2 loop
         L.Set_State (i);
         L.Develop;

         Put_Line ("Develop" & Natural'Image (L.Get_State) & ": '"
                & L.Get_Value & "'");
      end loop;

   else
      Put_Line ("L-System creation error:");
      Put_Line (B.Get_Error);
   end if;
   Close_File (F);

   Open_File (F, Out_File, Dest);
   Write_LSystem (F, L);
   Close_File (F);
end Main;
