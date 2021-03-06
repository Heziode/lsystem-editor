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

with Ada.Strings;
with Ada.Strings.Unbounded;

package body LSE.Model.L_System.Error.Missing_Save is

   function Initialize (Line, Column : Positive) return Instance
   is
   begin
      return Instance '(Error_Type.Missing_Save, Line, Column);
   end Initialize;

   function Get_Error (This : Instance) return String
   is
      use Ada.Strings;
      use Ada.Strings.Unbounded;

      Str_Line   : constant Unbounded_String :=
        Trim (To_Unbounded_String (Positive'Image (This.Line)), Left);
      Str_Column : constant Unbounded_String :=
        Trim (To_Unbounded_String (Positive'Image (This.Column)), Left);
   begin
      return "Missing restore character for save character defined at line " &
        To_String (Str_Line) & " column " & To_String (Str_Column);
   end Get_Error;

end LSE.Model.L_System.Error.Missing_Save;
