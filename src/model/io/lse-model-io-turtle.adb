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
with Ada.Float_Text_IO;

package body LSE.Model.IO.Turtle is

   procedure Set_Width (This : out Instance'Class; Value : Positive)
   is
   begin
      This.Width := Value;
   end Set_Width;

   procedure Set_Height (This : out Instance'Class; Value : Positive)
   is
   begin
      This.Height := Value;
   end Set_Height;

   procedure Set_Background_Color (This : out Instance'Class;
                                   Value : String)
   is
   begin
      This.Background_Color := To_Unbounded_String (Value);
   end Set_Background_Color;

   procedure Set_Forground_Color (This : out Instance'Class; Value : String)
   is
   begin
      This.Forground_Color := To_Unbounded_String (Value);
   end Set_Forground_Color;

   procedure Set_Line_Size (This : out Instance'Class; Value : Float)
   is
   begin
      This.Line_Size := Value;
   end Set_Line_Size;

   procedure Set_Angle (This : out Instance'Class;
                        Value : LSE.Utils.Angle.Angle)
   is
   begin
      This.Angle := Value;
   end Set_Angle;

   procedure Put (This : Instance'Class)
   is
      use Ada.Text_IO;
      use Ada.Float_Text_IO;
   begin
      Put_Line ("Turtle:");
      Put_Line ("    Width            :" & Positive'Image (This.Width));
      Put_Line ("    Height           :" & Positive'Image (This.Height));
      Put_Line ("    Background_Color : " & To_String (This.Background_Color));
      Put_Line ("    Forground_Color  : " & To_String (This.Forground_Color));
      Put ("    Line_Size        :");
      Put (Item => This.Line_Size, Aft => 2, Exp => 0);
      New_Line;
      Put ("    Angle            :");
      Put (Item => Float (This.Angle), Aft => 2, Exp => 0);
      New_Line;
      Put ("    Coordinate       :");
      Put (Item => This.Current_Coordinate.Get_X, Aft => 2, Exp => 0);
      Put (",");
      Put (Item => This.Current_Coordinate.Get_Y, Aft => 2, Exp => 0);
      New_Line;
   end Put;

end LSE.Model.IO.Turtle;
