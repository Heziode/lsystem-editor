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

   procedure Set_Angle (This : out Instance'Class;
                        Value : LSE.Utils.Angle.Angle)
   is
   begin
      This.Angle := Value;
   end Set_Angle;

   function Get_Max_X (This : Instance'Class) return Float
   is
   begin
      return This.Max_X;
   end Get_Max_X;

   function Get_Max_Y (This : Instance'Class) return Float
   is
   begin
      return This.Max_Y;
   end Get_Max_Y;

   function Get_Min_X (This : Instance'Class) return Float
   is
   begin
      return This.Min_X;
   end Get_Min_X;

   function Get_Min_Y (This : Instance'Class) return Float
   is
   begin
      return This.Min_Y;
   end Get_Min_Y;

   procedure Set_Max_X (This : out Instance'Class; Value : Float)
   is
   begin
      This.Max_X := Value;
   end Set_Max_X;

   procedure Set_Max_Y (This : out Instance'Class; Value : Float)
   is
   begin
      This.Max_Y := Value;
   end Set_Max_Y;

   procedure Set_Min_X (This : out Instance'Class; Value : Float)
   is
   begin
      This.Min_X := Value;
   end Set_Min_X;

   procedure Set_Min_Y (This : out Instance'Class; Value : Float)
   is
   begin
      This.Min_Y := Value;
   end Set_Min_Y;

   function Get_Margin_Top (This : Instance'Class) return Float
   is
   begin
      return This.Margin_Top;
   end Get_Margin_Top;

   function Get_Margin_Right (This : Instance'Class) return Float
   is
   begin
      return This.Margin_Right;
   end Get_Margin_Right;

   function Get_Margin_Bottom (This : Instance'Class) return Float
   is
   begin
      return This.Margin_Bottom;
   end Get_Margin_Bottom;

   function Get_Margin_Left (This : Instance'Class) return Float
   is
   begin
      return This.Margin_Left;
   end Get_Margin_Left;

   procedure Set_Margin_Top (This : out Instance'Class; Value : Float)
   is
   begin
      This.Margin_Top := Value;
   end Set_Margin_Top;

   procedure Set_Margin_Right (This : out Instance'Class; Value : Float)
   is
   begin
      This.Margin_Right := Value;
   end Set_Margin_Right;

   procedure Set_Margin_Bottom (This : out Instance'Class; Value : Float)
   is
   begin
      This.Margin_Bottom := Value;
   end Set_Margin_Bottom;

   procedure Set_Margin_Left (This : out Instance'Class; Value : Float)
   is
   begin
      This.Margin_Left := Value;
   end Set_Margin_Left;

   procedure Set_Margin (This : out Instance'Class; Value : Float)
   is
   begin
      This.Set_Margin_Top (Value);
      This.Set_Margin_Right (Value);
      This.Set_Margin_Bottom (Value);
      This.Set_Margin_Left (Value);
   end Set_Margin;

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
      Put ("    Line_Size        : ");
      Put (Item => This.Line_Size, Aft => 2, Exp => 0);
      New_Line;
      Put ("    Angle            :");
      Put (Item => Float (This.Angle), Aft => 2, Exp => 0);
      New_Line;
   end Put;

   procedure Make_Offset (This : in out Instance'Class)
   is
      Boxed_Width : constant Float :=
        Float (This.Width) - This.Margin_Right - This.Margin_Left;
      Boxed_Height : constant Float :=
        Float (This.Height) - This.Margin_Top - This.Margin_Bottom;
   begin
      if Boxed_Width / (This.Max_X - This.Min_X) <=
        Boxed_Height / (This.Max_Y - This.Min_Y)
      then
         --  X has the smallest delta
         This.Ratio := Boxed_Width / (This.Max_X - This.Min_X);
      else
         --  Y has the smallest delta
         This.Ratio := Boxed_Height / (This.Max_Y - This.Min_Y);
      end if;

      This.Offset_X := (Boxed_Width / 2.0) -
        (((This.Ratio * This.Max_X
         -  This.Ratio * This.Min_X) / 2.0)
         + This.Ratio * This.Min_X);

      This.Offset_Y := (Boxed_Height / 2.0) -
        (((This.Ratio * This.Max_Y
         - This.Ratio * This.Min_Y) / 2.0)
         + This.Ratio * This.Min_Y);
   end Make_Offset;

end LSE.Model.IO.Turtle;
