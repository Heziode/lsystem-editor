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

with Ada.Float_Text_IO;
with Ada.Numerics.Elementary_Functions;
with Ada.Text_IO;
with LSE.Utils.Coordinate_2D;
with LSE.Utils.Coordinate_2D_Ptr;
with LSE.Utils.Utils;

package body LSE.Model.IO.Turtle is

   function Initialize return Instance
   is
      This : Instance;
   begin
      return This;
   end Initialize;

   procedure Set_Width (This : out Instance; Value : Positive)
   is
   begin
      This.Width := Value;
   end Set_Width;

   procedure Set_Height (This : out Instance; Value : Positive)
   is
   begin
      This.Height := Value;
   end Set_Height;

   procedure Set_Background_Color (This : out Instance;
                                   Value : String)
   is
   begin
      This.Background_Color := To_Unbounded_String (Value);
   end Set_Background_Color;

   procedure Set_Foreground_Color (This : out Instance; Value : String)
   is
   begin
      This.Foreground_Color := To_Unbounded_String (Value);
   end Set_Foreground_Color;

   procedure Set_Angle (This : out Instance;
                        Value : LSE.Utils.Angle.Angle)
   is
   begin
      This.Angle := Value;
   end Set_Angle;

   function Get_Width (This : Instance) return Positive
   is
   begin
      return This.Width;
   end Get_Width;

   function Get_Height (This : Instance) return Positive
        is
   begin
      return This.Height;
   end Get_Height;

   function Get_Background_Color (This : Instance) return String
   is
   begin
      return To_String (This.Background_Color);
   end Get_Background_Color;

   function Get_Foreground_Color (This : Instance) return String
   is
   begin
      return To_String (This.Foreground_Color);
   end Get_Foreground_Color;

   function Get_Offset_X (This : Instance) return Float
   is
   begin
      return This.Offset_X;
   end Get_Offset_X;

   function Get_Offset_Y (This : Instance) return Float
   is
   begin
      return This.Offset_Y;
   end Get_Offset_Y;

   function Get_Max_X (This : Instance) return Float
   is
   begin
      return This.Max_X;
   end Get_Max_X;

   function Get_Max_Y (This : Instance) return Float
   is
   begin
      return This.Max_Y;
   end Get_Max_Y;

   function Get_Min_X (This : Instance) return Float
   is
   begin
      return This.Min_X;
   end Get_Min_X;

   function Get_Min_Y (This : Instance) return Float
   is
   begin
      return This.Min_Y;
   end Get_Min_Y;

   procedure Set_Max_X (This : out Instance; Value : Float)
   is
   begin
      This.Max_X := Value;
   end Set_Max_X;

   procedure Set_Max_Y (This : out Instance; Value : Float)
   is
   begin
      This.Max_Y := Value;
   end Set_Max_Y;

   procedure Set_Min_X (This : out Instance; Value : Float)
   is
   begin
      This.Min_X := Value;
   end Set_Min_X;

   procedure Set_Min_Y (This : out Instance; Value : Float)
   is
   begin
      This.Min_Y := Value;
   end Set_Min_Y;

   function Get_Margin_Top (This : Instance) return Float
   is
   begin
      return This.Margin_Top;
   end Get_Margin_Top;

   function Get_Margin_Right (This : Instance) return Float
   is
   begin
      return This.Margin_Right;
   end Get_Margin_Right;

   function Get_Margin_Bottom (This : Instance) return Float
   is
   begin
      return This.Margin_Bottom;
   end Get_Margin_Bottom;

   function Get_Margin_Left (This : Instance) return Float
   is
   begin
      return This.Margin_Left;
   end Get_Margin_Left;

   function Get_Medium (This : Instance)
                      return LSE.Model.IO.Drawing_Area.Drawing_Area_Ptr.Holder
   is
   begin
      return This.Medium;
   end Get_Medium;

   procedure Set_Margin_Top (This : out Instance; Value : Natural)
   is
   begin
      This.Margin_Top := Float (Value);
   end Set_Margin_Top;

   procedure Set_Margin_Right (This : out Instance; Value : Natural)
   is
   begin
      This.Margin_Right := Float (Value);
   end Set_Margin_Right;

   procedure Set_Margin_Bottom (This : out Instance; Value : Natural)
   is
   begin
      This.Margin_Bottom := Float (Value);
   end Set_Margin_Bottom;

   procedure Set_Margin_Left (This : out Instance; Value : Natural)
   is
   begin
      This.Margin_Left := Float (Value);
   end Set_Margin_Left;

   procedure Set_Margin (This : out Instance; Value : Natural)
   is
   begin
      This.Margin_Top    := Float (Value);
      This.Margin_Right  := Float (Value);
      This.Margin_Bottom := Float (Value);
      This.Margin_Left   := Float (Value);
   end Set_Margin;

   procedure Set_Medium (This  : out Instance;
                    Value : LSE.Model.IO.Drawing_Area.Drawing_Area_Ptr.Holder)
   is
   begin
      This.Medium := Value;
   end Set_Medium;

   procedure Set_Dry_Run (This : out Instance; Value : Boolean)
   is
   begin
      This.Dry_Run := Value;
   end Set_Dry_Run;

   procedure Put (This : Instance)
   is
      use Ada.Text_IO;
      use Ada.Float_Text_IO;
   begin
      Put_Line ("Turtle:");
      Put_Line ("    Width            :" & Positive'Image (This.Width));
      Put_Line ("    Height           :" & Positive'Image (This.Height));
      Put_Line ("    Background_Color : " & To_String (This.Background_Color));
      Put_Line ("    Foreground_Color : " & To_String (This.Foreground_Color));
      Put ("    Line_Size        : ");
      Put (Item => This.Line_Size, Aft => 2, Exp => 0);
      New_Line;
      Put ("    Angle            :");
      Put (Item => This.Angle, Aft => 2, Exp => 0);
      New_Line;

      Put ("    Max_X            :");
      Put (Item => This.Max_X, Aft => 2, Exp => 0);
      New_Line;

      Put ("    Max_Y            :");
      Put (Item => This.Max_Y, Aft => 2, Exp => 0);
      New_Line;

      Put ("    Min_X            :");
      Put (Item => This.Min_X, Aft => 2, Exp => 0);
      New_Line;

      Put ("    Min_Y            :");
      Put (Item => This.Min_Y, Aft => 2, Exp => 0);
      New_Line;

      Put ("    Ratio            :");
      Put (Item => This.Ratio, Aft => 2, Exp => 0);
      New_Line;

      Put ("    Offset_X         :");
      Put (Item => This.Offset_X, Aft => 2, Exp => 0);
      New_Line;

      Put ("    Offset_Y         :");
      Put (Item => This.Offset_Y, Aft => 2, Exp => 0);
      New_Line;

      Put ("    Margin_Top       :");
      Put (Item => This.Margin_Top, Aft => 2, Exp => 0);
      New_Line;

      Put ("    Margin_Right     :");
      Put (Item => This.Margin_Right, Aft => 2, Exp => 0);
      New_Line;

      Put ("    Margin_Bottom    :");
      Put (Item => This.Margin_Bottom, Aft => 2, Exp => 0);
      New_Line;

      Put ("    Margin_Left      :");
      Put (Item => This.Margin_Left, Aft => 2, Exp => 0);
      New_Line;
   end Put;

   procedure Make_Offset (This : in out Instance)
   is
      Boxed_Width : constant Float :=
        Float (This.Width) - This.Margin_Right - This.Margin_Left;
      Boxed_Height : constant Float :=
        Float (This.Height) - This.Margin_Top - This.Margin_Bottom;
   begin
      if This.Max_X - This.Min_X = 0.0 or This.Max_Y - This.Min_Y = 0.0
      then
         raise Divide_By_Zero;
      end if;

      if Boxed_Width / (This.Max_X - This.Min_X) <=
        Boxed_Height / (This.Max_Y - This.Min_Y)
      then
         --  X has the smallest delta
         This.Ratio := Boxed_Width / (This.Max_X - This.Min_X);
      else
         --  Y has the smallest delta
         This.Ratio := Boxed_Height / (This.Max_Y - This.Min_Y);
      end if;

      This.Offset_X := Boxed_Width / 2.0 -
        ((This.Ratio * This.Max_X
         -  This.Ratio * This.Min_X) / 2.0
         + This.Ratio * This.Min_X);

      This.Offset_Y := Boxed_Height / 2.0 -
        ((This.Ratio * This.Max_Y
         - This.Ratio * This.Min_Y) / 2.0
         + This.Ratio * This.Min_Y);
   end Make_Offset;

   procedure Configure (This : in out Instance)
   is
   begin
      if not This.Dry_Run then
         This.Make_Offset;
      end if;
      This.Stack_Angle.Clear;
      This.Stack_Coordinate.Clear;

      if This.Dry_Run then
         This.Max_X := 0.0;
         This.Max_Y := 0.0;
         This.Min_X := 0.0;
         This.Min_Y := 0.0;
      else
         --  Configure the medium
         This.Medium.Reference.Configure (This);
      end if;

      This.Stack_Angle.Append (LSE.Utils.Angle.To_Angle (90.0));
      This.Stack_Coordinate.Append (
                                    LSE.Utils.Coordinate_2D_Ptr.To_Holder (
                                      LSE.Utils.Coordinate_2D.Initialize));
   end Configure;

   procedure Draw (This : in out Instance)
   is
   begin
      if not This.Dry_Run then
         This.Medium.Reference.Draw;
      end if;
   end Draw;

   procedure Forward (This : in out Instance; Trace : Boolean := False)
   is

      ------------------------
      --  Methods prototype --
      ------------------------

      --  Callback of Update_Element of Stack_Coordinate
      procedure Update (Item : in out LSE.Utils.Coordinate_2D_Ptr.Holder);

      --  Update all corners of the L-System edges
      procedure Update_Corners (This : in out Instance);

      -----------------------------
      --  Declaration of methods --
      -----------------------------

      procedure Update (Item : in out LSE.Utils.Coordinate_2D_Ptr.Holder)
      is
         use Ada.Numerics.Elementary_Functions;

         Copy : LSE.Utils.Coordinate_2D_Ptr.Holder := Item;

         X    : constant Float := This.Ratio *
           This.Line_Size * Cos (This.Stack_Angle.Last_Element, Degrees_Cycle);
         Y    : constant Float := This.Ratio *
           This.Line_Size * Sin (This.Stack_Angle.Last_Element, Degrees_Cycle);
      begin
         Copy.Reference.Set_X (X);
         Copy.Reference.Set_Y (Y);

         Item.Move (Copy);
      end Update;

      procedure Update_Corners (This : in out Instance)
      is
         X, Y : Float := 0.0;
      begin
         for H : LSE.Utils.Coordinate_2D_Ptr.Holder
           of reverse This.Stack_Coordinate
         loop
            X := X + H.Reference.Get_X;
            Y := Y + H.Reference.Get_Y;
         end loop;

         if X < This.Min_X then
            This.Min_X := X;
         elsif X > This.Max_X then
            This.Max_X := X;
         end if;

         if Y < This.Min_Y then
            This.Min_Y := Y;
         elsif Y > This.Max_Y then
            This.Max_Y := Y;
         end if;
      end Update_Corners;

      ---------------
      -- Variables --
      ---------------

      Copy   : LSE.Utils.Coordinate_2D_Ptr.Holder :=
        This.Stack_Coordinate.Last_Element.Copy;
   begin  -- Forward
      This.Stack_Coordinate.Update_Element
        (Index   => This.Stack_Coordinate.Last_Index,
         Process => Update'Access);

      if not This.Dry_Run then
         This.Medium.Reference.Forward
           (This.Stack_Coordinate.Last_Element.Element, Trace);
      end if;

      Copy.Reference.Set_X (This.Stack_Coordinate.Last_Element.Element.Get_X +
                              Copy.Reference.Get_X);
      Copy.Reference.Set_Y (This.Stack_Coordinate.Last_Element.Element.Get_Y +
                              Copy.Reference.Get_Y);

      This.Stack_Coordinate.Delete_Last;

      This.Stack_Coordinate.Append (Copy);

      if This.Dry_Run then
         Update_Corners (This);
      end if;
   end Forward;

   procedure Rotate_Clockwise (This  : in out Instance)
   is
   begin
      This.Stack_Angle.Replace_Element
        (This.Stack_Angle.Last,
         To_Angle (This.Stack_Angle.Last_Element - This.Angle));

      if not This.Dry_Run then
         This.Medium.Reference.Rotate_Clockwise;
      end if;
   end Rotate_Clockwise;

   procedure Rotate_Anticlockwise (This  : in out Instance)
   is
   begin
      This.Stack_Angle.Replace_Element
        (This.Stack_Angle.Last,
         To_Angle (This.Stack_Angle.Last_Element + This.Angle));

      if not This.Dry_Run then
         This.Medium.Reference.Rotate_Anticlockwise;
      end if;
   end Rotate_Anticlockwise;

   procedure UTurn (This : in out Instance)
   is
   begin
      This.Stack_Angle.Replace_Element
        (This.Stack_Angle.Last,
         To_Angle (This.Stack_Angle.Last_Element + 180.0));

      if not This.Dry_Run then
         This.Medium.Reference.UTurn;
      end if;
   end UTurn;

   procedure Position_Save (This : in out Instance)
   is
   begin
      This.Stack_Coordinate.Append (LSE.Utils.Coordinate_2D_Ptr.To_Holder (
                                    LSE.Utils.Coordinate_2D.Initialize));
      This.Stack_Angle.Append (LSE.Utils.Angle.To_Angle (
                               This.Stack_Angle.Last_Element));

      if not This.Dry_Run then
         This.Medium.Reference.Position_Save;
      end if;
   end Position_Save;

   procedure Position_Restore (This : in out Instance)
   is
      use LSE.Utils.Utils;

      Item : LSE.Utils.Coordinate_2D_Ptr.Holder;
      X  : Fixed_Point;
      Y  : Fixed_Point;
   begin
      Item := This.Stack_Coordinate.Last_Element;
      X := -Fixed_Point (Item.Element.Get_X);
      Y := -Fixed_Point (Item.Element.Get_Y);

      This.Stack_Angle.Delete_Last;
      This.Stack_Coordinate.Delete_Last;

      if not This.Dry_Run then
         This.Medium.Reference.Position_Restore (X, Y);
      end if;
   end Position_Restore;

end LSE.Model.IO.Turtle;
