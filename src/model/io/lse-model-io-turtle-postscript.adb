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
with Ada.Numerics.Elementary_Functions;
with Ada.Float_Text_IO;
with Ada.Strings;
with Ada.Strings.Fixed;
with LSE.Model.IO.Text_File;
with LSE.Utils.Colors;
with LSE.Utils.Coordinate_2D;
with LSE.Utils.Coordinate_2D_List;
with LSE.Utils.Coordinate_2D_Ptr;

package body LSE.Model.IO.Turtle.PostScript is

   package L renames Ada.Characters.Latin_1;

   function Initialize (File_Path : String)
                        return Instance
   is
      This : Instance;
   begin
      This.File_Path := To_Unbounded_String (File_Path);
      return This;
   end Initialize;

   procedure Configure (This : in out Instance)
   is
      use Ada.Strings;
      use Ada.Strings.Fixed;
      use LSE.Model.IO.Text_File;
      use LSE.Utils.Colors;

      R, G, B : Float := 0.0;
   begin
      This.Make_Offset;
      This.Stack_Angle.Clear;
      This.Stack_Coordinate.Clear;

      Open_File (This.File.all, Out_File, To_String (This.File_Path));

      Put_Line (This.File.all, "%%Creator: Lindenmayer system editor" & L.LF &
                  "%%BoundingBox: 0 0" & Positive'Image (This.Width) &
                  Positive'Image (This.Height) & L.LF &
                  "%%EndComments" & L.LF &
                  "%%Page: picture");

      if This.Background_Color /= "" then
         To_RGB (To_String (This.Background_Color), R, G, B);
         Put_Line (This.File.all, RGB_To_String (R, G, B) & L.Space &
                     "setrgbcolor" & L.LF &
                     "newpath" & L.LF &
                     "0 0 moveto" & L.LF &
                     Trim (Positive'Image (This.Width), Left) &
                     " 0 lineto " & L.LF &
                     Trim (Positive'Image (This.Width), Left) &
                     Positive'Image (This.Height) & " lineto" & L.LF & "0" &
                     Positive'Image (This.Height) & " lineto" & L.LF &
                     "closepath" & L.LF &
                     "fill");
      end if;

      To_RGB (To_String (This.Forground_Color), R, G, B);
      Put_Line (This.File.all, RGB_To_String (R, G, B) & L.Space &
                  "setrgbcolor" & L.LF &
                  "newpath" & L.LF &
                  Trim (Fixed_Point'Image (Fixed_Point (This.Offset_X +
                    This.Margin_Left)), Left)
                & L.Space &
                  Trim (Fixed_Point'Image (Fixed_Point (This.Offset_Y +
                    This.Margin_Bottom)), Left)
                & L.Space & "moveto");

      This.Stack_Angle.Append (LSE.Utils.Angle.To_Angle (90.0));
      This.Stack_Coordinate.Append (
                                    LSE.Utils.Coordinate_2D_Ptr.To_Holder (
                                      LSE.Utils.Coordinate_2D.Initialize));
   end Configure;

   procedure Draw (This : in out Instance)
   is
      use LSE.Model.IO.Text_File;
   begin
      Put_Line (This.File.all, "closepath");
      Put_Line (This.File.all, "stroke");
      Put_Line (This.File.all, "showpage");
      Close_File (This.File.all);
   end Draw;

   procedure Forward (This : in out Instance; Trace : Boolean := False)
   is
      use Ada.Float_Text_IO;
      use Ada.Numerics.Elementary_Functions;

      ------------------------
      --  Methods prototype --
      ------------------------

      --  Callback of Update_Element of Stack_Coordinate
      procedure Update (Item : in out LSE.Utils.Coordinate_2D_Ptr.Holder);

      -----------------------------
      --  Declaration of methods --
      -----------------------------

      procedure Update (Item : in out LSE.Utils.Coordinate_2D_Ptr.Holder)
      is
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

      ---------------
      -- Variables --
      ---------------

      Copy   : LSE.Utils.Coordinate_2D_Ptr.Holder :=
        This.Stack_Coordinate.Last_Element.Copy;
   begin
      This.Stack_Coordinate.Update_Element
        (Index   => This.Stack_Coordinate.Last_Index,
         Process => Update'Access);

      Put (File  => This.File.all,
           Item => This.Stack_Coordinate.Last_Element.Element.Get_X,
           Aft  => 2,
           Exp  => 0);

      Put (This.File.all, L.Space);

      Put (File => This.File.all,
           Item => This.Stack_Coordinate.Last_Element.Element.Get_Y,
           Aft  => 2,
           Exp  => 0);

      Copy.Reference.Set_X (This.Stack_Coordinate.Last_Element.Element.Get_X +
                              Copy.Reference.Get_X);
      Copy.Reference.Set_Y (This.Stack_Coordinate.Last_Element.Element.Get_Y +
                              Copy.Reference.Get_Y);

      This.Stack_Coordinate.Delete_Last;

      This.Stack_Coordinate.Append (Copy);

      if Trace then
         Put_Line (This.File.all, " rlineto");
      else
         Put_Line (This.File.all, " rmoveto");
      end if;
   end Forward;

   procedure Rotate_Clockwise (This  : in out Instance)
   is
   begin
      This.Stack_Angle.Replace_Element
        (This.Stack_Angle.Last,
         To_Angle (This.Stack_Angle.Last_Element - This.Angle));

   end Rotate_Clockwise;

   procedure Rotate_Anticlockwise (This  : in out Instance)
   is
   begin
      This.Stack_Angle.Replace_Element
        (This.Stack_Angle.Last,
         To_Angle (This.Stack_Angle.Last_Element + This.Angle));
   end Rotate_Anticlockwise;

   procedure UTurn (This : in out Instance)
   is
   begin
      This.Stack_Angle.Replace_Element
        (This.Stack_Angle.Last,
         To_Angle (This.Stack_Angle.Last_Element + 180.0));
   end UTurn;

   procedure Position_Save (This : in out Instance)
   is
   begin
      This.Stack_Coordinate.Append (LSE.Utils.Coordinate_2D_Ptr.To_Holder (
                                    LSE.Utils.Coordinate_2D.Initialize));
      This.Stack_Angle.Append (LSE.Utils.Angle.To_Angle (
                               This.Stack_Angle.Last_Element));
   end Position_Save;

   procedure Position_Restore (This : in out Instance)
   is
      use Ada.Strings;

      Item : LSE.Utils.Coordinate_2D_Ptr.Holder;
      F_X  : Fixed_Point;
      F_Y  : Fixed_Point;
      R    : Unbounded_String;
   begin

      Item := This.Stack_Coordinate.Last_Element;
      F_X := -Fixed_Point (Item.Element.Get_X);
      F_Y := -Fixed_Point (Item.Element.Get_Y);

      This.Stack_Angle.Delete_Last;
      This.Stack_Coordinate.Delete_Last;

      R := Trim (To_Unbounded_String (Fixed_Point'Image (F_X)), Both) &
        L.Space & Trim (To_Unbounded_String (Fixed_Point'Image (F_Y)), Both) &
        L.Space & "rmoveto";

      Put_Line (This.File.all, To_String (R));
   end Position_Restore;

   procedure Put (This : Instance'Class)
   is
   begin
      Turtle.Put (This);
      Put_Line ("    File path        : " & To_String (This.File_Path));
   end Put;
end LSE.Model.IO.Turtle.PostScript;
