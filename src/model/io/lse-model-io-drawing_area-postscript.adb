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
with Ada.Float_Text_IO;
with Ada.Strings;
with Ada.Strings.Fixed;
with LSE.Model.IO.Text_File;
with LSE.Utils.Colors;

package body LSE.Model.IO.Drawing_Area.PostScript is

   package L renames Ada.Characters.Latin_1;

   function Initialize (File_Path : String)
                        return Instance
   is
      This : Instance;
   begin
      This.File_Path := To_Unbounded_String (File_Path);
      return This;
   end Initialize;

   procedure Configure (This   : in out Instance;
                        Turtle : LSE.Model.IO.Turtle.Instance)
   is
      use Ada.Strings;
      use Ada.Strings.Fixed;
      use LSE.Model.IO.Text_File;
      use LSE.Model.IO.Turtle;
      use LSE.Utils.Colors;

      R, G, B : Float := 0.0;
   begin
      Open_File (This.File.all, Out_File, To_String (This.File_Path));

      Put_Line (This.File.all, "%%Creator: Lindenmayer system editor" & L.LF &
                  "%%BoundingBox: 0 0" & Positive'Image (Turtle.Get_Width) &
                  Positive'Image (Turtle.Get_Height) & L.LF &
                  "%%EndComments" & L.LF &
                  "%%Page: picture");

      if Turtle.Get_Background_Color /= "" then
         To_RGB (Turtle.Get_Background_Color, R, G, B);
         Put_Line (This.File.all, RGB_To_String (R, G, B) & L.Space &
                     "setrgbcolor" & L.LF &
                     "newpath" & L.LF &
                     "0 0 moveto" & L.LF &
                     Trim (Positive'Image (Turtle.Get_Width), Left) &
                     " 0 lineto " & L.LF &
                     Trim (Positive'Image (Turtle.Get_Width), Left) &
                     Positive'Image (Turtle.Get_Height) & " lineto" & L.LF &
                     "0" & Positive'Image (Turtle.Get_Height) & " lineto" &
                     L.LF & "closepath" & L.LF &
                     "fill");
      end if;

      To_RGB (Turtle.Get_Foreground_Color, R, G, B);
      Put_Line (This.File.all, RGB_To_String (R, G, B) & L.Space &
                  "setrgbcolor" & L.LF &
                  "newpath" & L.LF &
                  Trim (Fixed_Point'Image (Fixed_Point (Turtle.Get_Offset_X +
                    Turtle.Get_Margin_Left)), Left)
                & L.Space &
                  Trim (Fixed_Point'Image (Fixed_Point (Turtle.Get_Offset_Y +
                    Turtle.Get_Margin_Bottom)), Left)
                & L.Space & "moveto");
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

   procedure Forward (This       : in out Instance;
                      Coordinate : LSE.Utils.Coordinate_2D.Coordinate;
                      Trace      : Boolean := False)
   is
      use Ada.Float_Text_IO;
   begin
      Put (File  => This.File.all,
           Item => Coordinate.Get_X,
           Aft  => 2,
           Exp  => 0);

      Put (This.File.all, L.Space);

      Put (File => This.File.all,
           Item => Coordinate.Get_Y,
           Aft  => 2,
           Exp  => 0);

      if Trace then
         Put_Line (This.File.all, " rlineto");
      else
         Put_Line (This.File.all, " rmoveto");
      end if;
   end Forward;

   procedure Rotate_Clockwise (This  : in out Instance)
   is
   begin
      null;
   end Rotate_Clockwise;

   procedure Rotate_Anticlockwise (This  : in out Instance)
   is
   begin
      null;
   end Rotate_Anticlockwise;

   procedure UTurn (This : in out Instance)
   is
   begin
      null;
   end UTurn;

   procedure Position_Save (This : in out Instance)
   is
   begin
      null;
   end Position_Save;

   procedure Position_Restore (This : in out Instance;
                               X, Y : Fixed_Point)
   is
      use Ada.Strings;

      R    : Unbounded_String;
   begin

      R := Trim (To_Unbounded_String (Fixed_Point'Image (X)), Both) &
        L.Space & Trim (To_Unbounded_String (Fixed_Point'Image (Y)), Both) &
        L.Space & "rmoveto";

      Put_Line (This.File.all, To_String (R));
   end Position_Restore;

end LSE.Model.IO.Drawing_Area.PostScript;
