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

with Glib;

package body LSE.IO.Drawing_Area is

   function Initialize (Cr : Cairo.Cairo_Context)
                        return Instance
   is
      This : Instance;
   begin
      This.Cr := Cr;
      return This;
   end Initialize;

   procedure Configure (This   : in out Instance;
                        Turtle : LSE.Model.IO.Turtle.Instance)
   is
      use Glib;
   begin
      Move_To (This.Cr,
               Gdouble (Turtle.Get_Offset_X + Turtle.Get_Margin_Right),
               Gdouble (Turtle.Get_Offset_Y + Turtle.Get_Margin_Bottom));
   end Configure;

   procedure Draw (This : in out Instance)
   is
   begin
      Stroke (This.Cr);
      Show_Page (This.Cr);
   end Draw;

   procedure Forward (This       : in out Instance;
                      Coordinate : LSE.Utils.Coordinate_2D.Coordinate;
                      Trace      : Boolean := False)
   is
      use Glib;
   begin
      if Trace then
         Rel_Line_To (This.Cr,
                  Gdouble (Coordinate.Get_X),
                  Gdouble (Coordinate.Get_Y));
      else
         Rel_Move_To (This.Cr,
                  Gdouble (Coordinate.Get_X),
                  Gdouble (Coordinate.Get_Y));
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
      use Glib;
   begin
      Rel_Move_To (This.Cr, Gdouble (X), Gdouble (Y));
   end Position_Restore;

end LSE.IO.Drawing_Area;
