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

with Ada.Numerics.Elementary_Functions;
with LSE.Utils.Coordinate_2D;
with LSE.Utils.Coordinate_2D_List;
with LSE.Utils.Coordinate_2D_Ptr;

package body LSE.Model.IO.Turtle.Fake is

   function Initialize
     return Instance
   is
      This : Instance;
   begin
      return This;
   end Initialize;

   procedure Configure (This : in out Instance)
   is
      use Ada.Strings;
      use LSE.Utils.Coordinate_2D_Ptr;
   begin
      This.Stack_Angle.Clear;
      This.Stack_Coordinate.Clear;

      This.Max_X := 0.0;
      This.Max_Y := 0.0;
      This.Min_X := 0.0;
      This.Min_Y := 0.0;

      This.Stack_Angle.Append (LSE.Utils.Angle.To_Angle (90.0));
      This.Stack_Coordinate.Append (
                                    LSE.Utils.Coordinate_2D_Ptr.To_Holder (
                                      LSE.Utils.Coordinate_2D.Initialize));
   end Configure;

   procedure Draw (This : in out Instance)
   is
   begin
      --  Nothing to do
      null;
   end Draw;

   procedure Forward (This : in out Instance; Trace : Boolean := False)
   is
      pragma Unreferenced (Trace);
      use Ada.Numerics.Elementary_Functions;
      use LSE.Utils.Coordinate_2D_Ptr;

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
         Copy : LSE.Utils.Coordinate_2D_Ptr.Holder := Item;

         X    : constant Float :=
           This.Line_Size * Cos (This.Stack_Angle.Last_Element, Degrees_Cycle);
         Y    : constant Float :=
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
         for H of reverse This.Stack_Coordinate loop
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
   begin
      This.Stack_Coordinate.Update_Element
        (Index   => This.Stack_Coordinate.Last_Index,
         Process => Update'Access);

      Copy.Reference.Set_X (This.Stack_Coordinate.Last_Element.Element.Get_X +
                              Copy.Reference.Get_X);
      Copy.Reference.Set_Y (This.Stack_Coordinate.Last_Element.Element.Get_Y +
                              Copy.Reference.Get_Y);

      This.Stack_Coordinate.Delete_Last;

      This.Stack_Coordinate.Append (Copy);

      Update_Corners (This);
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
   begin
      This.Stack_Angle.Delete_Last;
      This.Stack_Coordinate.Delete_Last;
   end Position_Restore;

end LSE.Model.IO.Turtle.Fake;
