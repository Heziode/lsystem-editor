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

--  @description
--  This package provides an 2D coordinate type
--
package body LSE.Utils.Coordinate_2D is

   function Initialize return Coordinate
   is
   begin
      return Coordinate '(0.0, 0.0);
   end Initialize;

   function Get_X (This : Coordinate) return Float
   is
   begin
      return This.X;
   end Get_X;

   procedure Set_X (This : out Coordinate; Value : Float)
   is
   begin
      This.X := Value;
   end Set_X;

   function Get_Y (This : Coordinate) return Float
   is
   begin
      return This.Y;
   end Get_Y;

   procedure Set_Y (This : out Coordinate; Value : Float)
   is
   begin
      This.Y := Value;
   end Set_Y;

end LSE.Utils.Coordinate_2D;
