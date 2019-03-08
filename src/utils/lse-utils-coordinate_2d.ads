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
package LSE.Utils.Coordinate_2D is

   --  Type 2D coordinate
   type Coordinate is tagged private;

   --  Constructor
   function Initialize return Coordinate;

   --  Accessor of abscissa
   function Get_X (This : Coordinate) return Float;

   --  Mutator of abscissa
   procedure Set_X (This : out Coordinate; Value : Float);

   --  Accessor of ordinate
   function Get_Y (This : Coordinate) return Float;

   --  Mutator of ordinate
   procedure Set_Y (This : out Coordinate; Value : Float);

private

   type Coordinate is tagged record
      --  Abscissa
      X : Float := 0.0;
      --  Ordinate
      Y : Float := 0.0;
   end record;

end LSE.Utils.Coordinate_2D;
