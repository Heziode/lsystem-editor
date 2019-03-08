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

with Ada.Containers.Indefinite_Vectors;

--  @description
--  This package provides an Angle type
--
package LSE.Utils.Angle is

   --  Define max range of cycle in degree
   Degrees_Cycle : constant Float := 360.00;

   --  Type angle in range [0.00;359.99]
   subtype Angle is Float range 0.00 .. 359.99;

   package List is new Ada.Containers.Indefinite_Vectors (Natural, Angle);

   --  Check if value passing in parameter is an angle
   --
   --  @param Value The value to check
   --  @return Return True if the value is an angle, False otherwise
   function Is_Angle (Value : String) return Boolean;

   --  Convert a Float value in valid Angle
   --
   --  @param Value The to convert
   --  @return Return the value in Angle
   function To_Angle (Value : Float) return Angle;

end LSE.Utils.Angle;
