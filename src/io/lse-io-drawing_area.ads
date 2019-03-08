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

with Ada.Strings.Unbounded;
with Cairo;
with LSE.Model.IO.Drawing_Area;
with LSE.Model.IO.Turtle;
with LSE.Utils.Coordinate_2D;
with LSE.Utils.Utils;

use Ada.Strings.Unbounded;
use Cairo;
use LSE.Model.IO.Drawing_Area;
use LSE.Model.IO.Turtle;
use LSE.Utils.Utils;

--  @description
--  Represent a LOGO Turtle on Gtk_Drawing_Area medium
--
package LSE.IO.Drawing_Area is

   --  Representing a LOGO Turtle for PostScript medium
   type Instance is new Services with private;

   --  Constructor
   --  @File_Path Location where save the representation
   function Initialize (Cr : Cairo.Cairo_Context)
                        return Instance;

   overriding
   procedure Configure (This   : in out Instance;
                        Turtle : LSE.Model.IO.Turtle.Instance);

   overriding
   procedure Draw (This : in out Instance);

   overriding
   procedure Forward (This       : in out Instance;
                      Coordinate : LSE.Utils.Coordinate_2D.Coordinate;
                      Trace      : Boolean := False);

   overriding
   procedure Rotate_Clockwise (This  : in out Instance);

   overriding
   procedure Rotate_Anticlockwise (This  : in out Instance);

   overriding
   procedure UTurn (This : in out Instance);

   overriding
   procedure Position_Save (This : in out Instance);

   overriding
   procedure Position_Restore (This : in out Instance;
                               X, Y : Fixed_Point);

private

   type Instance is new Services with record
      --  Cairo context where draw the L-System
      Cr : Cairo.Cairo_Context;
   end record;


end LSE.IO.Drawing_Area;
