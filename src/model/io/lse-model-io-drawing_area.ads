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

limited with LSE.Model.IO.Turtle;
with LSE.Utils.Coordinate_2D;
with LSE.Utils.Utils;

use LSE.Utils.Utils;

--  @description
--  Service of drawing area. It encapsulates a set of
--  structures and methods which is designed to draw what is dictated to him
--  on a specific medium.
--
package LSE.Model.IO.Drawing_Area is

   --  Service of drawing area
   type Services is interface;

   --  Special configuration depending to medium
   --  @param Turtle Reference to the turtle
   procedure Configure (This   : in out Services;
                        Turtle : LSE.Model.IO.Turtle.Instance) is abstract;

   --  Draw the final representation (save file, display in screen, etc.)
   procedure Draw (This : in out Services) is abstract;

   --  Go forward
   --  @param Coordinate Destination where to go
   --  @param Trace True for stroke, False otherwise
   procedure Forward (This       : in out Services;
                      Coordinate : LSE.Utils.Coordinate_2D.Coordinate;
                      Trace      : Boolean := False)
   is abstract;

   --  Positive rotation by angle
   procedure Rotate_Clockwise (This  : in out Services) is abstract;

   --  Negative rotation by angle
   procedure Rotate_Anticlockwise (This  : in out Services) is abstract;

   --  Go backward
   procedure UTurn (This : in out Services) is abstract;

   --  Save the current position in medium
   procedure Position_Save (This : in out Services) is abstract;

   --  Restore the previous saved location in medium
   --  @param X Position on the abscissa
   --  @param Y Position on the ordinate
   procedure Position_Restore (This : in out Services;
                               X, Y : Fixed_Point) is abstract;

end LSE.Model.IO.Drawing_Area;
