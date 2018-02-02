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
with LSE.Utils.Angle;
with LSE.Utils.Coordinate_2D;
with LSE.Utils.Coordinate_2D_List;

use Ada.Strings.Unbounded;
use LSE.Utils.Angle;
use LSE.Utils.Coordinate_2D;
use LSE.Utils.Coordinate_2D_List;

--  @summary
--  Represent an abstract LOGO Turtle.
--
--  @description
--  This package represent an abstract LOGO Turtle. It encapsulates a set of
--  structures and methods which is designed to draw what is dictated to him
--  on a specific medium.
--
package LSE.Model.IO.Turtle is

   --  Representing a LOGO Turtle
   type Instance is abstract tagged private;

   --  Default width of the medium
   Default_Width            : constant Positive := 600;
   --  Default height of the medium
   Default_Height           : constant Positive := 600;
   --  Default background color
   Default_Background_Color : constant Unbounded_String :=
     To_Unbounded_String ("#FFFFFF");
   --  Default forground color
   Default_Forground_Color  : constant Unbounded_String :=
     To_Unbounded_String ("#000000");
   --  Default size of line that will be drawn
   Default_Line_Size        : constant Float := 100.0;
   --  Default angle
   Default_Angle            : constant LSE.Utils.Angle.Angle := 0.0;

   --  Mutator of Width
   procedure Set_Width (This : out Instance'Class; Value : Positive);

   --  Mutator of Height
   procedure Set_Height (This : out Instance'Class; Value : Positive);

   --  Mutator of background color
   procedure Set_Background_Color (This : out Instance'Class;
                                   Value : String);

   --  Mutator of forground color
   procedure Set_Forground_Color (This : out Instance'Class; Value : String);

   --  Mutator of line size
   procedure Set_Line_Size (This : out Instance'Class; Value : Float);

   --  Mutator of angle
   procedure Set_Angle (This : out Instance'Class;
                        Value : LSE.Utils.Angle.Angle);

   --  Put this Turtle configuration in STDIO
   procedure Put (This : Instance'Class);

   --  Special configuration depending to medium
   procedure Configure (This : in out Instance) is abstract;

   --  Draw the final representation (save file, display in screen, etc.)
   procedure Draw (This : in out Instance) is abstract;

   --  Go forward
   procedure Forward (This : in out Instance) is abstract;

   --  Go forward and stroke
   procedure Forward_Trace (This : in out Instance) is abstract;

   --  Positive rotation by angle
   procedure Rotate_Positive (This  : in out Instance) is abstract;

   --  Negative rotation by angle
   procedure Rotate_Negative (This  : in out Instance) is abstract;

   --  Go backward
   procedure UTurn (This : in out Instance) is abstract;

   --  Save the current position in medium
   procedure Position_Save (This : in out Instance) is abstract;

   --  Restore the previous saved location in medium
   procedure Position_Restore (This : in out Instance) is abstract;

private

   type Instance is abstract tagged record
      --  Width of the medium
      Width              : Positive := Default_Width;
      --  Height of the medium
      Height             : Positive := Default_Height;
      --  Background color
      Background_Color   : Unbounded_String := Default_Background_Color;
      --  Forground color
      Forground_Color    : Unbounded_String := Default_Forground_Color;
      --  Size of line that will be drawn
      Line_Size          : Float := Default_Line_Size;
      --  Angle step
      Angle              : LSE.Utils.Angle.Angle := Default_Angle;
      --  Current angle use in medium
      Stack_Angle        : List.Vector;
      --  Current coordinate
      Current_Coordinate : Coordinate;
      --  "Stack_Coordinate" to save / restore coordinate
      Stack_Coordinate   : LSE.Utils.Coordinate_2D_List.Vector;
   end record;

end LSE.Model.IO.Turtle;
