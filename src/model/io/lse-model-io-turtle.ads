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
with LSE.Angle;

use Ada.Strings.Unbounded;
use LSE.Angle;

--  @summary
--  Represent an abstract LOGO Turtle.
--
--  @description
--  This package represent an abstract LOGO Turtle. It encapsulates a set of
--  structures and methods which is designed to draw what is dictated to him
--  on a specific medium.
--
package LSE.Model.IO.Turtle is

   --  TODO: make abstract and add abstract method for rotate, draw, etc.
   --  Representing a LOGO Turtle
   type Instance is tagged private;

   --  Default width of the medium
   Default_Width            : constant Positive := 1;
   --  Default height of the medium
   Default_Height           : constant Positive := 1;
   --  Default background color
   Default_Background_Color : constant Unbounded_String :=
     To_Unbounded_String ("#FFFFFF");
   --  Default forground color
   Default_Forground_Color  : constant Unbounded_String :=
     To_Unbounded_String ("#000000");
   --  Default size of line that will be drawn
   Default_Line_Size        : constant Positive := 1;
   --  Default angle
   Default_Angle            : constant LSE.Angle.Angle := 0.0;

   --  Contsructor
   procedure Initialize (This : out Instance);

   --  Mutator of Width
   procedure Set_Width (This : out Instance; Value : Positive);

   --  Mutator of Height
   procedure Set_Height (This : out Instance; Value : Positive);

   --  Mutator of background color
   procedure Set_Background_Color (This : out Instance; Value : String);

   --  Mutator of forground color
   procedure Set_Forground_Color (This : out Instance; Value : String);

   --  Mutator of line size
   procedure Set_Line_Size (This : out Instance; Value : Positive);

   --  Mutator of angle
   procedure Set_Angle (This : out Instance; Value : LSE.Angle.Angle);

   --  Put this Turtle configuration in STDIO
   procedure Put (This : Instance);

private

   type Instance is tagged record
      --  Width of the medium
      Width            : Positive;
      --  Height of the medium
      Height           : Positive;
      --  Background color
      Background_Color : Unbounded_String;
      --  Forground color
      Forground_Color  : Unbounded_String;
      --  Size of line that will be drawn
      Line_Size        : Positive;
      --  Angle
      Angle            : LSE.Angle.Angle;
   end record;

end LSE.Model.IO.Turtle;
