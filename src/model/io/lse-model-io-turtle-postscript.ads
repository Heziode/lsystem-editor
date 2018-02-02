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

with Ada.Text_IO;
with LSE.Model.IO.Turtle;
with LSE.Model.IO.Turtle_Utils;

use Ada.Text_IO;
use LSE.Model.IO.Turtle;
use LSE.Model.IO.Turtle_Utils;

--  @description
--  Represent a LOGO Turtle on PostScript medium
--
package LSE.Model.IO.Turtle.PostScript is

   --  Representing a LOGO Turtle for PostScript medium
   type Instance is new Turtle.Instance with private;

   --  Constructor
   --  @File_Path Location where save the representation
   function Initialize (File_Path : String)
                        return Instance;

   --  Put this Turtle configuration in STDIO
   procedure Put (This : Instance'Class);

   overriding
   procedure Configure (This : in out Instance);

   overriding
   procedure Draw (This : in out Instance);

   overriding
   procedure Forward (This : in out Instance);

   overriding
   procedure Forward_Trace (This : in out Instance);

   overriding
   procedure Rotate_Positive (This  : in out Instance);

   overriding
   procedure Rotate_Negative (This  : in out Instance);

   overriding
   procedure UTurn (This : in out Instance);

   overriding
   procedure Position_Save (This : in out Instance);

   overriding
   procedure Position_Restore (This : in out Instance);

private

   procedure Go_Forward (This : in out Instance);

   type Instance is new Turtle.Instance with record
      --  Location to save the representation
      File_Path : Unbounded_String;
      File      : access File_Type := new File_Type;
      Min_X,
      Min_Y     : Float := Float'Last;
      Max_X,
      Max_Y     : Float := Float'First;
   end record;

end LSE.Model.IO.Turtle.PostScript;
