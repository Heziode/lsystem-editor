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
private with LSE.Utils.Coordinate_2D_List;
with LSE.Model.IO.Drawing_Area.Drawing_Area_Ptr;

use Ada.Strings.Unbounded;
use LSE.Utils.Angle;
use LSE.Model.IO.Drawing_Area.Drawing_Area_Ptr;

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
   type Instance is tagged private;

   --  Raise when Max_X - Min_X or Max_Y - Min_Y give 0
   Divide_By_Zero : exception;

   --  Default width of the medium
   Default_Width            : constant Positive := 600;
   --  Default height of the medium
   Default_Height           : constant Positive := 600;
   --  Default background color
   Default_Background_Color : constant Unbounded_String :=
     To_Unbounded_String ("");
   --  Default foreground color
   Default_Foreground_Color  : constant Unbounded_String :=
     To_Unbounded_String ("#000000");
   --  Default size of line that will be drawn
   Default_Line_Size        : constant Float := 100.0;
   --  Default angle
   Default_Angle            : constant LSE.Utils.Angle.Angle := 0.0;

   Default_Margin_Top,
   Default_Margin_Right,
   Default_Margin_Bottom,
   Default_Margin_Left      : constant Float := 0.0;

   --  Constructor
   function Initialize return Instance;

   --  Mutator of Width
   procedure Set_Width (This : out Instance; Value : Positive);

   --  Mutator of Height
   procedure Set_Height (This : out Instance; Value : Positive);

   --  Mutator of background color
   procedure Set_Background_Color (This : out Instance;
                                   Value : String);

   --  Mutator of foreground color
   procedure Set_Foreground_Color (This : out Instance; Value : String);

   --  Mutator of angle
   procedure Set_Angle (This : out Instance;
                        Value : LSE.Utils.Angle.Angle);

   --  Accessor of width
   function Get_Width (This : Instance) return Positive;

   --  Accessor of height
   function Get_Height (This : Instance) return Positive;

   --  Accessor of background color
   function Get_Background_Color (This : Instance) return String;

   --  Accessor of foreground color
   function Get_Foreground_Color (This : Instance) return String;

   --  Accessor of offset x
   function Get_Offset_X (This : Instance) return Float;

   --  Accessor of offset y
   function Get_Offset_Y (This : Instance) return Float;

   --  Accessor of max X
   function Get_Max_X (This : Instance) return Float;

   --  Accessor of max Y
   function Get_Max_Y (This : Instance) return Float;

   --  Accessor of min X
   function Get_Min_X (This : Instance) return Float;

   --  Accessor of min Y
   function Get_Min_Y (This : Instance) return Float;

   --  Mutator of max X
   procedure Set_Max_X (This : out Instance; Value : Float);

   --  Mutator of max Y
   procedure Set_Max_Y (This : out Instance; Value : Float);

   --  Mutator of min X
   procedure Set_Min_X (This : out Instance; Value : Float);

   --  Mutator of min Y
   procedure Set_Min_Y (This : out Instance; Value : Float);

   --  Accessor of margin top
   function Get_Margin_Top (This : Instance) return Float;

   --  Accessor of margin right
   function Get_Margin_Right (This : Instance) return Float;

   --  Accessor of margin Bottom
   function Get_Margin_Bottom (This : Instance) return Float;

   --  Accessor of margin left
   function Get_Margin_Left (This : Instance) return Float;

   --  Accessor of medium
   function Get_Medium (This : Instance)
                      return LSE.Model.IO.Drawing_Area.Drawing_Area_Ptr.Holder;

   --  Mutator of margin top
   procedure Set_Margin_Top (This : out Instance; Value : Natural);

   --  Mutator of margin right
   procedure Set_Margin_Right (This : out Instance; Value : Natural);

   --  Mutator of margin Bottom
   procedure Set_Margin_Bottom (This : out Instance; Value : Natural);

   --  Mutator of margin left
   procedure Set_Margin_Left (This : out Instance; Value : Natural);

   --  Mutator of all margin of medium
   procedure Set_Margin (This : out Instance; Value : Natural);

   --  Mutator of medium
   procedure Set_Medium (This  : out Instance;
                         Value : LSE.Model.IO.Drawing_Area.
                           Drawing_Area_Ptr.Holder);

   --  Mutator of dry run
   procedure Set_Dry_Run (This : out Instance; Value : Boolean);

   --  Put this Turtle configuration in STDIO
   procedure Put (This : Instance);

   --  Configure the medium and turtle
   procedure Configure (This : in out Instance);

   --  Draw the final representation (save file, display in screen, etc.)
   procedure Draw (This : in out Instance);

   --  Go forward
   --  @param Trace True for stroke, False otherwise
   procedure Forward (This : in out Instance; Trace : Boolean := False);

   --  Positive rotation by angle
   procedure Rotate_Clockwise (This  : in out Instance);

   --  Negative rotation by angle
   procedure Rotate_Anticlockwise (This  : in out Instance);

   --  Go backward
   procedure UTurn (This : in out Instance);

   --  Save the current position in medium
   procedure Position_Save (This : in out Instance);

   --  Restore the previous saved location in medium
   procedure Position_Restore (This : in out Instance);

private
   use LSE.Utils.Coordinate_2D_List;

   type Instance is tagged record
      --  Width of the medium
      Width            : Positive := Default_Width;
      --  Height of the medium
      Height           : Positive := Default_Height;
      --  Background color
      Background_Color : Unbounded_String := Default_Background_Color;
      --  foreground color
      Foreground_Color : Unbounded_String := Default_Foreground_Color;
      --  Size of line that will be drawn
      Line_Size        : Float := Default_Line_Size;
      --  Angle step
      Angle            : LSE.Utils.Angle.Angle := Default_Angle;
      --  Current angle use in medium
      Stack_Angle      : List.Vector;
      --  "Stack_Coordinate" to save / restore coordinate
      Stack_Coordinate : LSE.Utils.Coordinate_2D_List.Vector;
      --  Maximum X coordinate of the L-System
      Max_X,
      --  Maximum Y coordinate of the L-System
      Max_Y,
      --  Minimum X coordinate of the L-System
      Min_X,
      --  Minimum Y coordinate of the L-System
      Min_Y            : Float := 0.0;
      --  Ratio of the L-System (to fit closely the medium)
      Ratio            : Float := 1.0;
      --  X shift on medium.
      --  Note: offset does not take margin in consideration because it is
      --  depending to the medium orientation.
      Offset_X,
      --  Y shift on medium
      --  Note: offset does not take margin in consideration because it is
      --  depending to the medium orientation.
      Offset_Y         : Float := 0.0;
      --  Margin top
      Margin_Top       : Float := Default_Margin_Top;
      --  Margin right
      Margin_Right     : Float := Default_Margin_Right;
      --  Margin bottom
      Margin_Bottom    : Float := Default_Margin_Bottom;
      --  Margin left
      Margin_Left      : Float := Default_Margin_Left;
      --  Medium where draw L-System
      Medium           : LSE.Model.IO.Drawing_Area.Drawing_Area_Ptr.Holder;
      --  Dry run it just used to compute l-system dimensions
      Dry_Run          : Boolean := False;
   end record;

   --  Make ratio and offsets for current medium
   procedure Make_Offset (This : in out Instance);

end LSE.Model.IO.Turtle;
