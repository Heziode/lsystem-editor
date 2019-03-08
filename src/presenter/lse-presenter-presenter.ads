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

limited with LSE.View.View;
with Ada.Characters.Latin_1;
with Ada.Command_Line;
with Gdk;
with Gdk.RGBA;
with Glib;
with GNAT.Directory_Operations;
with LSE.Model.L_System.Concrete_Builder;
with LSE.Model.L_System.L_System_Ptr;
with LSE.Model.IO.Drawing_Area.Drawing_Area_Ptr;
with LSE.Model.IO.Turtle_Utils;

use Ada.Command_Line;
use Gdk;
use Gdk.RGBA;
use Glib;
use GNAT.Directory_Operations;
use LSE.Model.L_System.Concrete_Builder;
use LSE.Model.L_System.L_System_Ptr;
use LSE.Model.IO.Drawing_Area.Drawing_Area_Ptr;
use LSE.Model.IO.Turtle_Utils;

--  @description
--  This package is the presenter used in Model-View-Presenter design pattern
--  used in this app.
--
package LSE.Presenter.Presenter is

   package L renames Ada.Characters.Latin_1;

   Default_LSystem : constant String := "60.0" & L.LF &
     "-F++F++F" & L.LF &
     "F F-F++F-F";

   --  Presenter
   type Instance is tagged private;

   Exec_Dir : constant String := Dir_Name (Command_Name);

   --  Constructor
   procedure Initialize (This : out Instance);

   --  Accessor of view
   function Get_View (This : Instance) return LSE.View.View.Instance;

   --  Launch the GUI
   procedure Start (This : Instance);

   --  Change level of development of the L-System
   procedure LS_Level (This : out Instance; Value : Integer)
     with Pre => Value >= 0;

   --  Validate the L-System
   function Validate (This : in out Instance; Input : String) return Boolean;

   --  Get L-System creation errors
   function Get_Error (This : Instance) return String;

   --  Develop the L-System
   procedure Develop (This : out Instance);

   --  Interpret the L-System
   procedure Interpret (This : out Instance);

   --  Export the L-System
   procedure Export (This        : in out Instance;
                     Format      : String;
                     Path        : String;
                     Width, Height,
                     Margin_Top,
                     Margin_Right,
                     Margin_Bottom,
                     Margin_Left : Gint;
                     Bg_Rgba,
                     Fg_Rgba     : Gdk_RGBA;
                     Have_Bg     : Boolean);

   --  Accessor of width
   function Get_Width (This : Instance) return Positive;

   --  Accessor of height
   function Get_Height (This : Instance) return Positive;

   --  Accessor of background color
   function Get_Background_Color (This : Instance) return String;

   --  Accessor of foreground color
   function Get_Foreground_Color (This : Instance) return String;

   --  Accessor of margin top
   function Get_Margin_Top (This : Instance) return Float;

   --  Accessor of margin right
   function Get_Margin_Right (This : Instance) return Float;

   --  Accessor of margin Bottom
   function Get_Margin_Bottom (This : Instance) return Float;

   --  Accessor of margin left
   function Get_Margin_Left (This : Instance) return Float;

   --  Mutator of Width
   procedure Set_Width (This : out Instance; Value : Gint);

   --  Mutator of Height
   procedure Set_Height (This : out Instance; Value : Gint);

   --  Mutator of background color
   procedure Set_Background_Color (This : out Instance; Value : Gdk_RGBA);

   --  Mutator of background color (set to no background)
   procedure Set_Background_Color_Clear (This : out Instance);

   --  Mutator of foreground color
   procedure Set_Foreground_Color (This : out Instance; Value : Gdk_RGBA);

   --  Mutator of margin top
   procedure Set_Margin_Top (This : out Instance; Value : Gint);

   --  Mutator of margin right
   procedure Set_Margin_Right (This : out Instance; Value : Gint);

   --  Mutator of margin Bottom
   procedure Set_Margin_Bottom (This : out Instance; Value : Gint);

   --  Mutator of margin left
   procedure Set_Margin_Left (This : out Instance; Value : Gint);

   --  Get extension of file
   function Get_Extension (This : Instance; Value : String) return String;

   --  Mutator of medium
   procedure Set_Medium (This  : out Instance;
                         Value : LSE.Model.IO.Drawing_Area.
                           Drawing_Area_Ptr.Holder);
private

   type Instance is tagged record
      LS      : LSE.Model.L_System.L_System_Ptr.Holder;
      View    : access LSE.View.View.Instance;
      Turtle  : LSE.Model.IO.Turtle_Utils.Holder;
      Builder : LSE.Model.L_System.Concrete_Builder.Instance;
   end record;

end LSE.Presenter.Presenter;
