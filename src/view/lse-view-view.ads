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
with Gdk.RGBA;
with Glib;
with Gtk.Builder;
with Gtk.Color_Button;
with Gtk.Drawing_Area;
with Gtk.Handlers;
with Gtk.Spin_Button;
with Gtk.Text_Buffer;
with Gtk.Widget;
with Gtk.Window;
with LSE.Presenter.Presenter;
with LSE.Utils.Utils;

use Ada.Strings.Unbounded;
use Cairo;
use Gdk.RGBA;
use Glib;
use Gtk.Builder;
use Gtk.Color_Button;
use Gtk.Drawing_Area;
use Gtk.Spin_Button;
use Gtk.Text_Buffer;
use Gtk.Widget;
use Gtk.Window;
use LSE.Presenter.Presenter;
use LSE.Utils.Utils.US_Ptr;

--  @description
--  This package provide a GUI make with Glade.
--
package LSE.View.View is

   --  Location of galde view
   Main_UI           : constant String := "ressources/view.glade";

   --  Default app name
   App_Name          : constant String := "Lindenmayer system editor";

   --  Default file name when save a L-System
   Default_File_Name : constant String := "Untitled";

   --  Default extension of L-System
   Default_LS_Extension : constant String := ".ls";

   --  Location of the icon of the app
   App_Icon_Location : constant String := "ressources/icon.png";

   --  Raise when app icon cannot be loaded
   ICON_NOT_LOADED : exception;

   --  Contains pointers of widget of the view
   type Instance is tagged private;

   --  Pointer of Instance (View)
   type Ptr is access all Instance;

   --  Constructor
   procedure Initialize (This : in out Instance; Builder : Gtk_Builder;
                         Presenter : access LSE.Presenter.Presenter.Instance);

   --  Constructor
   function Initialize (Builder : Gtk_Builder;
                        Presenter : access LSE.Presenter.Presenter.Instance)
                        return Instance;

   --  Launch the GUI
   procedure Start (This : Instance);

   --  Mutator of presenter
   procedure Set_Presenter (This  : in out Instance;
                            Value : LSE.Presenter.Presenter.Instance);

   --  Close the program
   procedure Stop_Program (This : Instance);

   --  Empty the text editor and create a new instance
   procedure New_File (This : Instance);

   --  Save content of text editor into a file
   --  @param Save_As True to force save as new file, False otherwise
   procedure Save (This : Instance; Save_As : Boolean := False);

   --  Validate the L-System
   procedure Validate (This : Instance);

   --  Open L-System from file
   procedure Open (This : Instance);

   --  Show about dialog
   procedure About (This : Instance);

   --  Change level of development of the L-System
   procedure LS_Level (This : Instance; Value : Integer);

   --  Change background color
   procedure Background_Color (This : Instance);

   --  Change foreground color
   procedure Foreground_Color (This : Instance);

   --  Export the L-System
   procedure Export (This   : Instance;
                     Format : String);

   --  Draw the L-System into the GUI
   procedure Draw (This : Instance; Cr : Cairo.Cairo_Context);

   --  Resize the drawing area
   procedure Size_Allocate (This : Instance);

private

   type Instance is tagged record
      Presenter      : access LSE.Presenter.Presenter.Instance;
      Window         : Gtk_Window;
      Level_Selector : Gtk_Spin_Button;
      Text_Error     : Gtk_Text_Buffer;
      Text_Editor    : Gtk_Text_Buffer;
      Render_Area    : Gtk_Drawing_Area;
   end record;

   --  Loaction to store the file
   File_Path : LSE.Utils.Utils.US_Ptr.Holder :=
     To_Holder (To_Unbounded_String (""));

   --  Used to check if need a first develop before render
   First_Run : Boolean := True;

   --  Get the content of the text editor
   function Get_Text_Editor_Content (This : Instance) return String;

   --  Render the L-System
   procedure Render (This : Instance);

   --  Display modal to get param export (width, height, etc.)
   procedure Get_Export_Param (This        : Instance;
                               Width, Height,
                               Margin_Top,
                               Margin_Right,
                               Margin_Bottom,
                               Margin_Left : in out Gint;
                               Bg_Rgba,
                               Fg_Rgba     : in out Gdk_RGBA;
                               Have_Bg     : in out Boolean;
                               Export      : out Boolean);

   package P_Handlers_Widget is new Gtk.Handlers.Callback (Gtk_Widget_Record);
   package P_Handlers_User_Callback_String is new Gtk.Handlers.User_Callback
     (Gtk_Widget_Record, String);
   package P_Handlers_User_Callback_Color is new Gtk.Handlers.User_Callback
     (Gtk_Widget_Record, Gtk_Color_Button);
   package P_Handlers_User_Callback_Cairo is new Gtk.Handlers.User_Callback
     (Gtk_Widget_Record, Cairo.Cairo_Context);

   use P_Handlers_Widget;
   use P_Handlers_User_Callback_String;
   use P_Handlers_User_Callback_Color;

end LSE.View.View;
