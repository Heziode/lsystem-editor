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

with Cairo;
with Gtk.Color_Button;
with Gtk.Widget;
with LSE.View.View;

use Cairo;
use Gtk.Color_Button;
use Gtk.Widget;
use LSE.View.View;

package LSE.View.Callbacks is

   --  Reference of the view. (VERY Very very ugly)
   View : Instance;

   --  Call to stop the program
   procedure Stop_Program (Emitter : access Gtk_Widget_Record'Class);

   --  Callback to change L-System level to develop
   procedure LS_Level_Cb (Emitter : access Gtk_Widget_Record'Class);

   --  Callback to start new file
   procedure New_File_Cb (Emitter : access Gtk_Widget_Record'Class);

   --  Callback to open a L-System from a file
   procedure Open_File_Cb (Emitter : access Gtk_Widget_Record'Class);

   --  Callback to save the L-System
   procedure Save_File_Cb (Emitter : access Gtk_Widget_Record'Class);

   --  Callback to save the L-System in new file
   procedure Save_As_File_Cb (Emitter : access Gtk_Widget_Record'Class);

   --  Callback to validate the L-System
   procedure Validate_Cb (Emitter : access Gtk_Widget_Record'Class);

   --  Callback to change the background color
   procedure Bg_Color_Cb (Emitter : access Gtk_Widget_Record'Class);

   --  Callback to change the foreground color
   procedure Fg_Color_Cb (Emitter : access Gtk_Widget_Record'Class);

   --  Callback to export the L-System
   procedure Export_Cb (Emitter : access Gtk_Widget_Record'Class;
                        Format  : String);

   --  Callback to show About dialog
   procedure About_Cb (Emitter : access Gtk_Widget_Record'Class);

   --  Callback when user want to add / remove background color in export
   procedure Export_Bg_Color_Cb (Emitter : access Gtk_Widget_Record'Class;
                                 Item    : Gtk_Color_Button);

   --  Callback when draw
   function Draw_Cb (Emitter : access Gtk_Widget_Record'Class;
                     Cr      : Cairo.Cairo_Context) return Boolean;

   --  Callback when window is resized
   procedure Size_Allocate_Cb (Emitter    : access Gtk_Widget_Record'Class;
                               Allocation : Gtk_Allocation);

end LSE.View.Callbacks;
