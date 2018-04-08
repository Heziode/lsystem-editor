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

with Gtk.Check_Button;
with Gtk.Spin_Button;

package body LSE.View.Callbacks is

   procedure Stop_Program (Emitter : access Gtk_Widget_Record'Class)
   is
      pragma Unreferenced (Emitter);
   begin
      View.Stop_Program;
   end Stop_Program;

   procedure LS_Level_Cb (Emitter : access Gtk_Widget_Record'Class)
   is
      use Gtk.Spin_Button;

      Spin : constant Gtk_Spin_Button := Gtk_Spin_Button (Emitter);
   begin
      View.LS_Level (Integer (Get_Value_As_Int (Spin)));
   end LS_Level_Cb;

   procedure New_File_Cb (Emitter : access Gtk_Widget_Record'Class)
   is
      pragma Unreferenced (Emitter);
   begin
      View.New_File;
   end New_File_Cb;

   procedure Open_File_Cb (Emitter : access Gtk_Widget_Record'Class)
   is
      pragma Unreferenced (Emitter);
   begin
      View.Open;
   end Open_File_Cb;

   procedure Save_File_Cb (Emitter : access Gtk_Widget_Record'Class)
   is
      pragma Unreferenced (Emitter);
   begin
      View.Save;
   end Save_File_Cb;

   procedure Save_As_File_Cb (Emitter : access Gtk_Widget_Record'Class)
   is
      pragma Unreferenced (Emitter);
   begin
      View.Save (True);
   end Save_As_File_Cb;

   procedure Validate_Cb (Emitter : access Gtk_Widget_Record'Class)
   is
      pragma Unreferenced (Emitter);
   begin
      View.Validate;
   end Validate_Cb;

   procedure Bg_Color_Cb (Emitter : access Gtk_Widget_Record'Class)
   is
      pragma Unreferenced (Emitter);
   begin
      View.Background_Color;
   end Bg_Color_Cb;

   procedure Fg_Color_Cb (Emitter : access Gtk_Widget_Record'Class)
   is
      pragma Unreferenced (Emitter);
   begin
      View.Foreground_Color;
   end Fg_Color_Cb;

   procedure Export_Cb (Emitter : access Gtk_Widget_Record'Class;
                        Format  : String)
   is
      pragma Unreferenced (Emitter);
   begin
      View.Export (Format);
   end Export_Cb;

   procedure About_Cb (Emitter : access Gtk_Widget_Record'Class)
   is
      pragma Unreferenced (Emitter);
   begin
      View.About;
   end About_Cb;

   procedure Export_Bg_Color_Cb (Emitter : access Gtk_Widget_Record'Class;
                                 Item    : Gtk_Color_Button)
   is
      use Gtk.Check_Button;
      This : constant Gtk_Check_Button := Gtk_Check_Button (Emitter);
   begin
      Item.Set_Sensitive (This.Get_Active);
   end Export_Bg_Color_Cb;

   function Draw_Cb (Emitter : access Gtk_Widget_Record'Class;
                     Cr      : Cairo.Cairo_Context) return Boolean
   is
      pragma Unreferenced (Emitter);
   begin
      View.Draw (Cr);
      return False;
   end Draw_Cb;

   procedure Size_Allocate_Cb (Emitter    : access Gtk_Widget_Record'Class;
                               Allocation : Gtk_Allocation)
   is
      pragma Unreferenced (Emitter, Allocation);
   begin
      View.Size_Allocate;
   end Size_Allocate_Cb;

end LSE.View.Callbacks;
