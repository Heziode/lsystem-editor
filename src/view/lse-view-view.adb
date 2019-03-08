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
with Gdk.Cairo;
with Gdk.Color;
with Glib.Convert;
with GNAT.Directory_Operations;
with GNAT.Strings;
with Gtk.About_Dialog;
with Gtk.Button;
with Gtk.Check_Button;
with Gtk.Color_Chooser_Dialog;
with Gtk.Dialog;
with Gtk.Enums;
with Gtk.File_Chooser;
with Gtk.File_Chooser_Dialog;
with Gtk.Image;
with Gtk.Image_Menu_Item;
with Gtk.Main;
with Gtk.Message_Dialog;
with Gtk.Text_Iter;
with Gtk.Tool_Button;
with Gtk.Style_Context;
with LSE.IO.Drawing_Area;
with LSE.Model.IO.Drawing_Area.Drawing_Area_Ptr;
with LSE.Model.IO.Text_File;
with LSE.View.Callbacks;

package body LSE.View.View is

   procedure Initialize (This : in out Instance; Builder : Gtk_Builder;
                         Presenter : access LSE.Presenter.Presenter.Instance)
   is
      pragma Unreferenced (Presenter);
      use Ada.Text_IO;
      use Gtk.Image_Menu_Item;
      use Gtk.Tool_Button;
      use LSE.View.Callbacks;

      Bar_Menu_Item  : Gtk_Image_Menu_Item;
      Icon_Menu_Item : Gtk_Tool_Button;
   begin
      This.Window         := Gtk_Window (Builder.Get_Object ("main_window"));
      This.Level_Selector := Gtk_Spin_Button (Builder.Get_Object ("ls_level"));
      This.Text_Error     :=
        Gtk_Text_Buffer (Builder.Get_Object ("text_error"));
      This.Text_Editor    :=
        Gtk_Text_Buffer (Builder.Get_Object ("text_editor"));
      This.Render_Area := Gtk_Drawing_Area
        (Builder.Get_Object ("render_area"));

      Connect (This.Window, "destroy", Stop_Program'Access);
      Connect (This.Level_Selector, "value-changed", LS_Level_Cb'Access);

      if not This.Window.Set_Icon_From_File (App_Icon_Location) then
         raise ICON_NOT_LOADED;
      end if;

      This.Window.Set_Title (App_Name);

      -------------------------
      --  Configure bar menu --
      -------------------------

      Bar_Menu_Item := Gtk_Image_Menu_Item
        (Builder.Get_Object ("menu_item_new"));
      Connect (Bar_Menu_Item, "activate", New_File_Cb'Access);

      Bar_Menu_Item := Gtk_Image_Menu_Item
        (Builder.Get_Object ("menu_item_open"));
      Connect (Bar_Menu_Item, "activate", Open_File_Cb'Access);

      Bar_Menu_Item := Gtk_Image_Menu_Item
        (Builder.Get_Object ("menu_item_save"));
      Connect (Bar_Menu_Item, "activate", Save_File_Cb'Access);

      Bar_Menu_Item := Gtk_Image_Menu_Item
        (Builder.Get_Object ("menu_item_save_as"));
      Connect (Bar_Menu_Item, "activate", Save_As_File_Cb'Access);

      Bar_Menu_Item := Gtk_Image_Menu_Item
        (Builder.Get_Object ("menu_item_quit"));
      Connect (Bar_Menu_Item, "activate", Stop_Program'Access);

      Bar_Menu_Item := Gtk_Image_Menu_Item
        (Builder.Get_Object ("menu_item_validate"));
      Connect (Bar_Menu_Item, "activate", Validate_Cb'Access);

      Bar_Menu_Item := Gtk_Image_Menu_Item
        (Builder.Get_Object ("menu_item_bg_color"));
      Connect (Bar_Menu_Item, "activate", Bg_Color_Cb'Access);

      Bar_Menu_Item := Gtk_Image_Menu_Item
        (Builder.Get_Object ("menu_item_fg_color"));
      Connect (Bar_Menu_Item, "activate", Fg_Color_Cb'Access);

      Bar_Menu_Item := Gtk_Image_Menu_Item
        (Builder.Get_Object ("menu_item_export_ps"));
      Connect (Bar_Menu_Item, "activate", Export_Cb'Access, "PS");

      Bar_Menu_Item := Gtk_Image_Menu_Item
        (Builder.Get_Object ("menu_item_about"));
      Connect (Bar_Menu_Item, "activate", About_Cb'Access);

      --------------------------
      --  Configure icon menu --
      --------------------------

      Icon_Menu_Item := Gtk_Tool_Button
        (Builder.Get_Object ("menu_icon_new"));
      Connect (Icon_Menu_Item, "clicked", New_File_Cb'Access);

      Icon_Menu_Item := Gtk_Tool_Button
        (Builder.Get_Object ("menu_icon_open"));
      Connect (Icon_Menu_Item, "clicked", Open_File_Cb'Access);

      Icon_Menu_Item := Gtk_Tool_Button
        (Builder.Get_Object ("menu_icon_save"));
      Connect (Icon_Menu_Item, "clicked", Save_File_Cb'Access);

      Icon_Menu_Item := Gtk_Tool_Button
        (Builder.Get_Object ("menu_icon_save_as"));
      Connect (Icon_Menu_Item, "clicked", Save_As_File_Cb'Access);

      Icon_Menu_Item := Gtk_Tool_Button
        (Builder.Get_Object ("menu_icon_quit"));
      Connect (Icon_Menu_Item, "clicked", Stop_Program'Access);

      Icon_Menu_Item := Gtk_Tool_Button
        (Builder.Get_Object ("menu_icon_validate"));
      Connect (Icon_Menu_Item, "clicked", Validate_Cb'Access);

      Icon_Menu_Item := Gtk_Tool_Button
        (Builder.Get_Object ("menu_icon_bg_color"));
      Connect (Icon_Menu_Item, "clicked", Bg_Color_Cb'Access);

      Icon_Menu_Item := Gtk_Tool_Button
        (Builder.Get_Object ("menu_icon_fg_color"));
      Connect (Icon_Menu_Item, "clicked", Fg_Color_Cb'Access);

      Icon_Menu_Item := Gtk_Tool_Button
        (Builder.Get_Object ("menu_icon_about"));
      Connect (Icon_Menu_Item, "clicked", About_Cb'Access);

      ---------------------------
      --  Set default L-System --
      ---------------------------

      This.Text_Editor.Set_Text (Default_LSystem);
      This.Render_Area.On_Draw (Draw_Cb'Access);
      This.Render_Area.On_Size_Allocate (Size_Allocate_Cb'Access);

   exception
      when ICON_NOT_LOADED => Put_Line ("App icon cannot be loaded");
   end Initialize;

   function Initialize (Builder : Gtk_Builder;
                        Presenter : access LSE.Presenter.Presenter.Instance)
                        return Instance
   is
      This : Instance;
   begin
      This.Initialize (Builder, Presenter);
      return This;
   end Initialize;

   procedure Start (This : Instance)
   is
   begin
      This.Validate;
   end Start;

   procedure Set_Presenter (This  : in out Instance;
                            Value : LSE.Presenter.Presenter.Instance)
   is
   begin
      This.Presenter := new LSE.Presenter.Presenter.Instance '(Value);
      LSE.View.Callbacks.View := This;
   end Set_Presenter;

   procedure Stop_Program (This : Instance)
   is
      pragma Unreferenced (This);
      use Gtk.Dialog;
      use Gtk.Main;
      use Gtk.Message_Dialog;

      Dialog : Gtk_Message_Dialog;
   begin
      Gtk_New (Dialog, null, 0, Message_Error, Buttons_Ok_Cancel,
               "Are you sure to want to quit the app ?");
      Dialog.Set_Title ("Confirmation exit the application");

      Dialog.Show_All;

      if Dialog.Run = Gtk_Response_OK then
         Dialog.Close;
         Main_Quit;
      else
         Dialog.Close;
      end if;
   end Stop_Program;

   procedure New_File (This : Instance)
   is
      use Gtk.Dialog;
      use Gtk.Message_Dialog;

      Dialog : Gtk_Message_Dialog;
   begin
      if File_Path.Element = "" then
         This.Text_Editor.Set_Text ("");
      else
         Gtk_New (Dialog, null, 0, Message_Error, Buttons_Ok_Cancel,
                  "Are you sure to want create a new file ?");
         Dialog.Set_Title ("Confirmation create a new file");

         Dialog.Show_All;

         if Dialog.Run = Gtk_Response_OK then
            Dialog.Close;
            This.Text_Editor.Set_Text ("");
            File_Path.Reference := To_Unbounded_String ("");
            This.Window.Set_Title (App_Name);
         else
            Dialog.Close;
         end if;
      end if;
   end New_File;

   procedure Save (This : Instance; Save_As : Boolean := False)
   is
      use Gtk.Button;
      use Gtk.Dialog;
      use Gtk.File_Chooser;
      use Gtk.File_Chooser_Dialog;
      use LSE.Model.IO.Text_File;

      Dialog : Gtk_File_Chooser_Dialog;
      Btn_Ok : Gtk_Button;
      Btn_Ko : Gtk_Button;
      Save   : Boolean := False;
   begin
      if File_Path.Element = "" or Save_As then
         Btn_Ko := Gtk_Button_New_With_Mnemonic ("_Cancel");
         Btn_Ok := Gtk_Button_New_With_Mnemonic ("_Save");

         Dialog := Gtk_File_Chooser_Dialog_New
           ("Save File", This.Window, Action_Save);
         Dialog.Set_Do_Overwrite_Confirmation (True);

         Dialog.Add_Action_Widget (Btn_Ko, Gtk.Dialog.Gtk_Response_Cancel);
         Dialog.Add_Action_Widget (Btn_Ok, Gtk.Dialog.Gtk_Response_Accept);
         Dialog.Set_Extra_Widget (Btn_Ok);
         Dialog.Set_Extra_Widget (Btn_Ko);

         Dialog.Set_Current_Name (Default_File_Name & Default_LS_Extension);

         if Dialog.Run = Gtk.Dialog.Gtk_Response_Accept then
            Save := True;
            File_Path.Reference :=
              To_Unbounded_String (Dialog.Get_Filename);
            This.Window.Set_Title (App_Name & " - " & Dialog.Get_Filename);
            Dialog.Destroy;
         else
            Dialog.Destroy;
         end if;
      else
         Save := True;
      end if;

      if Save then
         Save_To_File (To_String (File_Path.Element),
                       This.Get_Text_Editor_Content);
      end if;
   end Save;

   procedure Validate (This : Instance)
   is
   begin
      if This.Presenter.Validate
        (This.Get_Text_Editor_Content)
      then
         This.Text_Error.Set_Text ("");
         This.Level_Selector.Set_Value (0.0);
         This.Render;
      else
         This.Text_Error.Set_Text (This.Presenter.Get_Error);
      end if;
   end Validate;

   procedure Open (This : Instance)
   is
      use Gtk.Button;
      use Gtk.Dialog;
      use Gtk.File_Chooser;
      use Gtk.File_Chooser_Dialog;
      use LSE.Model.IO.Text_File;

      Dialog : Gtk_File_Chooser_Dialog;
      Btn_Ok : Gtk_Button;
      Btn_Ko : Gtk_Button;
   begin
      Btn_Ko := Gtk_Button_New_With_Mnemonic ("_Cancel");
      Btn_Ok := Gtk_Button_New_With_Mnemonic ("_Open");

      Dialog := Gtk_File_Chooser_Dialog_New
        ("Open File", This.Window, Action_Open);
      Dialog.Set_Do_Overwrite_Confirmation (True);

      Dialog.Add_Action_Widget (Btn_Ko, Gtk.Dialog.Gtk_Response_Cancel);
      Dialog.Add_Action_Widget (Btn_Ok, Gtk.Dialog.Gtk_Response_Accept);
      Dialog.Set_Extra_Widget (Btn_Ok);
      Dialog.Set_Extra_Widget (Btn_Ko);

      if Dialog.Run = Gtk.Dialog.Gtk_Response_Accept then
         File_Path.Reference :=
           To_Unbounded_String (Dialog.Get_Filename);
         This.Window.Set_Title (App_Name & " - " & Dialog.Get_Filename);
         This.Text_Editor.Set_Text (Read_From_File (Dialog.Get_Filename));
         Dialog.Destroy;
      else
         Dialog.Destroy;
      end if;
   end Open;

   procedure About (This : Instance)
   is
      use GNAT.Strings;
      use Gtk.About_Dialog;
      use Gtk.Dialog;
      use Gtk.Image;

      pragma Unreferenced (This);

      Dialog : Gtk_About_Dialog;
      List1  : GNAT.Strings.String_List (1 .. 1);
      Image  : Gtk_Image;
   begin
      List1 (1) := new String '("Quentin Dauprat (Heziode)");

      --  Create about dialog
      Gtk_New (Dialog);

      Gtk_New (Image, App_Icon_Location);
      Dialog.Set_Logo (Image.Get);

      Dialog.Set_Program_Name ("Lindenmayer system editor");
      Dialog.Set_Comments ("This program is an editor of L-System");
      Dialog.Set_License_Type (License_Mit_X11);
      Dialog.Set_Version ("1.0.0");

      Dialog.Set_Website ("https://github.com/Heziode/lsystem-editor");
      Dialog.Set_Website_Label ("Source-code");

      Dialog.Set_Authors (List1);

      --        Dialog.Show_All;
      if Dialog.Run = Gtk.Dialog.Gtk_Response_Close then
         Dialog.Destroy;
      else
         Dialog.Destroy;
      end if;
   end About;

   procedure LS_Level (This : Instance; Value : Integer)
   is
   begin
      This.Presenter.LS_Level (Value);
      This.Render;
   end LS_Level;

   procedure Background_Color (This : Instance)
   is
      use Gdk.RGBA;
      use Gtk.Color_Chooser_Dialog;
      use Gtk.Dialog;

      Dialog : Gtk_Color_Chooser_Dialog;
      Rgba   : Gdk_RGBA;
   begin
      Dialog := Gtk_Color_Chooser_Dialog_New ("Background Color", This.Window);

      if Dialog.Run = Gtk.Dialog.Gtk_Response_OK then
         Dialog.Get_Rgba (Rgba);
         This.Presenter.Set_Background_Color (Rgba);
         This.Render_Area.Queue_Draw;
         Dialog.Destroy;
      else
         Dialog.Destroy;
      end if;
   end Background_Color;

   procedure Foreground_Color (This : Instance)
   is
      use Gdk.RGBA;
      use Gtk.Color_Chooser_Dialog;
      use Gtk.Dialog;

      Dialog : Gtk_Color_Chooser_Dialog;
      Rgba   : Gdk_RGBA;
   begin
      Dialog := Gtk_Color_Chooser_Dialog_New ("Foreground Color", This.Window);

      if Dialog.Run = Gtk.Dialog.Gtk_Response_OK then
         Dialog.Get_Rgba (Rgba);
         This.Presenter.Set_Foreground_Color (Rgba);
         This.Render_Area.Queue_Draw;
         Dialog.Destroy;
      else
         Dialog.Destroy;
      end if;
   end Foreground_Color;

   procedure Export (This   : Instance;
                     Format : String)
   is
      use Glib;
      use GNAT.Directory_Operations;
      use Gtk.Button;
      use Gtk.Dialog;
      use Gtk.File_Chooser;
      use Gtk.File_Chooser_Dialog;

      Unknown_Color : exception;

      Dialog        : Gtk_File_Chooser_Dialog;
      Btn_Ok        : Gtk_Button;
      Btn_Ko        : Gtk_Button;
      Path          : Unbounded_String;
      Width         : Gint := Gint (This.Presenter.Get_Width);
      Height        : Gint := Gint (This.Presenter.Get_Height);
      Margin_Top    : Gint := Gint (This.Presenter.Get_Margin_Top);
      Margin_Right  : Gint := Gint (This.Presenter.Get_Margin_Right);
      Margin_Bottom : Gint := Gint (This.Presenter.Get_Margin_Bottom);
      Margin_Left   : Gint := Gint (This.Presenter.Get_Margin_Left);
      Bg_Rgba       : Gdk_RGBA;
      Fg_Rgba       : Gdk_RGBA;
      Export        : Boolean;
      Success       : Boolean;
      Have_Bg       : Boolean := This.Presenter.Get_Background_Color /= "";
      File_Choose   : Boolean := False;
   begin
      --  While user give bad file
      Choose : while not File_Choose loop
         Btn_Ko := Gtk_Button_New_With_Mnemonic ("_Cancel");
         Btn_Ok := Gtk_Button_New_With_Mnemonic ("_Save");

         Dialog := Gtk_File_Chooser_Dialog_New
           ("Export File", This.Window, Action_Save);
         Dialog.Set_Do_Overwrite_Confirmation (True);

         Dialog.Add_Action_Widget (Btn_Ko, Gtk.Dialog.Gtk_Response_Cancel);
         Dialog.Add_Action_Widget (Btn_Ok, Gtk.Dialog.Gtk_Response_Accept);
         Dialog.Set_Extra_Widget (Btn_Ok);
         Dialog.Set_Extra_Widget (Btn_Ko);

         Dialog.Set_Current_Name (Default_File_Name);

         if Dialog.Run = Gtk.Dialog.Gtk_Response_Accept then
            Path := To_Unbounded_String (Dialog.Get_Filename);

            Dialog.Destroy;

            --  Check if file is valid
            Check_File : declare
               Name : constant String := Base_Name (To_String (Path));
               File_Format : constant String :=
                 This.Presenter.Get_Extension (Format);
            begin
               if Name'Length < File_Format'Length then
                  File_Choose := True;
                  Path := Path & File_Format;
               elsif Name'Length = 0 then
                  File_Choose := False;
               elsif Name (Name'Last - File_Format'Length + 1 .. Name'Last)
                 /= File_Format
               then
                  File_Choose := True;
                  Path := Path & File_Format;
               else
                  File_Choose := True;
               end if;
            end Check_File;
         else
            Dialog.Destroy;
            exit Choose;
         end if;
      end loop Choose;

      if File_Choose then
         --  Configure export

         Parse (Bg_Rgba, This.Presenter.Get_Background_Color, Success);
         if This.Presenter.Get_Background_Color /= "" and then not Success then
            raise Unknown_Color;
         end if;

         Parse (Fg_Rgba, This.Presenter.Get_Foreground_Color, Success);
         if not Success then
            raise Unknown_Color;
         end if;

         This.Get_Export_Param (Width         => Width,
                                Height        => Height,
                                Margin_Top    => Margin_Top,
                                Margin_Right  => Margin_Right,
                                Margin_Bottom => Margin_Bottom,
                                Margin_Left   => Margin_Left,
                                Bg_Rgba       => Bg_Rgba,
                                Fg_Rgba       => Fg_Rgba,
                                Have_Bg       => Have_Bg,
                                Export        => Export);
         if Export then
            This.Presenter.Export (Format,
                                   To_String (Path),
                                   Width,
                                   Height,
                                   Margin_Top,
                                   Margin_Right,
                                   Margin_Bottom,
                                   Margin_Left,
                                   Bg_Rgba,
                                   Fg_Rgba,
                                   Have_Bg);
         end if;
      end if;
   end Export;

   function Get_Text_Editor_Content (This : Instance) return String
   is
      use Gtk.Text_Iter;

      Start, The_End : Gtk_Text_Iter;
   begin
      Get_Bounds (This.Text_Editor, Start, The_End);

      return Get_Text (This.Text_Editor, Start, The_End);
   end Get_Text_Editor_Content;

   procedure Render (This : Instance)
   is
   begin
      This.Presenter.Develop;
      This.Presenter.Interpret;
      This.Render_Area.Queue_Draw;
   end Render;

   procedure Get_Export_Param (This        : Instance;
                               Width, Height,
                               Margin_Top,
                               Margin_Right,
                               Margin_Bottom,
                               Margin_Left : in out Gint;
                               Bg_Rgba,
                               Fg_Rgba     : in out Gdk_RGBA;
                               Have_Bg     : in out Boolean;
                               Export      : out Boolean)
   is
      pragma Unreferenced (This);
      use Glib.Convert;
      use Gtk.Builder;
      use Gtk.Button;
      use Gtk.Check_Button;
      use Gtk.Dialog;
      use LSE.View.Callbacks;

      Builder : Gtk_Builder;
      Dialog  : Gtk_Dialog := Gtk_Dialog_New;
      Btn_Ok  : Gtk_Button;
      Btn_Ko  : Gtk_Button;
      Check   : Gtk_Check_Button;
      Spin    : Gtk_Spin_Button;
      Color   : Gtk_Color_Button;
   begin
      Gtk_New_From_File (Builder, Locale_To_UTF8
                         ("ressources/dialog_export.glade"));
      Dialog := Gtk_Dialog (Builder.Get_Object ("dialog_export"));
      Btn_Ko := Gtk_Button (Builder.Get_Object ("btn_cancel"));
      Btn_Ok := Gtk_Button (Builder.Get_Object ("btn_export"));
      Dialog.Add_Action_Widget (Btn_Ko, Gtk.Dialog.Gtk_Response_Cancel);
      Dialog.Add_Action_Widget (Btn_Ok, Gtk.Dialog.Gtk_Response_Accept);

      --  Set background color
      Color := Gtk_Color_Button (Builder.Get_Object ("bg_color"));
      if Have_Bg then
         Color.Set_Rgba (Bg_Rgba);
      end if;

      --  Toggle checkbox for background color
      Check := Gtk_Check_Button (Builder.Get_Object ("checkbox_bg"));
      Check.Set_Active (Have_Bg);
      Color.Set_Sensitive (Have_Bg);

      Connect (Check, "toggled", Export_Bg_Color_Cb'Access, Color);

      --  Set foreground color
      Color := Gtk_Color_Button (Builder.Get_Object ("fg_color"));
      Color.Set_Rgba (Fg_Rgba);

      --  Set Width
      Spin := Gtk_Spin_Button (Builder.Get_Object ("spin_width"));
      Spin.Set_Value (Gdouble (Width));

      --  Set Height
      Spin := Gtk_Spin_Button (Builder.Get_Object ("spin_height"));
      Spin.Set_Value (Gdouble (Height));

      --  Set margin top
      Spin := Gtk_Spin_Button (Builder.Get_Object ("spin_margin_top"));
      Spin.Set_Value (Gdouble (Margin_Top));

      --  Set margin right
      Spin := Gtk_Spin_Button (Builder.Get_Object ("spin_margin_right"));
      Spin.Set_Value (Gdouble (Margin_Right));

      --  Set margin bottom
      Spin := Gtk_Spin_Button (Builder.Get_Object ("spin_margin_bottom"));
      Spin.Set_Value (Gdouble (Margin_Bottom));

      --  Set margin left
      Spin := Gtk_Spin_Button (Builder.Get_Object ("spin_margin_left"));
      Spin.Set_Value (Gdouble (Margin_Left));

      if Dialog.Run = Gtk.Dialog.Gtk_Response_Accept then

         --  Set background color
         if Check.Get_Active then
            Color := Gtk_Color_Button (Builder.Get_Object ("bg_color"));
            Color.Get_Rgba (Bg_Rgba);
            Have_Bg := True;
         else
            Have_Bg := False;
         end if;

         --  Set foreground color
         Color := Gtk_Color_Button (Builder.Get_Object ("fg_color"));
         Color.Get_Rgba (Fg_Rgba);

         --  Set Width
         Spin := Gtk_Spin_Button (Builder.Get_Object ("spin_width"));
         Width := Spin.Get_Value_As_Int;

         --  Set Height
         Spin := Gtk_Spin_Button (Builder.Get_Object ("spin_height"));
         Height := Spin.Get_Value_As_Int;

         --  Set margin top
         Spin := Gtk_Spin_Button (Builder.Get_Object ("spin_margin_top"));
         Margin_Top := Spin.Get_Value_As_Int;

         --  Set margin right
         Spin := Gtk_Spin_Button (Builder.Get_Object ("spin_margin_right"));
         Margin_Right := Spin.Get_Value_As_Int;

         --  Set margin bottom
         Spin := Gtk_Spin_Button (Builder.Get_Object ("spin_margin_bottom"));
         Margin_Bottom := Spin.Get_Value_As_Int;

         --  Set margin left
         Spin := Gtk_Spin_Button (Builder.Get_Object ("spin_margin_left"));
         Margin_Left := Spin.Get_Value_As_Int;

         Dialog.Destroy;
         Export := True;
      else
         Dialog.Destroy;
         Export := False;
      end if;
   end Get_Export_Param;

   procedure Draw (This : Instance; Cr : Cairo.Cairo_Context)
   is
      use Gdk.Cairo;
      use Gdk.Color;
      use Gtk.Enums;
      use Gtk.Style_Context;
      use LSE.IO.Drawing_Area;
      use LSE.Model.IO.Drawing_Area.Drawing_Area_Ptr;

      Context : constant Gtk_Style_Context :=
        Get_Style_Context (This.Render_Area);
      Fg_Color : constant Gdk_Color :=
        Parse (This.Presenter.Get_Foreground_Color);
      Bg_Color : Gdk_Color := Null_Color;
   begin
      This.Presenter.Set_Medium (To_Holder (Initialize (Cr)));

      Render_Background (Context, Cr, 0.0, 0.0,
                         Gdouble (This.Render_Area.Get_Allocated_Width),
                         Gdouble (This.Render_Area.Get_Allocated_Height));

      --  Set foreground
      if This.Presenter.Get_Background_Color /= "" then
         Bg_Color := Parse (This.Presenter.Get_Background_Color);
      end if;
      Set_Source_Color (Cr, Fg_Color);

      --  Set background
      This.Render_Area.Modify_Bg (State_Normal, Bg_Color);

      if First_Run then
         This.Presenter.Set_Width (This.Render_Area.Get_Allocated_Width);
         This.Presenter.Set_Height (This.Render_Area.Get_Allocated_Height);
         This.Presenter.Develop;
         First_Run := False;
      end if;

      This.Presenter.Interpret;
   end Draw;

   procedure Size_Allocate (This : Instance)
   is
   begin
      This.Presenter.Set_Width (This.Render_Area.Get_Allocated_Width);
      This.Presenter.Set_Height (This.Render_Area.Get_Allocated_Height);

      This.Render;
   end Size_Allocate;

end LSE.View.View;
