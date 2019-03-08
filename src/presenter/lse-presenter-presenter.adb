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

with Ada.Directories;
with Glib.Convert;
with Gtk.Builder;
with Gtk.Main;
with LSE.IO.Export_Factory;
with LSE.Model.IO.Turtle;
with LSE.Model.L_System.L_System;
with LSE.Utils.Colors;
with LSE.View.View;

package body LSE.Presenter.Presenter is

   --  Constructor
   procedure Initialize (This : out Instance)
   is
      use Glib.Convert;
      use Gtk.Builder;
      use LSE.Model.IO.Turtle;
      use LSE.Model.L_System.Concrete_Builder;
      use LSE.Model.L_System.L_System;
      use LSE.View.View;

      Builder    : Gtk_Builder;
      LS_Builder : LSE.Model.L_System.Concrete_Builder.Instance;
   begin
      Ada.Directories.Set_Directory (Exec_Dir);

      Gtk.Main.Init;
      Gtk_New_From_File (Builder, Locale_To_UTF8 (Main_UI));

      This.View   := new LSE.View.View.Instance '(Initialize (Builder,
                                                  This'Access));
      This.Turtle := To_Holder (Initialize);

      if LS_Builder.Make (Default_LSystem) then
         This.LS := To_Holder (LS_Builder.Get_Product (This.Turtle));
      end if;
   end Initialize;

   function Get_View (This : Instance) return LSE.View.View.Instance
   is
   begin
      return This.View.all;
   end Get_View;

   procedure Start (This : Instance)
   is
      pragma Unreferenced (This);
   begin
      Gtk.Main.Main;
   end Start;

   procedure LS_Level (This : out Instance; Value : Integer)
   is
   begin
      This.LS.Reference.Set_State (Natural (Value));
   end LS_Level;

   function Validate (This : in out Instance; Input : String) return Boolean
   is
   begin
      if This.Builder.Make (Input) then
         This.LS := To_Holder (This.Builder.Get_Product (This.Turtle));

         return True;
      else
         return False;
      end if;
   end Validate;

   function Get_Error (This : Instance) return String
   is
   begin
      return This.Builder.Get_Error;
   end Get_Error;

   procedure Develop (This : out Instance)
   is
   begin
      This.LS.Reference.Develop;
   end Develop;

   procedure Interpret (This : out Instance)
   is
   begin
      This.LS.Reference.Interpret (This.Turtle);
   end Interpret;

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
                     Have_Bg     : Boolean)
   is
      use LSE.IO.Export_Factory;
      use LSE.Model.IO.Drawing_Area.Drawing_Area_Ptr;
      use LSE.Model.IO.Turtle;
      use LSE.Utils.Colors;

      Turtle : LSE.Model.IO.Turtle_Utils.Holder;
      Medium : LSE.Model.IO.Drawing_Area.Drawing_Area_Ptr.Holder;
   begin
      Turtle := To_Holder (Initialize);
      Make (Medium, Format, Path);
      Turtle.Reference.Set_Medium (Medium);

      Turtle.Reference.Set_Width (Positive (Width));
      Turtle.Reference.Set_Height (Positive (Height));
      Turtle.Reference.Set_Margin_Top (Natural (Margin_Top));
      Turtle.Reference.Set_Margin_Right (Natural (Margin_Right));
      Turtle.Reference.Set_Margin_Bottom (Natural (Margin_Bottom));
      Turtle.Reference.Set_Margin_Left (Natural (Margin_Left));

      if Have_Bg then
         Turtle.Reference.Set_Background_Color
           (RGB_To_Hex_String (Float (Bg_Rgba.Red),
            Float (Bg_Rgba.Green),
            Float (Bg_Rgba.Blue)));
      else
         Turtle.Reference.Set_Background_Color ("");
      end if;

      Turtle.Reference.Set_Foreground_Color
        (RGB_To_Hex_String (Float (Fg_Rgba.Red),
         Float (Fg_Rgba.Green),
         Float (Fg_Rgba.Blue)));

      This.LS.Reference.Interpret (Turtle);
   end Export;

   function Get_Width (This : Instance) return Positive
   is
   begin
      return This.Turtle.Element.Get_Width;
   end Get_Width;

   function Get_Height (This : Instance) return Positive
   is
   begin
      return This.Turtle.Element.Get_Height;
   end Get_Height;

   function Get_Background_Color (This : Instance) return String
   is
   begin
      return This.Turtle.Element.Get_Background_Color;
   end Get_Background_Color;

   function Get_Foreground_Color (This : Instance) return String
   is
   begin
      return This.Turtle.Element.Get_Foreground_Color;
   end Get_Foreground_Color;

   function Get_Margin_Top (This : Instance) return Float
   is
   begin
      return This.Turtle.Element.Get_Margin_Top;
   end Get_Margin_Top;

   function Get_Margin_Right (This : Instance) return Float
   is
   begin
      return This.Turtle.Element.Get_Margin_Right;
   end Get_Margin_Right;

   function Get_Margin_Bottom (This : Instance) return Float
   is
   begin
      return This.Turtle.Element.Get_Margin_Bottom;
   end Get_Margin_Bottom;

   function Get_Margin_Left (This : Instance) return Float
   is
   begin
      return This.Turtle.Element.Get_Margin_Left;
   end Get_Margin_Left;

   --  Mutator of Width
   procedure Set_Width (This : out Instance; Value : Gint)
   is
   begin
      This.Turtle.Reference.Set_Width (Positive (Value));
   end Set_Width;

   --  Mutator of Height
   procedure Set_Height (This : out Instance; Value : Gint)
   is
   begin
      This.Turtle.Reference.Set_Height (Positive (Value));
   end Set_Height;

   --  Mutator of background color
   procedure Set_Background_Color (This : out Instance; Value : Gdk_RGBA)
   is
      use LSE.Utils.Colors;
   begin
      This.Turtle.Reference.Set_Background_Color
        (RGB_To_Hex_String (Float (Value.Red),
         Float (Value.Green),
         Float (Value.Blue)));
   end Set_Background_Color;

   procedure Set_Background_Color_Clear (This : out Instance)
   is
   begin
      This.Turtle.Reference.Set_Background_Color ("");
   end Set_Background_Color_Clear;

   --  Mutator of foreground color
   procedure Set_Foreground_Color (This : out Instance; Value : Gdk_RGBA)
   is
      use LSE.Utils.Colors;
   begin
      This.Turtle.Reference.Set_Foreground_Color
        (RGB_To_Hex_String (Float (Value.Red),
         Float (Value.Green),
         Float (Value.Blue)));
   end Set_Foreground_Color;

   --  Mutator of margin top
   procedure Set_Margin_Top (This : out Instance; Value : Gint)
   is
   begin
      This.Turtle.Reference.Set_Margin_Top (Natural (Value));
   end Set_Margin_Top;

   --  Mutator of margin right
   procedure Set_Margin_Right (This : out Instance; Value : Gint)
   is
   begin
      This.Turtle.Reference.Set_Margin_Right (Natural (Value));
   end Set_Margin_Right;

   --  Mutator of margin Bottom
   procedure Set_Margin_Bottom (This : out Instance; Value : Gint)
   is
   begin
      This.Turtle.Reference.Set_Margin_Bottom (Natural (Value));
   end Set_Margin_Bottom;

   --  Mutator of margin left
   procedure Set_Margin_Left (This : out Instance; Value : Gint)
   is
   begin
      This.Turtle.Reference.Set_Margin_Left (Natural (Value));
   end Set_Margin_Left;

   function Get_Extension (This : Instance; Value : String) return String
   is
      pragma Unreferenced (This);
      use LSE.IO.Export_Factory;
   begin
      return Get_Extension (Value);
   end Get_Extension;

   procedure Set_Medium (This  : out Instance;
                         Value : LSE.Model.IO.Drawing_Area.
                           Drawing_Area_Ptr.Holder)
   is
   begin
      This.Turtle.Reference.Set_Medium (Value);
   end Set_Medium;

end LSE.Presenter.Presenter;
