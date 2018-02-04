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
with GNAT.Command_Line;
with GNAT.Strings;
with LSE.Model.IO.Text_File;
with LSE.Model.IO.Turtle;
with LSE.Model.IO.Turtle_Factory;
with LSE.Model.IO.Turtle_Utils;
with LSE.Model.L_System.L_System;
with LSE.Model.L_System.Concrete_Builder;

use Ada.Text_IO;
use GNAT.Command_Line;
use GNAT.Strings;
use LSE.Model.IO.Text_File;
use LSE.Model.IO.Turtle;
use LSE.Model.IO.Turtle_Factory;
use LSE.Model.IO.Turtle_Utils;
use LSE.Model.L_System.L_System;
use LSE.Model.L_System.Concrete_Builder;

--  @description
--  Entry point of the app
--
procedure Main is
   No_Input_File       : exception;
   No_Export_File      : exception;
   No_Export           : exception;
   No_Develop          : exception;
   LS_Creation         : exception;
   Not_Implemented_Yet : exception;

   Config           : Command_Line_Configuration;
   GUI              : aliased Boolean := False;
   Input_File       : aliased String_Access;
   Output_File      : aliased String_Access;
   Export           : aliased String_Access;
   Export_File      : aliased String_Access;
   Develop          : aliased Integer := 0;
   Width            : aliased Integer := 0;
   Height           : aliased Integer := 0;
   Background_Color : aliased String_Access;
   Foreground_Color : aliased String_Access;
   Margin_Top       : aliased Integer := 0;
   Margin_Right     : aliased Integer := 0;
   Margin_Bottom    : aliased Integer := 0;
   Margin_Left      : aliased Integer := 0;


   T : LSE.Model.IO.Turtle_Utils.Holder;
   B : LSE.Model.L_System.Concrete_Builder.Instance;
   L : LSE.Model.L_System.L_System.Instance;
   F : File_Type;
begin
   Define_Switch (Config, GUI'Access,
                  Long_Switch => "--gui",
                  Help => "True for no-gui, False otherwise [default False]");

   Define_Switch (Config, Input_File'Access, "-i:",
                  Long_Switch => "--input=",
                  Help => "Input file that contains a L-System");

   Define_Switch (Config, Output_File'Access, "-o:",
                  Long_Switch => "--output=",
                  Help => "Output file that will contains a L-System");

   Define_Switch (Config, Export'Access, "-e:",
                  Long_Switch => "--export=",
                  Help => "Export format");

   Define_Switch (Config, Export_File'Access, "-p:",
                  Long_Switch => "--export-file=",
                  Help => "Output file that will contains the " &
                    "representation of the L-System");

   Define_Switch (Config, Develop'Access, "-d:",
                  Long_Switch => "--develop=",
                  Help => "number of step to develop");

   Define_Switch (Config, Width'Access, "-w:",
                  Long_Switch => "--width=",
                  Help => "Width of the output representation " &
                    "(must be greater than 0 else is ignored)");

   Define_Switch (Config, Height'Access, "-h:",
                  Long_Switch => "--height=",
                  Help => "Height of the output representation " &
                    "(must be greater than 0 else is ignored)");

   Define_Switch (Config, Background_Color'Access, "-b:",
                  Long_Switch => "--background-color=",
                  Help => "background color of the output representation " &
                    "(in Hex, like #AABBCC or AABBCC)");

   Define_Switch (Config, Foreground_Color'Access, "-f:",
                  Long_Switch => "--foreground-color=",
                  Help => "foreground color of the output representation " &
                    "(in Hex, like #AABBCC or AABBCC)");

   Define_Switch (Config, Margin_Top'Access, "-mt:",
                  Long_Switch => "--margin-top=",
                  Help => "Enable margin top of the output representation " &
                    "(must be greater than 0 else is ignored)");

   Define_Switch (Config, Margin_Right'Access, "-mr:",
                  Long_Switch => "--margin-right=",
                  Help => "Enable margin right of the output representation " &
                    "(must be greater than 0 else is ignored)");

   Define_Switch (Config, Margin_Bottom'Access, "-mb:",
                  Long_Switch => "--margin-bottom=",
                  Help => "Enable margin bottom of the output representation" &
                    " (must be greater than 0 else is ignored)");

   Define_Switch (Config, Margin_Left'Access, "-ml:",
                  Long_Switch => "--margin-left=",
                  Help => "Enable margin left of the output representation " &
                    "(must be greater than 0 else is ignored)");

   Getopt (Config);

   if not GUI then
      if Input_File.all = "" then
         raise No_Input_File;
      elsif Export.all = "" then
         raise No_Export;
      elsif Export_File.all = "" then
         raise No_Export_File;
      elsif Develop < 0 then
         raise No_Develop;
      end if;

      Make (T, Export.all, Export_File.all);

      if Width > 0 then
         T.Reference.Set_Width (Width);
      end if;

      if Height > 0 then
         T.Reference.Set_Height (Height);
      end if;

      if Background_Color.all /= "" then
         T.Reference.Set_Background_Color (Background_Color.all);
      end if;

      if Foreground_Color.all /= "" then
         T.Reference.Set_Foreground_Color (Foreground_Color.all);
      end if;

      if Margin_Top > 0 then
         T.Reference.Set_Margin_Top (Natural (Margin_Top));
      end if;

      if Margin_Right > 0 then
         T.Reference.Set_Margin_Right (Margin_Right);
      end if;

      if Margin_Bottom > 0 then
         T.Reference.Set_Margin_Bottom (Margin_Bottom);
      end if;

      if Margin_Left > 0 then
         T.Reference.Set_Margin_Left (Margin_Left);
      end if;

      Initialize (B);

      Open_File (F, In_File, Input_File.all, False);
      if Read_LSystem (F, B, L) then
         Put_Line ("L-System:");
         Put_Line (L.Get_LSystem);

         L.Set_State (Develop);
         L.Develop;
         L.Interpret (T);

         Close_File (F);

         Put_Line ("L-System rendered for level" & Integer'Image (Develop));
      else
         Put_Line ("L-System creation error:");
         Put_Line (B.Get_Error);
         Close_File (F);
         raise LS_Creation;
      end if;
   else
      --  GUI Mode
      raise Not_Implemented_Yet;
   end if;

exception
   when GNAT.Command_Line.Exit_From_Command_Line =>
      return;
end Main;
