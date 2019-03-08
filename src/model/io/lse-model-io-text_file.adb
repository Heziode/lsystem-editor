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

with Ada.Characters.Latin_1;
with Ada.Directories;

package body LSE.Model.IO.Text_File is

   package L renames Ada.Characters.Latin_1;

   procedure Open_File (File : in out File_Type;
                        Mode :        File_Mode;
                        Path :        String;
                        Auto :        Boolean := True)
   is
      use Ada.Directories;

      --  The parameter "wcem=8" indicates that we want to open a file in
      --  UTF-8 format (which can therefore contain accents)
      FILE_FORM : constant String := "wcem=8";
   begin
      if Exists (Path) then
         Open (File, Mode, Path, FILE_FORM);
      else
         if not Auto then
            raise Ada.Directories.Name_Error;
         end if;
         Create (File, Mode, Path, FILE_FORM);
      end if;
   end Open_File;

   procedure Close_File (File : in out File_Type)
   is
   begin
      Close (File);
   end Close_File;

   procedure Read (File   :        File_Type;
                   Result :    out Unbounded_String)
   is
   begin
      Result := To_Unbounded_String ("");
      while not End_Of_File (File) loop
         if End_Of_Line (File) then
            Skip_Line (File);
         elsif End_Of_Page (File) then
            Skip_Page (File);
         else
            Result := Result & To_Unbounded_String (Get_Line (File)) & L.LF;
         end if;
      end loop;
   end Read;

   procedure Write (File : in out File_Type;
                    Item :        String)
   is
   begin
      Put_Line (File, Item);
   end Write;

   function Read_LSystem (File    :        File_Type;
                          Builder : in out
                            LSE.Model.L_System.Concrete_Builder.Instance;
                          Turtle  : LSE.Model.IO.Turtle_Utils.Holder;
                          LS      : in out
                            LSE.Model.L_System.L_System.Instance)
                       return Boolean
   is
      use LSE.Model.L_System.Concrete_Builder;

      Item : Unbounded_String;
   begin
      Read (File, Item);
      if Builder.Make (To_String (Item)) then
         LS := Builder.Get_Product (Turtle);
         return True;
      else
         return False;
      end if;
   end Read_LSystem;

   procedure Write_LSystem (File : in out File_Type;
                            LS   : LSE.Model.L_System.L_System.Instance)
   is
   begin
      Put_Line (File, LS.Get_LSystem);
   end Write_LSystem;

   procedure Save_To_File (File_Path : String; Content : String)
   is
      File : File_Type;
   begin
      Open_File (File, Out_File, File_Path);
      Write (File, Content);
      Close_File (File);
   end Save_To_File;

   function Read_From_File (File_Path : String) return String
   is
      File : File_Type;
      Item : Unbounded_String;
   begin
      Open_File (File, In_File, File_Path);
      Read (File, Item);
      Close_File (File);
      return To_String (Item);
   end Read_From_File;

end LSE.Model.IO.Text_File;
