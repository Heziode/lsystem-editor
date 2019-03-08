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
with Ada.Text_IO;
with LSE.Model.IO.Turtle_Utils;
with LSE.Model.L_System.Concrete_Builder;
with LSE.Model.L_System.L_System;

use Ada.Strings.Unbounded;
use Ada.Text_IO;
use LSE.Model.L_System.L_System;

--  @description
--  This package provide a set of methods to read and write in text file.
--
package LSE.Model.IO.Text_File is

   --  Open a file. By default, if file does not exist, it create it.
   --  @param Mode Mode to open the file
   --  @param Path Location of the file
   --  @param Auto True if auto create the file if it does not exist,
   --    false otherwise
   procedure Open_File (File : in out File_Type;
                        Mode :        File_Mode;
                        Path :        String;
                        Auto :        Boolean := True);

   --  Close a file.
   procedure Close_File (File : in out File_Type);

   --  Read the entire content of the file en place it into a variable
   --  @param Result Variable that will contains the result
   procedure Read (File   :        File_Type;
                   Result :    out Unbounded_String);

   --  Write the the content of a string in a file
   --  @param Item Element to write
   procedure Write (File : in out File_Type;
                    Item :        String);

   --  Get L-System from a file.
   --  @param Builder L-System builder
   --  @param Turtle Reference of the turtle
   --  @param LS Reference of the L-System
   --  @return Return True if a L-System has been read from the file
   function Read_LSystem (File    :        File_Type;
                          Builder : in out
                            LSE.Model.L_System.Concrete_Builder.Instance;
                          Turtle  : LSE.Model.IO.Turtle_Utils.Holder;
                          LS      : in out
                            LSE.Model.L_System.L_System.Instance)
                          return Boolean;

   --  Write L-System in a file.
   --  @param LS L-System to write
   procedure Write_LSystem (File : in out File_Type;
                            LS   : LSE.Model.L_System.L_System.Instance);

   --  Save text data into a file
   --  @param File_Path Path to the file
   --  @param Content Content to save into the file
   procedure Save_To_File (File_Path : String; Content : String);

   --  Read text data from file
   --  @param File_Path Path to the file
   --  @return The content of the file
   function Read_From_File (File_Path : String) return String;

end LSE.Model.IO.Text_File;
