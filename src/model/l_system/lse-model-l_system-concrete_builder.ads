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

private with Ada.Containers.Indefinite_Vectors;
private with Ada.Strings.Unbounded;
private with LSE.Model.Grammar.Symbol_Utils;
with LSE.Model.IO.Turtle_Utils;
private with LSE.Model.L_System.Error;
private with LSE.Model.L_System.Growth_Rule_Utils;
with LSE.Model.L_System.L_System;
private with LSE.Utils.Angle;

--  @description
--  This package provide a set of methods to build a L-System.
--
package LSE.Model.L_System.Concrete_Builder is

   --  Builder of L-System
   type Instance is tagged private;

   --  Constructor
   procedure Initialize (This : out Instance);

   --  Build product
   --  @return Return True if no error was encountered, False otherwise
   function Make (This : out Instance; Item : String) return Boolean;

   --  Get the finished product
   --  @param Turtle Reference to the turtle
   --  @return Return the finished product
   function Get_Product (This   : Instance;
                         Turtle : LSE.Model.IO.Turtle_Utils.Holder)
                         return L_System.Instance;

   --  Get the finished product
   --  @param Turtle Reference to the turtle
   --  @param LS Reference to the L-System
   --  @return Return the finished product
   procedure Get_Product (This   :     Instance;
                          LS     : out L_System.Instance;
                          Turtle :     LSE.Model.IO.Turtle_Utils.Holder);

   --  Get encountered errors
   --  @return Return all errors messages
   function Get_Error (This : Instance) return String;

private
   use Ada.Containers;
   use Ada.Strings.Unbounded;
   use LSE.Model.L_System.Error;
   use LSE.Utils.Angle;

   package Error_Vector is new
     Indefinite_Vectors (Natural, Error.Instance'Class);
   use Error_Vector;

   package String_Vector is new Indefinite_Vectors (Natural, String);
   use String_Vector;

   --  Regex to find unexpected characters
   Regexp_Global : constant String := "([^\s0-9a-zA-Z-+|\[\].]+)";
   --  Regex to get angle
   Regexp_Angle  : constant String := "^([0-9]+([.][0-9]*)?|[.][0-9]+)";
   --  Regex to get axiom
   Regexp_Axiom  : constant String := "^([a-zA-Z-+|\[\]]+)";
   --  Regex to get rule
   Regexp_Rule   : constant String := "^([a-zA-Z-+|\[\]]\s+[a-zA-Z-+|\[\]]+)";

   type Instance is tagged record
      --  Axiom
      Axiom : LSE.Model.Grammar.Symbol_Utils.P_List.List;
      --  Angle
      Angle : LSE.Utils.Angle.Angle;
      --  Growth rules
      Rules : LSE.Model.L_System.Growth_Rule_Utils.P_List.List;
      --  Contains all errors found
      Error : Error_Vector.Vector;
   end record;

   --  Split string into several other.
   --  @param Item String to split
   --  @param Separator Separator used to split
   --  @param Vec List that will contains all splitted strings
   procedure Parse (Item      :        String;
                    Separator :        String;
                    Vec       : in out String_Vector.Vector);

   --  Concatenate a list of string into one.
   --  @param Item Unified string
   --  @param Separator Separator used to concatenate
   --  @param Vec List that contains all strings to concatenate
   procedure Concat (Item      :    out Unbounded_String;
                     Separator :        String;
                     Vec       : in out String_Vector.Vector);

   --  Check if a pattern is matched in string.
   --  @param Expression Regex
   --  @param Search_In String to check
   --  @param First Location of first character matched
   --  @param Last Location of last character matched
   --  @param Found True if Expression found in Search_In, False otherwise
   procedure Search_For_Pattern (Expression, Search_In :     String;
                                 First, Last           : out Positive;
                                 Found                 : out Boolean);

   --  Get next usable character (skip blanks char, etc.).
   --  @param Item String where working
   --  @param Last_Index Last index of the string
   --  @param First Location of first character matched to the last expression
   --  @param Last Location of last character matched to the last expression
   --  @return Return True if next sequence available, False otherwise
   function Get_Next_Token (Item        : out Unbounded_String;
                            Last_Index  : out Positive;
                            First, Last :     Positive)
                            return Boolean;

   --  Make angle.
   --  @param This Reference of the instance
   --  @param Input String where working
   --  @param First Location of first character matched
   --  @param Last Location of last character matched
   --  @return Return True if angle are get, False otherwise
   function Make_Angle (This        :    out Instance;
                        Input       :        Unbounded_String;
                        First, Last : in out Positive)
                        return Boolean;

   --  Make axiom.
   --  @param This Reference of the instance
   --  @param Input String where working
   --  @param First Location of first character matched
   --  @param Last Location of last character matched
   --  @return Return True if axiom are get, False otherwise
   function Make_Axiom (This        :    out Instance;
                        Input       :    out Unbounded_String;
                        First, Last : in out Positive)
                        return Boolean;

   --  Make rule (at least one rule are required).
   --  @param This Reference of the instance
   --  @param Input String where working
   --  @param First Location of first character matched
   --  @param Last Location of last character matched
   --  @return Return True if rule(s) are get, False otherwise
   function Make_Rule (This        :    out Instance;
                       Input       :    out Unbounded_String;
                       First, Last : in out Positive)
                       return Boolean;

end LSE.Model.L_System.Concrete_Builder;
