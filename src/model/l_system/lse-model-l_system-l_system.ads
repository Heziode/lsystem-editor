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

private with Ada.Strings.Unbounded;
with LSE.Utils.Angle;
with LSE.Model.Interpreter;
with LSE.Model.L_System.Growth_Rule_Utils;
with LSE.Model.Grammar.Symbol_Utils;
with LSE.Model.IO.Turtle_Utils;

use LSE.Model.IO.Turtle_Utils;

--  @description
--  This package represent a L-System.
--
package LSE.Model.L_System.L_System is

   --  Representing a L-System
   type Instance is new Interpreter.Instance with private;

   --  Constructor
   procedure Initialize (This   : out Instance;
                         Axiom  : LSE.Model.Grammar.Symbol_Utils.P_List.List;
                         Angle  : LSE.Utils.Angle.Angle;
                         Rules  :
                         LSE.Model.L_System.Growth_Rule_Utils.P_List.List;
                         Turtle : LSE.Model.IO.Turtle_Utils.Holder);

   --  Accessor of current state
   function Get_State (This : Instance) return Natural;

   --  Mutator of current state
   procedure Set_State (This : out Instance; Value : Natural);

   --  Getting a string representation of this L-System
   function Get_LSystem (This : Instance) return String;

   --  Getting a string representation of the current development
   function Get_Value (This : Instance) return String;

   --  Getting the current development
   function Get_Value (This : Instance)
                       return LSE.Model.Grammar.Symbol_Utils.P_List.List;

   --  Accessor of Turtle
   function Get_Turtle (This : Instance)
                        return LSE.Model.IO.Turtle_Utils.Holder;

   --  Mutator of Turtle
   procedure Set_Turtle (This  : out Instance;
                         Value : LSE.Model.IO.Turtle_Utils.Holder);

   --  Develop this L-System to the next step
   procedure Develop (This : out Instance);

   overriding
   procedure Interpret (This : in out Instance;
                        T    : in out Holder);

private
   use Ada.Strings.Unbounded;

   type Instance is new Interpreter.Instance with record
      --  State to develop
      State         : Natural := 0;
      --  Currently developed state
      Current_State : Natural := 0;
      --  Axiom
      Axiom         : LSE.Model.Grammar.Symbol_Utils.P_List.List;
      --  Angle
      Angle         : LSE.Utils.Angle.Angle;
      --  Growth rules
      Rules         : LSE.Model.L_System.Growth_Rule_Utils.P_List.List;
      --  List of symbol of the currently developed state
      Current_Value : LSE.Model.Grammar.Symbol_Utils.P_List.List;
      --  "Empty" turtle to compute L-System dimensions during develop
      Turtle        : LSE.Model.IO.Turtle_Utils.Holder;
   end record;

   --  Getting the string representation of a symbol list
   function Get_Symbol_List (This :
                                LSE.Model.Grammar.Symbol_Utils.P_List.List)
                             return Unbounded_String;

   --  Compute dimension of the L-System
   not overriding procedure Compute_Dimension (This : in out Instance);

end LSE.Model.L_System.L_System;
