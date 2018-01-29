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

with LSE.Angle;
with LSE.Model.L_System.Growth_Rule_Utils;
with LSE.Model.Grammar.Symbol_Utils;

--  @description
--  This package represent a L-System.
--
package LSE.Model.L_System.L_System is

   --  Representing a L-System
   type Instance is tagged private;

   --  Constructor
   procedure Initialize (This  : out Instance;
                         Axiom : LSE.Model.Grammar.Symbol_Utils.P_List.List;
                         Angle : LSE.Angle.Angle;
                         Rules :
                         LSE.Model.L_System.Growth_Rule_Utils.P_List.List);

   --  Accessor of current state
   function Get_Current_State (This : Instance) return Natural;

   --  Mutator of current state
   procedure Set_Current_State (This : out Instance; Value : Natural);

   --  Getting a string representation of this L-System
   function Get_LSystem (This : Instance) return String;

   --  Develop this L-System to the next step
   procedure Develop (This : out Instance);

private

   type Instance is tagged record
      --  Currently developed state
      Current_State : Natural := 0;
      --  Axiom
      Axiom         : LSE.Model.Grammar.Symbol_Utils.P_List.List;
      --  Angle
      Angle         : LSE.Angle.Angle;
      --  Growth rules
      Rules         : LSE.Model.L_System.Growth_Rule_Utils.P_List.List;
      --  List of symbol of the currently developed state
      Current_Value : LSE.Model.Grammar.Symbol_Utils.P_List.List;
   end record;

end LSE.Model.L_System.L_System;
