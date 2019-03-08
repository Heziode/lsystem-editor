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

private with Ada.Containers.Hashed_Maps;
with LSE.Utils.Angle;
with LSE.Model.Grammar.Symbol_Utils;
with LSE.Model.L_System.Growth_Rule;

use LSE.Model.Grammar.Symbol_Utils.P_List;
use LSE.Model.Grammar.Symbol_Utils.Ptr;

--  @description
--  This package provides a factory for making a L-System.
--
package LSE.Model.L_System.Factory is

   --  Make an axiom
   --  @param Value String to convert to a list of Symbol
   --  @return Return the list created
   function Make_Axiom (Value : String)
                        return LSE.Model.Grammar.Symbol_Utils.P_List.List;

   --  Make an angle
   --  @param Value String to convert to an angle
   --  @return Return the angle created
   function Make_Angle (Value : String) return LSE.Utils.Angle.Angle;

   --  Make a growth rule
   --  @param Value String to convert to a growth rule
   --  @return Return the growth rule created
   function Make_Rule (Head : Character; Rule : String)
                       return Growth_Rule.Instance;

private
   use Ada.Containers;

   --  Get hash of a character
   --  @param Key Character to get hash
   --  @return Return the hash corresponding to character
   function ID_Hashed (Key : Character) return Hash_Type;

   package Know_Symbol is new Ada.Containers.Hashed_Maps
     (Key_Type => Character,
      Element_Type => Holder,
      Hash => ID_Hashed,
      Equivalent_Keys => "=");

   --  List of static symbol (optimized to prevent RAM overflow)
   Symbol_List : Know_Symbol.Map;

   --  Get the symbol corresponding to the character
   --  @param Key Character used to get corresponding symbol
   --  @return Return a pointer to the corresponding symbol
   function Get_Symbol (Key : Character) return Holder;

   --  Create a list of symbol
   --  @param Value String to convert to symbol list
   --  @return Return the list of symbol created
   function Make_Symbol_List (Value : String)
                             return LSE.Model.Grammar.Symbol_Utils.P_List.List;

end LSE.Model.L_System.Factory;
