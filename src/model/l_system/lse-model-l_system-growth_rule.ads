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

with LSE.Model.Grammar.Symbol;
with LSE.Model.Grammar.Symbol_Utils;

use LSE.Model.Grammar.Symbol_Utils;

--  @description
--  This package define a L-System growth rule.
--
package LSE.Model.L_System.Growth_Rule is

   --  Growth rule
   type Instance is tagged private;

   --  Constructor
   --  @param H Head of the growth rule
   --  @param B Body of the growth rule
   procedure Initialize (This : out Instance;
                         H    : Ptr.Holder;
                         B    : LSE.Model.Grammar.Symbol_Utils.P_List.List);

   --  Getting the head of this growth rule
   --  @return Return the symbol that compose the head
   function Get_Head (This : Instance)
                      return LSE.Model.Grammar.Symbol.Instance'Class;

   --  Getting the head of this growth rule
   --  @return Return the symbol that compose the head
   function Get_Head (This : Instance)
                      return Ptr.Holder;

   --  Getting the body of this growth rule
   --  @return Return the symbol list that compose the body
   function Get_Body (This : Instance)
                      return LSE.Model.Grammar.Symbol_Utils.P_List.List;

private

   type Instance is tagged record
      --  Head
      H : Ptr.Holder;
      --  Body
      B : LSE.Model.Grammar.Symbol_Utils.P_List.List;
   end record;
end LSE.Model.L_System.Growth_Rule;
