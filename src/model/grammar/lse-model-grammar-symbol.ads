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

with LSE.Model.Interpreter;

--  @summary
--  Represent an abstract Symbol.
--
--  @description
--  This package represent an abstract Symbol. It allow to interpret the symbol
--  for drawing on medium or get a representation of this.
--
package LSE.Model.Grammar.Symbol is

   --  Representation of symbole
   type Instance is abstract
   new LSE.Model.Interpreter.Instance with private;

   --  Constructor
   procedure Initialize (This : out Instance)
   is abstract;

   --  Getting representation of this symbol
   function Get_Representation (This : Instance) return Character;

   --  Put this symbol in STDIO
   procedure Put (This : Instance);

private
   type Instance is abstract new LSE.Model.Interpreter.Instance with record
      --  Representation of the Symbol
      Representation : Character;
   end record;
end LSE.Model.Grammar.Symbol;
