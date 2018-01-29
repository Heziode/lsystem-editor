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

package body LSE.Model.L_System.Growth_Rule is

   procedure Initialize (This : out Instance;
                         H    : Ptr.Holder;
                         B    : LSE.Model.Grammar.Symbol_Utils.P_List.List)
   is
   begin
      This := Instance '(H => H,
                         B => B);
   end Initialize;

   function Get_Head (This : Instance)
                      return LSE.Model.Grammar.Symbol.Instance'Class
   is
   begin
      return This.H.Element;
   end Get_Head;

   function Get_Head (This : Instance)
                      return Ptr.Holder
   is
   begin
      return This.H;
   end Get_Head;

   function Get_Body (This : Instance)
                      return LSE.Model.Grammar.Symbol_Utils.P_List.List
   is
   begin
      return This.B;
   end Get_Body;
end LSE.Model.L_System.Growth_Rule;
