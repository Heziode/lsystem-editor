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

with LSE.Model.Grammar.Symbol.LogoAngleMinus;
with LSE.Model.Grammar.Symbol.LogoAnglePlus;
with LSE.Model.Grammar.Symbol.LogoForward;
with LSE.Model.Grammar.Symbol.LogoForwardTrace;
with LSE.Model.Grammar.Symbol.LogoPositionRestore;
with LSE.Model.Grammar.Symbol.LogoPositionSave;
with LSE.Model.Grammar.Symbol.LogoUTurn;
with LSE.Model.Grammar.Symbol.OtherSymbol;

package body LSE.Model.L_System.Factory is

   function Make_Axiom (Value : String)
                        return LSE.Model.Grammar.Symbol_Utils.P_List.List
   is
   begin
      return Make_Symbol_List (Value);
   end Make_Axiom;

   function Make_Angle (Value : String) return LSE.Utils.Angle.Angle
   is
      Result : constant LSE.Utils.Angle.Angle :=
        LSE.Utils.Angle.Angle'Value (Value);
   begin
      return Result;
   end Make_Angle;

   function Make_Rule (Head : Character; Rule : String)
                       return Growth_Rule.Instance
   is
      Result : Growth_Rule.Instance;
   begin
      Result.Initialize (Get_Symbol (Head), Make_Symbol_List (Rule));
      return Result;
   end Make_Rule;

   function ID_Hashed (Key : Character) return Hash_Type is
   begin
      return Hash_Type'Val (Character'Pos (Key));
   end ID_Hashed;

   function Get_Symbol (Key : Character) return Holder
   is
      ------------------------
      --  Methods prototype --
      ------------------------

      function make_LogoAngleMinus return Holder;

      function make_LogoAnglePlus return Holder;

      function make_LogoForward return Holder;

      function make_LogoForwardTrace return Holder;

      function make_LogoPositionRestore return Holder;

      function make_LogoPositionSave return Holder;

      function make_LogoUTurn return Holder;

      function make_OtherSymbol (Key : Character) return Holder;

      -----------------------------
      --  Declaration of methods --
      -----------------------------

      function make_LogoAngleMinus return Holder
      is
         use LSE.Model.Grammar.Symbol.LogoAngleMinus;
         Value : Instance;
      begin
         Initialize (Value);
         Symbol_List.Insert ('-', To_Holder (Value));
         return Symbol_List.Element ('-');
      end make_LogoAngleMinus;

      function make_LogoAnglePlus return Holder
      is
         use LSE.Model.Grammar.Symbol.LogoAnglePlus;
         Value : Instance;
      begin
         Initialize (Value);
         Symbol_List.Insert ('+', To_Holder (Value));
         return Symbol_List.Element ('+');
      end make_LogoAnglePlus;

      function make_LogoForward return Holder
      is
         use LSE.Model.Grammar.Symbol.LogoForward;
         Value : Instance;
      begin
         Initialize (Value);
         Symbol_List.Insert ('f', To_Holder (Value));
         return Symbol_List.Element ('f');
      end make_LogoForward;

      function make_LogoForwardTrace return Holder
      is
         use LSE.Model.Grammar.Symbol.LogoForwardTrace;
         Value : Instance;
      begin
         Initialize (Value);
         Symbol_List.Insert ('F', To_Holder (Value));
         return Symbol_List.Element ('F');

      end make_LogoForwardTrace;

      function make_LogoPositionRestore return Holder
      is
         use LSE.Model.Grammar.Symbol.LogoPositionRestore;
         Value : Instance;
      begin
         Initialize (Value);
         Symbol_List.Insert (']', To_Holder (Value));
         return Symbol_List.Element (']');

      end make_LogoPositionRestore;

      function make_LogoPositionSave return Holder
      is
         use LSE.Model.Grammar.Symbol.LogoPositionSave;
         Value : Instance;
      begin
         Initialize (Value);
         Symbol_List.Insert ('[', To_Holder (Value));
         return Symbol_List.Element ('[');

      end make_LogoPositionSave;

      function make_LogoUTurn return Holder
      is
         use LSE.Model.Grammar.Symbol.LogoUTurn;
         Value : Instance;
      begin
         Initialize (Value);
         Symbol_List.Insert ('|', To_Holder (Value));
         return Symbol_List.Element ('|');

      end make_LogoUTurn;

      function make_OtherSymbol (Key : Character) return Holder
      is
         use LSE.Model.Grammar.Symbol.OtherSymbol;
         Value : Instance;
      begin
         Initialize (Value, Key);
         Symbol_List.Insert (Key, To_Holder (Value));
         return Symbol_List.Element (Key);

      end make_OtherSymbol;
   begin  -- Get_Symbol

      if Symbol_List.Contains (Key) then
         --  Get element
         return Symbol_List.Element (Key);
      else
         --  Create element
         return (case Key is
                    when '-' =>
                       make_LogoAngleMinus,
                    when '+' =>
                       make_LogoAnglePlus,
                    when 'f' =>
                       make_LogoForward,
                    when 'F' =>
                       make_LogoForwardTrace,
                    when ']' =>
                       make_LogoPositionRestore,
                    when '[' =>
                       make_LogoPositionSave,
                    when '|' =>
                       make_LogoUTurn,
                    when others =>
                       make_OtherSymbol (Key)
                );
      end if;
   end Get_Symbol;

   function Make_Symbol_List (Value : String)
                              return LSE.Model.Grammar.Symbol_Utils.P_List.List
   is
      Result : LSE.Model.Grammar.Symbol_Utils.P_List.List;
   begin
      for C : Character of Value loop
         Result.Append (Get_Symbol (C));
      end loop;
      return Result;
   end Make_Symbol_List;

end LSE.Model.L_System.Factory;
