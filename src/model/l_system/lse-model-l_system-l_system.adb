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
with Ada.Float_Text_IO;
with Ada.Strings;
with Ada.Strings.Unbounded;
with LSE.Model.Grammar.Symbol;

package body LSE.Model.L_System.L_System is

   package L renames Ada.Characters.Latin_1;

   procedure Initialize (This  : out Instance;
                         Axiom : LSE.Model.Grammar.Symbol_Utils.P_List.List;
                         Angle : LSE.Angle.Angle;
                         Rules :
                         LSE.Model.L_System.Growth_Rule_Utils.P_List.List)
   is
   begin
      This := Instance '(Current_State => 0,
                         Axiom         => Axiom,
                         Angle         => Angle,
                         Rules         => Rules,
                         Current_Value => Axiom);
   end Initialize;

   function Get_Current_State (This : Instance) return Natural
   is
   begin
      return This.Current_State;
   end Get_Current_State;

   procedure Set_Current_State (This : out Instance; Value : Natural)
   is
   begin
      This.Current_State := Value;
   end Set_Current_State;

   function Get_LSystem (This : Instance) return String
   is
      use Ada.Strings;
      use Ada.Float_Text_IO;
      use Ada.Strings.Unbounded;
      use LSE.Model.Grammar.Symbol;

      ------------------------
      --  Methods prototype --
      ------------------------

      function Get_Symbol_List (This :
                                LSE.Model.Grammar.Symbol_Utils.P_List.List)
                                return Unbounded_String;

      function Get_Rules (This :
                          LSE.Model.L_System.Growth_Rule_Utils.P_List.List)
                          return Unbounded_String;

      -----------------------------
      --  Declaration of methods --
      -----------------------------

      function Get_Symbol_List (This :
                                LSE.Model.Grammar.Symbol_Utils.P_List.List)
                                return Unbounded_String
      is
         Result : Unbounded_String := To_Unbounded_String ("");
      begin
         for S of This loop
            Result := Result &
              To_Unbounded_String (Get_Representation (
                                     LSE.Model.Grammar.Symbol_Utils
                                     .Ptr.Element (S)) & "");
         end loop;
         return Result;
      end Get_Symbol_List;

      function Get_Rules (This :
                          LSE.Model.L_System.Growth_Rule_Utils.P_List.List)
                          return Unbounded_String
      is
         Result : Unbounded_String := To_Unbounded_String ("");
      begin
         for Rule of This loop
            Result := Result & L.LF &
              To_Unbounded_String (Get_Representation (Rule.Get_Head) & "") &
              L.Space & Get_Symbol_List (Rule.Get_Body);
         end loop;
         return Result;
      end Get_Rules;

      ---------------
      -- Variables --
      ---------------

      Angle_Str : String (1 .. Angle.Angle'Digits);
   begin
      Put (To   => Angle_Str,
           Item => Float (This.Angle),
           Aft  => 2,
           Exp  => 0);

      return To_String (Trim (To_Unbounded_String (Angle_Str), Both) & L.LF &
                        Get_Symbol_List (This.Axiom) & Get_Rules (This.Rules));
   end Get_LSystem;

   procedure Develop (This : out Instance)
   is
   begin
      --  TODO
      null;
   end Develop;

end LSE.Model.L_System.L_System;
