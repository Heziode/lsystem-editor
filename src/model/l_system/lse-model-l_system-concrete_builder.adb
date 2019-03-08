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
with Ada.Strings;
with Ada.Strings.Fixed;
with GNAT.Regpat;
with LSE.Model.L_System.Error.Invalid_Rule;
with LSE.Model.L_System.Error.Missing_Angle;
with LSE.Model.L_System.Error.Missing_Axiom;
with LSE.Model.L_System.Error.Missing_Restore;
with LSE.Model.L_System.Error.Missing_Rule;
with LSE.Model.L_System.Error.Missing_Save;
with LSE.Model.L_System.Error.Not_A_Angle;
with LSE.Model.L_System.Error.Unexpected_Character;
with LSE.Model.L_System.Factory;

package body LSE.Model.L_System.Concrete_Builder is

   package L   renames Ada.Characters.Latin_1;
   package Pat renames GNAT.Regpat;

   procedure Initialize (This : out Instance)
   is
      Axiom : LSE.Model.Grammar.Symbol_Utils.P_List.List;
      Rules : LSE.Model.L_System.Growth_Rule_Utils.P_List.List;
      Error : Error_Vector.Vector;
   begin
      This := Instance '(Axiom, 0.0, Rules, Error);
   end Initialize;

   function Make (This : out Instance; Item : String) return Boolean
   is
      use Ada.Strings;
      use Ada.Strings.Fixed;

      --------------------------------
      -- Constants, types, packages --
      --------------------------------

      Make_Error : exception;

      ---------------
      -- Variables --
      ---------------

      V             : String_Vector.Vector;
      Current_First : Positive;
      First, Last   : Positive := 1;
      Found         : Boolean;
      Error         : Boolean := False;
      Line          : Positive := 1;
      Input         : Unbounded_String :=
        To_Unbounded_String (Trim (Item, Both));
   begin
      This.Initialize;

      --  If Item is empty, no L-System found
      if Item = "" then
         This.Error.Append (Missing_Angle.Initialize);
         raise Missing_Angle.Error;
      end if;

      --  Check syntax
      Parse (To_String (Input), L.LF & "", V);
      for Str : String of V loop
         Current_First := Str'First;
         First := 1;
         Last := 1;
         loop
            Search_For_Pattern (Regexp_Global,
                                Str (Current_First .. Str'Last),
                                First, Last, Found);
            exit when not Found;
            Error := True;
            This.Error.
              Append (Unexpected_Character.Initialize (
                      Line   => Line,
                      Column => First,
                      Value  => To_Unbounded_String (Str (First .. Last))));
            Current_First := Last + 1;
         end loop;
         Line := Line + 1;
      end loop;

      if Error then
         raise Unexpected_Character.Error;
      end if;

      --  Check Save/Restore
      declare
         Counter  : Integer := 0;
         Line_S,
         Column_S : Positive := 1;
      begin
         Line := 1;
         Check_Save_Restore : for Str : String of V loop
            Current_First := Str'First;
            First := 1;
            Last := 1;

            for C : Character of Str loop
               case C is
               when '[' =>
                  if Counter = 0 then
                     Line_S   := Line;
                     Column_S := Current_First;
                  end if;
                  Counter := Counter + 1;
               when ']' =>
                  Counter := Counter - 1;
                  if Counter < 0 then
                     exit Check_Save_Restore;
                  end if;
               when others =>
                  null;
               end case;
               Current_First := Current_First + 1;
            end loop;
            Line := Line + 1;
         end loop Check_Save_Restore;

         case Counter is
            when Integer'First .. -1 =>
               This.Error.Append (Missing_Restore.Initialize (Line,
                                  Current_First));
               raise Missing_Restore.Error;
            when 1 .. Integer'Last =>
               This.Error.Append (Missing_Save.Initialize (Line_S, Column_S));
               raise Missing_Save.Error;
            when others =>
               null;
         end case;
      end;

      --  Clean String (removing CR,LF and useless space)
      V.Clear;
      Parse (To_String (Input), L.LF & "", V);
      Concat (Input, " ", V);
      V.Clear;
      Parse (To_String (Input), L.CR & "", V);
      Concat (Input, " ", V);
      Trim (Input, Both);

      Current_First := 1;
      loop
         if Element (Input, Current_First) = ' ' then
            if Current_First + 1 <= Length (Input) and then
              Element (Input, Current_First + 1) = ' '
            then
               Delete (Input, Current_First, Current_First + 1);
            end if;
         end if;
         exit when Current_First = Length (Input);
         Current_First := Current_First + 1;
      end loop;
      V.Clear;

      --  Make angle
      if not Make_Angle (This, Input, First, Last) then
         raise Make_Error;
      end if;

      --  Make axiom
      if not Make_Axiom (This, Input, First, Last) then
         raise Make_Error;
      end if;

      --  Make rules
      if not Make_Rule (This, Input, First, Last) then
         raise Make_Error;
      end if;

      return True;
   exception
      when Missing_Angle.Error | Unexpected_Character.Error |
           Missing_Save.Error | Missing_Restore.Error | Make_Error =>
         return False;
   end Make;

   function Get_Product (This   : Instance;
                         Turtle : LSE.Model.IO.Turtle_Utils.Holder)
                         return L_System.Instance
   is
      use LSE.Model.L_System.L_System;

      LS : L_System.Instance;
   begin
      Initialize (LS, This.Axiom, This.Angle, This.Rules, Turtle);
      return LS;
   end Get_Product;

   procedure Get_Product (This   :     Instance;
                          LS     : out L_System.Instance;
                          Turtle :     LSE.Model.IO.Turtle_Utils.Holder)
   is
      use LSE.Model.L_System.L_System;
   begin
      Initialize (LS, This.Axiom, This.Angle, This.Rules, Turtle);
   end Get_Product;

   function Get_Error (This : Instance) return String
   is
      Str : Unbounded_String;
   begin
      for Error : LSE.Model.L_System.Error.Instance'Class of This.Error loop
         Str := Str & Error.Get_Error & L.LF;
      end loop;

      return To_String (Str);
   end Get_Error;

   procedure Parse (Item      :        String;
                    Separator :        String;
                    Vec       : in out String_Vector.Vector)
   is
      use Ada.Strings.Fixed;
      I : constant Integer := Index (Item, Separator);
   begin
      if Item'Length > 0 then
         if I < Item'First then
            Vec.Append (Item);
         else
            Vec.Append (Item (Item'First .. I - 1));
            Parse (To_String (
                   To_Unbounded_String (Item (I + 1 .. Item'Last))),
                   L.LF & "", Vec);
         end if;
      end if;
   end Parse;

   procedure Concat (Item      :    out Unbounded_String;
                     Separator :        String;
                     Vec       : in out String_Vector.Vector)
   is
      Result : Unbounded_String := To_Unbounded_String ("");
   begin
      for Str : String of Vec loop
         Result := Result & Separator & To_Unbounded_String (Str);
      end loop;
      Item := Result;
   end Concat;

   procedure Search_For_Pattern (Expression, Search_In :     String;
                                 First, Last           : out Positive;
                                 Found                 : out Boolean)
   is
      Compiled_Expression : constant Pat.Pattern_Matcher :=
        Pat.Compile (Expression);

      Result : Pat.Match_Array (0 .. 1);
   begin
      Pat.Match (Compiled_Expression, Search_In, Result);
      Found := Pat."="(Result (1), Pat.No_Match);
      if not Found then
         First := Result (1).First;
         Last := Result (1).Last;
      end if;
   end Search_For_Pattern;

   function Get_Next_Token (Item        : out Unbounded_String;
                            Last_Index  : out Positive;
                            First, Last :     Positive)
                            return Boolean
   is
      use Ada.Strings;
   begin
      Delete (Item, First, Last);
      Trim (Item, Left);

      if Item = "" then
         return False;
      end if;

      Last_Index  := Length (Item);
      return True;
   end Get_Next_Token;

   function Make_Angle (This        :    out Instance;
                        Input       :        Unbounded_String;
                        First, Last : in out Positive)
                        return Boolean
   is
      Last_Index : Positive;
      Found      : Boolean;
   begin
      Last_Index  := To_String (Input)'Last;
      Search_For_Pattern (Regexp_Angle,
                          To_String (Input) (1 .. Last_Index),
                          First, Last, Found);

      if not Found then
         This.Error.Append (Missing_Angle.Initialize);
         raise Missing_Angle.Error;
      end if;

      if not Is_Angle (To_String (Input) (First .. Last)) then
         This.Error.Append (Not_A_Angle.Initialize);
         raise Not_A_Angle.Error;
      end if;

      This.Angle := Factory.Make_Angle (To_String (Input) (First .. Last));
      return True;
   exception
      when Missing_Angle.Error | Not_A_Angle.Error =>
         return False;
   end Make_Angle;

   function Make_Axiom (This        :    out Instance;
                        Input       :    out Unbounded_String;
                        First, Last : in out Positive)
                        return Boolean
   is
      Last_Index : Positive;
      Found      : Boolean;
   begin
      if not Get_Next_Token (Input, Last_Index, First, Last) then
         This.Error.Append (Missing_Axiom.Initialize);
         raise Missing_Axiom.Error;
      end if;

      Search_For_Pattern (Regexp_Axiom,
                          To_String (Input) (1 .. Last_Index),
                          First, Last, Found);

      if not Found then
         This.Error.Append (Missing_Axiom.Initialize);
         raise Missing_Axiom.Error;
      end if;

      This.Axiom := Factory.Make_Axiom
        (To_String (Input)
         (1 + First - 1 .. 1 + Last - 1));

      return True;
   exception
      when Missing_Axiom.Error =>
         return False;
   end Make_Axiom;

   function Make_Rule (This        :    out Instance;
                       Input       :    out Unbounded_String;
                       First, Last : in out Positive)
                       return Boolean
   is
      use Ada.Strings;
      use Ada.Strings.Fixed;

      Last_Index : Positive;
      Found      : Boolean := False;
   begin
      loop
         --  If it is the end of Input
         if not Get_Next_Token (Input, Last_Index, First, Last) then
            exit;
         end if;

         Search_For_Pattern (Regexp_Rule, To_String (
                             Input) (1 .. Last_Index),
                             First, Last, Found);
         exit when not Found;

         declare
            Result : String (1 + First - 1 .. 1 + Last - 1);
         begin
            Result := To_String (Input) (1 + First - 1 .. 1 + Last - 1);

            This.Rules.Append (Factory.Make_Rule (Result (Result'First),
                               Trim (Result (Result'First + 1 .. Result'Last),
                                 Both)));
         end;
      end loop;

      if not Found
      then
         if
           LSE.Model.L_System.Growth_Rule_Utils.P_List.Length (This.Rules)
           = 0
         then
            This.Error.Append (Missing_Rule.Initialize);
            raise Missing_Rule.Error;
         else
            declare
               Result_Error : constant String   := To_String (Input);
               First_I      : constant Positive := Result_Error'First;
               Last_I       : constant Positive :=
                 (if First_I + Invalid_Rule.Max_String_Length <=
                    Result_Error'Last then
                       First_I + Invalid_Rule.Max_String_Length else
                          Result_Error'Last);
            begin
               This.Error.Append (Invalid_Rule.Initialize (
                                  Result_Error (First_I .. Last_I)));
            end;
            raise Invalid_Rule.Error;
         end if;
      end if;

      return True;
   exception
      when Missing_Rule.Error | Invalid_Rule.Error =>
         return False;
   end Make_Rule;

end LSE.Model.L_System.Concrete_Builder;
