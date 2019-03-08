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

with Ada.Characters.Handling;
with Ada.Characters.Latin_1;
with Ada.Integer_Text_IO;
with Ada.Strings;
with Ada.Strings.Fixed;

package body LSE.Utils.Colors is

   package L renames Ada.Characters.Latin_1;

   function Str_Hex_To_Int (Input : String) return Natural
   is
      use Ada.Characters.Handling;

      Str_Length : Natural := Input'Length;
      Result : Natural := 0;
   begin

      for C : Character of Input loop
         case To_Upper (C) is
            when 'F' =>
               Result := Result + 15**Str_Length;
            when 'E' =>
               Result := Result + 14**Str_Length;
            when 'D' =>
               Result := Result + 13**Str_Length;
            when 'C' =>
               Result := Result + 12**Str_Length;
            when 'B' =>
               Result := Result + 11**Str_Length;
            when 'A' =>
               Result := Result + 10**Str_Length;
            when '0' .. '9' =>
               Result := Result + Natural'Value (C & "");
            when others =>
               raise UNEXPECTED_CHARACTER;
         end case;
         Str_Length := Str_Length - 1;
      end loop;

      return Result;
   end Str_Hex_To_Int;

   function Str_Hex_To_Float (Input : String) return Float
   is
   begin
      return Float (Str_Hex_To_Int (Input)) / 255.0;
   end Str_Hex_To_Float;

   procedure To_RGB (Input : String; R, G, B : out Natural)
   is
      use Ada.Text_IO;

      Index : Positive := Input'First;
   begin
      if Input'Length not in 6 .. 7 then
         raise STRING_LENGTH;
      end if;

      if Input (Index) = '#' then
         Index := Index + 1;
      end if;

      R := Str_Hex_To_Int (Input (Index .. Index + 1));
      Index := Index + 2;
      G := Str_Hex_To_Int (Input (Index .. Index + 1));
      Index := Index + 2;
      B := Str_Hex_To_Int (Input (Index .. Index + 1));
   exception
      when STRING_LENGTH =>
         Put_Line ("Error: bad length of string for RGB conversion");
      when UNEXPECTED_CHARACTER =>
         Put_Line ("Error: Unexpected character encountered");
      when others =>
         Put_Line ("Error: cannot convert string to RGB");
   end To_RGB;

   procedure To_RGB (Input : String; R, G, B : out Float)
   is
      use Ada.Text_IO;

      Index : Positive := Input'First;
   begin
      if Input'Length not in 6 .. 7 then
         raise STRING_LENGTH;
      end if;

      if Input (Index) = '#' then
         Index := Index + 1;
      end if;

      R := Str_Hex_To_Float (Input (Index .. Index + 1));
      Index := Index + 2;
      G := Str_Hex_To_Float (Input (Index .. Index + 1));
      Index := Index + 2;
      B := Str_Hex_To_Float (Input (Index .. Index + 1));
   exception
      when STRING_LENGTH =>
         Put_Line ("Error: bad length of string for RGB conversion");
      when UNEXPECTED_CHARACTER =>
         Put_Line ("Error: Unexpected character encountered");
      when others =>
         Put_Line ("Error: cannot convert string to RGB");
   end To_RGB;

   --  Convert RGB value to string
   function RGB_To_String (R, G, B : Natural) return String
   is
      use Ada.Strings;
      use Ada.Strings.Fixed;
   begin
      return Trim (Natural'Image (R), Left) & L.Space &
        Natural'Image (G) & L.Space & Natural'Image (B);
   end RGB_To_String;

   --  Convert RGB value to string
   function RGB_To_String (R, G, B : Float) return String
   is
      use Ada.Strings;
      use Ada.Strings.Fixed;
   begin
      return Trim (Fixed_Point'Image (Fixed_Point (R)), Left) &
        L.Space & Fixed_Point'Image (Fixed_Point (G)) &
        L.Space & Fixed_Point'Image (Fixed_Point (B));
   end RGB_To_String;

   function RGB_To_Hex_String (R, G, B : Natural) return String
   is
      Rs : String (1 .. 6);
      Gs : String (1 .. 6);
      Bs : String (1 .. 6);
   begin
      Ada.Integer_Text_IO.Put (Rs, R, 16);
      Ada.Integer_Text_IO.Put (Gs, G, 16);
      Ada.Integer_Text_IO.Put (Bs, B, 16);

      return "#" & (if Rs (4 .. 4) = "#" then "0" else Rs (4 .. 4)) &
      (if Rs (4 .. 4) = "#" then Rs (5 .. 5) else Rs (4 .. 4)) &
      (if Gs (4 .. 4) = "#" then "0" else Gs (4 .. 4)) &
      (if Gs (4 .. 4) = "#" then Gs (5 .. 5) else Gs (4 .. 4)) &
      (if Bs (4 .. 4) = "#" then "0" else Bs (4 .. 4)) &
      (if Bs (4 .. 4) = "#" then Bs (5 .. 5) else Bs (4 .. 4));
   end RGB_To_Hex_String;

   function RGB_To_Hex_String (R, G, B : Float) return String
   is
   begin
      return RGB_To_Hex_String (Natural (R * 255.0),
                                Natural (G * 255.0),
                                Natural (B * 255.0));
   end RGB_To_Hex_String;

end LSE.Utils.Colors;
