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

private with Ada.Text_IO.Editing;

--  @description
--  This package provides methods for colors processing
--
package LSE.Utils.Colors is

   --  Bad string length
   STRING_LENGTH : exception;

   --  Unexpected character encountered during string conversion
   UNEXPECTED_CHARACTER : exception;

   --  Convert Hexa value to Natural
   function Str_Hex_To_Int (Input : String) return Natural;

   --  Convert Hexa value to Float
   function Str_Hex_To_Float (Input : String) return Float;

   --  Convert RGB String (in Hex foramt, ex #AABBCC) to Natural (0 .. 255)
   procedure To_RGB (Input : String; R, G, B : out Natural);

   --  Convert RGB String (in Hex foramt, ex #AABBCC) to Float (0.0 .. 1.0)
   procedure To_RGB (Input : String; R, G, B : out Float);

   --  Convert RGB value to string
   function RGB_To_String (R, G, B : Natural) return String
     with Pre => R <= 255 and
                    G <= 255 and
                      B <= 255;

   --  Convert RGB value to string
   function RGB_To_String (R, G, B : Float) return String
     with Pre => R in 0.0 .. 1.0 and
                    G in 0.0 .. 1.0 and
                      B in 0.0 .. 1.0;

   --  Convert RGB value to string (in Hex format, ex #AABBCC)
   function RGB_To_Hex_String (R, G, B : Natural) return String
     with Pre => R <= 255 and
                    G <= 255 and
                      B <= 255;

   --  Convert RGB value to string (in Hex format, ex #AABBCC)
   function RGB_To_Hex_String (R, G, B : Float) return String
     with Pre => R in 0.0 .. 1.0 and
                    G in 0.0 .. 1.0 and
                      B in 0.0 .. 1.0;

private
   type Fixed_Point is delta 0.001 digits 18;

   package Formatted_IO is
     new Ada.Text_IO.Editing.Decimal_Output (Fixed_Point);
   --  use Formatted_IO;

end LSE.Utils.Colors;
