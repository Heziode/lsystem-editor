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

with LSE.Model.IO.Drawing_Area.PostScript;

package body LSE.IO.Export_Factory is

   procedure Make (This  : out
                     LSE.Model.IO.Drawing_Area.Drawing_Area_Ptr.Holder;
                   Value : String;
                   Path  : String)
   is

      Found : Boolean := False;
   begin
      case Available_Export'Value (Value) is
         when PS =>
            Found := True;
            This := To_Holder
              (LSE.Model.IO.Drawing_Area.PostScript.Initialize (Path));
      end case;

      if not Found then
         raise Unknown_Drawing_Area_Type;
      end if;
   end Make;

   function Get_Extension (Value : String) return String
   is
   begin
      case Available_Export'Value (Value) is
         when PS =>
            return ".ps";
      end case;

      --  raise Unknown_Drawing_Area_Type;
   end Get_Extension;

end LSE.IO.Export_Factory;
