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

with LSE.Model.IO.Drawing_Area.Drawing_Area_Ptr;

use LSE.Model.IO.Drawing_Area.Drawing_Area_Ptr;

--  @description
--  This package provide an export (drawing area) factory.
--  It is used by GUI to found the good export format.
--
package LSE.IO.Export_Factory is

   --  Error raise when the type of drawing area is unknown
   Unknown_Drawing_Area_Type : exception;

   --  Make the drawing area
   --  @param Value Type of the drawing area
   --  @param Path Path where to save the L-System drawed
   procedure Make (This  : out Holder;
                   Value :     String;
                   Path  :     String);

   --  Get file extension of export type
   function Get_Extension (Value : String) return String;

private

   --  Available drawing areas (export format)
   type Available_Export is (PS);


end LSE.IO.Export_Factory;
