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

--  @description
--  This package provide a abstract error system.
--
package LSE.Model.L_System.Error is

   type Instance is abstract tagged private;

   package Error_Type is
      --  Type of errors supported for the L-System
      type Instance is (
                        --  Input contains unexpected character
                        Unexpected_Character,
                        --  Angle not found
                        Missing_Angle,
                        --  Value for angle is not in range
                        Not_A_Angle,
                        --  Axiom not found
                        Missing_Axiom,
                        --  Rule not found
                        Missing_Rule,
                        --  Ununderstandable rule found
                        Invalid_Rule,
                        --  Save character as missing
                        Missing_Save,
                        --  Restore character as missing
                        Missing_Restore
                       );
   end Error_Type;

   --  Getting the error
   function Get_Error (This : Instance) return String is abstract;

   --  Getting the type of this error
   function Get_Error_Type (This : Instance) return Error_Type.Instance;

private

   type Instance is abstract tagged record
      --  Type of this error
      Error : Error_Type.Instance;
   end record;
end LSE.Model.L_System.Error;
