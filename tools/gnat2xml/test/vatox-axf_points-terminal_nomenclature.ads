------------------------------------------------------------------------------
--                                                                          --
--                           AVATOX COMPONENTS                              --
--                                                                          --
--                        VATOX (Via Asis To Xml)                           --st
--                                                                          --
--                                                                          --
--                Copyright (c) 2006, McKae Technologies.                   --
--                                                                          --
-- Avatox is free software; you can redistribute it and/or modify it        --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software Foundation;  either version 2,  or  (at your option)  any later --
-- version. Avatox is distributed in the hope  that it will be useful,      --
-- but WITHOUT ANY WARRANTY; without even the implied warranty of MER-      --
-- CHANTABILITY or  FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General  --
-- Public License for more details. You should have received a copy of the  --
-- GNU General Public License distributed with GNAT; see file COPYING. If   --
-- not, write to the Free Software Foundation, 59 Temple Place Suite 330,   --
-- Boston, MA 02111-1307, USA.                                              --
--                                                                          --
-- Avatox is now maintained by McKae Technologies (http://www.mckae.com)    --                                               --
------------------------------------------------------------------------------

package Vatox.Axf_Points.Terminal_Nomenclature is

   ------------------------------------------------------------------------
   -- This package contains standard nomenclature for low-level lexical or
   -- "terminals" of a programming languages.  The philosophy of the naming
   -- of these identifiers is syntax-oriented, rather than trying to deduce
   -- their use within source code.

   -- For example, "+" could be unary-plus, binary-plus, or concatenation,
   -- depending on the programming language. So instead of forcing the
   -- programming language-to-XML converter to determine which of these meanings
   -- is meant, it is simply identified as "plus" and it is left to the AXF
   -- processor to determine its appropriate meaning--if that processor even
   -- cares.

   -- Not all terminal identifiers have a program language representation in
   -- all programming languages. For example, Axf_Shift_Left/Axf_Output, "<<",
   -- is an operator in C (shift left) and C++ (shift left and iostreams
   -- output), but is not present as such in Ada.

   type Operator_Names is
     (		   -- Examples...
      Axf_Plus, 	-- +
      Axf_Minus,	-- -
      Axf_Multiply,	-- *
      Axf_Divide, 	-- /, div
      Axf_Concat,	-- &, +, .
      Axf_Mod, 		-- mod, %
      Axf_Rem, 		-- rem
      Axf_Exponent, 	-- **, ^
      Axf_Eq, 		-- =, ==
      Axf_NE,		-- /=, !=
      Axf_LT, 		-- <, .LT.
      Axf_GT, 		-- >, .GT.
      Axf_LE, 		-- <=, .LE.
      Axf_GE,		-- >=, .GE.
      Axf_And,		-- and, &, &&
      Axf_Or, 		-- or, |, ||
      Axf_Xor, 		-- xor
      Axf_Not, 		-- not, !
      Axf_Abs,		-- abs
      Axf_And_Short, 	-- and then, Ada short circuit
      Axf_Or_Short, 	-- or else, Ada short circuit
      Axf_Shift_Left,	-- <<
      Axf_Shift_Right,	-- >>
      Axf_Output,	-- <<
      Axf_Input, 	-- >>
      Axf_Deref		-- ^, *
     );


   -- AXF attribute designating a standard operator axfPoint
   Axf_Oper_Attr    : constant String := "axfOper";

   -- Exception raised when there is no known correspondence between a given
   -- literal and the operators known to that language.
   Unknown_Operator : exception;

   ----------------------------------------------------------------------------
   -- Representations of numeric literals, both integer and real, are depicted
   -- using based notation if they are other than the default base 10.  The Ada
   -- representation is used, despite its verbosity, because it can represent
   -- more numeric bases than 2, 8, 10, and 16.

   Axf_Numeric_Attr : constant String := "axfNumber";

end Vatox.Axf_Points.Terminal_Nomenclature;
