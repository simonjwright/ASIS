------------------------------------------------------------------------------
--                                                                          --
--                           AVATOX COMPONENTS                              --
--                                                                          --
--                        VATOX (Via Asis To Xml)                           --
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

with Ada.Containers.Indefinite_Ordered_Maps;
with Asis.Declarations;
with Asis.Elements;
with Mckae.Text.Lexicals;

package body Vatox.Axf_Points.Terminal_Nomenclature.Ada_Terms is

   package Operators_Mapping is new Ada.Containers.Indefinite_Ordered_Maps
     (String, Operator_Names);
   Operators_Map : Operators_Mapping.Map;

   ------------------------------------------------------------------------------

   function Lookup_Operator
     (S : String
     ) return String is

      use Mckae.Text;
      use type Operators_Mapping.Cursor;

      Operator_Cursor : Operators_Mapping.Cursor;
   begin
      Operator_Cursor := Operators_Mapping.Find (Operators_Map, S);
      if Operator_Cursor /= Operators_Mapping.No_Element then
         return Lexicals.Transform
           (Operator_Names'Image (Operators_Mapping.Element (Operator_Cursor)),
            Lexicals.Xml_Attrs, Remove_Underscores => True);
      else
         raise Unknown_Operator;
      end if;
   end Lookup_Operator;

   ------------------------------------------------------------------------------

   function Based_Representation
     (N : String
     ) return String is
   begin
      return N;
   end Based_Representation;

   -----------------------------------------------------------------------------

begin
   Operators_Mapping.Insert (Operators_Map, "+", Axf_Plus);
   Operators_Mapping.Insert (Operators_Map, "-",  Axf_Minus);
   Operators_Mapping.Insert (Operators_Map, "*", Axf_Multiply);
   Operators_Mapping.Insert (Operators_Map, "/", Axf_Divide);
   Operators_Mapping.Insert (Operators_Map, "&", Axf_Concat);
   Operators_Mapping.Insert (Operators_Map, "mod", Axf_Mod);
   Operators_Mapping.Insert (Operators_Map, "rem", Axf_Rem);
   Operators_Mapping.Insert (Operators_Map, "=", Axf_Eq);
   Operators_Mapping.Insert (Operators_Map, "/=", Axf_NE);
   Operators_Mapping.Insert (Operators_Map, "<", Axf_LT);
   Operators_Mapping.Insert (Operators_Map, ">", Axf_GT);
   Operators_Mapping.Insert (Operators_Map, "<=", Axf_LE);
   Operators_Mapping.Insert (Operators_Map, ">=", Axf_GE);
   Operators_Mapping.Insert (Operators_Map, "and", Axf_And);
   Operators_Mapping.Insert (Operators_Map, "or", Axf_Or);
   Operators_Mapping.Insert (Operators_Map, "not", Axf_Not);
   Operators_Mapping.Insert (Operators_Map, "abs", Axf_Abs);
   Operators_Mapping.Insert (Operators_Map, "xor", Axf_Xor);
   Operators_Mapping.Insert (Operators_Map, "and then", Axf_And_Short);
   Operators_Mapping.Insert (Operators_Map, "or else", Axf_Or_Short);
   Operators_Mapping.Insert (Operators_Map, "**", Axf_Exponent);

end Vatox.Axf_Points.Terminal_Nomenclature.Ada_Terms;
