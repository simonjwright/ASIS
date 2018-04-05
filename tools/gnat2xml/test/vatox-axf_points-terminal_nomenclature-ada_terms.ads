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

with Asis;

package Vatox.Axf_Points.Terminal_Nomenclature.Ada_Terms is

   -- Ada language implementation of terminal nomenclature

   -- Look up the operator name corresponding to the contents of the provided
   -- string. Returns an XML style string corresponding to the operator. If
   -- there is no corresponding operator name, raise the
   -- Unknown_Operator exception.
   function Lookup_Operator
     (S : String
      -- Representation of a programming language terminal)
     ) return String;

   -- Convert the original numerical literal representation into the AXFPoint
   -- formatted version.  (For Ada, this is trivial, since AXF uses Ada's
   -- format.)
   function Based_Representation
     (N : String
      -- Original representation of the numeric literal
     ) return String;

end Vatox.Axf_Points.Terminal_Nomenclature.Ada_Terms;
