------------------------------------------------------------------------------
--                                                                          --
--                           AVATOX COMPONENTS                              --
--                                                                          --
--                        VATOX (Via Asis To Xml)                           --
--                                                                          --
--                                                                          --
--                Copyright (c) 2007, McKae Technologies.                   --
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

package Vatox.Axf_Points.References.Ada_Refs is

   -- Create a fully qualified ID for a declaration.
   function Get_Scope_Sequence
     (Asis_Decl : Asis.Declaration
      -- The declaration whose scope sequence is needed
     ) return Wide_String;

   -- Return the list of scope references (which are also used to generate the
   -- scope sequence id) as a reference list
   function Get_Scope_Refs
     (Asis_Decl : Asis.Declaration
      -- The declaration whose list of scope references is needed
     ) return Ref_List;

end Vatox.Axf_Points.References.Ada_Refs;
