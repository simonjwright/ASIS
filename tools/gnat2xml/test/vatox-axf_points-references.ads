------------------------------------------------------------------------------
--                                                                          --
--                           AVATOX COMPONENTS                              --
--                                                                          --
--                        VATOX (Via Asis To Xml)                           --st
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

with Ada.Strings.Wide_Unbounded;

package Vatox.Axf_Points.References is

   ----------------------------------------------------------------------------
   -- Scope/nesting information about identifiers is added to aid determining
   -- whether multiple appearances of the same identifer name refer to the
   -- same or different entities.
   --
   -- The structure of the scope name ID is a list of scope/namespace names,
   -- from highest to lowest, delimited by blanks.  The reason blanks are used
   -- is because no programming language of any reasonably widespread usage
   -- allows blanks as part of identifier names.
   Axf_Scope_ID_Attr : constant String := "axfScope";

   -- The individual declaration enclosing scopes are output as axfPoint
   -- elements so that the scope ID string doesn't have to be parsed to get
   -- at the individual scope levels.
   Axf_Scope_Name_Attr  : constant String := "axfScopeName";
   Axf_Scope_Level_Attr : constant String := "axfScopeLevel";

   -- Cross-references of identifiers, operator symbols, and enumeration and
   -- character literals are handle analogously to the scope enclosures.
   Axf_Xref_ID_Attr    : constant String := "axfXref";
   Axf_Xref_Name_Attr  : constant String := "axfXrefName";
   Axf_Xref_Level_Attr : constant String := "axfXrefLevel";

   -- Structure for returning a list of scope names
   type Ref_List is array (Positive range <>)
   of Ada.Strings.Wide_Unbounded.Unbounded_Wide_String;

end Vatox.Axf_Points.References;
