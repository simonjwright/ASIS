------------------------------------------------------------------------------
--                                                                          --
--                            GNAT2XML COMPONENTS                           --
--                                                                          --
--       G N A T 2 X M L . A D A _ T R E E S . R E P _ C L A U S E S        --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                     Copyright (C) 2014, AdaCore, Inc.                    --
--                                                                          --
-- Gnat2xml is free software; you can redistribute it and/or modify it      --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software Foundation;  either version 2,  or  (at your option)  any later --
-- version. Gnat2xml is distributed  in the hope  that it will be useful,   --
-- but WITHOUT ANY WARRANTY; without even the implied warranty of MER-      --
-- CHANTABILITY or  FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General  --
-- Public License for more details. You should have received a copy of the  --
-- GNU General Public License distributed with GNAT; see file COPYING. If   --
-- not, write to the Free Software Foundation, 59 Temple Place Suite 330,   --
-- Boston, MA 02111-1307, USA.                                              --
-- The gnat2xml tool was derived from the Avatox sources.                   --
------------------------------------------------------------------------------

pragma Ada_2012;

package Ada_Trees.Rep_Clauses is

   procedure Append_Rep_Clauses
     (L : in out Ada_Tree_Vector; E : Asis.Element; T : Ada_Tree);
   --  E is an Asis element, and T is the corresponding Ada_Tree constructed by
   --  Asis_To_Tree. This generates any appropriate representation clauses for
   --  E/T, appending them onto L.

end Ada_Trees.Rep_Clauses;
