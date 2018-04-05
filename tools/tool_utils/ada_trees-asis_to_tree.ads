------------------------------------------------------------------------------
--                                                                          --
--                            GNAT2XML COMPONENTS                           --
--                                                                          --
--       G N A T 2 X M L . A D A _ T R E E S . A S I S _ T O _ T R E E      --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                      Copyright (C) 2013-2016, AdaCore                    --
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

--  This package provides Compilation_Unit_To_Tree, for converting ASIS
--  elements into Ada_Trees.

with Asis; use Asis;

with Pp.Scanner;

package Ada_Trees.Asis_To_Tree is

   function Compilation_Unit_To_Tree
     (The_Unit : Asis.Compilation_Unit;
      Gen_Regions : Scanner.Token_Vector_Ptr := null)
     return Ada_Tree;
   --  Converts an ASIS compilation unit into an Ada_Tree. If Gen_Regions is
   --  nonnull, skip sections of code in the regions thereof.

   --  The following _Value functions (and the ones in the body) return the
   --  value of the relevant component of Ada_Tree_Rec; for example, Def_Value
   --  returns the value for the Def component. The ones here need to be
   --  exported for Rep_Clauses.

   function Def_Value (Def_Id : Defining_Name) return String;
   function Def_Value (Def_Id : Defining_Name) return Name_Id is
     (Name_Find (Def_Value (Def_Id)));

   function Def_Name_Value (Def_Id : Defining_Name) return String;
   function Def_Name_Value (Def_Id : Defining_Name) return Name_Id is
     (Name_Find (Def_Name_Value (Def_Id)));

end Ada_Trees.Asis_To_Tree;
