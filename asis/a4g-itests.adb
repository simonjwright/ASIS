------------------------------------------------------------------------------
--                                                                          --
--                 ASIS-for-GNAT IMPLEMENTATION COMPONENTS                  --
--                                                                          --
--                           A 4 G . I T E S T S                            --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--            Copyright (C) 1995-2015, Free Software Foundation, Inc.       --
--                                                                          --
-- ASIS-for-GNAT is free software; you can redistribute it and/or modify it --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software Foundation;  either version 3,  or  (at your option)  any later --
-- version. ASIS-for-GNAT is distributed  in the hope  that it will be use- --
-- ful, but WITHOUT ANY WARRANTY; without even the implied warranty of MER- --
-- CHANTABILITY or  FITNESS FOR A PARTICULAR PURPOSE.                       --
--                                                                          --
--                                                                          --
--                                                                          --
--                                                                          --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
-- ASIS-for-GNAT was originally developed  by the ASIS-for-GNAT team at the --
-- Software  Engineering  Laboratory  of  the Swiss  Federal  Institute  of --
-- Technology (LGL-EPFL) in Lausanne,  Switzerland, in cooperation with the --
-- Scientific  Research  Computer  Center of  Moscow State University (SRCC --
-- MSU), Russia,  with funding partially provided  by grants from the Swiss --
-- National  Science  Foundation  and  the  Swiss  Academy  of  Engineering --
-- Sciences. ASIS-for-GNAT is now maintained by AdaCore                     --
-- (http://www.adacore.com).                                                --
--                                                                          --
------------------------------------------------------------------------------

--  with Sinput;       use Sinput;
with Atree;        use Atree;

package body A4G.Itests is

   -------------------------------
   -- Is_Inherited_Discriminant --
   -------------------------------

   function Is_Inherited_Discriminant (N : Node_Id) return Boolean is
      Type_Decl_Node : Node_Id;
   begin
      --  the idea of the test is to test if the node corresponding to the
      --  enclosing type declaration has been inserted (namely inserted,
      --  because a rewritten node corresponds to the situation when
      --  a derived type has its own known_discriminant part; and we cannot
      --  use Comes_From_Source flag, because it is set ON (???!!!) for
      --  such incerted nodes (at least, in 3.05)

      Type_Decl_Node := Parent (Parent (N));

      return Is_Rewrite_Insertion (Type_Decl_Node);
   end Is_Inherited_Discriminant;

end A4G.Itests;
