------------------------------------------------------------------------------
--                                                                          --
--                          GNATCHECK COMPONENTS                            --
--                                                                          --
--   A S I S _ U L . G L O B A L _ S T A T E . C G. C O N D I T I O N S     --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                       Copyright (C) 2009, AdaCore                        --
--                                                                          --
-- Asis Utility Library (ASIS UL) is free software; you can redistribute it --
-- and/or  modify  it  under  terms  of  the  GNU General Public License as --
-- published by the Free Software Foundation; either version 2, or (at your --
-- option)  any later version.  ASIS UL  is distributed in the hope that it --
-- will  be  useful,  but  WITHOUT  ANY  WARRANTY; without even the implied --
-- warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the --
-- GNU  General Public License for more details. You should have received a --
-- copy of the  GNU General Public License  distributed with GNAT; see file --
-- COPYING. If not,  write  to the  Free Software Foundation,  51 Franklin  --
-- Street, Fifth Floor, Boston, MA 02110-1301, USA.                         --
--                                                                          --
-- ASIS UL is maintained by AdaCore (http://www.adacore.com).               --
--                                                                          --
------------------------------------------------------------------------------

--  This package defines the condions that allows to build some specific
--  versions of call graph. For example, the call graph that contains only
--  inlined subpograms. Or a call graph that contains only dispatching
--  operations

package ASIS_UL.Global_State.CG.Conditions is

   --  When creating the call graph, we may either represent in this graph all
   --  possible calling nodes (unconditional mode), or we can represent only
   --  nodes (and links?) with specific properties. By defaul unconditional
   --  mode is ON.
   --
   --  Artificial type initialization procedures and type discriminant
   --  initialization procedures are included in the call graph
   --  inconditionally.

   function Unconditional_Call_Graph return Boolean;
   --  Checks if all possible nodes and calls should be included in the call
   --  graph.

   procedure Set_Unconditional_Call_Graph (On : Boolean);
   --  If the parameter is True, sets the unconditional call graph mode ON,
   --  otherwise set it OFF.

   ---------------------------
   -- Specifying conditions --
   ---------------------------

   --  For now, we use a rather simple-minded approach to specifying the
   --  conditions: all possible conditions are enumerated, and if a new
   --  condition is needed, the corresponding value should be added to the
   --  enumeration type below, the corresponding test function should be
   --  provided, and the corresponding item should be added to the check
   --  look-up table (see the body for full details).

   type Check_Kinds is
     (Inlined_Subprograms  --  If a node represents an inlined subprogram,
                           --  it should be represented in the call graph
     );

   procedure Set_Condition (Cond : Check_Kinds);
   --  Activate the specified condition.

   function Should_Be_In_CG (E : Asis.Element) return Boolean;
   --  Checks if the node corresponding to E should be represented in the call
   --  graph. If the call link points to tne node that corresponds to the
   --  Element for that this function returns False, this link is not included
   --  in the call graph.
   --
   --  Always returns True if Unconditional_Call_Graph returns True.

end ASIS_UL.Global_State.CG.Conditions;
