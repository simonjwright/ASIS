------------------------------------------------------------------------------
--                                                                          --
--                          GNATCHECK COMPONENTS                            --
--                                                                          --
--           G N A T C H E C K . R U L E S. T R A V E R S I N G             --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                    Copyright (C) 2004-2009, AdaCore                      --
--                                                                          --
-- GNATCHECK  is  free  software;  you can redistribute it and/or modify it --
-- under terms of the  GNU  General Public License as published by the Free --
-- Software Foundation;  either version 2, or ( at your option)  any  later --
-- version.  GNATCHECK  is  distributed in the hope that it will be useful, --
-- but  WITHOUT  ANY  WARRANTY;   without  even  the  implied  warranty  of --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General --
-- Public License for more details.  You should have received a copy of the --
-- GNU  General Public License distributed with GNAT; see file  COPYING. If --
-- not,  write to the  Free Software Foundation,  51 Franklin Street, Fifth --
-- Floor, Boston, MA 02110-1301, USA.                                       --
--                                                                          --
-- GNATCHECK is maintained by AdaCore (http://www.adacore.com).             --
--                                                                          --
------------------------------------------------------------------------------

with Asis.Iterator; use Asis.Iterator;

--  This package defines the traversal engine for gnatcheck. It includes both
--  the recursive traversing of the units syntax structure and traversing
--  the global structure.

package Gnatcheck.Rules.Traversing is

   procedure All_Rules_Pre_Op
     (Element :        Asis.Element;
      Control : in out Traverse_Control;
      State   : in out Rule_Traversal_State);
   --  If needed, collects all the global state information from the argument
   --  element.
   --
   --  If State.Check_Rules is ON and if Element is not from expanded generic,
   --  checks rules by performing rule-specific Pre-Operation on a an argument
   --  Element for any active rule. Before calling each rule-specific
   --  Pre-Operation, sets State.Detected OFF, and if after completing this
   --  pre-operation State.Detected is ON, reports the rule violation for this
   --  rule. When all the calls to rule-specific Pre-Operations are complete,
   --  put the record corresponding to the Element being visited at the top of
   --  the traversal stack.
   --
   --  If the argument Element is an instantiation, applies the Check_Rules
   --  traversal routine to the expanded spec and the expanded body to collect
   --  the global information (if needed).

   procedure All_Rules_Post_Op
     (Element :        Asis.Element;
      Control : in out Traverse_Control;
      State   : in out Rule_Traversal_State);
   --  Pops the traversal stack.
   --
   --  If State.Check_Rules is ON and if Element is not from expanded generic,
   --  performs all the rule-specific Post-operations
   --
   --  If needed, completes computing the global information corresponding to
   --  the argument Element.

   procedure Check_Rules is new Traverse_Element
     (Rule_Traversal_State,
      All_Rules_Pre_Op,
      All_Rules_Post_Op);
   --  Traverses the syntax structure of the argument Element. Checks the local
   --  rules and collect the information for building the global structure.

   procedure Check_Global_Rules;
   --  Checks all the active global rules.

   procedure Reset_State (State : in out Rule_Traversal_State);
   --  Resets the traversal state to "no rule violation detected", should be
   --  called before applying the next rule-specific pre- or post-operation,
   --  may be used in a rule-specific routines to cancel the changes in State
   --  that reflects the detected violation

end Gnatcheck.Rules.Traversing;
