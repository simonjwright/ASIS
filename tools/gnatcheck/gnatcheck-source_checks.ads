------------------------------------------------------------------------------
--                                                                          --
--                          GNATCHECK COMPONENTS                            --
--                                                                          --
--              G N A T C H E C K . S O U R C E _ C H E C K S               --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                       Copyright (C) 2010, AdaCore                        --
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

--  This package contains resources needed for rules formulated and checked for
--  source code of program units without explicit relations to specific
--  syntax elements

with Asis;            use Asis;
with Gnatcheck.Rules; use Gnatcheck.Rules;

package Gnatcheck.Source_Checks is

   procedure Init_Source_Text_Checks (Unit : Asis.Element);
   --  Initializes the internal data structures needed for checking the source
   --  code rules for the unit that encloses the argument Element.

   procedure Check_Text_Rules
     (Up_To :        Asis.Element;
      State : in out Rule_Traversal_State);
   --  Checks all the enabled source code rules for the source lines of the
   --  currently checked unit starting from the line next to the last checked
   --  (or from the first line if this is the first call of this procedure for
   --  a given unit) down to the first line of the argument span.
   --  State parameter is used to keep information about detected violation
   --  in the same way as Pre- and Post-operations in ASIS traversing do.

   procedure Check_Text_Rules_For_Remaining_Lines
     (Unit  :        Asis.Element;
      State : in out Rule_Traversal_State);
   --  Checks all the enabled source code rules for the source lines of the
   --  currently checked unit starting from the line next to the last checked
   --  (or from the first line if for a given unit Check_Text_Rules procedure
   --  has never be called) down to the last line of the currently checked
   --  unit. Actual for Unit is used as a means to get access to the source of
   --  the analyzed unit.
   --  State parameter is used to keep information about detected violation
   --  in the same way as Pre- and Post-operations in ASIS traversing do.

end Gnatcheck.Source_Checks;
