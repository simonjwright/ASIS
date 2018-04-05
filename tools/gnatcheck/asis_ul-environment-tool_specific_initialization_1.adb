------------------------------------------------------------------------------
--                                                                          --
--                          GNATCHECK COMPONENTS                            --
--                                                                          --
--           ASIS_UL.ENVIRONMENT.TOOL_SPECIFIC_INITIALIZATION_1             --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                     Copyright (C) 2004-2010, AdaCore                     --
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

--  Here all the calls to Gnatcheck.Rules.Register_Rule for all the rules
--  currently implemented in gnatcheck should be placed

with ASIS_UL.Global_State.CG.Conditions;
with Gnatcheck.Rules;

separate (ASIS_UL.Environment)
procedure Tool_Specific_Initialization_1 is
begin
   ASIS_UL.Global_State.CG.Conditions.Set_Unconditional_Call_Graph (False);
   Gnatcheck.Rules.Register_Rules;
end Tool_Specific_Initialization_1;
