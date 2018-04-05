------------------------------------------------------------------------------
--                                                                          --
--                     ASIS UTILITY LIBRARY COMPONENTS                      --
--                                                                          --
-- A S I S _ U L . E N V I R O N M E N T . C H E C K  _ P A R A M E T E R S --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                    Copyright (C) 2004-2014, AdaCore                      --
--                                                                          --
-- Asis Utility Library (ASIS UL) is free software; you can redistribute it --
-- and/or  modify  it  under  terms  of  the  GNU General Public License as --
-- published by the Free Software Foundation; either version 2, or (at your --
-- option)  any later version.  ASIS UL  is distributed in the hope that it --
-- will  be  useful,  but  WITHOUT  ANY  WARRANTY; without even the implied --
-- warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the --
-- GNU  General Public License for more details. You should have received a --
-- copy of the  GNU General Public License  distributed with GNAT; see file --
-- COPYING. If not,  write  to the  Free Software Foundation,  51 Franklin --
-- Street, Fifth Floor, Boston, MA 02110-1301, USA.                         --
--                                                                          --
-- ASIS UL is maintained by AdaCore (http://www.adacore.com).               --
--                                                                          --
------------------------------------------------------------------------------

--  This is a placeholder for the routine that checks tool parameters. Because
--  all the checks are tool-specific, this placeholder only set up the Arg_List
--  global variable.

with ASIS_UL.Compiler_Options;

separate (ASIS_UL.Environment)
procedure Check_Parameters is
begin
   Set_Arg_List;

-- 8<--8<--8<--8<--8<--8<--8<--8<--8<--8<--8<--8<--
   --  This is needed for multiple files driver only

   if Last_Source < First_SF_Id then
      Error ("No input source file set");
      --  A call to Brief_Help should be here, but this is tool-specific
      raise Parameter_Error;
   end if;

   Total_Sources := Natural (Last_Source);
   Sources_Left  := Total_Sources;
-- >8-->8-->8-->8-->8-->8-->8-->8-->8-->8-->8-->8--

end Check_Parameters;
