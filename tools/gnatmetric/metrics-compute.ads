------------------------------------------------------------------------------
--                                                                          --
--                      GNAT METRICS TOOLS COMPONENTS                       --
--                                                                          --
--                     M E T R I C S . C O M P U T E                        --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                    Copyright (C) 2002-2014, AdaCore                      --
--                                                                          --
-- GNAT Metrics Toolset  is free software;  you can  redistribute it and/or --
-- modify it under terms of the  GNU General Public License as published by --
-- the Free Software Foundation;  either version 2, or (at your option) any --
-- later version.  GNAT Metrics Toolset is  distributed in the hope that it --
-- will be useful, but  WITHOUT ANY WARRANTY; without even the implied war- --
-- ranty of  MERCHANTABILITY or  FITNESS FOR A PARTICULAR PURPOSE.  See the --
-- GNU General Public License for more details.  You should have received a --
-- copy of the  GNU General Public License distributed with  GNAT; see file --
-- COPYING.  If not,  write to the  Free Software  Foundation,  51 Franklin --
-- Street, Fifth Floor, Boston, MA 02110-1301, USA.                         --
--                                                                          --
-- GNAT Metrics Toolset is maintained by AdaCore (http://www.adacore.com).  --
--                                                                          --
------------------------------------------------------------------------------

--  This package defines different routines for computing metrics.

with Asis;                 use Asis;
with ASIS_UL.Source_Table; use ASIS_UL.Source_Table;

package METRICS.Compute is

   procedure Compute_Unit_Metrics
     (The_CU : Asis.Compilation_Unit;
      SF     : SF_Id);
   --  This procedure computes and reports metrics for the given pair of
   --  ASIS Compilation Unit and its source file (given with full directory
   --  information in absolute form), provided that this Compilation_Unit
   --  exists in The_Context (which is currently open). The caller is
   --  responsible for the fact that this pair Unit - file name is correct, and
   --  that the unit exists in the context and that the context is open.

end METRICS.Compute;
