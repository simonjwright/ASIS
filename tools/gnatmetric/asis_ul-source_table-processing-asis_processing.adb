------------------------------------------------------------------------------
--                                                                          --
--                          GNATMETRIC COMPONENTS                           --
--                                                                          --
--     A S I S _ U L . S O U R C E _ T A B L E . P R O C E S S I N G .      --
--                      A S I S _ P R O C E S S I N G                       --
--                                                                          --
--            (adapted for gnatmetric from ASIS Utility Library)            --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                     Copyright (C) 2009-2014, AdaCore                     --
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

with METRICS.Compute;

separate (ASIS_UL.Source_Table.Processing)
procedure ASIS_Processing (CU : Asis.Compilation_Unit; SF : SF_Id) is
begin
   pragma Assert (not Incremental_Mode);

   METRICS.Compute.Compute_Unit_Metrics (CU, SF);
end ASIS_Processing;
