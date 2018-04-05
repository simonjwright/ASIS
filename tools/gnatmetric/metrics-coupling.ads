------------------------------------------------------------------------------
--                                                                          --
--                      GNAT METRICS TOOLS COMPONENTS                       --
--                                                                          --
--                     M E T R I C S . C O U P L I N G                      --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                    Copyright (C) 2008-2011, AdaCore                      --
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

--  This package defines routines for collecting and computing the coupling
--  metrics.

with Asis;                 use Asis;

package METRICS.Coupling is

   procedure Collect_Coupling_Dependencies (CU : Asis.Compilation_Unit);
   --  Collect the coupling-related dependencies for a given unit.
   --  Coupling-related dependencies took into account only units for that
   --  Has_Coupling_Metrics returns True. When counting coupling dependencies,
   --  we consider only units that are arguments of the gnatmetric call.

   procedure Compute_Coupling_Metrics;
   --  Computes the coupling metrics using the global information collected by
   --  the calls to Collect_Coupling_Dependencies made during the traversing of
   --  all the argument units.
   --
   --  Now this procedure does nothing. All the computations are performed by
   --  Report_Coupling_Metrics

   procedure Report_Coupling_Metrics
     (Text : Boolean;
      XML  : Boolean);
   --  Generates the coupling metrics report depending on the coupling metrics
   --  in effect and output files settings. The text output is sent to ???,
   --  the XML output is appended to the XML output file.
   --  What kind of output is generated is defined by the actuals.

   procedure Print_Unit_Table;
   --  Prints into StdErr the debug image of the unit table with all the
   --  dependencies information.

end METRICS.Coupling;
