------------------------------------------------------------------------------
--                                                                          --
--                      GNAT METRICS TOOLS COMPONENTS                       --
--                                                                          --
--                 M E T R I C S . S O U R C E _ T A B L E                  --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                     Copyright (C) 2002-2009, AdaCore                     --
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

--  This package defines complementary table for the standard source file
--  table - it stores for each source file the corresponding global line
--  metrics.

with ASIS_UL.Metrics.Definitions; use ASIS_UL.Metrics.Definitions;
with ASIS_UL.Source_Table;        use ASIS_UL.Source_Table;

package METRICS.Source_Table is

   Low_SF_Bound   : constant := 0;
   High_SF_Bound : constant := 999_999;
   --  Almost 1_000_000 source files for one run of the tool

   procedure Set_Source_Metrics_Table;
   --  Sets the table of the approptiate size

   ----------------------------------------------
   -- Access/update routines for metric values --
   ----------------------------------------------

   function Get_All_Lines      (SF : SF_Id) return Metric_Count;
   function Get_Code_Lines     (SF : SF_Id) return Metric_Count;
   function Get_Comment_Lines  (SF : SF_Id) return Metric_Count;
   function Get_EOL_Comments   (SF : SF_Id) return Metric_Count;
   function Get_Blank_Lines    (SF : SF_Id) return Metric_Count;

   procedure Set_All_Lines      (SF : SF_Id; Value : Metric_Count);
   procedure Set_Code_Lines     (SF : SF_Id; Value : Metric_Count);
   procedure Set_Comment_Lines  (SF : SF_Id; Value : Metric_Count);
   procedure Set_EOL_Comments   (SF : SF_Id; Value : Metric_Count);
   procedure Set_Blank_Lines    (SF : SF_Id; Value : Metric_Count);

end METRICS.Source_Table;
