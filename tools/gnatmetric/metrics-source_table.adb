------------------------------------------------------------------------------
--                                                                          --
--                      GNAT METRICS TOOLS COMPONENTS                       --
--                                                                          --
--                 M E T R I C S . S O U R C E _ T A B L E                  --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                     Copyright (C) 2003-2017, AdaCore                     --
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

with GNAT.Table;

with METRICS.Metric_Definitions; use METRICS.Metric_Definitions;

package body METRICS.Source_Table is

   ------------------------------
   -- Source File metric table --
   ------------------------------

   package Source_File_Metric_Table is new GNAT.Table (
     Table_Component_Type => Line_Metrics_Record,
     Table_Index_Type     => SF_Id,
     Table_Low_Bound      => First_SF_Id,
     Table_Initial        => 100,
     Table_Increment      => 100,
     Table_Name           => "Source file metric table");

   Source_Metric_Table : Source_File_Metric_Table.Table_Ptr renames
      Source_File_Metric_Table.Table;

   -------------------
   -- Get_All_Lines --
   -------------------

   function Get_All_Lines (SF : SF_Id) return Metric_Count is
   begin
      return Source_Metric_Table (SF).All_Lines;
   end Get_All_Lines;

   ---------------------
   -- Get_Blank_Lines --
   ---------------------

   function Get_Blank_Lines (SF : SF_Id) return Metric_Count is
   begin
      return Source_Metric_Table (SF).Blank_Lines;
   end Get_Blank_Lines;

   --------------------
   -- Get_Code_Lines --
   --------------------

   function Get_Code_Lines (SF : SF_Id) return Metric_Count is
   begin
      return Source_Metric_Table (SF).Code_Lines;
   end Get_Code_Lines;

   -----------------------
   -- Get_Comment_Lines --
   -----------------------

   function Get_Comment_Lines (SF : SF_Id) return Metric_Count is
   begin
      return Source_Metric_Table (SF).Comment_Lines;
   end Get_Comment_Lines;

   ----------------------
   -- Get_EOL_Comments --
   ----------------------

   function Get_EOL_Comments (SF : SF_Id) return Metric_Count is
   begin
      return Source_Metric_Table (SF).EOL_Comments;
   end Get_EOL_Comments;

   -------------------
   -- Set_All_Lines --
   -------------------

   procedure Set_All_Lines (SF : SF_Id; Value : Metric_Count) is
   begin
      Source_Metric_Table (SF).All_Lines := Value;
   end Set_All_Lines;

   ---------------------
   -- Set_Blank_Lines --
   ---------------------

   procedure Set_Blank_Lines (SF : SF_Id; Value : Metric_Count) is
   begin
      Source_Metric_Table (SF).Blank_Lines := Value;
   end Set_Blank_Lines;

   --------------------
   -- Set_Code_Lines --
   --------------------

   procedure Set_Code_Lines (SF : SF_Id; Value : Metric_Count) is
   begin
      Source_Metric_Table (SF).Code_Lines := Value;
   end Set_Code_Lines;

   -----------------------
   -- Set_Comment_Lines --
   -----------------------

   procedure Set_Comment_Lines (SF : SF_Id; Value : Metric_Count) is
   begin
      Source_Metric_Table (SF).Comment_Lines := Value;
   end Set_Comment_Lines;

   ----------------------
   -- Set_EOL_Comments --
   ----------------------

   procedure Set_EOL_Comments (SF : SF_Id; Value : Metric_Count) is
   begin
      Source_Metric_Table (SF).EOL_Comments := Value;
   end Set_EOL_Comments;

   ------------------------------
   -- Set_Source_Metrics_Table --
   ------------------------------

   procedure Set_Source_Metrics_Table is
   begin
      Source_File_Metric_Table.Set_Last (SF_Id (Total_Sources));
   end Set_Source_Metrics_Table;

end METRICS.Source_Table;
