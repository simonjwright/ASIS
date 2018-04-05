------------------------------------------------------------------------------
--                                                                          --
--                      GNAT METRICS TOOLS COMPONENTS                       --
--                                                                          --
--           M E T R I C S . M E T R I C _ D E F I N I T I O N S            --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                     Copyright (C) 2002-2016, AdaCore                     --
--                                                                          --
-- GNATMETRIC  is  free software; you can  redistribute it and/or modify it --
-- under terms of the  GNU  General Public License as published by the Free --
-- Software  Foundation;  either version 3, or (at your option)  any  later --
-- version. GNATMETRIC  is  distributed in the hope that it will be useful, --
-- but  WITHOUT  ANY  WARRANTY;   without  even  the  implied  warranty  of --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General --
-- Public License for more details.  You should have received a copy of the --
-- GNU General Public License distributed with GNAT; see file  COPYING3. If --
-- not,  go  to  http://www.gnu.org/licenses  for  a  complete  copy of the --
-- license.                                                                 --
--                                                                          --
-- GNATMETRIC is maintained by AdaCore (http://www.adacore.com).            --
--                                                                          --
------------------------------------------------------------------------------

--  This package contains the definition of different metrics and the
--  corresponding data structures

with ASIS_UL.Metrics.Definitions; use ASIS_UL.Metrics.Definitions;

package METRICS.Metric_Definitions is

   type Real_Val_To_Print is delta 0.01 digits 8;
   --  This type is intended to be used for the real values that are supposed
   --  to be converted into string by applying 'Img attribute and then printed
   --  out. The idea is to have high precision and a nice-readable format with
   --  two digits after a dot.

   ------------------------
   -- Complexity metrics --
   ------------------------

   Units_Compute_Average_Complexity_For   : Natural      := 0;
   Total_Cyclomatic_Complexity            : Metric_Count := 0;
   Average_Cyclomatic_Complexity          : Float        := -1.0;
   Average_Cyclomatic_Complexity_To_Print : Real_Val_To_Print;
   --  Counters used to compute average cyclomatic complexity

   ------------------
   -- Line metrics --
   ------------------

   --  We define the set of line metrics as a record type to make it possible
   --  to store these metrics for each source being processed

   type Line_Metrics_Record is record
      All_Lines : Metric_Count;
      --  The total number of lines in the given source

      Code_Lines : Metric_Count;
      --  Lines containing at least one character belonging to the non-comment
      --  Ada code

      Comment_Lines : Metric_Count;
      --  Comment lines

      EOL_Comments : Metric_Count;
      --  Lines containing both Ada code and comments, or, in other words,
      --  End-Of-Line comments

      Blank_Lines : Metric_Count;
      --  Lines containing only space characters and/or format effectors

   end record;

   Zero_Line_Metrics : constant Line_Metrics_Record :=
     (All_Lines     => 0,
      Code_Lines    => 0,
      Comment_Lines => 0,
      EOL_Comments  => 0,
      Blank_Lines   => 0);

   --  Comment/code persentage metric:

   subtype Comment_Code_Percentage is Real_Val_To_Print range 0.0 .. 100.0;

   Comment_Code_Ratio          : Float;
   Comment_Code_Ratio_To_Print : Comment_Code_Percentage;

   --  Average code lines in processes metric:

   Num_Of_Processes_Bodies                  : Natural      := 0;
   Lines_In_Process_Bodies                  : Metric_Count := 0;
   Average_Lines_In_Process_Bodies          : Float        := -1.0;
   Average_Lines_In_Process_Bodies_To_Print : Real_Val_To_Print;

   ---------------------
   -- Element Metrics --
   ---------------------

   --  We define the set of line metrics as a record type to make it possible
   --  to store these metrics for each source being processed

   type Public_Types_Details is record
      Abstract_Types  : Metric_Count;
      Tagged_Types    : Metric_Count;
      Private_Types   : Metric_Count;
      Task_Types      : Metric_Count;
      Protected_Types : Metric_Count;
   end record;

   Zero_Public_Types_Details : constant Public_Types_Details :=
     (Abstract_Types  => 0,
      Tagged_Types    => 0,
      Private_Types   => 0,
      Task_Types      => 0,
      Protected_Types => 0);

   type Param_Num is record
      All_Params    : Metric_Count;
      In_Params     : Metric_Count;
      Out_Params    : Metric_Count;
      In_Out_Params : Metric_Count;
   end record;

   Zero_Param_Num : Param_Num  :=
     (All_Params    => 0,
      In_Params     => 0,
      Out_Params    => 0,
      In_Out_Params => 0);

   type Contract_Details is record   --  ??? do we need this?
      With_Contracts : Metric_Count;
      With_Post      : Metric_Count;
   end record;

   Zero_Contract_Details : constant Contract_Details :=
     (With_Contracts => 0,
      With_Post      => 0);

   type All_Metrics is record
      Computed_Line_Metrics : Natural;
      --  The number of units for which line metrics are safely computed

      Computed_Element_Metrics : Natural;
      --  The number of units for which element metrics are safely computed

      Computed_Public_Subprograms : Natural;
      --  The number of units for which public subprograms are safely
      --  computed

      Computed_All_Subprograms : Natural;
      --  The number of units for which all the subprograms are safely
      --  computed

      Computed_Public_Types : Natural;
      Computed_All_Types    : Natural;

      Line_Metrics    : Line_Metrics_Record;
      Syntax_Metrics  : Syntax_Metric_Counter;

      Public_Subprograms : Metric_Count;
      All_Subprograms    : Metric_Count;

      Public_Types          : Metric_Count;
      Public_Types_Detailed : Public_Types_Details;
      All_Types             : Metric_Count;

      --  We count public and all subprograms and types separately from
      --  Element metrics because these metrics are computed for compilation
      --  units only, not for nested program units.
   end record;

   Global_Statistics : All_Metrics;
   --  Here we collect the integrated metrics values for all the units being
   --  processed

   function Computed (Value : Metric_Count) return Boolean;
   --  Checks if Value is computed (that is, neither disabled nor undefined)

   procedure Init_Global_Statistics;
   --  This procedure initializes the fields of Global_Statistics global
   --  variable according to the metrics computation options

   procedure Add_Public_Types_Details (Value : Public_Types_Details);
   --  Adds detailed counters for public types to the corresponding fields
   --  of Global_Statistics.

   function Details_Present (Value : Public_Types_Details) return Boolean;
   --  Checks if at least one field of the argument is non-zero.

   procedure Set_Global_Metrics_Flags;
   --  Sets values of the flags defined in METRICS.Common which indicate if
   --  this or that "number of..." metric should be computed for a currently
   --  processed unit.

end METRICS.Metric_Definitions;
