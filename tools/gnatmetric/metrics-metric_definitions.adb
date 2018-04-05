------------------------------------------------------------------------------
--                                                                          --
--                      GNAT METRICS TOOLS COMPONENTS                       --
--                                                                          --
--           M E T R I C S . M E T R I C _ D E F I N I T I O N S            --
--                                                                          --
--                                 B o d y                                  --
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

with Asis;            use Asis;

with METRICS.Options; use METRICS.Options;
with METRICS.Common;  use METRICS.Common;

package body METRICS.Metric_Definitions is

   ------------------------------
   -- Add_Public_Types_Details --
   ------------------------------

   procedure Add_Public_Types_Details (Value : Public_Types_Details) is
      ITD : Public_Types_Details
        renames Global_Statistics.Public_Types_Detailed;
   begin
      ITD.Abstract_Types  := ITD.Abstract_Types  + Value.Abstract_Types;
      ITD.Tagged_Types    := ITD.Tagged_Types    + Value.Tagged_Types;
      ITD.Private_Types   := ITD.Private_Types   + Value.Private_Types;
      ITD.Task_Types      := ITD.Task_Types      + Value.Task_Types;
      ITD.Protected_Types := ITD.Protected_Types + Value.Protected_Types;

   end Add_Public_Types_Details;

   --------------
   -- Computed --
   --------------

   function Computed (Value : Metric_Count) return Boolean is
   begin
      return Value > Metric_Count_Undefined;
   end Computed;

   ---------------------
   -- Details_Present --
   ---------------------

   function Details_Present (Value : Public_Types_Details) return Boolean is
   begin

      return False
        or else Value.Abstract_Types  > 0
        or else Value.Tagged_Types    > 0
        or else Value.Private_Types   > 0
        or else Value.Task_Types      > 0
        or else Value.Protected_Types > 0;

   end Details_Present;

   ----------------------------
   -- Init_Global_Statistics --
   ----------------------------

   procedure Init_Global_Statistics is
   begin

      Global_Statistics.Computed_Line_Metrics       := 0;
      Global_Statistics.Computed_Element_Metrics    := 0;
      Global_Statistics.Computed_Public_Subprograms := 0;
      Global_Statistics.Computed_All_Subprograms    := 0;
      Global_Statistics.Computed_Public_Types       := 0;
      Global_Statistics.Computed_All_Types          := 0;

      if Compute_All_Lines then
         Global_Statistics.Line_Metrics.All_Lines := 0;
      else
         Global_Statistics.Line_Metrics.All_Lines := Metric_Count_Disabled;
      end if;

      if Compute_Code_Lines
        or else
         Compute_Comment_Code_Ratio
      then
         Global_Statistics.Line_Metrics.Code_Lines := 0;
      else
         Global_Statistics.Line_Metrics.Code_Lines := Metric_Count_Disabled;
      end if;

      if Compute_Comment_Lines
        or else
         Compute_Comment_Code_Ratio
      then
         Global_Statistics.Line_Metrics.Comment_Lines := 0;
      else
         Global_Statistics.Line_Metrics.Comment_Lines := Metric_Count_Disabled;
      end if;

      if Compute_EOL_Comments
        or else
         Compute_Comment_Code_Ratio
      then
         Global_Statistics.Line_Metrics.EOL_Comments := 0;
      else
         Global_Statistics.Line_Metrics.EOL_Comments := Metric_Count_Disabled;
      end if;

      if Compute_Blank_Lines then
         Global_Statistics.Line_Metrics.Blank_Lines := 0;
      else
         Global_Statistics.Line_Metrics.Blank_Lines := Metric_Count_Disabled;
      end if;

      if Compute_All_Statements then
         Global_Statistics.Syntax_Metrics.All_Statements := 0;
      else
         Global_Statistics.Syntax_Metrics.All_Statements :=
           Metric_Count_Disabled;
      end if;

      if Compute_All_Declarations then
         Global_Statistics.Syntax_Metrics.All_Declarations := 0;
      else
         Global_Statistics.Syntax_Metrics.All_Declarations :=
           Metric_Count_Disabled;
      end if;

      if Compute_Public_Subprograms then
         Global_Statistics.Public_Subprograms := 0;
      else
         Global_Statistics.Public_Subprograms := Metric_Count_Disabled;
      end if;

      if Compute_All_Subprograms then
         Global_Statistics.All_Subprograms := 0;
      else
         Global_Statistics.All_Subprograms := Metric_Count_Disabled;
      end if;

      if Compute_Public_Types then
         Global_Statistics.Public_Types := 0;
         Global_Statistics.Public_Types_Detailed := (others => 0);
      else
         Global_Statistics.Public_Types := Metric_Count_Disabled;
      end if;

      if Compute_All_Types then
         Global_Statistics.All_Types := 0;
      else
         Global_Statistics.All_Types := Metric_Count_Disabled;
      end if;

   end Init_Global_Statistics;

   ------------------------------
   -- Set_Global_Metrics_Flags --
   ------------------------------

   procedure Set_Global_Metrics_Flags is
   begin
      May_Have_Public_Subprograms := False;
      May_Have_Subprogram_Bodies  := False;
      May_Have_Public_Types       := False;
      May_Have_Type_Definitions   := False;

      if Compute_Public_Subprograms and then
         CU_Class not in A_Private_Declaration .. A_Separate_Body
      then

         case CU_Kind is
            when A_Procedure .. A_Generic_Package =>
               May_Have_Public_Subprograms := True;
            when A_Procedure_Body .. A_Function_Body =>

               if CU_Class = A_Public_Declaration_And_Body then
                  May_Have_Public_Subprograms := True;
               end if;

            when others =>
               null;
         end case;

      end if;

      if Compute_All_Subprograms
        and then
         (CU_Kind in A_Library_Unit_Body or else
          CU_Kind in A_Subunit)
      then
         May_Have_Subprogram_Bodies := True;
      end if;

      if Compute_Public_Types and then
         CU_Class not in A_Private_Declaration .. A_Separate_Body
      then

         case CU_Kind is
            when A_Package         |
                 A_Generic_Package =>
               May_Have_Public_Types := True;
            when others =>
               null;
         end case;

      end if;

      if Compute_All_Types then

         case CU_Kind is
            when A_Package                                    |
                 A_Generic_Package                            |
                 A_Procedure_Body .. A_Protected_Body_Subunit =>
               May_Have_Type_Definitions := True;
            when others =>
               null;
         end case;

      end if;

   end Set_Global_Metrics_Flags;

end METRICS.Metric_Definitions;
