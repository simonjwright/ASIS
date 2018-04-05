------------------------------------------------------------------------------
--                                                                          --
--                          GNATCHECK COMPONENTS                            --
--                                                                          --
--              G N A T C H E C K . R U L E S . M E T R I C S               --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                     Copyright (C) 2008-2016, AdaCore                     --
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

pragma Ada_2012;

with ASIS_UL.Metrics.Compute;        use ASIS_UL.Metrics.Compute;
with ASIS_UL.Metrics.Definitions;    use ASIS_UL.Metrics.Definitions;
with ASIS_UL.Utilities;              use ASIS_UL.Utilities;

package body Gnatcheck.Rules.Metrics is

   Complexity_Metrics : Complexity_Metric_Counter;
   --  This variable collects complexity metrics. We make it global to allow to
   --  call the routine that computes all the complexity metrics for a given
   --  element only once and then to use the results for both cyclomatic
   --  complexity and essential complexity checks.

   -----------------------------------
   -- Metrics_Cyclomatic_Complexity --
   -----------------------------------

   -------------------------------------------------------
   -- Rule_Check_Pre_Op (Metrics_Cyclomatic_Complexity) --
   -------------------------------------------------------

   procedure Rule_Check_Pre_Op
     (Rule    : in out Metrics_Cyclomatic_Complexity_Rule_Type;
      Element :        Asis.Element;
      Control : in out Traverse_Control;
      State   : in out Rule_Traversal_State)
   is
      pragma Unreferenced (Control);
      Cyclomatic_Complexity : Metric_Count;
   begin

      if Is_Executable_Body (Element) then
         Compute_Complexity_Metrics (Element, Complexity_Metrics);
         Cyclomatic_Complexity := Complexity_Metrics.Statement_Complexity +
            Complexity_Metrics.Expression_Complexity;

         if Cyclomatic_Complexity > Metric_Count (Rule.Rule_Limit) then
            State.Detected    := True;
            State.Diag_Params :=
              Enter_String ("%1%" & Cyclomatic_Complexity'Img);
         end if;

      end if;

   end Rule_Check_Pre_Op;

   -----------------------------------------------
   -- Init_Rule (Metrics_Cyclomatic_Complexity) --
   -----------------------------------------------

   procedure
     Init_Rule (Rule : in out Metrics_Cyclomatic_Complexity_Rule_Type)
   is
   begin
      Init_Rule (One_Integer_Parameter_Rule_Template (Rule));
      --  Common rule fields:
      Rule.Name       := new String'("Metrics_Cyclomatic_Complexity");
      Rule.Rule_Status := Fully_Implemented;
      Rule.Help_Info  := new String'("(metrics) high cyclomatic complexity");
      Rule.Diagnosis  := new String'("cyclomatic complexity is too high: %1%");
   end Init_Rule;

   ----------------------------------
   -- Metrics_Essential_Complexity --
   ----------------------------------

   ------------------------------------------------------
   -- Rule_Check_Pre_Op (Metrics_Essential_Complexity) --
   ------------------------------------------------------

   procedure Rule_Check_Pre_Op
     (Rule    : in out Metrics_Essential_Complexity_Rule_Type;
      Element :        Asis.Element;
      Control : in out Traverse_Control;
      State   : in out Rule_Traversal_State)
   is
      pragma Unreferenced (Control);
   begin
      if Is_Executable_Body (Element) then

         if not Is_Enable (Metrics_Cyclomatic_Complexity_Rule) then
            Compute_Complexity_Metrics (Element, Complexity_Metrics);
         end if;

         if Complexity_Metrics.Essential_Complexity >
              Metric_Count (Rule.Rule_Limit)
         then
            State.Detected    := True;
            State.Diag_Params :=
              Enter_String ("%1%" &
                            Complexity_Metrics.Essential_Complexity'Img);
         end if;
      end if;

   end Rule_Check_Pre_Op;

   ----------------------------------------------
   -- Init_Rule (Metrics_Essential_Complexity) --
   ----------------------------------------------

   procedure Init_Rule
     (Rule : in out Metrics_Essential_Complexity_Rule_Type)
   is
   begin
      Init_Rule (One_Integer_Parameter_Rule_Template (Rule));

      --  Common rule fields:
      Rule.Name        := new String'("Metrics_Essential_Complexity");
      Rule.Rule_Status := Fully_Implemented;
      Rule.Help_Info   := new String'("(metrics) high essential complexity");
      Rule.Diagnosis   := new String'("essential complexity is too high: %1%");
   end Init_Rule;

   -------------------
   -- Metrics_LSLOC --
   -------------------

   -------------------------------------------
   -- Activate_In_Test_Mode (Metrics_LSLOC) --
   -------------------------------------------

   overriding procedure Activate_In_Test_Mode
     (Rule : in out Metrics_LSLOC_Rule_Type)
   is
   begin
      Process_Rule_Parameter
        (Rule       => Rule,
         Param      => "30",
         Enable     => True,
         Defined_At => "");
   end Activate_In_Test_Mode;

   ---------------------------------------
   -- Rule_Check_Pre_Op (Metrics_LSLOC) --
   ---------------------------------------

   procedure Rule_Check_Pre_Op
     (Rule    : in out Metrics_LSLOC_Rule_Type;
      Element :        Asis.Element;
      Control : in out Traverse_Control;
      State   : in out Rule_Traversal_State)
   is
      pragma Unreferenced (Control);
      Counter   : Syntax_Metric_Counter;
      LSLOC     : Metric_Count;
   begin

      if Is_Program_Unit (Element) then
         Compute_Syntaxt_Metrics (Element, Counter);
         LSLOC := Counter.All_Statements + Counter.All_Declarations;

         if LSLOC > Metric_Count (Rule.Rule_Limit) then
            State.Detected    := True;
            State.Diag_Params := Enter_String ("%1%" & LSLOC'Img);
         end if;

      end if;

   end Rule_Check_Pre_Op;

   -------------------------------
   -- Init_Rule (Metrics_LSLOC) --
   -------------------------------

   procedure Init_Rule (Rule : in out Metrics_LSLOC_Rule_Type) is
   begin
      Init_Rule (One_Integer_Parameter_Rule_Template (Rule));

      --  Common rule fields:
      Rule.Name        := new String'("Metrics_LSLOC");
      Rule.Rule_Status := Fully_Implemented;
      Rule.Help_Info   := new String'("(metrics) high LSLOC value");
      Rule.Diagnosis   := new String'("LSLOC is too high: %1%");
   end Init_Rule;

end Gnatcheck.Rules.Metrics;
