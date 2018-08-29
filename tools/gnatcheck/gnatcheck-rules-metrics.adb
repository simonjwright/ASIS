------------------------------------------------------------------------------
--                                                                          --
--                          GNATCHECK COMPONENTS                            --
--                                                                          --
--              G N A T C H E C K . R U L E S . M E T R I C S               --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                     Copyright (C) 2008-2017, AdaCore                     --
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

with Ada.Characters.Handling;     use Ada.Characters.Handling;

with Asis.Elements;               use Asis.Elements;

with ASIS_UL.Metrics.Compute;     use ASIS_UL.Metrics.Compute;
with ASIS_UL.Metrics.Definitions; use ASIS_UL.Metrics.Definitions;
with ASIS_UL.Output;              use ASIS_UL.Output;
with ASIS_UL.Utilities;           use ASIS_UL.Utilities;

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

   function Check_LSLOC_On_This (Element : Asis.Element) return Boolean;
   --  Checks if Metrics_LSLOC rule should be checked for this Element
   --  according to rule settings.

   Exceptions_Enabled : Boolean := False;

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

   -----------------------------------------
   -- Check_LSLOC_On_This (Metrics_LSLOC) --
   -----------------------------------------

   function Check_LSLOC_On_This (Element : Asis.Element) return Boolean is
      Result : Boolean := False;
      Arg_Kind : constant Declaration_Kinds := Declaration_Kind (Element);
   begin
      if Exceptions_Enabled then
         if Metrics_LSLOC_Rule.Subprograms_Only then
            Result := Arg_Kind in
              A_Procedure_Body_Declaration |
              A_Function_Body_Declaration;
         end if;
      else
         Result := Is_Program_Unit (Element);
      end if;

      return Result;
   end Check_LSLOC_On_This;

   --------------------------------
   -- Print_Rule (Metrics_LSLOC) --
   --------------------------------

   overriding procedure Print_Rule
     (Rule         : Metrics_LSLOC_Rule_Type;
      Indent_Level : Natural := 0)
   is
   begin
      Print_Rule (Rule         => One_Integer_Parameter_Rule_Template (Rule),
                  Indent_Level => Indent_Level);

      if Rule.Subprograms_Only then
         Report_No_EOL (", Subprograms");
      end if;
   end Print_Rule;

   ----------------------------------------
   -- Print_Rule_To_File (Metrics_LSLOC) --
   ----------------------------------------

   overriding procedure Print_Rule_To_File
     (Rule         : Metrics_LSLOC_Rule_Type;
      Rule_File    : File_Type;
      Indent_Level : Natural := 0)
   is
   begin
      Print_Rule_To_File
        (Rule         => One_Integer_Parameter_Rule_Template (Rule),
         Rule_File    => Rule_File,
         Indent_Level => Indent_Level);

      if Rule.Subprograms_Only then
         Put (Rule_File, ", Subprograms");
      end if;
   end Print_Rule_To_File;

   --------------------------------------------
   -- Process_Rule_Parameter (Metrics_LSLOC) --
   --------------------------------------------

   overriding procedure Process_Rule_Parameter
     (Rule       : in out Metrics_LSLOC_Rule_Type;
      Param      :        String;
      Enable     :        Boolean;
      Defined_At : String)
   is
      Is_Number  : Boolean := True;
      Norm_Param : String (Param'Range);
   begin
      if Param = "" then

         if Enable then
            Error ("(" & Rule.Name.all & ") parameter is required for +R");
         else
            Rule.Rule_State       := Disabled;
            Rule.Subprograms_Only := False;
            Exceptions_Enabled    := False;
         end if;

      else

         if Enable then

            for J in Param'Range loop
               if not Is_Digit (Param (J)) then
                  Is_Number := False;
                  exit;
               end if;
            end loop;

            if Is_Number then
               Process_Rule_Parameter
                 (Rule       => One_Integer_Parameter_Rule_Template (Rule),
                  Param      => Param,
                  Enable     => True,
                  Defined_At => Defined_At);
            else
               --  Note that these parameters do not enable the rule!!!
               Norm_Param := To_Lower (Param);

               if Norm_Param = "subprograms" then
                  Exceptions_Enabled    := True;
                  Rule.Subprograms_Only := True;
               else
                  Error ("wrong parameter (" & Param & ") for rule " &
                         Rule.Name.all);

               end if;
            end if;
         else
            Error ("(" & Rule.Name.all & ") no parameter allowed for -R");
         end if;

      end if;

   end Process_Rule_Parameter;

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

      if Check_LSLOC_On_This (Element) then
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
