------------------------------------------------------------------------------
--                                                                          --
--                      GNAT METRICS TOOLS COMPONENTS                       --
--                                                                          --
--                      M E T R I C S . O P T I O N S                       --
--                                                                          --
--                                 B o d y                                  --
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

package body METRICS.Options is

   ----------------------------
   -- Complexity_Metrics_Off --
   ----------------------------

   procedure Complexity_Metrics_Off is
   begin
      Compute_Cyclomatic_Complexity := False;
      Compute_Essential_Complexity  := False;
      Compute_Average_Complexity    := False;
      Compute_Loop_Nesting          := False;
      Compute_Extra_Exit_Points     := False;
   end Complexity_Metrics_Off;

   ---------------------------
   -- Complexity_Metrics_On --
   ---------------------------

   procedure Complexity_Metrics_On is
   begin
      Compute_Cyclomatic_Complexity := True;
      Compute_Essential_Complexity  := True;
      Compute_Average_Complexity    := True;
      Compute_Loop_Nesting          := True;
      Compute_Extra_Exit_Points     := True;
   end Complexity_Metrics_On;

   ----------------------------
   -- Complexity_Metrics_Set --
   ----------------------------

   function Complexity_Metrics_Set return Boolean is
   begin
      return False
        or else Compute_Cyclomatic_Complexity
        or else Compute_Essential_Complexity
        or else Compute_Loop_Nesting
        or else Compute_Extra_Exit_Points;

   end Complexity_Metrics_Set;

   --------------------------
   -- Contract_Metrics_Off --
   --------------------------

   procedure Contract_Metrics_Off is
   begin
      Compute_Contracts := False;
      Compute_Post      := False;
   end Contract_Metrics_Off;

   -------------------------
   -- Contract_Metrics_On --
   -------------------------

   procedure Contract_Metrics_On is
   begin
      Compute_Contracts := True;
      Compute_Post      := True;
   end Contract_Metrics_On;

   --------------------------
   -- Contract_Metrics_Set --
   --------------------------

   function Contract_Metrics_Set return Boolean is
   begin
      return False
        or else Compute_Contracts
        or else Compute_Post;
   end Contract_Metrics_Set;

   -------------------------
   -- Coupling_Metric_Off --
   -------------------------

   procedure Coupling_Metric_Off is
   begin
      Compute_OO_Package_Efferent_Coupling := False;
      Compute_Category_Efferent_Coupling   := False;
      Compute_OO_Package_Afferent_Coupling := False;
      Compute_Category_Afferent_Coupling   := False;

      Compute_Control_Efferent_Coupling    := False;
      Compute_Control_Afferent_Coupling    := False;

      Compute_Unit_Efferent_Coupling       := False;
      Compute_Unit_Afferent_Coupling       := False;
   end Coupling_Metric_Off;

   ------------------------
   -- Coupling_Metric_On --
   ------------------------

   procedure Coupling_Metric_On is
   begin
      Compute_OO_Package_Efferent_Coupling := True;
      Compute_Category_Efferent_Coupling   := True;
      Compute_OO_Package_Afferent_Coupling := True;
      Compute_Category_Afferent_Coupling   := True;

      Compute_Control_Efferent_Coupling    := True;
      Compute_Control_Afferent_Coupling    := True;

      Compute_Unit_Efferent_Coupling       := True;
      Compute_Unit_Afferent_Coupling       := True;
   end Coupling_Metric_On;

   --------------------------
   -- Coupling_Metrics_Set --
   --------------------------

   function Coupling_Metrics_Set return Boolean is
   begin
      return False
        or else Compute_OO_Package_Efferent_Coupling
        or else Compute_Category_Efferent_Coupling
        or else Compute_OO_Package_Afferent_Coupling
        or else Compute_Category_Afferent_Coupling
        or else Compute_Control_Efferent_Coupling
        or else Compute_Control_Afferent_Coupling
        or else Compute_Unit_Efferent_Coupling
        or else Compute_Unit_Afferent_Coupling;
   end Coupling_Metrics_Set;

   -------------------------
   -- Element_Metrics_Off --
   -------------------------

   procedure Element_Metrics_Off is
   begin
      Compute_All_Statements      := False;
      Compute_All_Declarations    := False;
      Compute_Public_Subprograms  := False;
      Compute_All_Subprograms     := False;
      Compute_Public_Types        := False;
      Compute_All_Types           := False;
      Compute_Progam_Unit_Nesting := False;
      Compute_Construct_Nesting   := False;
      Compute_Spb_Pars_Num        := False;
   end Element_Metrics_Off;

   ------------------------
   -- Element_Metrics_On --
   -------------------------

   procedure Element_Metrics_On is
   begin
      Compute_All_Statements      := True;
      Compute_All_Declarations    := True;
      Compute_Public_Subprograms  := True;
      Compute_All_Subprograms     := True;
      Compute_Public_Types        := True;
      Compute_All_Types           := True;
      Compute_Progam_Unit_Nesting := True;
      Compute_Construct_Nesting   := True;
      Compute_Spb_Pars_Num        := True;
   end Element_Metrics_On;

   -------------------------
   -- Element_Metrics_Set --
   -------------------------

   function Element_Metrics_Set return Boolean is
   begin
      return Global_Element_Metrics_Set
         or else Compute_Progam_Unit_Nesting
         or else Compute_Construct_Nesting
         or else Compute_Spb_Pars_Num;

   end Element_Metrics_Set;

   --------------------------------
   -- Global_Element_Metrics_Set --
   --------------------------------

   function Global_Element_Metrics_Set return Boolean is
   begin
      return Compute_All_Statements
         or else Compute_All_Declarations
         or else Compute_Public_Subprograms
         or else Compute_All_Subprograms
         or else Compute_Public_Types
         or else Compute_All_Types;

   end Global_Element_Metrics_Set;

   ----------------------
   -- Line_Metrics_Off --
   ----------------------

   procedure Line_Metrics_Off is
   begin
      Compute_All_Lines               := False;
      Compute_Code_Lines              := False;
      Compute_Comment_Lines           := False;
      Compute_EOL_Comments            := False;
      Compute_Blank_Lines             := False;
      Compute_Comment_Code_Ratio      := False;
      Compute_Average_Lines_In_Bodies := False;
   end Line_Metrics_Off;

   ---------------------
   -- Line_Metrics_On --
   ---------------------

   procedure Line_Metrics_On is
   begin
      Compute_All_Lines               := True;
      Compute_Code_Lines              := True;
      Compute_Comment_Lines           := True;
      Compute_EOL_Comments            := True;
      Compute_Blank_Lines             := True;
      Compute_Comment_Code_Ratio      := True;
      Compute_Average_Lines_In_Bodies := True;
   end Line_Metrics_On;

   ----------------------
   -- Line_Metrics_Set --
   ----------------------

   function Line_Metrics_Set return Boolean is
   begin
      return Compute_All_Lines or else Selective_Line_Metrics_Set;
   end Line_Metrics_Set;

   -----------------------------
   -- OO_Coupling_Metrics_Set --
   -----------------------------

   function OO_Coupling_Metrics_Set return Boolean is
   begin
      return False
        or else Compute_OO_Package_Efferent_Coupling
        or else Compute_Category_Efferent_Coupling
        or else Compute_OO_Package_Afferent_Coupling
        or else Compute_Category_Afferent_Coupling;
   end OO_Coupling_Metrics_Set;

   --------------------------------
   -- Selective_Line_Metrics_Set --
   --------------------------------

   function Selective_Line_Metrics_Set return Boolean is
   begin
      return False
         or else Compute_Code_Lines
         or else Compute_Comment_Lines
         or else Compute_EOL_Comments
         or else Compute_Blank_Lines
         or else Compute_Comment_Code_Ratio;
   end Selective_Line_Metrics_Set;

   ----------------------
   -- Unit_Metrics_Set --
   ----------------------

   function Unit_Metrics_Set return Boolean is
   begin
      return Line_Metrics_Set
          or else Complexity_Metrics_Set
          or else Element_Metrics_Set;
   end Unit_Metrics_Set;

end METRICS.Options;
