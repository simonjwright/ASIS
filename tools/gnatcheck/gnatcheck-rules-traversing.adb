------------------------------------------------------------------------------
--                                                                          --
--                          GNATCHECK COMPONENTS                            --
--                                                                          --
--           G N A T C H E C K . R U L E S. T R A V E R S I N G             --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                     Copyright (C) 2004-2016, AdaCore                     --
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

with Asis.Elements;                    use Asis.Elements;
with Asis.Exceptions;
with Asis.Declarations;                use Asis.Declarations;

with ASIS_UL.Common;                   use ASIS_UL.Common;
with ASIS_UL.Debug;                    use ASIS_UL.Debug;
with ASIS_UL.Global_State;             use ASIS_UL.Global_State;
with ASIS_UL.Global_State.CG;
with ASIS_UL.Options;                  use ASIS_UL.Options;
with ASIS_UL.Output;                   use ASIS_UL.Output;

with Gnatcheck.Diagnoses;              use Gnatcheck.Diagnoses;
with Gnatcheck.Ids;                    use Gnatcheck.Ids;
with Gnatcheck.Options;                use Gnatcheck.Options;
with Gnatcheck.Rules.Output;           use Gnatcheck.Rules.Output;
with Gnatcheck.Rules.Rule_Table;       use Gnatcheck.Rules.Rule_Table;
with Gnatcheck.Source_Checks;
with Gnatcheck.Traversal_Stack;

with ASIS_UL.Global_State.Utilities;   use ASIS_UL.Global_State.Utilities;

package body Gnatcheck.Rules.Traversing is

   -----------------------
   -- Local subprograms --
   -----------------------

   procedure Check_Node_For_Global_Rules
     (N : ASIS_UL.Global_State.GS_Node_Id);
   --  Incapsulates all the global rule checks that can be done on node by
   --  node basis

--   Definition          : Asis.Element;
--   Is_Global_Reference : Boolean;
--   Reference_Kind      : ASIS_UL.Global_State.Reference_Kinds;
   --  These variables are used for collecting the global information about
   --  data objects/ We define them as global for Pre_Operations because of
   --  performance reasons (to avoid their allocation for each identifier
   --  element being visited during traversal)

   ----------------------
   -- All_Rules_Pre_Op --
   ----------------------

   procedure All_Rules_Pre_Op
     (Element :        Asis.Element;
      Control : in out Traverse_Control;
      State   : in out Rule_Traversal_State)
   is
      Expanded_Code : Asis.Element;
      --  Used to place the expanded generic spec or body into
   begin

      if Analyse_Source_Text then
         Gnatcheck.Source_Checks.Check_Text_Rules
           (Up_To => Element,
            State => State);
      end if;

      if Is_Exemption_Pragma (Element) then
         Process_Exemption_Pragma (Element);
      end if;

      if Is_Non_Executable_Construct (Element) then
         Increase_Nonexec_Level (State);
      end if;

      if ASIS_UL.Options.Buld_Call_Graph
        and then
         In_Executable_Code (State)
      then

         begin

            ASIS_UL.Global_State.CG.Add_CG_Info (Element);

            for J in First_Rule .. All_Rules.Last loop

               if Is_Enable (All_Rules.Table (J).all)
                 and then
                  All_Rules.Table (J).all in Global_Rule_Template'Class
               then
                  Collect_Global_Info_Pre_Op
                    (Rule => Global_Rule_Template'Class
                               (All_Rules.Table (J).all),
                     Element => Element,
                     Control => Control,
                     State   => State);
               end if;

            end loop;

            --  Disabled for now
--            if ASIS_UL.Options.Collect_Data_Info
--              and then
--               Expression_Kind (Element) = An_Identifier
--            then

--               .Global_State.Data.Check_If_Global_Reference
--                 (Element                => Element,
--                  Definition             => Definition,
--                  Is_Global_Reference    => Is_Global_Reference,
--                  Reference_Kind         => Reference_Kind,
--                  Compute_Reference_Kind => True);

--               if Is_Global_Reference then

--                  ASIS_UL.Global_State.Data.Process_Global_Reference
--                    (Element, Definition, Reference_Kind);
--               end if;

--            end if;

         exception
            when ASIS_UL.Common.Non_Implemented_Error =>
               Set_Source_Status (State.SF, Error_Detected);

               Store_Diagnosis
                 (Text           => Build_GNAT_Location (Element) & ": " &
                                    Tool_Name.all & " internal error",
                  Diagnosis_Kind => Compiler_Error,
                  SF             => State.SF);

               Reset_State (State);

               Error ("global state construction fails " &
                      "because of non-implemented feature");
               Error (Build_GNAT_Location (Element));

            when Ex : Asis.Exceptions.ASIS_Inappropriate_Context          |
                      Asis.Exceptions.ASIS_Inappropriate_Container        |
                      Asis.Exceptions.ASIS_Inappropriate_Compilation_Unit |
                      Asis.Exceptions.ASIS_Inappropriate_Element          |
                      Asis.Exceptions.ASIS_Inappropriate_Line             |
                      Asis.Exceptions.ASIS_Inappropriate_Line_Number      |
                      Asis.Exceptions.ASIS_Failed                         =>

               Set_Source_Status (State.SF, Error_Detected);

               Store_Diagnosis
                 (Text           => Build_GNAT_Location (Element) & ": " &
                                    Tool_Name.all & " internal error",
                  Diagnosis_Kind => Compiler_Error,
                  SF             => State.SF);

               Reset_State (State);

               Error ("ASIS failure in pre-op when constructing global " &
                      "structure, report to report@adacore.com");
               Error (Build_GNAT_Location (Element));

               ASIS_UL.Output.Report_Unhandled_ASIS_Exception (Ex);

            when Ex : others =>
               Set_Source_Status (State.SF, Error_Detected);

               Store_Diagnosis
                 (Text           => Build_GNAT_Location (Element) & ": " &
                                    Tool_Name.all & " internal error",
                  Diagnosis_Kind => Compiler_Error,
                  SF             => State.SF);

               Reset_State (State);

               Error ("pre-operation failed when constructing global " &
                      "structure, report to report@adacore.com");
               Error (Build_GNAT_Location (Element));

               ASIS_UL.Output.Report_Unhandled_Exception (Ex);
               raise Fatal_Error;
         end;

      end if;

      for J in First_Rule .. All_Rules.Last loop

         begin

            if Is_Enable (All_Rules.Table (J).all) then

               if not Is_Part_Of_Instance (Element)
                 or else
                  Checked_On_Expanded_Code (All_Rules.Table (J).all)
               then
                  Reset_State (State);

                  Rule_Check_Pre_Op
                    (Rule    => All_Rules.Table (J).all,
                     Element => Element,
                     Control => Control,
                     State   => State);

                  if State.Detected then
                     Report_Detection
                       (For_Rule      => J,
                        On            => Element,
                        In_SF         => State.SF,
                        Justification => Exemption_Justification (J),
                        Diagnosis_Num => State.Diagnosis,
                        Diag_Actuals  => State.Diag_Params,
                        Diag_Line     => State.Line,
                        Diag_Column   => State.Column);
                  end if;

               end if;

            end if;

         exception
            when ASIS_UL.Common.Non_Implemented_Error =>
               Set_Source_Status (State.SF, Error_Detected);

               Store_Diagnosis
                 (Text           => Build_GNAT_Location (Element) & ": " &
                                    Tool_Name.all & " internal error",
                  Diagnosis_Kind => Compiler_Error,
                  SF             => State.SF);

               Reset_State (State);

               Error
                 ("(" & All_Rules.Table (J).Name. all & ") check fails " &
                  "because of non-implemented feature");
               Error (Build_GNAT_Location (Element));

            when Ex : Asis.Exceptions.ASIS_Inappropriate_Context          |
                      Asis.Exceptions.ASIS_Inappropriate_Container        |
                      Asis.Exceptions.ASIS_Inappropriate_Compilation_Unit |
                      Asis.Exceptions.ASIS_Inappropriate_Element          |
                      Asis.Exceptions.ASIS_Inappropriate_Line             |
                      Asis.Exceptions.ASIS_Inappropriate_Line_Number      |
                      Asis.Exceptions.ASIS_Failed                         =>

               Set_Source_Status (State.SF, Error_Detected);

               Store_Diagnosis
                 (Text           => Build_GNAT_Location (Element) & ": " &
                                    Tool_Name.all & " internal error",
                  Diagnosis_Kind => Compiler_Error,
                  SF             => State.SF);

               Reset_State (State);

               Error
                 ("(" & All_Rules.Table (J).Name. all & ") ASIS failure " &
                  "in pre-operation, report to report@adacore.com");
               Error (Build_GNAT_Location (Element));

               ASIS_UL.Output.Report_Unhandled_ASIS_Exception (Ex);

            when Ex : others =>
               Set_Source_Status (State.SF, Error_Detected);

               Store_Diagnosis
                 (Text           => Build_GNAT_Location (Element) & ": " &
                                    Tool_Name.all & " internal error",
                  Diagnosis_Kind => Compiler_Error,
                  SF             => State.SF);

               Reset_State (State);

               Error
                 ("(" & All_Rules.Table (J).Name. all & ") " &
                  "pre-operation failed, report to report@adacore.com");
               Error (Build_GNAT_Location (Element));

               ASIS_UL.Output.Report_Unhandled_Exception (Ex);
         end;

      end loop;

      Gnatcheck.Traversal_Stack.Push (Element);

      if Declaration_Kind (Element) in
           A_Package_Instantiation .. A_Function_Instantiation
         and then
           Analyse_Expanded_Code
      then
         Expanded_Code := Corresponding_Declaration (Element);

         Check_Rules
           (Element => Expanded_Code,
            Control => Control,
            State   => State);

         Expanded_Code := Corresponding_Body (Element);

         if not Is_Nil (Expanded_Code) then
            Check_Rules
              (Element => Expanded_Code,
               Control => Control,
               State   => State);
         end if;

      end if;

   exception
      when Ex : others =>
         Set_Source_Status (State.SF, Error_Detected);

         Store_Diagnosis
           (Text           => Build_GNAT_Location (Element) & ": " &
                              Tool_Name.all & " internal error",
            Diagnosis_Kind => Compiler_Error,
            SF             => State.SF);

         Error
           ("pre-operation failed (collecting global info?), " &
            "report to report@adacore.com)");
         Error (Build_GNAT_Location (Element));

         ASIS_UL.Output.Report_Unhandled_Exception (Ex);

   end All_Rules_Pre_Op;

   -----------------------
   -- All_Rules_Post_Op --
   -----------------------

   procedure All_Rules_Post_Op
     (Element :        Asis.Element;
      Control : in out Traverse_Control;
      State   : in out Rule_Traversal_State)
   is
   begin

--      if Gnatcheck.Global_State.Buld_Call_Graph
--       and then
--         Declaration_Kind (Element) in
--           A_Package_Instantiation .. A_Function_Instantiation
--      then
--         Expanded_Code := Corresponding_Declaration (Element);

--         Extract_Global_Information
--           (Element => Expanded_Code,
--            Control => Control,
--            State   => State);

--         Expanded_Code := Corresponding_Body (Element);

--         if not Is_Nil (Expanded_Code) then
--            Extract_Global_Information
--              (Element => Expanded_Code,
--               Control => Control,
--               State   => State);
--         end if;

--      end if;

      Gnatcheck.Traversal_Stack.Pop;

      for J in First_Rule .. All_Rules.Last loop

         begin

            if Is_Enable (All_Rules.Table (J).all) then

               if not Is_Part_Of_Instance (Element)
                 or else
                  Checked_On_Expanded_Code (All_Rules.Table (J).all)
               then

                  Reset_State (State);

                  Rule_Check_Post_Op
                    (Rule    => All_Rules.Table (J).all,
                     Element => Element,
                     Control => Control,
                     State   => State);

                  if State.Detected then
                     Report_Detection
                       (For_Rule      => J,
                        On            => Element,
                        In_SF         => State.SF,
                        Justification => Exemption_Justification (J),
                        Diagnosis_Num => State.Diagnosis,
                        Diag_Actuals  => State.Diag_Params,
                        Diag_Line     => State.Line,
                        Diag_Column   => State.Column);
                  end if;

               end if;

            end if;

         exception
            when ASIS_UL.Common.Non_Implemented_Error =>
               Set_Source_Status (State.SF, Error_Detected);

               Store_Diagnosis
                 (Text           => Build_GNAT_Location (Element) & ": " &
                                    Tool_Name.all & " internal error",
                  Diagnosis_Kind => Compiler_Error,
                  SF             => State.SF);

               Reset_State (State);

               Error
                 ("(" & All_Rules.Table (J).Name. all & ") check fails " &
                  "because of non-implemented feature");
               Error (Build_GNAT_Location (Element));

            when Ex : Asis.Exceptions.ASIS_Inappropriate_Context          |
                      Asis.Exceptions.ASIS_Inappropriate_Container        |
                      Asis.Exceptions.ASIS_Inappropriate_Compilation_Unit |
                      Asis.Exceptions.ASIS_Inappropriate_Element          |
                      Asis.Exceptions.ASIS_Inappropriate_Line             |
                      Asis.Exceptions.ASIS_Inappropriate_Line_Number      |
                      Asis.Exceptions.ASIS_Failed                         =>

               Set_Source_Status (State.SF, Error_Detected);

               Store_Diagnosis
                 (Text           => Build_GNAT_Location (Element) & ": " &
                                    Tool_Name.all & " internal error",
                  Diagnosis_Kind => Compiler_Error,
                  SF             => State.SF);

               Reset_State (State);

               Error
                 ("(" & All_Rules.Table (J).Name. all & ") ASIS failure " &
                  "in post-operation, report to report@adacore.com");
               Error (Build_GNAT_Location (Element));

               ASIS_UL.Output.Report_Unhandled_ASIS_Exception (Ex);

            when Ex : others =>
               Set_Source_Status (State.SF, Error_Detected);

               Store_Diagnosis
                 (Text           => Build_GNAT_Location (Element) & ": " &
                                    Tool_Name.all & " internal error",
                  Diagnosis_Kind => Compiler_Error,
                  SF             => State.SF);

               Reset_State (State);

               Error
                 ("(" & All_Rules.Table (J).Name. all & ") " &
                  "post-operation failed, report to report@adacore.com");
               Error (Build_GNAT_Location (Element));

               ASIS_UL.Output.Report_Unhandled_Exception (Ex);
         end;

      end loop;

      if ASIS_UL.Options.Buld_Call_Graph
        and then
         In_Executable_Code (State)
      then
         ASIS_UL.Global_State.CG.Complete_CG_Info (Element);

         for J in First_Rule .. All_Rules.Last loop

            if Is_Enable (All_Rules.Table (J).all)
              and then
               All_Rules.Table (J).all in Global_Rule_Template'Class
            then
               Collect_Global_Info_Post_Op
                 (Rule => Global_Rule_Template'Class
                            (All_Rules.Table (J).all),
                  Element => Element,
                  Control => Control,
                  State   => State);
            end if;

         end loop;

      end if;

      if Is_Non_Executable_Construct (Element) then
         Decrease_Nonexec_Level (State);
      end if;

   exception
      when Ex : others =>
         Set_Source_Status (State.SF, Error_Detected);

         Store_Diagnosis
           (Text           => Build_GNAT_Location (Element) & ": " &
                              Tool_Name.all & " internal error",
            Diagnosis_Kind => Compiler_Error,
            SF             => State.SF);

         Error
           ("post-operation failed (collecting global info?), " &
            "report to report@adacore.com)");
         Error (Build_GNAT_Location (Element));

         ASIS_UL.Output.Report_Unhandled_Exception (Ex);

   end All_Rules_Post_Op;

   ------------------------
   -- Check_Global_Rules --
   ------------------------

   procedure Check_Global_Rules is
   begin

      --  If the following condition is false, we just have nothing to do!

      if ASIS_UL.Options.Buld_Call_Graph then

         --  First, preparing the global structure (the transitive closure of
         --  the call graph has been already done)

         if Debug_Flag_2 then
            Info ("Check global rules - preparing the global structure ... ");
         end if;

         for J in First_Rule .. All_Rules.Last loop

            if All_Rules.Table (J).Rule_State = Enabled
              and then
               All_Rules.Table (J).all in Global_Rule_Template'Class
            then
               Analyze_Global_Structure
                 (Global_Rule_Template'Class (All_Rules.Table (J).all));
            end if;

         end loop;

         if Debug_Flag_2 then
            Info ("...Done");
         end if;

         --  Do the checks that can be done on node by node  basis

         if Debug_Flag_2 then
            Info ("Check global rules - node by node checks ... ");
         end if;

         for J in 1 .. ASIS_UL.Global_State.Last_Node loop

            if not (not Process_RTL_Units
                  and then
                    ASIS_UL.Global_State.Is_RTL_Node (J))
            then
               Check_Node_For_Global_Rules (J);
            end if;

         end loop;

         if Debug_Flag_2 then
            Info ("...Done");
         end if;

         --  ???

      end if;

   end Check_Global_Rules;

   ---------------------------------
   -- Check_Node_For_Global_Rules --
   ---------------------------------

   procedure Check_Node_For_Global_Rules
     (N : ASIS_UL.Global_State.GS_Node_Id)
   is
      Detected : Boolean := False;
   begin
      for J in First_Rule .. All_Rules.Last loop

         if All_Rules.Table (J).all in Global_Rule_Template'Class
           and then
            All_Rules.Table (J).Rule_State = Enabled
         then
            Check_Global_Structure_Node
              (Rule          =>
                Global_Rule_Template'Class (All_Rules.Table (J).all),
               N             => N,
               Detected      => Detected);

            if Detected then
               Report_Global_Rule_Detection
                 (For_Rule      => J,
                  On            => N);
            end if;

         end if;

      end loop;
   end Check_Node_For_Global_Rules;

   -----------------
   -- Reset_State --
   -----------------

   procedure Reset_State (State : in out Rule_Traversal_State) is
   begin
      State.Detected    := False;
      State.Diagnosis   := 0;
      State.Diag_Params := Nil_String_Loc;
      State.Line        := 0;
      State.Column      := 0;
   end Reset_State;

end Gnatcheck.Rules.Traversing;
