------------------------------------------------------------------------------
--                                                                          --
--                          GNATCHECK COMPONENTS                            --
--                                                                          --
--               G N A T C H E C K . R U L E S . G L O B A L                --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                     Copyright (C) 2009-2018, AdaCore                     --
--                                                                          --
-- GNATCHECK  is  free  software;  you can redistribute it and/or modify it --
-- under terms of the  GNU  General Public License as published by the Free --
-- Software Foundation;  either version 3, or ( at your option)  any  later --
-- version.  GNATCHECK  is  distributed in the hope that it will be useful, --
-- but  WITHOUT  ANY  WARRANTY;   without  even  the  implied  warranty  of --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General --
-- Public License for more details.  You should have received a copy of the --
-- GNU General Public License distributed with GNAT; see file  COPYING3. If --
-- not,  go  to  http://www.gnu.org/licenses  for  a  complete  copy of the --
-- license.                                                                 --
--                                                                          --
-- GNATCHECK is maintained by AdaCore (http://www.adacore.com).             --
--                                                                          --
------------------------------------------------------------------------------

pragma Ada_2012;

with Ada.Characters.Handling;    use Ada.Characters.Handling;
with Ada.Strings.Fixed;          use Ada.Strings.Fixed;

with Asis.Elements;                  use Asis.Elements;

with Ada.Containers;                 use Ada.Containers;

with ASIS_UL.Global_State;           use ASIS_UL.Global_State;
with ASIS_UL.Global_State.CG;        use ASIS_UL.Global_State.CG;
with ASIS_UL.Global_State.CG.Conditions;
use  ASIS_UL.Global_State.CG.Conditions;
with ASIS_UL.Misc;                   use ASIS_UL.Misc;
with ASIS_UL.Options;                use ASIS_UL.Options;
with ASIS_UL.Output;                 use ASIS_UL.Output;
with ASIS_UL.Utilities;              use ASIS_UL.Utilities;

package body Gnatcheck.Rules.Global is

   -----------------------------------
   -- Usage node flags in gnatcheck --
   -----------------------------------

   --  Application_Flag_1 - inlined subprograms
   --  ...  -   ....

   ----------------------------------------
   --  Condition-related node properties --
   ----------------------------------------

   procedure Set_Is_Inlined (N : GS_Node_Id);
   --  Marks N as inlined subprogram

   function Is_Inlined (N : GS_Node_Id) return Boolean;
   --  Checks if N represents an inlined subprogram

   ----------------------------------
   -- Bodies for local subprograms --
   ----------------------------------

   ----------------
   -- Is_Inlined --
   ----------------

   function Is_Inlined (N : GS_Node_Id) return Boolean renames
     ASIS_UL.Global_State.Get_Application_Flag_1;

   --------------------
   -- Set_Is_Inlined --
   --------------------

   procedure Set_Is_Inlined (N : GS_Node_Id) is
   begin
      ASIS_UL.Global_State.Set_Application_Flag_1 (N, True);
   end Set_Is_Inlined;

   ----------------------------------
   -- Bodies for rules' operations --
   ----------------------------------

   ----------------------------
   -- Deeply_Nested_Inlining --
   ----------------------------

   procedure Try_Call_Depth
     (Start_From :        GS_Node_Id;
      Length     :        Positive;
      Success    : in out Boolean);
   --  This procedure tries to check if there exist a non-recursive call chain
   --  that starts from Start_From node, that consists of inlined nodes only
   --  and that is at least Length long. If this is the case, Success is set
   --  ON, otherwise it is set OFF.

   Already_In_Chain : Node_Lists.Set;
   --  Set that contains nodes that are already in call chain, used to avoid
   --  recursive chains

   ----------------------------------------------------
   -- Activate_In_Test_Mode (Deeply_Nested_Inlining) --
   ----------------------------------------------------

   overriding procedure Activate_In_Test_Mode
     (Rule : in out Deeply_Nested_Inlining_Rule_Type)
   is
   begin
      Process_Rule_Parameter
        (Rule       => Rule,
         Param      => "3",
         Enable     => True,
         Defined_At => "");
   end Activate_In_Test_Mode;

   ------------------------------------------------
   -- More_Rule_Comment (Deeply_Nested_Inlining) --
   ------------------------------------------------

   function More_Rule_Comment
     (Rule          : Deeply_Nested_Inlining_Rule_Type;
      Template_Kind : Template_Coding_Standard_Kinds)
      return          String
   is
      pragma Unreferenced (Rule);
   begin
      if Template_Kind = Template_All_ON then
         return "possibly meaningless default parameter used!";
      else
         return "provide a proper parameter value if the rule is enabled!";
      end if;
   end More_Rule_Comment;

   -------------------------------------------------
   -- Print_Rule_To_File (Deeply_Nested_Inlining) --
   -------------------------------------------------

   overriding procedure Print_Rule_To_File
     (Rule         : Deeply_Nested_Inlining_Rule_Type;
      Rule_File    : File_Type;
      Indent_Level : Natural := 0)
   is
   begin
      Print_Rule_To_File (Rule_Template (Rule), Rule_File, Indent_Level);
      Put (Rule_File, ": " & Image (Rule.Rule_Limit));
   end Print_Rule_To_File;

   -----------------------------------------------------
   -- Process_Rule_Parameter (Deeply_Nested_Inlining) --
   -----------------------------------------------------

   procedure Process_Rule_Parameter
   (Rule       : in out Deeply_Nested_Inlining_Rule_Type;
    Param      :        String;
    Enable     :        Boolean;
    Defined_At : String)
   is
   begin
      if Param = "" then

         if Enable then
            Error ("(" & Rule.Name.all & ") parameter is required for +R");
         else
            Rule.Rule_State := Disabled;
         end if;

      else

         if Enable then

            if Gnatcheck.Options.Check_Param_Redefinition
              and then
               Rule.Rule_State = Enabled
            then
               Error
                ("redefining at " &
                 (if Defined_At = "" then
                     "command line"
                  else
                     Defined_At) &
                 " parameter for rule " & Rule.Name.all &
                 " defined at "  &
                 (if Rule.Defined_At = Nil_String_Loc then
                     "command line"
                  else
                     Get_String (Rule.Defined_At)));
            end if;

            begin
               Rule.Rule_Limit := Positive'Value (Param);
               Rule.Rule_State := Enabled;
               Rule.Defined_At := Enter_String (Defined_At);
            exception
               when Constraint_Error =>
                  Error ("(" & Rule.Name.all & ") wrong parameter: " & Param);
            end;

         else
            Error ("(" & Rule.Name.all & ") no parameter allowed for -R");
         end if;

      end if;
   end Process_Rule_Parameter;

   ------------------------------------------
   -- Rule_Option (Deeply_Nested_Inlining) --
   ------------------------------------------

   function Rule_Option
     (Rule          : Deeply_Nested_Inlining_Rule_Type;
      Template_Kind : Template_Coding_Standard_Kinds)
      return          String
   is
   begin
      if Template_Kind = Template_All_ON then
         return Rule_Option (Rule_Template (Rule), Template_Kind) & " : 1";
      else
         return Rule_Option (Rule_Template (Rule), Template_Kind);
      end if;
   end Rule_Option;

   ----------------------------------------------------
   -- Init_Global_Structure (Deeply_Nested_Inlining) --
   ----------------------------------------------------

   procedure Init_Global_Structure (Rule : Deeply_Nested_Inlining_Rule_Type)
   is
      pragma Unreferenced (Rule);
   begin
      ASIS_UL.Global_State.CG.Conditions.Set_Condition (Inlined_Subprograms);
   end Init_Global_Structure;

   ---------------------------------------------------------
   -- Collect_Global_Info_Pre_Op (Deeply_Nested_Inlining) --
   ---------------------------------------------------------

   procedure Collect_Global_Info_Pre_Op
     (Rule    : in out Deeply_Nested_Inlining_Rule_Type;
      Element :        Asis.Element;
      Control : in out Traverse_Control;
      State   : in out Rule_Traversal_State)
   is
      pragma Unreferenced (Rule, Control, State);
   begin

      if Declaration_Kind (Element) not in
          A_Generic_Procedure_Declaration .. A_Generic_Function_Declaration
       and then
         Has_Pragma_Inline (Element)
      then
         Set_Is_Inlined (Corresponding_Node (Element));
      end if;

   end Collect_Global_Info_Pre_Op;

   ----------------------------------------------------------
   -- Check_Global_Structure_Node (Deeply_Nested_Inlining) --
   ----------------------------------------------------------

   procedure Check_Global_Structure_Node
     (Rule          :     Deeply_Nested_Inlining_Rule_Type;
      N             :     ASIS_UL.Global_State.GS_Node_Id;
      Detected      : out Boolean)
   is
   begin
      Detected := False;

      if Is_Callable_Node (N)
        and then
         Is_Inlined (N)
        and then
         SLOC_Node_Lists.Length (Direct_Calls (N).all) > 0
      then

         if not Transitive_Closure_Done
          and then
            Node_Lists.Length (All_Calls (N).all) = 0
         then
            Close_Node (N);
         end if;

         if Positive (Node_Lists.Length (All_Calls (N).all)) >
           Rule.Rule_Limit
         then
            --  We have a node that calls more than Rule.Rule_Limit other
            --  nodes. Only in this case we have a chance to exceed the limit
            --  set by the rule.
            Node_Lists.Clear (Already_In_Chain);

            Try_Call_Depth
               (Start_From => N,
                Length     => Rule.Rule_Limit + 1,
                Success    => Detected);
         end if;

         if not Detected then
            --  Check if the call graph info for this node is complete:
            --  to be continued...
            Check_Node_Completeness (N);
         end if;
      end if;

   end Check_Global_Structure_Node;

   ----------------------------------------------------
   -- Init_Global_Structure (Deeply_Nested_Inlining) --
   ----------------------------------------------------

   procedure Init_Rule (Rule : in out Deeply_Nested_Inlining_Rule_Type) is
   begin
      Init_Rule (Global_Rule_Template (Rule));

      Rule.Name        := new String'("Deeply_Nested_Inlining");
      Rule.Rule_Status := Fully_Implemented;
      Rule.Help_Info   := new String'("deeply nested inlining (global check)");
      Rule.Diagnosis   := new String'("deeply nested inlining");
   end Init_Rule;

   ---------------------------------------------
   -- Try_Call_Depth (Deeply_Nested_Inlining) --
   ---------------------------------------------

   procedure Try_Call_Depth
     (Start_From :       GS_Node_Id;
      Length     :        Positive;
      Success    : in out Boolean)
   is
      Next_Call : SLOC_Node_Lists.Cursor;
   begin

      Next_Call := SLOC_Node_Lists.First (Direct_Calls (Start_From).all);

      while SLOC_Node_Lists.Has_Element (Next_Call) loop

         if (Is_Inlined (SLOC_Node_Lists.Element (Next_Call).Node)
            or else
             GS_Node_Kind (SLOC_Node_Lists.Element (Next_Call).Node) in
               A_Type_Discr_Init_Procedure | A_Type_Init_Procedure)
           and then
            not Node_Lists.Contains
                  (Already_In_Chain,
                   SLOC_Node_Lists.Element (Next_Call).Node)
         then
            if Length = 1 then
               Success := True;
            else
               Node_Lists.Insert
                 (Already_In_Chain, SLOC_Node_Lists.Element (Next_Call).Node);

               Try_Call_Depth
                  (Start_From => SLOC_Node_Lists.Element (Next_Call).Node,
                   Length     => Length - 1,
                   Success    => Success);
            end if;

            exit when Success;
         end if;

         Next_Call := SLOC_Node_Lists.Next (Next_Call);
      end loop;

   end Try_Call_Depth;

   --------------------
   -- XML_Print_Rule --
   --------------------

   overriding procedure XML_Print_Rule
     (Rule         : Deeply_Nested_Inlining_Rule_Type;
      Indent_Level : Natural := 0)
   is
   begin
      XML_Report
        ("<rule id=""" & Rule_Name (Rule) & """>",
         Indent_Level);

      XML_Report
        ("<parameter>" & Image (Rule.Rule_Limit) & "</parameter>",
         Indent_Level + 1);

      XML_Report ("</rule>", Indent_Level);
   end XML_Print_Rule;

   --------------------------------------------
   -- XML_Rule_Help (Deeply_Nested_Inlining) --
   --------------------------------------------

   procedure XML_Rule_Help
     (Rule  : Deeply_Nested_Inlining_Rule_Type;
      Level : Natural)
   is
   begin
      Info (Level * Ident_String             &
            "<spin switch=""+R"              &
            Rule.Name.all                    &
            """ label="""                    &
            Rule.Help_Info.all               &
            """ min="""                      &
            Image (1)                        &
            """ max=""99999"""               &
            " default="""                    &
            Image (0)                        &
            """ separator="":"""             &
            "/>");
   end XML_Rule_Help;

   ---------------------------
   -- Recursive_Subprograms --
   ---------------------------

   ---------------------------------------------------------
   -- Check_Global_Structure_Node (Recursive_Subprograms) --
   ---------------------------------------------------------

   procedure Check_Global_Structure_Node
     (Rule          :     Recursive_Subprograms_Rule_Type;
      N             :     ASIS_UL.Global_State.GS_Node_Id;
      Detected      : out Boolean)
   is
      pragma Unreferenced (Rule);
   begin

      Detected :=
         (ASIS_UL.Global_State.GS_Node_Kind (N) = A_Procedure
          or else
          ASIS_UL.Global_State.GS_Node_Kind (N) = A_Function)
        and then
         ASIS_UL.Global_State.CG.Is_Recursive_Node (N);

   end Check_Global_Structure_Node;

   ---------------------------------------------------
   -- Init_Global_Structure (Recursive_Subprograms) --
   ---------------------------------------------------

   procedure Init_Global_Structure (Rule : Recursive_Subprograms_Rule_Type) is
   begin
      ASIS_UL.Global_State.CG.Conditions.Set_Unconditional_Call_Graph (True);
      ASIS_UL.Options.Do_Transitive_Closure := True;

      if Rule.Skip_Dispatching_Calls then
         Skip_Dispatching_Calls := True;
      end if;
   end Init_Global_Structure;

   ---------------------------------------
   -- Init_Rule (Recursive_Subprograms) --
   ---------------------------------------

   procedure Init_Rule (Rule : in out Recursive_Subprograms_Rule_Type) is
   begin
      Init_Rule (Global_Rule_Template (Rule));

      Rule.Name        := new String'("Recursive_Subprograms");
      Rule.Rule_Status := Fully_Implemented;
      Rule.Help_Info   := new String'("recursion (call graph cycles)");
      Rule.Diagnosis   := new String'("recursive subprogram");
   end Init_Rule;

   ------------------------------------------------
   -- Print_Rule_To_File (Recursive_Subprograms) --
   ------------------------------------------------

   overriding procedure Print_Rule_To_File
     (Rule         : Recursive_Subprograms_Rule_Type;
      Rule_File    : File_Type;
      Indent_Level : Natural := 0)
   is
   begin
      Print_Rule_To_File (Rule_Template (Rule), Rule_File, Indent_Level);

      if Rule.Skip_Dispatching_Calls then
         Put (Rule_File, ": Skip_Dispatching_Calls");
      end if;

   end Print_Rule_To_File;

   ----------------------------------------------------
   -- Process_Rule_Parameter (Recursive_Subprograms) --
   ----------------------------------------------------

   overriding procedure Process_Rule_Parameter
    (Rule       : in out Recursive_Subprograms_Rule_Type;
     Param      :        String;
     Enable     :        Boolean;
     Defined_At : String)
   is
   begin
      if Param = "" then

         if Enable then
            Rule.Rule_State := Enabled;
            Rule.Defined_At := Enter_String (Defined_At);
         else
            Rule.Skip_Dispatching_Calls := False;
            Rule.Rule_State             := Disabled;
         end if;

      else

         if Enable then

            if To_Lower (Param) = "skip_dispatching_calls" then

               if Gnatcheck.Options.Check_Param_Redefinition
                 and then
                  Rule.Rule_State = Enabled
                 and then
                  Rule.Skip_Dispatching_Calls = True
               then
                  Error
                   ("redefining at " &
                    (if Defined_At = "" then
                        "command line"
                     else
                        Defined_At) &
                    " exception case for rule " & Rule.Name.all &
                    " defined at "  &
                    (if Rule.Defined_At = Nil_String_Loc then
                        "command line"
                     else
                        Get_String (Rule.Defined_At)));
               end if;

               Rule.Skip_Dispatching_Calls := True;
               Rule.Rule_State := Enabled;
               Rule.Defined_At := Enter_String (Defined_At);

            else
               Error ("(" & Rule.Name.all & ") wrong parameter: " &
                      Param);
               Rule.Rule_State := Disabled;
            end if;

         else
            Error ("(" & Rule.Name.all & ") no parameter allowed for -R");
            Rule.Skip_Dispatching_Calls := False;
            Rule.Rule_State := Disabled;
         end if;

      end if;

   end Process_Rule_Parameter;

   --------------------------------------------
   -- XML_Print_Rule (Recursive_Subprograms) --
   --------------------------------------------

   overriding procedure XML_Print_Rule
     (Rule         : Recursive_Subprograms_Rule_Type;
      Indent_Level : Natural := 0)
   is
   begin
      XML_Report
        ("<rule id=""" & Rule_Name (Rule) & """>",
         Indent_Level);

      if Rule.Skip_Dispatching_Calls then
         XML_Report
           ("<parameter>Skip_Dispatching_Calls""</parameter>",
            Indent_Level + 1);
      end if;

      XML_Report ("</rule>", Indent_Level);
   end XML_Print_Rule;

   ---------------------------
   -- Side_Effect_Functions --
   ---------------------------

--   procedure Collect_Global_Info_Pre_Op
--     (Rule    : in out Side_Effect_Functions_Rule_Type;
--      Element :        Asis.Element;
--      Control : in out Traverse_Control;
--      State   : in out Rule_Traversal_State)
--   is
--      pragma Unreferenced (Rule, Control);

--      Current_SE_Status : Side_Effect_Statuses;
--      --  Side effect that the enclosing scope that is known at the moment of
--      --  the call

--      Added_Side_Effect           : Side_Effect_Statuses;
--      Local_SE_Level              : Scope_Levels;
--  --      Side_Effect_SLOC            : String_Loc;
--      Calls_To_Unknown_SE_Routine : Boolean;
--   begin

--      if Is_Global_Scope (Current_Scope)
--         --  no side effect can be in a global scope!
--        or else
--         Side_Effect_Defined (Current_Scope)
--        or else not In_Executable_Code (State)
--      then
--         return;
--      end if;

--      if Can_Cause_Side_Effect (Element) then

--         Current_SE_Status := Side_Effect_Status (Current_Scope);

--         Define_Side_Effect
--           (Element              => Element,
--            New_SE_Status        => Added_Side_Effect,
--            Is_Unresolved_Call   => Calls_To_Unknown_SE_Routine,
--            Change_Data_At_Level => Local_SE_Level);

--         if Added_Side_Effect > Current_SE_Status
--           and then
--            not (Added_Side_Effect = Local_Side_Effect and then
--                 Local_SE_Level >= Scope_Level (Current_Scope))
--         then
--            Set_Side_Effect_Status
--              (Current_Scope,
--               Added_Side_Effect,
--               Build_GNAT_Location (Element));

--            if Added_Side_Effect = Global_Side_Effect then
--               Set_Side_Effect_Defined (Current_Scope);
--            elsif Added_Side_Effect = Local_Side_Effect then
--               Set_Local_Side_Effect_Level
--                 (Current_Scope, Local_SE_Level);
--            end if;

--         elsif Added_Side_Effect = Current_SE_Status
--             and then
--               Added_Side_Effect = Local_Side_Effect
--             and then
--               Local_Side_Effect_Level (Current_Scope) > Local_SE_Level
--         then
--            Set_Side_Effect_Status
--              (Current_Scope,
--               Added_Side_Effect,
--               Build_GNAT_Location (Element));

--            Set_Local_Side_Effect_Level
--              (Current_Scope, Local_SE_Level);
--         end if;

--         if not Side_Effect_Defined (Current_Scope)
--           and then
--            Calls_To_Unknown_SE_Routine
--         then
--            Set_Call_To_Unknown_SE (Current_Scope);
--         end if;

--      end if;

--   end Collect_Global_Info_Pre_Op;

--   procedure Collect_Global_Info_Post_Op
--     (Rule    : in out Side_Effect_Functions_Rule_Type;
--      Element :        Asis.Element;
--      Control : in out Traverse_Control;
--      State   : in out Rule_Traversal_State)
--   is
--      pragma Unreferenced (Rule, Control, State);
--      Next_Call              : Call_Lists.Cursor;
--      Call_Unknown_CE_Entity : Boolean := False;
--   begin

--      if Is_Scope (Element)
--        and then
--  --         Declaration_Kind (Element) not in
--  --            A_Package_Declaration .. A_Package_Body_Declaration
--  --        and then
--         not Side_Effect_Defined (Current_Scope)
--      then
--         --  Iterate through all the direct calls (at the moment we know all
--         --  of them) and look if we can make any final conclusion concerning
--         --  the side effect of the current scope.

--         Next_Call :=
--           Call_Lists.First (GS_Nodes.Table (Current_Scope).Calls_Chain);

--         while Next_Call /= Call_Lists.No_Element loop

--            Correct_Side_Effect_Status
--              (For_Node  => Current_Scope,
--               From_Node => Call_Lists.Element (Next_Call));

--            if Side_Effect_Status (Current_Scope) = Global_Side_Effect then
--               --  Nothing else to check, so
--               exit;
--          elsif not Side_Effect_Defined (Call_Lists.Element (Next_Call)) then
--               Call_Unknown_CE_Entity := True;
--            end if;

--            Next_Call := Call_Lists.Next (Next_Call);
--         end loop;

--         if not Call_Unknown_CE_Entity
--           or else
--            Side_Effect_Status (Current_Scope) = Global_Side_Effect
--         then
--            Set_Side_Effect_Defined (Current_Scope);
--         elsif Call_Unknown_CE_Entity then
--            Set_Call_To_Unknown_SE (Current_Scope);
--         end if;

--      end if;

--   end Collect_Global_Info_Post_Op;

--   procedure Analyze_Global_Structure
--     (Rule : Side_Effect_Functions_Rule_Type)
--   is
--      pragma Unreferenced (Rule);
--   begin
--      Gnatcheck.Global_State.CG.Set_Side_Effect;
--   end Analyze_Global_Structure;

--   procedure Check_Global_Structure_Node
--     (Rule     :     Side_Effect_Functions_Rule_Type;
--      N        :     Gnatcheck.Global_State.GS_Node_Id;
--      Detected : out Boolean)
--   is
--      pragma Unreferenced (Rule);
--   begin

--      Detected :=
--         Is_Local_Scope (N)
--        and then
--         GS_Node_Kind (N) = A_Function
--        and then
--         not (Side_Effect_Defined (N)
--             and then
--              Side_Effect_Status (N) = No_Side_Effect);

--   end Check_Global_Structure_Node;

--   procedure Init_Rule (Rule : in out Side_Effect_Functions_Rule_Type) is
--   begin
--      Rule.Name       := new String'("Side_Effect_Functions");
--      Rule.Rule_State := Disabled;
--      Rule.Help_Info  := new String'("side effect functions");
--      Rule.Diagnosis  := new String'("function has side effect");
--   end Init_Rule;

end Gnatcheck.Rules.Global;
