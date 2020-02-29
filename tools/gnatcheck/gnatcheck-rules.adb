------------------------------------------------------------------------------
--                                                                          --
--                          GNATCHECK COMPONENTS                            --
--                                                                          --
--                      G N A T C H E C K . R U L E S                       --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                     Copyright (C) 2004-2019, AdaCore                     --
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

with Ada.Strings.Fixed;          use Ada.Strings.Fixed;

with ASIS_UL.Common;
with ASIS_UL.Debug;              use ASIS_UL.Debug;
with ASIS_UL.Global_State.CG.Conditions;
with ASIS_UL.Misc;               use ASIS_UL.Misc;
with ASIS_UL.Output;             use ASIS_UL.Output;

with Gnatcheck.Categories;       use Gnatcheck.Categories;
with Gnatcheck.Rules.Custom_1;   use Gnatcheck.Rules.Custom_1;
with Gnatcheck.Rules.Custom_2;   use Gnatcheck.Rules.Custom_2;
with Gnatcheck.Rules.Custom_3;   use Gnatcheck.Rules.Custom_3;
with Gnatcheck.Rules.Custom_4;   use Gnatcheck.Rules.Custom_4;
with Gnatcheck.Rules.Default;    use Gnatcheck.Rules.Default;
with Gnatcheck.Rules.Global;     use Gnatcheck.Rules.Global;
with Gnatcheck.Rules.Metrics;    use Gnatcheck.Rules.Metrics;
with Gnatcheck.Rules.Rule_Table; use Gnatcheck.Rules.Rule_Table;
with Gnatcheck.Rules.Text;       use Gnatcheck.Rules.Text;

package body Gnatcheck.Rules is

   -----------------------
   -- Local subprograms --
   -----------------------

   procedure Register_Rule
     (Rule        : Rule_Access;
      In_Category : Category_Access := Root_Category'Access);
   --  Initializes its parameter (by calling Init_Rule for it) and creates
   --  the corresponding element in the rule table for this rule. Ensures that
   --  all the rules have unique names (casing is not important)

   function Rem_Level (Rule : Rule_Template) return String;
   --  If Debug_Flag_RR is ON Returns the (string image of the) rule
   --  remediation level. Otherwise returns null string.

   ---------------------------
   -- Activate_In_Test_Mode --
   ---------------------------

   procedure Activate_In_Test_Mode (Rule : in out Rule_Template) is
   begin
      Rule.Rule_State := Enabled;
   end Activate_In_Test_Mode;

   overriding procedure Activate_In_Test_Mode
     (Rule : in out One_Integer_Parameter_Rule_Template)
   is
   begin
      Process_Rule_Parameter
        (Rule       => One_Integer_Parameter_Rule_Template'Class (Rule),
         Param      => "4",
         Enable     => True,
         Defined_At => "");
   end Activate_In_Test_Mode;

   ------------------------------
   -- Analyze_Global_Structure --
   ------------------------------

   procedure Analyze_Global_Structure (Rule : Global_Rule_Template) is
      pragma Unreferenced (Rule);
   begin
      null;
   end Analyze_Global_Structure;

   -------------------
   -- Annotate_Rule --
   -------------------

   function Annotate_Rule
     (Rule : Rule_Template;
      Var  : Diagnosis_Variant := 0)
      return String
   is
      pragma Unreferenced (Var);
   begin
      if Gnatcheck.Options.Mapping_Mode then
         if Has_Synonym (Rule) then
            return " [" & Rule_Synonym (Rule) & "]";
         else
            return (" [" & Rule_Name (Rule) & "]");
         end if;
      else
         return "";
      end if;
   end Annotate_Rule;

   overriding function Annotate_Rule
     (Rule : Rule_With_Exceptions_Template;
      Var  : Diagnosis_Variant := 0)
      return String
   is
      pragma Unreferenced (Var);
      Result : String_Access;
      Tmp    : String_Access;
      Is_First_Par : Boolean := True;
   begin
      if not Gnatcheck.Options.Mapping_Mode then
         return "";
      elsif Has_Synonym (Rule) then
         return " [" & Rule_Synonym (Rule) & "]";

      end if;

      Result := new String'(" [" & Rule_Name (Rule));

      for J in First_Exception .. Rule.Exception_Num loop
         if Rule.Exceptions (J) then
            Tmp := new String'(Result.all);
            Free (Result);

            if Is_First_Par then
               Is_First_Par := False;
               Result := new String'(Tmp.all & ':' &
                 Exception_Name
                   (Rule_With_Exceptions_Template'Class (Rule), J));
            else
               Result := new String'(Tmp.all & ',' &
                 Exception_Name
                   (Rule_With_Exceptions_Template'Class (Rule), J));
            end if;
            Free (Tmp);
         end if;
      end loop;

      declare
         Final_Res : constant String := Result.all & "]";
      begin
         Free (Result);
         return Final_Res;
      end;

   end Annotate_Rule;

   ---------------------------------
   -- Check_Global_Structure_Node --
   ---------------------------------

   procedure Check_Global_Structure_Node
     (Rule          :     Global_Rule_Template;
      N             :     ASIS_UL.Global_State.GS_Node_Id;
      Detected      : out Boolean)
   is
      pragma Unreferenced (Rule);
      pragma Unreferenced (N);
      pragma Unreferenced (Detected);
   begin
      null;
   end Check_Global_Structure_Node;

   ------------------------------
   -- Checked_On_Expanded_Code --
   ------------------------------

   function Checked_On_Expanded_Code (Rule : Rule_Template) return Boolean is
   begin
      return Rule.Check_In_Expanded_Generics;
   end Checked_On_Expanded_Code;

   --------------------------------
   -- Collect_Global_Info_Pre_Op --
   --------------------------------

   procedure Collect_Global_Info_Pre_Op
     (Rule    : in out Global_Rule_Template;
      Element :        Asis.Element;
      Control : in out Traverse_Control;
      State   : in out Rule_Traversal_State)
   is
      pragma Unreferenced (Rule);
      pragma Unreferenced (Element);
      pragma Unreferenced (Control);
      pragma Unreferenced (State);
   begin
      null;
   end Collect_Global_Info_Pre_Op;

   ---------------------------------
   -- Collect_Global_Info_Post_Op --
   ---------------------------------

   procedure Collect_Global_Info_Post_Op
     (Rule    : in out Global_Rule_Template;
      Element :        Asis.Element;
      Control : in out Traverse_Control;
      State   : in out Rule_Traversal_State)
   is
      pragma Unreferenced (Rule);
      pragma Unreferenced (Element);
      pragma Unreferenced (Control);
      pragma Unreferenced (State);
   begin
      null;
   end Collect_Global_Info_Post_Op;

   ----------------------------
   -- Decrease_Nonexec_Level --
   ----------------------------

   procedure Decrease_Nonexec_Level (State : in out Rule_Traversal_State)
   is
   begin
      State.Level_Of_Nonexecutable_Construct :=
        State.Level_Of_Nonexecutable_Construct - 1;
   end Decrease_Nonexec_Level;

   -----------------------
   -- Default_Parameter --
   -----------------------

   function Default_Parameter
     (Rule : One_Integer_Parameter_Rule_Template)
      return Integer
   is
      pragma Unreferenced (Rule);
   begin
      return 1;
   end Default_Parameter;

   ----------------------------------------------------
   -- Exception_Name (Rule_With_Exceptions_Template) --
   ----------------------------------------------------

   function Exception_Name
     (Rule      : Rule_With_Exceptions_Template;
      Exc_Index : Exception_Index)
      return      String
   is
      pragma Unreferenced (Rule, Exc_Index);
   begin
      --  This function shall be overridden anyway...
      return "";
   end Exception_Name;

   ------------------------------------------------------
   -- Exception_Number (Rule_With_Exceptions_Template) --
   ------------------------------------------------------

   function Exception_Number
     (Rule     : Rule_With_Exceptions_Template;
      Exc_Name : String)
      return     Exception_Numbers
   is
      pragma Unreferenced (Exc_Name, Rule);
   begin
      --  This function shall be overridden anyway...
      return Not_An_Exception;
   end Exception_Number;

   -----------------
   -- Has_Synonym --
   -----------------

   function Has_Synonym (Rule : Rule_Template) return Boolean is
   begin
      return Rule.User_Synonym /= null;
   end Has_Synonym;

   -------------
   -- Has_Tip --
   -------------

   function Has_Tip (Rule : Rule_Template) return Boolean is
      pragma Unreferenced (Rule);
   begin
      return False;
   end Has_Tip;

   ----------------------------
   -- Increase_Nonexec_Level --
   ----------------------------

   procedure Increase_Nonexec_Level (State : in out Rule_Traversal_State)
   is
   begin
      State.Level_Of_Nonexecutable_Construct :=
        State.Level_Of_Nonexecutable_Construct + 1;
   end Increase_Nonexec_Level;

   ---------------------------
   -- Init_Global_Structure --
   ---------------------------

   procedure Init_Global_Structure (Rule : Global_Rule_Template) is
      pragma Unreferenced (Rule);
   begin
      ASIS_UL.Global_State.CG.Conditions.Set_Unconditional_Call_Graph (True);
   end Init_Global_Structure;

   --------------------------------------
   -- Init_Rule (Global_Rule_Template) --
   --------------------------------------

   procedure Init_Rule (Rule : in out Global_Rule_Template) is
   begin
      Init_Rule (Rule_Template (Rule));
      Rule.Check_In_Expanded_Generics := True;
   end Init_Rule;

   -----------------------------------------------------
   -- Init_Rule (One_Integer_Parameter_Rule_Template) --
   -----------------------------------------------------

   procedure Init_Rule (Rule : in out One_Integer_Parameter_Rule_Template) is
   begin
      Init_Rule (Rule_Template (Rule));
      Rule.Rule_Bound := 1;
   end Init_Rule;

   -------------------------------
   -- Init_Rule (Rule_Template) --
   --------------------------------

   procedure Init_Rule (Rule : in out Rule_Template) is
   begin
      Rule.Rule_State                 := Disabled;
      Rule.Rule_Status                := Under_Construction;
      Rule.Remediation_Level          := Easy;
      Rule.Rule_Category              := No_Category;
      Rule.Next_In_Category           := No_Rule;
      Rule.Check_In_Expanded_Generics := False;
   end Init_Rule;

   -----------------------------------------------
   -- Init_Rule (Rule_With_Exceptions_Template) --
   -----------------------------------------------

   overriding procedure Init_Rule
     (Rule : in out Rule_With_Exceptions_Template)
   is
   begin
      Init_Rule (Rule_Template (Rule));
      Rule.Exceptions := (others => False);
   end Init_Rule;

   ------------------------
   -- In_Executable_Code --
   ------------------------

   function In_Executable_Code
     (State : Rule_Traversal_State)
      return  Boolean
   is
   begin
      return State.Level_Of_Nonexecutable_Construct = 0;
   end In_Executable_Code;

   ---------------
   -- Is_Enable --
   ---------------

   function Is_Enable (Rule : Rule_Template) return Boolean is
   begin
      return Rule.Rule_State = Enabled
           and then
             Rule.Rule_Status >= Non_Documented;
   end Is_Enable;

   function Is_Enable (Rule : Internal_Rule_Template) return Boolean is
   begin
      return Is_Enable (Rule_Template (Rule))
           and then
            Is_Enable (Rule_Template (Rule.Implements.all));
   end Is_Enable;

   ----------------
   -- Line_Check --
   ----------------

   procedure Line_Check
     (Rule               : in out Text_Rule_Template;
      Line_Num           :        Line_Number_Positive;
      Full_Line_Image    :        Program_Text_Access;
      Ada_Line_Image     :        Program_Text_Access;
      Comment_Line_Image :        Program_Text_Access;
      State              : in out Rule_Traversal_State)
   is
   begin
      null;
   end Line_Check;

   ---------------------------------------
   -- More_Rule_Comment (Rule_Template) --
   ---------------------------------------

   function More_Rule_Comment
     (Rule          : Rule_Template;
      Template_Kind : Template_Coding_Standard_Kinds)
      return         String
   is
      pragma Unreferenced (Rule, Template_Kind);
   begin
      return "";
   end More_Rule_Comment;

   -------------------------------------------------------------
   -- More_Rule_Comment (One_Integer_Parameter_Rule_Template) --
   -------------------------------------------------------------

   function More_Rule_Comment
     (Rule          : One_Integer_Parameter_Rule_Template;
      Template_Kind : Template_Coding_Standard_Kinds)
     return String
   is
      pragma Unreferenced (Rule);
   begin
      if Template_Kind = Template_All_ON then
         return "possibly meaningless default parameter used!";
      else
         return "provide a proper parameter value if the rule is enabled!";
      end if;
   end More_Rule_Comment;

   ----------------
   -- Print_Rule --
   ----------------

   procedure Print_Rule
     (Rule         : Rule_Template;
      Indent_Level : Natural := 0)
   is
   begin
      Report_No_EOL (Rule_Name (Rule), Indent_Level);
   end Print_Rule;

   overriding procedure Print_Rule
     (Rule         : One_Integer_Parameter_Rule_Template;
      Indent_Level : Natural := 0)
   is
   begin
      Print_Rule (Rule_Template (Rule), Indent_Level);
      Report_No_EOL (": " & Image (Rule.Rule_Limit));
   end Print_Rule;

   overriding procedure Print_Rule
     (Rule         : Rule_With_Exceptions_Template;
      Indent_Level : Natural := 0)
   is
      First_Param : Boolean := True;
      Rule_Name_Pad : constant String (1 .. Rule_Name (Rule)'Length + 2) :=
        (others => ' ');
   begin
      Print_Rule (Rule_Template (Rule), Indent_Level);

      for J in First_Exception ..  Rule.Exception_Num loop
         if Rule.Exceptions (J) then
            if First_Param then
               Report_No_EOL
                 (": " &
                  Exception_Name
                    (Rule_With_Exceptions_Template'Class (Rule), J));
               First_Param := False;
            else
               Report (",");
               Report_No_EOL
                 (Rule_Name_Pad &
                  Exception_Name
                    (Rule_With_Exceptions_Template'Class (Rule), J),
                     Indent_Level);
            end if;
         end if;
      end loop;
   end Print_Rule;

   ------------------------
   -- Print_Rule_To_File --
   ------------------------

   procedure Print_Rule_To_File
     (Rule         : Rule_Template;
      Rule_File    : File_Type;
      Indent_Level : Natural := 0)
   is
   begin
      for J in 1 .. Indent_Level loop
         Put (Rule_File, Get_Indent_String);
      end loop;

      Put (Rule_File, "+R" & Rule_Name (Rule));
   end Print_Rule_To_File;

   overriding procedure Print_Rule_To_File
     (Rule         : One_Integer_Parameter_Rule_Template;
      Rule_File    : File_Type;
      Indent_Level : Natural := 0)
   is
   begin
      Print_Rule_To_File (Rule_Template (Rule), Rule_File, Indent_Level);
      Put (Rule_File, ": " & Image (Rule.Rule_Limit));
   end Print_Rule_To_File;

   overriding procedure Print_Rule_To_File
     (Rule         : Rule_With_Exceptions_Template;
      Rule_File    : File_Type;
      Indent_Level : Natural := 0)
   is
      First_Param : Boolean := True;
      Rule_Name_Pad : constant String (1 .. Rule_Name (Rule)'Length + 2) :=
        (others => ' ');
   begin
      Print_Rule_To_File (Rule_Template (Rule), Rule_File, Indent_Level);

      for J in First_Exception ..  Rule.Exception_Num loop
         if Rule.Exceptions (J) then
            if First_Param then
               Put (Rule_File, ": " &
                    Exception_Name
                     (Rule_With_Exceptions_Template'Class (Rule), J));
               First_Param := False;
            else
               Put_Line (Rule_File, ",");

               for J in 1 .. Indent_Level loop
                  Put (Rule_File, Get_Indent_String);
               end loop;

               Put (Rule_File,
                    Rule_Name_Pad &
                    Exception_Name
                      (Rule_With_Exceptions_Template'Class (Rule), J));
            end if;
         end if;
      end loop;
   end Print_Rule_To_File;

   ---------------------
   -- Print_Rule_Help --
   ---------------------

   procedure Print_Rule_Help (Rule : Rule_Template) is
   begin
      Info
        (Message  => " " & Rule.Name.all  & " - " & Rule.Help_Info.all &
                     Rem_Level (Rule),
         Line_Len => 79,
         Spacing  => 2);
   end Print_Rule_Help;

   procedure Print_Rule_Help (Rule : Internal_Rule_Template) is
   begin
      Info
        (Message  => " " & Rule_Name (Rule) & " - " & Rule.Help_Info.all,
         Line_Len => 79,
         Spacing  => 2);
   end Print_Rule_Help;

   ----------------------------
   -- Process_Rule_Parameter --
   ----------------------------

   procedure Process_Rule_Parameter
     (Rule       : in out Rule_Template;
      Param      :        String;
      Enable     :        Boolean;
      Defined_At : String)
   is
      pragma Unreferenced (Defined_At);
   begin

      if Param /= "" then
         Error ("no parameter can be set for rule " & Rule.Name.all & ", " &
                Param & " ignored");
      else

         if Enable then
            Rule.Rule_State := Enabled;
         else
            Rule.Rule_State := Disabled;
         end if;

      end if;

   end Process_Rule_Parameter;

   overriding procedure Process_Rule_Parameter
     (Rule       : in out One_Integer_Parameter_Rule_Template;
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
               Rule.Rule_Limit := Integer'Value (Param);

               if Rule.Rule_Limit >= Rule.Rule_Bound then
                  Rule.Rule_State := Enabled;
                  Rule.Defined_At := Enter_String (Defined_At);
               else
                  Error ("(" & Rule.Name.all & ") wrong parameter: " & Param);
                  Rule.Rule_State := Disabled;
               end if;

            exception
               when Constraint_Error =>
                  Error ("(" & Rule.Name.all & ") wrong parameter: " & Param);
                  Rule.Rule_State := Disabled;
            end;

         else
            Error ("(" & Rule.Name.all & ") no parameter allowed for -R");
         end if;

      end if;

   end Process_Rule_Parameter;

   overriding procedure Process_Rule_Parameter
     (Rule       : in out Rule_With_Exceptions_Template;
      Param      :        String;
      Enable     :        Boolean;
      Defined_At : String)
   is
      Exception_Num : Exception_Numbers;
   begin
      if Param = "" then

         if Enable then
            Rule.Rule_State := Enabled;
            Rule.Defined_At := Enter_String (Defined_At);
         else
            Rule.Exceptions := (others => False);
            Rule.Rule_State := Disabled;
         end if;

      else

         if Enable then

            Exception_Num :=
              Exception_Number
                (Rule_With_Exceptions_Template'Class (Rule), Param);

            if Exception_Num = Not_An_Exception then
               Error ("(" & Rule.Name.all & ") wrong parameter: " &
                      Param);
               Rule.Rule_State := Disabled;
            else
               if Gnatcheck.Options.Check_Param_Redefinition
                 and then
                  Rule.Rule_State = Enabled
                 and then
                  Rule.Exceptions (Exception_Num) = False
               then
                  Error
                   ("redefining at " &
                    (if Defined_At = "" then
                        "command line"
                     else
                        Defined_At) &
                    " set of exception cases for rule " & Rule.Name.all &
                    " defined at "  &
                    (if Rule.Defined_At = Nil_String_Loc then
                        "command line"
                     else
                        Get_String (Rule.Defined_At)));
               end if;

               Rule.Exceptions (Exception_Num) := True;
               Rule.Rule_State := Enabled;
               Rule.Defined_At := Enter_String (Defined_At);
            end if;

         else
            Error ("(" & Rule.Name.all & ") no parameter allowed for -R");
            Rule.Exceptions := (others => False);
            Rule.Rule_State := Disabled;
         end if;

      end if;
   end Process_Rule_Parameter;

   -------------------
   -- Register_Rule --
   -------------------

   procedure Register_Rule
     (Rule        : Rule_Access;
      In_Category : Category_Access := Root_Category'Access)
   is
      New_Rule : Rule_Id;
   begin
      --  First, check if the category is in the category hierarchy

      if No (In_Category.Self_Id) then
         Error ("Category not in hierarchy");
         raise ASIS_UL.Common.Fatal_Error;
      end if;

      Init_Rule (Rule.all);

      New_Rule := Get_Rule (Rule.Name.all);

      if not Present (New_Rule)
        and then
         Rule.Synonym /= null
      then
         --  Check if the Rule synonym, if any, name conflicts with any
         --  existing rule.
         New_Rule := Get_Rule (Rule.Synonym.all);
      end if;

      if Present (New_Rule) then
         Error ("duplicated rule - " & Rule.Name.all);
         raise ASIS_UL.Common.Fatal_Error;
      end if;

      All_Rules.Append (Rule);

      Rule.Rule_Category := In_Category.Self_Id;

      --  We assume that rules are registered in the proper order, so all we
      --  have to do is to update Next_In_Category link for the rule that was
      --  the last rule in the given category before registering this rule.

      if No (In_Category.First_Rule) then
         --  First rule in the given category
         In_Category.First_Rule := All_Rules.Last;
      else
         --  Use New_Rule as a temporary storage:
         New_Rule := In_Category.First_Rule;

         while Present (All_Rules.Table (New_Rule).Next_In_Category) loop
            New_Rule := All_Rules.Table (New_Rule).Next_In_Category;
         end loop;

         All_Rules.Table (New_Rule).Next_In_Category := All_Rules.Last;
      end if;

   end Register_Rule;

   --------------------
   -- Register_Rules --
   --------------------

   procedure Register_Rules is
   begin
      Register_Rule (Rule        => Abort_Statements_Rule'Access,
                     In_Category => Feature_Use_Detection'Access);

      Register_Rule (Rule        => Abstract_Type_Declarations_Rule'Access,
                     In_Category => Feature_Use_Detection'Access);

      Register_Rule
        (Rule        =>
           Address_Specifications_For_Initialized_Objects_Rule'Access,
         In_Category => Programming_Practices'Access);

      Register_Rule
        (Rule        => Address_Specifications_For_Local_Objects_Rule'Access,
         In_Category => Programming_Practices'Access);

      Register_Rule (Rule        => Annotated_Comments_Rule'Access,
                     In_Category => Spark'Access);

      Register_Rule (Rule        => Anonymous_Arrays_Rule'Access,
                     In_Category => Programming_Practices'Access);

      Register_Rule (Rule        => Anonymous_Subtypes_Rule'Access,
                     In_Category => Feature_Use_Detection'Access);

      Register_Rule (Rule        => Binary_Case_Statements_Rule'Access,
                     In_Category => Programming_Practices'Access);

      Register_Rule
        (Rule        => Bit_Records_Without_Layout_Definition_Rule'Access,
         In_Category => Portability'Access);

      Register_Rule (Rule        => Blocks_Rule'Access,
                     In_Category => Feature_Use_Detection'Access);

      Register_Rule (Rule        => Boolean_Relational_Operators_Rule'Access,
                     In_Category => Spark'Access);

      --    Register_Rule (Ceiling_Violations_Rule'Access);

      Register_Rule (Rule        => Complex_Inlined_Subprograms_Rule'Access,
                     In_Category => Feature_Use_Detection'Access);

      Register_Rule (Rule        => Conditional_Expressions_Rule'Access,
                     In_Category => Feature_Use_Detection'Access);

      Register_Rule (Rule        => Constructors_Rule'Access,
                     In_Category => Object_Oriented_Features'Access);

      Register_Rule (Rule        => Controlled_Type_Declarations_Rule'Access,
                     In_Category => Feature_Use_Detection'Access);

      Register_Rule (Rule        => Declarations_In_Blocks_Rule'Access,
                     In_Category => Feature_Use_Detection'Access);

      Register_Rule (Rule        => Deep_Inheritance_Hierarchies_Rule'Access,
                     In_Category => Object_Oriented_Features'Access);

      Register_Rule (Rule        => Deeply_Nested_Generics_Rule'Access,
                     In_Category => Program_Structure'Access);

      Register_Rule (Rule        => Deep_Library_Hierarchy_Rule'Access,
                     In_Category => Program_Structure'Access);

      Register_Rule (Rule        => Deeply_Nested_Inlining_Rule'Access,
                     In_Category => Feature_Use_Detection'Access);

      Register_Rule (Rule        => Deeply_Nested_Local_Inlining_Rule'Access,
                     In_Category => Feature_Use_Detection'Access);

      Register_Rule (Rule        => Default_Parameters_Rule'Access,
                     In_Category => Feature_Use_Detection'Access);

      Register_Rule
        (Rule        => Default_Values_For_Record_Components_Rule'Access,
         In_Category => Programming_Practices'Access);

      Register_Rule (Rule        => Deriving_From_Predefined_Type_Rule'Access,
                     In_Category => Programming_Practices'Access);

      Register_Rule (Rule        => Direct_Calls_To_Primitives_Rule'Access,
                     In_Category => Object_Oriented_Features'Access);

      Register_Rule (Rule        => Discriminated_Records_Rule'Access,
                     In_Category => Feature_Use_Detection'Access);

      Register_Rule (Rule        => Downward_View_Conversions_Rule'Access,
                     In_Category => Object_Oriented_Features'Access);

      Register_Rule
        (Rule        => Enumeration_Ranges_In_CASE_Statements_Rule'Access,
         In_Category => Programming_Practices'Access);

      Register_Rule
        (Rule        => Enumeration_Representation_Clauses_Rule'Access,
         In_Category => Feature_Use_Detection'Access);

      Register_Rule (Rule        => Exceptions_As_Control_Flow_Rule'Access,
                     In_Category => Programming_Practices'Access);

      Register_Rule (Rule        => Exits_From_Conditional_Loops_Rule'Access,
                     In_Category => Programming_Practices'Access);

      Register_Rule
        (Rule        => EXIT_Statements_With_No_Loop_Name_Rule'Access,
         In_Category => Programming_Practices'Access);

      Register_Rule (Rule        => Expanded_Loop_Exit_Names_Rule'Access,
                     In_Category => Spark'Access);

      Register_Rule (Rule        => Explicit_Full_Discrete_Ranges_Rule'Access,
                     In_Category => Feature_Use_Detection'Access);

      Register_Rule (Rule        => Expression_Functions_Rule'Access,
                     In_Category => Feature_Use_Detection'Access);

      Register_Rule (Rule        => Fixed_Equality_Checks_Rule'Access,
                     In_Category => Feature_Use_Detection'Access);

      Register_Rule (Rule        => Float_Equality_Checks_Rule'Access,
                     In_Category => Feature_Use_Detection'Access);

      Register_Rule (Rule        => Forbidden_Attributes_Rule'Access,
                     In_Category => Portability'Access);

      Register_Rule (Rule        => Forbidden_Pragmas_Rule'Access,
                     In_Category => Portability'Access);

      Register_Rule (Rule        => Function_Style_Procedures_Rule'Access,
                     In_Category => Feature_Use_Detection'Access);

      Register_Rule (Rule        => Generics_In_Subprograms_Rule'Access,
                     In_Category => Feature_Use_Detection'Access);

      Register_Rule
        (Rule        => Global_Variables_Rule'Access,
         In_Category => Programming_Practices'Access);

      Register_Rule (Rule        => GOTO_Statements_Rule'Access,
                     In_Category => Programming_Practices'Access);

      Register_Rule (Rule        => Identifier_Casing_Rule'Access,
                     In_Category => Readability'Access);

      Register_Rule (Rule        => Identifier_Suffixes_Rule'Access,
                     In_Category => Readability'Access);

      Register_Rule (Rule        => Identifier_Prefixes_Rule'Access,
                     In_Category => Readability'Access);

      Register_Rule (Rule        => Implicit_IN_Mode_Parameters_Rule'Access,
                     In_Category => Feature_Use_Detection'Access);

      Register_Rule
        (Rule        => Implicit_SMALL_For_Fixed_Point_Types_Rule'Access,
         In_Category => Portability'Access);

      Register_Rule (Rule        => Improper_Returns_Rule'Access,
                     In_Category => Programming_Practices'Access);

      --   Register_Rule (Improperly_Called_Protected_Entries_Rule'Access);

      Register_Rule
        (Rule        => Improperly_Located_Instantiations_Rule'Access,
         In_Category => Feature_Use_Detection'Access);

      Register_Rule
        (Rule        => Incomplete_Representation_Specifications_Rule'Access,
         In_Category => Portability'Access);

      Register_Rule (Rule        => Library_Level_Subprograms_Rule'Access,
                     In_Category => Feature_Use_Detection'Access);

      Register_Rule (Rule        => Local_Packages_Rule'Access,
                     In_Category => Program_Structure'Access);

      Register_Rule (Rule        => Local_USE_Clauses_Rule'Access,
                     In_Category => Programming_Practices'Access);

      Register_Rule (Rule        => Max_Identifier_Length_Rule'Access,
                     In_Category => Readability'Access);

      Register_Rule (Rule        => Metrics_Cyclomatic_Complexity_Rule'Access,
                     In_Category => Gnatcheck.Categories.Metrics'Access);

      Register_Rule (Rule        => Metrics_Essential_Complexity_Rule'Access,
                     In_Category => Gnatcheck.Categories.Metrics'Access);

      Register_Rule (Rule        => Metrics_LSLOC_Rule'Access,
                     In_Category => Gnatcheck.Categories.Metrics'Access);

      Register_Rule
        (Rule        => Maximum_Parameters_Rule'Access,
         In_Category => Programming_Practices'Access);

      Register_Rule
        (Rule        => Membership_Tests_Rule'Access,
         In_Category => Feature_Use_Detection'Access);

      Register_Rule
        (Rule        => Misnamed_Controlling_Parameters_Rule'Access,
         In_Category => Readability'Access);

      Register_Rule
        (Rule        => Misplaced_Representation_Items_Rule'Access,
         In_Category => Programming_Practices'Access);

      Register_Rule
        (Rule        => Multiple_Entries_In_Protected_Definitions_Rule'Access,
         In_Category => Concurrency'Access);

      Register_Rule (Rule        => Name_Clashes_Rule'Access,
                     In_Category => Readability'Access);

      Register_Rule (Rule        => Nested_Subprograms_Rule'Access,
                     In_Category => Programming_Practices'Access);

      Register_Rule (Rule        => No_Inherited_Classwide_Pre_Rule'Access,
                     In_Category => Object_Oriented_Features'Access);

      Register_Rule (Rule        => No_Explicit_Real_Range_Rule'Access,
                     In_Category => Portability'Access);

      Register_Rule
        (Rule        => No_Scalar_Storage_Order_Specified_Rule'Access,
         In_Category => Portability'Access);

      Register_Rule (Rule        => Non_Qualified_Aggregates_Rule'Access,
                     In_Category => Feature_Use_Detection'Access);

      Register_Rule (Rule        => Non_Short_Circuit_Operators_Rule'Access,
                     In_Category => Programming_Practices'Access);

      Register_Rule (Rule        => Non_SPARK_Attributes_Rule'Access,
                     In_Category => Spark'Access);

      Register_Rule (Rule        => Non_Tagged_Derived_Types_Rule'Access,
                     In_Category => Spark'Access);

      Register_Rule (Rule        => Non_Visible_Exceptions_Rule'Access,
                     In_Category => Program_Structure'Access);

      Register_Rule (Rule        => Null_Paths_Rule'Access,
                     In_Category => Programming_Practices'Access);

      Register_Rule (Rule        => Number_Declarations_Rule'Access,
                     In_Category => Feature_Use_Detection'Access);

      Register_Rule (Rule        => Numeric_Indexing_Rule'Access,
                     In_Category => Feature_Use_Detection'Access);

      Register_Rule (Rule        => Numeric_Literals_Rule'Access,
                     In_Category => Feature_Use_Detection'Access);

      Register_Rule
        (Rule        => Object_Declarations_Out_Of_Order_Rule'Access,
         In_Category => Readability'Access);

      Register_Rule (Rule        => Objects_Of_Anonymous_Types_Rule'Access,
                     In_Category => Programming_Practices'Access);

      Register_Rule (Rule        => One_Construct_Per_Line_Rule'Access,
                     In_Category => Readability'Access);

      Register_Rule (Rule        => OTHERS_In_Aggregates_Rule'Access,
                     In_Category => Programming_Practices'Access);

      Register_Rule (Rule        => OTHERS_In_CASE_Statements_Rule'Access,
                     In_Category => Programming_Practices'Access);

      Register_Rule (Rule        => OTHERS_In_Exception_Handlers_Rule'Access,
                     In_Category => Programming_Practices'Access);

      Register_Rule (Rule        => Outbound_Protected_Assignments_Rule'Access,
                     In_Category => Programming_Practices'Access);

      Register_Rule (Rule        => Outer_Loop_Exits_Rule'Access,
                     In_Category => Spark'Access);

      Register_Rule (Rule        => Overloaded_Operators_Rule'Access,
                     In_Category => Spark'Access);

      Register_Rule
        (Rule        => Overly_Nested_Control_Structures_Rule'Access,
         In_Category => Programming_Practices'Access);

      Register_Rule (Rule        => Parameters_Out_Of_Order_Rule'Access,
                     In_Category => Feature_Use_Detection'Access);

      Register_Rule
        (Rule => Positional_Actuals_For_Defaulted_Generic_Parameters_Rule
                          'Access,
         In_Category => Programming_Practices'Access);

      Register_Rule
        (Rule => Positional_Actuals_For_Defaulted_Parameters_Rule 'Access,
         In_Category => Programming_Practices'Access);

      Register_Rule (Rule        => POS_On_Enumeration_Types_Rule'Access,
                     In_Category => Programming_Practices'Access);

      Register_Rule (Rule        => Positional_Components_Rule'Access,
                     In_Category => Programming_Practices'Access);

      Register_Rule (Rule        => Positional_Generic_Parameters_Rule'Access,
                     In_Category => Programming_Practices'Access);

      Register_Rule (Rule        => Positional_Parameters_Rule'Access,
                     In_Category => Programming_Practices'Access);

      Register_Rule (Rule        => Predefined_Numeric_Types_Rule'Access,
                     In_Category => Portability'Access);

      Register_Rule (Rule        => Predicate_Testing_Rule'Access,
                     In_Category => Feature_Use_Detection'Access);

      Register_Rule (Rule        => Printable_ASCII_Rule'Access,
                     In_Category => Portability'Access);

      Register_Rule (Rule        => Quantified_Expressions_Rule'Access,
                     In_Category => Feature_Use_Detection'Access);

      Register_Rule (Rule        => Raising_External_Exceptions_Rule'Access,
                     In_Category => Program_Structure'Access);
      --   ??? may be, should be put into Programming_Practices???

      Register_Rule (Rule        => Raising_Predefined_Exceptions_Rule'Access,
                     In_Category => Feature_Use_Detection'Access);

      Register_Rule (Rule        => Recursive_Subprograms_Rule'Access,
                     In_Category => Programming_Practices'Access);

      Register_Rule (Rule        => Relative_Delay_Statements_Rule'Access,
                     In_Category => Feature_Use_Detection'Access);

      Register_Rule (Rule        => Representation_Specifications_Rule'Access,
                     In_Category => Feature_Use_Detection'Access);

      --    Register_Rule (Side_Effect_Functions_Rule'Access);

      Register_Rule
        (Rule        => Separate_Numeric_Error_Handlers_Rule'Access,
         In_Category => Portability'Access);

      Register_Rule (Rule        => Single_Value_Enumeration_Types_Rule'Access,
                     In_Category => Programming_Practices'Access);

      Register_Rule (Rule        => Slices_Rule'Access,
                     In_Category => Spark'Access);

      Register_Rule (Rule        => Specific_Parent_Type_Invariant_Rule'Access,
                     In_Category => Object_Oriented_Features'Access);

      Register_Rule (Rule        => Specific_Pre_Post_Rule'Access,
                     In_Category => Object_Oriented_Features'Access);

      Register_Rule (Rule        => Specific_Type_Invariants_Rule'Access,
                     In_Category => Object_Oriented_Features'Access);

      Register_Rule (Rule        => Subprogram_Access_Rule'Access,
                     In_Category => Feature_Use_Detection'Access);

      Register_Rule (Rule        => Too_Many_Dependencies_Rule'Access,
                     In_Category => Feature_Use_Detection'Access);

      Register_Rule (Rule        => Too_Many_Parents_Rule'Access,
                     In_Category => Object_Oriented_Features'Access);

      Register_Rule (Rule        => Too_Many_Primitives_Rule'Access,
                     In_Category => Object_Oriented_Features'Access);

      Register_Rule (Rule        => Unassigned_OUT_Parameters_Rule'Access,
                     In_Category => Feature_Use_Detection'Access);

      Register_Rule (Rule        => Unchecked_Address_Conversions_Rule'Access,
                     In_Category => Programming_Practices'Access);

      Register_Rule
        (Rule        => Unchecked_Conversions_As_Actuals_Rule'Access,
         In_Category => Programming_Practices'Access);

      Register_Rule
        (Rule        => Uncommented_BEGIN_In_Package_Bodies_Rule'Access,
         In_Category => Readability'Access);

      Register_Rule (Rule        => Unconstrained_Array_Returns_Rule'Access,
                     In_Category => Feature_Use_Detection'Access);

      Register_Rule (Rule        => Unconditional_Exits_Rule'Access,
                     In_Category => Feature_Use_Detection'Access);

      Register_Rule (Rule        => Unconstrained_Arrays_Rule'Access,
                     In_Category => Feature_Use_Detection'Access);

      Register_Rule (Rule        => Uninitialized_Global_Variables_Rule'Access,
                     In_Category => Programming_Practices'Access);

      Register_Rule (Rule        => Universal_Ranges_Rule'Access,
                     In_Category => Spark'Access);

      Register_Rule (Rule        => Unnamed_Blocks_And_Loops_Rule'Access,
                     In_Category => Programming_Practices'Access);

      --    Register_Rule (Unused_Subprograms_Rule'Access);
      Register_Rule (Rule        => USE_PACKAGE_Clauses_Rule'Access,
                     In_Category => Programming_Practices'Access);

      Register_Rule (Rule        => Visible_Components_Rule'Access,
                     In_Category => Object_Oriented_Features'Access);

      Register_Rule
        (Rule        => Volatile_Objects_Without_Address_Clauses_Rule'Access,
         In_Category => Concurrency'Access);

   end Register_Rules;

   ---------------
   -- Rem_Level --
   ---------------

   function Rem_Level (Rule : Rule_Template) return String is
   begin

      if Debug_Flag_RR then
         return " - " & Rule.Remediation_Level'Img;
      else
         return "";
      end if;

   end Rem_Level;

   ------------------------
   -- Rule_Check_Post_Op --
   ------------------------

   procedure Rule_Check_Post_Op
     (Rule    : in out Rule_Template;
      Element :        Asis.Element;
      Control : in out Traverse_Control;
      State   : in out Rule_Traversal_State)
   is
      pragma Unreferenced (Rule);
      pragma Unreferenced (Element);
      pragma Unreferenced (Control);
      pragma Unreferenced (State);
   begin
      null;
   end Rule_Check_Post_Op;

   -----------------------
   -- Rule_Check_Pre_Op --
   -----------------------

   procedure Rule_Check_Pre_Op
     (Rule    : in out Rule_Template;
      Element :        Asis.Element;
      Control : in out Traverse_Control;
      State   : in out Rule_Traversal_State)
   is
      pragma Unreferenced (Rule);
      pragma Unreferenced (Element);
      pragma Unreferenced (Control);
      pragma Unreferenced (State);
   begin
      null;
   end Rule_Check_Pre_Op;

   ------------------
   -- Rule_Comment --
   ------------------

   function Rule_Comment (Rule : Rule_Template) return String is
   begin
      return Rule.Help_Info.all;
   end Rule_Comment;

   ---------------
   -- Rule_Name --
   ---------------

   function Rule_Name (Rule : Rule_Template) return String is
   begin
      return Rule.Name.all;
   end Rule_Name;

   function Rule_Name (Rule : Internal_Rule_Template) return String is
   begin
      return Rule.Implements.all.Name.all &
             "("                          &
              Rule.Name.all               &
              ")";
   end Rule_Name;

   ---------------------------------
   -- Rule_Option (Rule_Template) --
   ---------------------------------

   function Rule_Option
     (Rule          : Rule_Template;
      Template_Kind : Template_Coding_Standard_Kinds)
      return          String
   is
      Switch : Character := '-';
   begin
      if Template_Kind = Template_All_ON then
         Switch := '+';
      end if;

      return Switch & "R " & Rule_Name (Rule);
   end Rule_Option;

   -------------------------------------------------------
   -- Rule_Option (One_Integer_Parameter_Rule_Template) --
   -------------------------------------------------------

   function Rule_Option
     (Rule          : One_Integer_Parameter_Rule_Template;
      Template_Kind : Template_Coding_Standard_Kinds)
      return String
   is
   begin
      if Template_Kind = Template_All_ON then
         return Rule_Option (Rule_Template (Rule), Template_Kind) & " :" &
                Default_Parameter
                  (One_Integer_Parameter_Rule_Template'Class (Rule))'Img;
      else
         return Rule_Option (Rule_Template (Rule), Template_Kind);
      end if;
   end Rule_Option;

   --------------------
   -- Rule_Parameter --
   --------------------

   function Rule_Parameter
     (Rule  : Rule_Template;
      Diag  : String)
      return String
   is
      pragma Unreferenced (Diag);
   begin
      pragma Assert (Allows_Parametrized_Exemption (Rule));
      return "";
   end Rule_Parameter;

   ------------------
   -- Rule_Synonym --
   ------------------

   function Rule_Synonym (Rule : Rule_Template) return String is
   begin
      if Has_Synonym (Rule) then
         return Rule.User_Synonym.all;
      else
         return "";
      end if;
   end Rule_Synonym;

   ------------------
   -- Sample_Image --
   ------------------

   procedure Sample_Image
     (Rule          : Rule_Template'Class;
      Template_Kind : Template_Coding_Standard_Kinds;
      Sample_File   : File_Type;
      Comment_From  : Positive)
   is
      More_Comments : constant String :=
        More_Rule_Comment (Rule, Template_Kind);
   begin
      Put (Sample_File, Rule_Option (Rule, Template_Kind));
      Set_Col (Sample_File, Positive_Count (Comment_From));
      Put_Line (Sample_File, "-- " & Rule_Comment (Rule));

      if More_Comments /= "" then
         Set_Col (Sample_File, Positive_Count (Comment_From));
         Put_Line (Sample_File, "-- " & More_Comments);
      end if;
   end Sample_Image;

   --------------------
   -- XML_Print_Rule --
   --------------------

   procedure XML_Print_Rule
     (Rule         : Rule_Template;
      Indent_Level : Natural := 0)
   is
   begin
      XML_Report
        ("<rule id=""" & Rule_Name (Rule) & """></rule>",
         Indent_Level);
   end XML_Print_Rule;

   overriding procedure XML_Print_Rule
     (Rule         : One_Integer_Parameter_Rule_Template;
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

   overriding procedure XML_Print_Rule
     (Rule         : Rule_With_Exceptions_Template;
      Indent_Level : Natural := 0)
   is
   begin
      XML_Report
        ("<rule id=""" & Rule_Name (Rule) & """>",
         Indent_Level);

      for J in First_Exception ..  Rule.Exception_Num loop
         XML_Report
           ("<parameter>"                                                  &
            Exception_Name (Rule_With_Exceptions_Template'Class (Rule), J) &
            "</parameter>",
            Indent_Level + 1);
      end loop;

      XML_Report ("</rule>", Indent_Level);
   end XML_Print_Rule;

   -------------------
   -- XML_Rule_Help --
   -------------------

   procedure XML_Rule_Help (Rule : Rule_Template; Level : Natural) is
   begin

      Info_No_EOL (Level * Ident_String &
                   "<check switch=""+R" &
                   Rule.Name.all        &
                   """ label="""        &
                   Rule.Help_Info.all   &
                   """");

      if Has_Tip (Rule_Template'Class (Rule)) then
         Info (">");
         XML_Rule_Help_Tip (Rule_Template'Class (Rule), Level + 1);
         Info (Level * Ident_String &
              "</check>");
      else
         Info ("/>");
      end if;

   end XML_Rule_Help;

   overriding procedure XML_Rule_Help
     (Rule  : One_Integer_Parameter_Rule_Template;
      Level : Natural)
   is
   begin
      Info (Level * Ident_String             &
            "<spin switch=""+R"              &
            Rule.Name.all                    &
            """ label="""                    &
            Rule.Help_Info.all               &
            """ min="""                      &
            Image (Rule.Rule_Bound)          &
            """ max=""99999"""               &
            " default="""                    &
            Image (Rule.Rule_Bound - 1)      &
            """ separator="":"""             &
            "/>");
   end XML_Rule_Help;

   overriding procedure XML_Rule_Help
     (Rule  : Rule_With_Exceptions_Template;
      Level : Natural)
   is
   begin
      XML_Rule_Help (Rule_Template (Rule), Level);
      --  ???
   end XML_Rule_Help;

   -----------------------
   -- XML_Rule_Help_Tip --
   -----------------------

   procedure XML_Rule_Help_Tip (Rule : Rule_Template; Level : Natural) is
   begin
      null;
   end XML_Rule_Help_Tip;

end Gnatcheck.Rules;
