------------------------------------------------------------------------------
--                                                                          --
--                          GNATCHECK COMPONENTS                            --
--                                                                          --
--              G N A T C H E C K . R U L E S . D E F A U L T               --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                     Copyright (C) 2004-2017, AdaCore                     --
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

with Ada.Characters.Handling;    use Ada.Characters.Handling;
with Ada.Strings.Fixed;          use Ada.Strings.Fixed;

with Asis.Compilation_Units;     use Asis.Compilation_Units;
with Asis.Declarations;          use Asis.Declarations;
with Asis.Definitions;           use Asis.Definitions;
with Asis.Elements;              use Asis.Elements;
with Asis.Expressions;           use Asis.Expressions;
with Asis.Extensions;            use Asis.Extensions;
with Asis.Extensions.Flat_Kinds; use Asis.Extensions.Flat_Kinds;
with Asis.Statements;            use Asis.Statements;

with ASIS_UL.Output;             use ASIS_UL.Output;
with ASIS_UL.Utilities;          use ASIS_UL.Utilities;

with Gnatcheck.ASIS_Utilities;   use Gnatcheck.ASIS_Utilities;
with Gnatcheck.Name_Dictionary;
with Gnatcheck.Traversal_Stack;  use Gnatcheck.Traversal_Stack;

package body Gnatcheck.Rules.Default is

   --------------------------------
   -- Abstract_Type_Declarations --
   --------------------------------

   procedure Init_Rule (Rule : in out Abstract_Type_Declarations_Rule_Type) is
   begin
      Init_Rule (Rule_Template (Rule));
      Rule.Name        := new String'("Abstract_Type_Declarations");
      Rule.Synonym     := new String'("Abstr_Types");
      Rule.Rule_Status := Fully_Implemented;
      Rule.Help_Info   := new String'("abstract types");
      Rule.Diagnosis   := new String'("declaration of abstract type");
   end Init_Rule;

   procedure Rule_Check_Pre_Op
     (Rule    : in out Abstract_Type_Declarations_Rule_Type;
      Element :        Asis.Element;
      Control : in out Traverse_Control;
      State   : in out Rule_Traversal_State)
   is
      El_Kind : constant Flat_Element_Kinds := Flat_Element_Kind (Element);
      pragma Unreferenced (Control);
      pragma Unreferenced (Rule);
   begin

      case El_Kind is
         when A_Tagged_Record_Type_Definition         |
              A_Derived_Record_Extension_Definition   |
              A_Private_Type_Declaration              |
              A_Private_Extension_Declaration         |
              A_Formal_Tagged_Private_Type_Definition |
              A_Formal_Derived_Type_Definition        =>

            if Trait_Kind (Element) in An_Abstract_Trait ..
               An_Abstract_Limited_Private_Trait
            then
               State.Detected := True;
            end if;

         when others =>
            null;
      end case;

   end Rule_Check_Pre_Op;

   ------------------------
   -- Anonymous_Subtypes --
   ------------------------

   procedure Init_Rule (Rule : in out Anonymous_Subtypes_Rule_Type) is
   begin
      Init_Rule (Rule_Template (Rule));
      Rule.Name        := new String'("Anonymous_Subtypes");
      Rule.Rule_Status := Fully_Implemented;
      Rule.Help_Info   := new String'("anonymous subtypes");
      Rule.Diagnosis   := new String'("anonymous subtype");
   end Init_Rule;

   procedure Rule_Check_Pre_Op
     (Rule    : in out Anonymous_Subtypes_Rule_Type;
      Element :        Asis.Element;
      Control : in out Traverse_Control;
      State   : in out Rule_Traversal_State)
   is
      pragma Unreferenced (Rule, Control);
      El_Kind   : constant Flat_Element_Kinds := Flat_Element_Kind (Element);
      Encl_Kind :          Flat_Element_Kinds;
   begin

      case El_Kind is
         when A_Subtype_Indication =>

            Encl_Kind := Flat_Element_Kind (Get_Enclosing_Element);

            if Encl_Kind /= A_Subtype_Declaration
              and then
               not Is_Nil (Subtype_Constraint (Element))
              and then
               not Constraint_Depends_On_Discriminant
                     (Subtype_Constraint (Element))
              and then
               not Self_Ref_Discr_Constraint (Subtype_Constraint (Element))
            then
               State.Detected := True;
            end if;

         when A_Discrete_Subtype_Indication_As_Subtype_Definition |
              A_Discrete_Subtype_Indication                       =>

            --  The enclosing context for sure can not be a subtype
            --  declaration, so if we have a constraint - we should detect
            --  this situation

            if not Is_Nil (Subtype_Constraint (Element)) then
               State.Detected := True;
            end if;

         when A_Discrete_Range_Attribute_Reference_As_Subtype_Definition |
              A_Discrete_Simple_Expression_Range_As_Subtype_Definition   |
              A_Discrete_Range_Attribute_Reference                       |
              A_Discrete_Simple_Expression_Range                         =>

            Encl_Kind := Flat_Element_Kind (Get_Enclosing_Element);

            if not (Encl_Kind = A_Component_Clause
                  or else
                    Constraint_Depends_On_Discriminant
                      (Get_Enclosing_Element))
            then
               --  Definitely the use of an anonymous subtype
               State.Detected := True;
            end if;

         when A_Simple_Expression_Range =>

            if Flat_Element_Kind (Get_Enclosing_Element) in
               An_In_Membership_Test ..
               A_Not_In_Membership_Test
            then
               State.Detected := True;
            end if;

         when others =>
            null;
      end case;

   end Rule_Check_Pre_Op;

   ------------
   -- Blocks --
   ------------

   procedure Init_Rule (Rule : in out Blocks_Rule_Type) is
   begin
      Init_Rule (Rule_Template (Rule));
      Rule.Name        := new String'("Blocks");
      Rule.Rule_Status := Fully_Implemented;
      Rule.Help_Info   := new String'("block statements");
      Rule.Diagnosis   := new String'("block statement");
   end Init_Rule;

   procedure Rule_Check_Pre_Op
     (Rule    : in out Blocks_Rule_Type;
      Element :        Asis.Element;
      Control : in out Traverse_Control;
      State   : in out Rule_Traversal_State)
   is
      pragma Unreferenced (Control);
      pragma Unreferenced (Rule);
   begin

      if Statement_Kind (Element) = A_Block_Statement then
         State.Detected := True;
      end if;

   end Rule_Check_Pre_Op;

   ----------------------------------
   -- Boolean_Relational_Operators --
   ----------------------------------

   procedure Init_Rule (Rule : in out Boolean_Relational_Operators_Rule_Type)
   is
   begin
      Init_Rule (Rule_Template (Rule));
      Rule.Name        := new String'("Boolean_Relational_Operators");
      Rule.Synonym     := new String'("Bool_Relation_Ops");
      Rule.Rule_Status := Fully_Implemented;
      Rule.Help_Info   := new String'("comparisons of Boolean values");
      Rule.Diagnosis   := new String'("comparison of Boolean values");
   end Init_Rule;

   procedure Rule_Check_Pre_Op
     (Rule    : in out Boolean_Relational_Operators_Rule_Type;
      Element :        Asis.Element;
      Control : in out Traverse_Control;
      State   : in out Rule_Traversal_State)
   is
      pragma Unreferenced (Rule);
      pragma Unreferenced (Control);

      Call_Element : Asis.Element;
   begin

      if Operator_Kind (Element) in
            An_Equal_Operator .. A_Greater_Than_Or_Equal_Operator

      then
         Call_Element := Get_Call_Element;

         if Expression_Kind (Call_Element) = A_Function_Call
           and then
            Is_Predefined (Element)
           and then
            Is_Standard_Boolean
              (Actual_Parameter (Function_Call_Parameters (Call_Element) (1)))
         then
            State.Detected := True;
         end if;

      end if;

   end Rule_Check_Pre_Op;

   ------------------------
   -- Ceiling_Violations --
   ------------------------

--   procedure Init_Rule (Rule : in out Ceiling_Violations_Rule_Type)
--   is
--   begin
--      Rule.Name       := new String'("Ceiling_Violations");
--      Rule.Synonym    := new String'("Ceiling_Priority_Consistency");
--      Rule.Rule_State := Disabled;
--      Rule.Help_Info  := new String'("priority inversion for protected " &
--                                     "operations");
--      Rule.Diagnosis  := new String'("protected operation is called by " &
--                                     "higher priority caller;");
--   end Init_Rule;

--   procedure Collect_Global_Info_Pre_Op
--     (Rule    : in out Ceiling_Violations_Rule_Type;
--      Element :        Asis.Element;
--      Control : in out Traverse_Control;
--      State   : in out Rule_Traversal_State)
--   is
--      Enclosing_Entity_Node : GS_Node_Id;

--      Priority_Expr : Asis.Element := Nil_Element;

--      Priority            : Any_Priority;
--      Is_Dynamic_Priority : Boolean := False;

--      pragma Unreferenced (Control, State, Rule);

--   begin

--      if Pragma_Kind (Element) = A_Priority_Pragma
--        or else
--         Pragma_Kind (Element) = An_Interrupt_Priority_Pragma
--      then

--         if Declaration_Kind (Get_Enclosing_Element) in
--            A_Procedure_Body_Declaration .. A_Function_Body_Declaration
--         then

--            if Is_Nil (Get_Enclosing_Element (Steps_Up => 1))
--              and then
--               Can_Be_Main_Program (Enclosing_Compilation_Unit (Element))
--            then
--               --  This pragma defines the priority of an environment task
--               Enclosing_Entity_Node := Environment_Task_Node;
--            else
--               --  This pragma has no effect
--               return;
--            end if;

--         elsif Definition_Kind (Get_Enclosing_Element) in
--               A_Task_Definition .. A_Protected_Definition
--         then
--            Enclosing_Entity_Node :=
--              Corresponding_Node (Get_Enclosing_Element);

--         else
--            null;
--            pragma Assert (False);
--         end if;

--         declare
--            Pragma_Args : constant Asis.Association_List :=
--              Pragma_Argument_Associations (Element);
--         begin

--            if not Is_Nil (Pragma_Args) then
--               Priority_Expr := Actual_Parameter (Pragma_Args (1));

--               if Is_Static (Priority_Expr) then
--                  Priority :=
--                    Any_Priority'Wide_Value
--                      (Static_Expression_Value_Image (Priority_Expr));
--               else
--                  --  Priority is set by a dynamic expression, so
--                  Is_Dynamic_Priority := True;
--               end if;

--            else
--               --  Pragma Interrupt_Priority with no arguments
--               Priority := Interrupt_Priority'Last;
--            end if;
--         end;

--         if Is_Dynamic_Priority then
--            Set_Dynamic_Priority (Enclosing_Entity_Node);
--         else
--            Set_Priority (Enclosing_Entity_Node, Priority);
--         end if;

--      end if;

--   end Collect_Global_Info_Pre_Op;

--   procedure Analyze_Global_Structure
--     (Rule : Ceiling_Violations_Rule_Type)
--   is
--      pragma Unreferenced (Rule);
--   begin
--      Gnatcheck.Global_State.CG.Set_Priorities;
--   end Analyze_Global_Structure;

--   procedure Check_Global_Structure_Node
--     (Rule     :     Ceiling_Violations_Rule_Type;
--      N        :     Gnatcheck.Global_State.GS_Node_Id;
--      Detected : out Boolean)
--   is
--      pragma Unreferenced (Rule);
--      Next_N : GS_Node_Id;
--   begin
--      Detected := False;

--      if GS_Node_Kind (N) in A_Protected_Procedure .. A_Protected_Entry then

--         Reset_Itrerator (For_Node => N, Call_Set => Callers);
--         Next_N := Next_Node;

--         while Present (Next_N) loop

--            pragma Assert (Priority_Defined (Next_N));

--            if Has_Dynamic_Priority (Next_N)
--              or else
--               Node_Priority (Next_N) >
--                 Node_Priority (N)
--            then
--               Detected := True;
--               exit;
--            end if;

--            Next_N := Next_Node;
--         end loop;

--      end if;

--   end Check_Global_Structure_Node;

   -------------------------------
   -- Complex_Concurr_Structure --
   -------------------------------

--   procedure Init_Rule (Rule : in out Complex_Concurr_Structure_Rule_Type) is
--   begin
--      Rule.Name       := new String'("Complex_Concurr_Structure");
--      Rule.Rule_State := Disabled;
--      Rule.Help_Info  := new String'("complex concurrent structures");
--      Rule.Diagnosis  :=
--        new String'("declaration of complex concurrent structure");
--   end Init_Rule;

--   procedure Rule_Check_Pre_Op
--     (Rule    : in out Complex_Concurr_Structure_Rule_Type;
--      Element :        Asis.Element;
--      Control : in out Traverse_Control;
--      State   : in out Rule_Traversal_State)
--   is
--      pragma Unreferenced (Rule);
--      pragma Unreferenced (Control);
--   begin
--   end Rule_Check_Pre_Op;

   ----------------------------------
   -- Controlled_Type_Declarations --
   ----------------------------------

   procedure Init_Rule (Rule : in out Controlled_Type_Declarations_Rule_Type)
   is
   begin
      Init_Rule (Rule_Template (Rule));
      Rule.Name        := new String'("Controlled_Type_Declarations");
      Rule.Synonym     := new String'("Contr_Types");
      Rule.Rule_Status := Fully_Implemented;
      Rule.Help_Info   := new String'("controlled types");
      Rule.Diagnosis   := new String'("declaration of controlled type");
   end Init_Rule;

   procedure Rule_Check_Pre_Op
     (Rule    : in out Controlled_Type_Declarations_Rule_Type;
      Element :        Asis.Element;
      Control : in out Traverse_Control;
      State   : in out Rule_Traversal_State)
   is
      Type_Name : Asis.Element;
      pragma Unreferenced (Rule);
      pragma Unreferenced (Control);
   begin

      case Declaration_Kind (Element) is
         when An_Ordinary_Type_Declaration    |
              A_Private_Type_Declaration      |
              A_Private_Extension_Declaration =>
            Type_Name := Names (Element) (1);

            if Is_Controlled (Type_Name) then
               State.Detected := True;
            end if;

         when others =>
            null;
      end case;

   end Rule_Check_Pre_Op;

   ----------------------------
   -- Declarations_In_Blocks --
   ----------------------------

   procedure Init_Rule (Rule : in out Declarations_In_Blocks_Rule_Type) is
   begin
      Init_Rule (Rule_Template (Rule));
      Rule.Name        := new String'("Declarations_In_Blocks");
      Rule.Synonym     := new String'("Decl_Blocks");
      Rule.Rule_Status := Fully_Implemented;
      Rule.Help_Info   := new String'("block statements with local " &
                                       "declarations");
      Rule.Diagnosis   := new String'("block statement with local " &
                                       "declarations");
   end Init_Rule;

   procedure Rule_Check_Pre_Op
     (Rule    : in out Declarations_In_Blocks_Rule_Type;
      Element :        Asis.Element;
      Control : in out Traverse_Control;
      State   : in out Rule_Traversal_State)
   is
      pragma Unreferenced (Rule);
      pragma Unreferenced (Control);
   begin

      if Statement_Kind (Element) = A_Block_Statement then

         declare
            Dcls : constant Asis.Element_List :=
                     Block_Declarative_Items (Element);
         begin

            for J in Dcls'Range loop
               if Element_Kind (Dcls (J)) = A_Declaration then
                  State.Detected := True;
                  exit;
               end if;
            end loop;

         end;

      end if;

   end Rule_Check_Pre_Op;

   ------------------------
   -- Default_Parameters --
   ------------------------

   procedure Init_Rule (Rule : in out Default_Parameters_Rule_Type) is
   begin
      Init_Rule (Rule_Template (Rule));
      Rule.Name        := new String'("Default_Parameters");
      Rule.Synonym     := new String'("Default_Par");
      Rule.Rule_Status := Fully_Implemented;
      Rule.Help_Info   := new String'("declarations of default " &
                                       "subprogram parameters");
      Rule.Diagnosis   := new String'("default value for subprogram " &
                                      "parameter");
   end Init_Rule;

   procedure Rule_Check_Pre_Op
     (Rule    : in out Default_Parameters_Rule_Type;
      Element :        Asis.Element;
      Control : in out Traverse_Control;
      State   : in out Rule_Traversal_State)
   is
      pragma Unreferenced (Rule);
      pragma Unreferenced (Control);
   begin
      if Declaration_Kind (Element) = A_Parameter_Specification
        and then
         not Is_Nil (Initialization_Expression (Element))
      then
         State.Detected := True;
      end if;
   end Rule_Check_Pre_Op;

   ---------------------------
   -- Discriminated_Records --
   ---------------------------

   procedure Init_Rule (Rule : in out Discriminated_Records_Rule_Type) is
   begin
      Init_Rule (Rule_Template (Rule));
      Rule.Name        := new String'("Discriminated_Records");
      Rule.Synonym     := new String'("Discr_Rec");
      Rule.Rule_Status := Fully_Implemented;
      Rule.Help_Info   := new String'("discriminated records");
      Rule.Diagnosis   := new String'("declaration of discriminated record");
   end Init_Rule;

   procedure Rule_Check_Pre_Op
     (Rule    : in out Discriminated_Records_Rule_Type;
      Element :        Asis.Element;
      Control : in out Traverse_Control;
      State   : in out Rule_Traversal_State)
   is
      Encl_Type_Decl : Asis.Element;
      pragma Unreferenced (Rule);
      pragma Unreferenced (Control);
   begin

      if Definition_Kind (Element) = A_Known_Discriminant_Part then
         Encl_Type_Decl := Enclosing_Element (Element);

         if Declaration_Kind (Encl_Type_Decl) =
            An_Ordinary_Type_Declaration
         then
            Encl_Type_Decl := Type_Declaration_View (Encl_Type_Decl);

            if Type_Kind (Encl_Type_Decl) =
               A_Derived_Record_Extension_Definition
              or else
               Type_Kind (Encl_Type_Decl) in A_Record_Type_Definition ..
                  A_Tagged_Record_Type_Definition
            then
               State.Detected := True;
            end if;

         end if;

      end if;

   end Rule_Check_Pre_Op;

   ----------------------------
   -- Expanded_Loop_Exit_Names --
   ----------------------------

   procedure Init_Rule (Rule : in out Expanded_Loop_Exit_Names_Rule_Type) is
   begin
      Init_Rule (Rule_Template (Rule));
      Rule.Name        := new String'("Expanded_Loop_Exit_Names");
      Rule.Synonym     := new String'("Simple_Loop_Exit_Names");
      Rule.Rule_Status := Fully_Implemented;
      Rule.Help_Info  := new String'("expanded loop names in exit statements");
      Rule.Diagnosis   := new String'("expanded loop name in exit statement");
   end Init_Rule;

   procedure Rule_Check_Pre_Op
     (Rule    : in out Expanded_Loop_Exit_Names_Rule_Type;
      Element :        Asis.Element;
      Control : in out Traverse_Control;
      State   : in out Rule_Traversal_State)
   is
      pragma Unreferenced (Rule);
      pragma Unreferenced (Control);
   begin

      if Statement_Kind (Element) = An_Exit_Statement
        and then
         Expression_Kind (Exit_Loop_Name (Element)) = A_Selected_Component
      then
         State.Detected  := True;
      end if;

   end Rule_Check_Pre_Op;

   ---------------------------
   -- Float_Equality_Checks --
   ---------------------------

   procedure Init_Rule (Rule : in out Float_Equality_Checks_Rule_Type) is
   begin
      Init_Rule (Rule_Template (Rule));
      Rule.Name        := new String'("Float_Equality_Checks");
      Rule.Rule_Status := Fully_Implemented;
      Rule.Help_Info   := new String'("equality for float values");
      Rule.Diagnosis   := new String'("use of equality operation " &
                                      "for float values");
   end Init_Rule;

   procedure Rule_Check_Pre_Op
     (Rule    : in out Float_Equality_Checks_Rule_Type;
      Element :        Asis.Element;
      Control : in out Traverse_Control;
      State   : in out Rule_Traversal_State)
   is
      pragma Unreferenced (Rule);
      pragma Unreferenced (Control);

      Call_Element : Asis.Element;
   begin

      if Operator_Kind (Element) in
           An_Equal_Operator .. A_Not_Equal_Operator
      then
         Call_Element := Get_Call_Element;

         if Expression_Kind (Call_Element) = A_Function_Call
           and then
            Is_Predefined (Element)
           and then
            Is_Float
              (Actual_Parameter (Function_Call_Parameters (Call_Element) (1)))
         then
            State.Detected := True;
         end if;

      end if;

   end Rule_Check_Pre_Op;

   ---------------------
   -- GOTO_Statements --
   ---------------------

   procedure Init_Rule (Rule : in out GOTO_Statements_Rule_Type) is
   begin
      Init_Rule (Rule_Template (Rule));

      Rule.Name        := new String'("GOTO_Statements");
      Rule.Synonym     := new String'("Goto");
      Rule.Rule_Status := Fully_Implemented;
      Rule.Help_Info   := new String'("goto statements");
      Rule.Diagnosis   := new String'("goto statement");
   end Init_Rule;

   procedure Rule_Check_Pre_Op
     (Rule    : in out GOTO_Statements_Rule_Type;
      Element :        Asis.Element;
      Control : in out Traverse_Control;
      State   : in out Rule_Traversal_State)
   is
      pragma Unreferenced (Rule);
      pragma Unreferenced (Control);
   begin

      if Statement_Kind (Element) = A_Goto_Statement then
         State.Detected := True;
      end if;

   end Rule_Check_Pre_Op;

   ----------------------
   -- Improper_Returns --
   ----------------------

   procedure Init_Rule (Rule : in out Improper_Returns_Rule_Type) is
   begin
      Init_Rule (Rule_Template (Rule));

      Rule.Name        := new String'("Improper_Returns");
      Rule.Synonym     := new String'("Proper_Returns");
      Rule.Rule_Status := Fully_Implemented;
      Rule.Help_Info   := new String'("improper use of return statements");
      Rule.Diagnosis   :=
        new String'("#1#return statement in a procedure body" &
                    "#2#more than one return statement in a function body");
   end Init_Rule;

   procedure Rule_Check_Pre_Op
     (Rule    : in out Improper_Returns_Rule_Type;
      Element :        Asis.Element;
      Control : in out Traverse_Control;
      State   : in out Rule_Traversal_State)
   is
      Enclosing_Body_Idx : Elmt_Idx;
      pragma Unreferenced (Rule);
      pragma Unreferenced (Control);
   begin

      if Statement_Kind (Element) = A_Return_Statement then
         Enclosing_Body_Idx := Enclosing_Subprogram_Body;

         pragma Assert (Present (Enclosing_Body_Idx));

         case Declaration_Kind (Get_Element (Enclosing_Body_Idx)) is
            when A_Procedure_Body_Declaration =>
               State.Detected  := True;
               State.Diagnosis := 1;

            when A_Function_Body_Declaration =>

               if Has_Return (Enclosing_Body_Idx) then
                  State.Detected  := True;
                  State.Diagnosis := 2;
               else
                  Set_Has_Return (Enclosing_Body_Idx);
               end if;

            when others =>
               null;
         end case;

      end if;

   end Rule_Check_Pre_Op;

   -----------------------------------------
   -- Improperly_Called_Protected_Entries --
   -----------------------------------------

--   procedure Init_Rule
--     (Rule : in out Improperly_Called_Protected_Entries_Rule_Type)
--   is
--   begin
--      Rule.Name       := new String'("Improperly_Called_Protected_Entries");
--      Rule.Synonym    := new String'("Max_Calls_To_Protected_Entry");
--      Rule.Rule_State := Disabled;
--    Rule.Help_Info  := new String'("max number of calls to protected entry");
--      Rule.Diagnosis  := new String'("more than one call to this entry");
--   end Init_Rule;

--   procedure Check_Global_Structure_Node
--     (Rule     :     Improperly_Called_Protected_Entries_Rule_Type;
--      N        :     Gnatcheck.Global_State.GS_Node_Id;
--      Detected : out Boolean)
--   is
--      pragma Unreferenced (Rule);

--      Next_N       : GS_Node_Id;
--      Num_Of_Calls : Natural := 0;
--   begin
--      Detected := False;

--      if GS_Node_Kind (N) = A_Protected_Entry then

--         Reset_Itrerator (For_Node => N, Call_Set => Calls);
--         Next_N := Next_Node;

--         while Present (Next_N) loop

--            if GS_Node_Kind (Next_N) = A_Task_Object then
--               Num_Of_Calls := Num_Of_Calls + 1;

--               if Num_Of_Calls > 1 then
--                  Detected := True;
--                  exit;
--               end if;

--            end if;

--            Next_N := Next_Node;
--         end loop;

--      end if;

--   end Check_Global_Structure_Node;

   -----------------------
   -- Limited_Renamings --
   -----------------------

   procedure Init_Rule (Rule : in out Limited_Renamings_Rule_Type) is
   begin
      Init_Rule (Rule_Template (Rule));

      Rule.Name        := new String'("Limited_Renamings");
      Rule.Help_Info   := new String'("restrictions on renaming declarations");
      Rule.Diagnosis   :=
        new String'("#1#this form of renaming is not allowed" &
                    "#2#222222222222222222");
   end Init_Rule;

   --  This rule combines a part of the SPARK limitations imposed on renamings.
   --  It is formulated for renaming declarations only, not for the use of
   --  renamed entities. The following situations should be detected:

   --  Renamed entities: only subprograms declared immediately within package
   --  specs and only child packages can be renamed. All other cases of
   --  subprogram and package renamings should be detected.

   --  Limitations on new and old names: the name after RENAMES should have
   --  the form package_name.entity_name, the new name should be exactly the
   --  same as entity_name, entity_name should not itself be detected by a
   --  renaming declaration. Moreover, in case of a subprogram renaming, the
   --  new and the old profiles should be the same (the same parameter and
   --  subtype names should be used). All the renamings that do not follow
   --  these rules should be detected.

   --  Placement of renamings. According to the definition of SPARK, the
   --  following restrictions are imposed on the placement of renaming
   --  declarations:
   --
   --  for subprogram renamings (that are not operator renamings):
   --  - if the package where the subprogram is declared is a local package,
   --    renaming is allowed immediately after the declaration of this
   --    package;
   --  - if the package where the subprogram is declared is a library-level
   --    package, renaming is allowed at the start of the body of a library
   --    package (or library subprogram) that "withes" this package;
   --
   --  for operator renamings: the same is allowed as for subprogram renamings
   --  plus renamings are allowed at the start of the visible or private part
   --  of the library package that "withes" the package where the operator is
   --  declared;
   --
   --  for package renamings: the renaming of a (child!) package is allowed
   --  only immediately within a library package or library subprogram that
   --  "withes" this package;
   --
   --  All the other cases of renaming declaration placement should be
   --  detected.
   --
   --  Note that the rules about allowed renaming placement are taken from the
   --  definition of the SPARK subset. This definition uses the following
   --  wording: "at the start of the body", "at the start of the visible
   --  (private) part", and it the same time the SPARK book contains examples
   --  where an (allowed) renaming follows an object declaration. So we do
   --  not check that renamings precede all the non-renaming declarations.

   --  All the things stated above are checked and detected in the order given.
   --  If some feature is detected for a renaming declaration, all the other
   --  things are not checked for this renaming declaration

   --  Current implementation limitation: an the moment this rule is not
   --  completely checked for renaming of predefined operators.

   procedure Rule_Check_Pre_Op
     (Rule    : in out Limited_Renamings_Rule_Type;
      Element :        Asis.Element;
      Control : in out Traverse_Control;
      State   : in out Rule_Traversal_State)
   is
   pragma Unreferenced (Rule, Control);
   begin

      --  Syntax of renaming declarations: all forms of  renaming declarations
      --  except package and subprogram renaming declarations should be
      --  detected.

      case Declaration_Kind (Element) is
         when An_Object_Renaming_Declaration           |
              An_Exception_Renaming_Declaration        |
              A_Generic_Package_Renaming_Declaration   |
              A_Generic_Procedure_Renaming_Declaration =>

            State.Detected  := True;
            State.Diagnosis := 1;
            return;

         when A_Package_Renaming_Declaration   |
              A_Procedure_Renaming_Declaration |
              A_Function_Renaming_Declaration  =>

            --  We have to analyze this renaming in more details
            null;

         when others =>
            --  Not a renaming declaration, so - nothing to do!
            return;
      end case;

      --  Renamed entities: only subprograms declared immediately within
      --  package specs and only child packages can be renamed. All other cases
      --  of subprogram and package renamings should be detected.

      --  ... to be continued...
   end Rule_Check_Pre_Op;

   -------------------------------
   -- Library_Level_Subprograms --
   -------------------------------

   procedure Init_Rule (Rule : in out Library_Level_Subprograms_Rule_Type) is
   begin
      Init_Rule (Rule_Template (Rule));

      Rule.Name        := new String'("Library_Level_Subprograms");
      Rule.Synonym     := new String'("LL_Subpr");
      Rule.Rule_Status := Fully_Implemented;
      Rule.Help_Info   := new String'("library level subprograms");
      Rule.Diagnosis   := new String'("declaration of library level " &
                                     "subprogram");
   end Init_Rule;

   procedure Rule_Check_Pre_Op
     (Rule    : in out Library_Level_Subprograms_Rule_Type;
      Element :        Asis.Element;
      Control : in out Traverse_Control;
      State   : in out Rule_Traversal_State)
   is
      El_Kind : constant Flat_Element_Kinds := Flat_Element_Kind (Element);
      pragma Unreferenced (Rule);
      pragma Unreferenced (Control);
   begin

      case El_Kind is
         when A_Procedure_Declaration      |
              A_Function_Declaration       |
              A_Procedure_Instantiation    |
              A_Function_Instantiation     |
              A_Procedure_Body_Declaration |
              A_Function_Body_Declaration  =>

            if Is_Nil (Enclosing_Element (Element))
              and then
               (El_Kind not in A_Procedure_Body_Declaration ..
                               A_Function_Body_Declaration
                 or else
                Is_Nil (Corresponding_Declaration (Element)))
              and then
               Unit_Class (Enclosing_Compilation_Unit (Element)) /=
               A_Separate_Body
            then
               --  If both spec and body are provided for library-level
               --  subprogram, we do not detect the rule violation for the
               --  body
               State.Detected := True;
            end if;

         when others =>
            null;
      end case;

   end Rule_Check_Pre_Op;

   --------------------
   -- Local_Packages --
   --------------------

   procedure Init_Rule (Rule : in out Local_Packages_Rule_Type) is
   begin
      Init_Rule (Rule_Template (Rule));

      Rule.Name        := new String'("Local_Packages");
      Rule.Synonym     := new String'("Local_Pckg");
      Rule.Rule_Status := Fully_Implemented;
      Rule.Help_Info   := new String'("local packages");
      Rule.Diagnosis   := new String'("declaration of local package");
   end Init_Rule;

   procedure Rule_Check_Pre_Op
     (Rule    : in out Local_Packages_Rule_Type;
      Element :        Asis.Element;
      Control : in out Traverse_Control;
      State   : in out Rule_Traversal_State)
   is
      Decl_Kind : constant Declaration_Kinds := Declaration_Kind (Element);
      CU_Kind   : constant Unit_Kinds        := Unit_Kind (
        Enclosing_Compilation_Unit (Element));
      pragma Unreferenced (Rule);
      pragma Unreferenced (Control);
   begin

      if Decl_Kind = A_Package_Declaration
       and then
         (CU_Kind = A_Package or else CU_Kind = A_Generic_Package)
       and then
         not Is_Nil (Enclosing_Element (Element))
      then
         State.Detected := True;
      end if;

   end Rule_Check_Pre_Op;

   -----------------------------------------------
   -- Multiple_Entries_In_Protected_Definitions --
   -----------------------------------------------

   procedure Init_Rule
     (Rule : in out Multiple_Entries_In_Protected_Definitions_Rule_Type)
   is
   begin
      Init_Rule (Rule_Template (Rule));

      Rule.Name := new String'("Multiple_Entries_In_Protected_Definitions");
      Rule.Synonym     := new String'("One_Entry_In_PO");
      Rule.Rule_Status := Fully_Implemented;
      Rule.Help_Info   := new String'("protected definitions with more than " &
                                      "one entry");
      Rule.Diagnosis   := new String'("more than one entry in protected " &
                                      "definition");
   end Init_Rule;

   procedure Rule_Check_Pre_Op
     (Rule    : in out Multiple_Entries_In_Protected_Definitions_Rule_Type;
      Element :        Asis.Element;
      Control : in out Traverse_Control;
      State   : in out Rule_Traversal_State)
   is
      pragma Unreferenced (Rule);
      pragma Unreferenced (Control);
      Enclosing_Def : Asis.Element;
   begin

      --  If we would check this rule not for an entry declaration, but for the
      --  whole protected definition, this would be more effective

      if Declaration_Kind (Element) = An_Entry_Declaration then
         Enclosing_Def := Get_Enclosing_Element;

         if Definition_Kind (Enclosing_Def) = A_Protected_Definition then

            declare
               Visible_Entries : constant Asis.Element_List :=
                 Visible_Part_Items (Enclosing_Def);
               Visible_Entries_Num : Natural := 0;
               First_Visible_Entry : Natural := 0;

               Private_Entries : constant Asis.Element_List :=
                 Private_Part_Items (Enclosing_Def);
               Private_Entries_Num : Natural := 0;
               First_Private_Entry : Natural := 0;
            begin

               for J in Visible_Entries'Range loop

                  if Declaration_Kind (Visible_Entries (J)) =
                     An_Entry_Declaration
                  then
                     Visible_Entries_Num := Visible_Entries_Num + 1;

                     if First_Visible_Entry = 0 then
                        First_Visible_Entry := First_Visible_Entry + 1;
                     end if;
                  end if;

               end loop;

               if First_Visible_Entry /= 0
                 and then
                  Is_Equal (Element, Visible_Entries (First_Visible_Entry))
               then
                  --  This is the first declared entry, in no case we can
                  --  generate a diagnosis for it
                  return;
               end if;

               if Visible_Entries_Num > 0 then
                  State.Detected := True;
                  --  No need to analyze private entries, if any
                  return;
               else

                  for J in Private_Entries'Range loop

                     if Declaration_Kind (Private_Entries (J)) =
                        An_Entry_Declaration
                     then
                        Private_Entries_Num := Private_Entries_Num + 1;

                        if First_Private_Entry = 0 then
                           First_Private_Entry := First_Private_Entry + 1;
                        end if;
                     end if;

                  end loop;

                  if not Is_Equal (Element,
                         Private_Entries (First_Private_Entry))
                  then
                     State.Detected := True;
                  end if;

               end if;

            end;

         end if;

      end if;

   end Rule_Check_Pre_Op;

   ------------------
   -- Name_Clashes --
   ------------------

   ------------------------------------------
   -- Activate_In_Test_Mode (Name_Clashes) --
   ------------------------------------------

   overriding procedure Activate_In_Test_Mode
     (Rule : in out Name_Clashes_Rule_Type)
   is
   begin
      Activate_In_Test_Mode (Rule_Template (Rule));

      --  And now add something to the dictionary
      Gnatcheck.Name_Dictionary.Add_To_Dictionary ("one");
      Gnatcheck.Name_Dictionary.Add_To_Dictionary ("two");
      Gnatcheck.Name_Dictionary.Add_To_Dictionary ("three");
   end Activate_In_Test_Mode;

   procedure Init_Rule (Rule : in out Name_Clashes_Rule_Type) is
   begin
      Init_Rule (Rule_Template (Rule));

      Rule.Name        := new String'("Name_Clashes");
      Rule.Synonym     := new String'("Restrict_Name_Space");
      Rule.Rule_Status := Fully_Implemented;
      Rule.Help_Info   := new String'("restrictions on name space");
      Rule.Diagnosis   := new String'("use of forbidden identifier");
   end Init_Rule;

   --------------------------------------
   -- More_Rule_Comment (Name_Clashes) --
   --------------------------------------

   function More_Rule_Comment
     (Rule          : Name_Clashes_Rule_Type;
      Template_Kind : Template_Coding_Standard_Kinds)
      return String
   is
      pragma Unreferenced (Rule);
   begin
      if Template_Kind = Template_All_ON then
         return "provide an actual dictionary file and then activate the rule";
      else
         return "provide a proper dictionary file as a parameter " &
                "if the rule is enabled";
      end if;
   end More_Rule_Comment;

   procedure Rule_Check_Pre_Op
     (Rule    : in out Name_Clashes_Rule_Type;
      Element :        Asis.Element;
      Control : in out Traverse_Control;
      State   : in out Rule_Traversal_State)
   is
      pragma Unreferenced (Rule);
      pragma Unreferenced (Control);
   begin

      if (Defining_Name_Kind (Element) = A_Defining_Identifier
         or else
          Defining_Name_Kind (Element) = A_Defining_Enumeration_Literal)
        and then
         Gnatcheck.Name_Dictionary.Name_In_Dictionary
           (Defining_Name_Image (Element))
      then
         State.Detected  := True;
      end if;

   end Rule_Check_Pre_Op;

   -------------------------------------------
   -- Process_Rule_Parameter (Name_Clashes) --
   -------------------------------------------

   procedure Process_Rule_Parameter
     (Rule       : in out Name_Clashes_Rule_Type;
      Param      :        String;
      Enable     :        Boolean;
      Defined_At :        String)
   is
      pragma Unreferenced (Defined_At);
      Success : Boolean := False;
   begin

      if Param = "" then

         if Enable then
            Error ("(" & Rule.Name.all & ") parameter is required for +R");
         else
            Rule.Rule_State := Disabled;
         end if;

      else

         if Enable then
            Gnatcheck.Name_Dictionary.Scan_Dictionary_File (Param, Success);

            if Success then
               Rule.Rule_State := Enabled;
            else
               Error ("(" & Rule.Name.all & "): cannot set rule parameter");
            end if;
         else
            Error ("(" & Rule.Name.all & ") no parameter allowed for -R");
         end if;

      end if;

   end Process_Rule_Parameter;

   --------------------------------
   -- Rule_Option (Name_Clashes) --
   --------------------------------

   function Rule_Option
     (Rule          : Name_Clashes_Rule_Type;
      Template_Kind : Template_Coding_Standard_Kinds)
      return          String
   is
   begin
      if Template_Kind = Template_All_ON then
         return "-- " & Rule_Option (Rule_Template (Rule), Template_Kind) &
                 " : dictionary";
      else
         return Rule_Option (Rule_Template (Rule), Template_Kind);
      end if;
   end Rule_Option;

   procedure XML_Rule_Help
     (Rule  : Name_Clashes_Rule_Type;
      Level : Natural)
   is
   begin
      Info (Level * Ident_String             &
            "<field switch=""+R"              &
            Rule.Name.all                    &
            """ label="""                    &
            Rule.Help_Info.all               &
            " (specify dictionary of forbidden names) """ &
            " as-file=""true""/>");
   end XML_Rule_Help;

   ------------------------------
   -- Non_Qualified_Aggregates --
   ------------------------------

   procedure Init_Rule (Rule : in out Non_Qualified_Aggregates_Rule_Type) is
   begin
      Init_Rule (Rule_Template (Rule));

      Rule.Name        := new String'("Non_Qualified_Aggregates");
      Rule.Synonym     := new String'("Qualified_Aggr");
      Rule.Rule_Status := Fully_Implemented;
      Rule.Help_Info   := new String'("non-qualified aggregates");
      Rule.Diagnosis   := new String'("aggregate is not a part of a " &
                                      "qualified expression");
   end Init_Rule;

   procedure Rule_Check_Pre_Op
     (Rule    : in out Non_Qualified_Aggregates_Rule_Type;
      Element :        Asis.Element;
      Control : in out Traverse_Control;
      State   : in out Rule_Traversal_State)
   is
      pragma Unreferenced (Rule);
      pragma Unreferenced (Control);
   begin

      if Expression_Kind (Element) in
         A_Record_Aggregate .. A_Named_Array_Aggregate
       and then
         not Is_Nil (Corresponding_Expression_Type (Element))
         --  This condition filters out aggregates of anonymous array types
       and then
         Expression_Kind (Get_Enclosing_Element) /= A_Qualified_Expression
      then
         State.Detected  := True;
      end if;

   end Rule_Check_Pre_Op;

   --------------------------
   -- Non_SPARK_Attributes --
   --------------------------

   procedure Init_Rule (Rule : in out Non_SPARK_Attributes_Rule_Type) is
   begin
      Init_Rule (Rule_Template (Rule));

      Rule.Name        := new String'("Non_SPARK_Attributes");
      Rule.Synonym     := new String'("SPARK_Attributes");
      Rule.Rule_Status := Fully_Implemented;
      Rule.Help_Info   := new String'("attributes that are not from the " &
                                      "SPARK subset");
      Rule.Diagnosis  := new String'("attribute is not from the SPARK subset");
   end Init_Rule;

   procedure Rule_Check_Pre_Op
     (Rule    : in out Non_SPARK_Attributes_Rule_Type;
      Element :        Asis.Element;
      Control : in out Traverse_Control;
      State   : in out Rule_Traversal_State)
   is
      pragma Unreferenced (Rule);
      pragma Unreferenced (Control);
   begin

      if Expression_Kind (Element) = An_Attribute_Reference then

         case Attribute_Kind (Element) is
            when An_Adjacent_Attribute          |
                 An_Aft_Attribute               |
                 A_Base_Attribute               |
                 A_Ceiling_Attribute            |
                 A_Component_Size_Attribute     |
                 A_Compose_Attribute            |
                 A_Copy_Sign_Attribute          |
                 A_Delta_Attribute              |
                 A_Denorm_Attribute             |
                 A_Digits_Attribute             |
                 An_Exponent_Attribute          |
                 A_First_Attribute              |
                 A_Floor_Attribute              |
                 A_Fore_Attribute               |
                 A_Fraction_Attribute           |
                 A_Last_Attribute               |
                 A_Leading_Part_Attribute       |
                 A_Length_Attribute             |
                 A_Machine_Attribute            |
                 A_Machine_Emax_Attribute       |
                 A_Machine_Emin_Attribute       |
                 A_Machine_Mantissa_Attribute   |
                 A_Machine_Overflows_Attribute  |
                 A_Machine_Radix_Attribute      |
                 A_Machine_Rounds_Attribute     |
                 A_Max_Attribute                |
                 A_Min_Attribute                |
                 A_Model_Attribute              |
                 A_Model_Emin_Attribute         |
                 A_Model_Epsilon_Attribute      |
                 A_Model_Mantissa_Attribute     |
                 A_Model_Small_Attribute        |
                 A_Modulus_Attribute            |
                 A_Pos_Attribute                |
                 A_Pred_Attribute               |
                 A_Range_Attribute              |
                 A_Remainder_Attribute          |
                 A_Rounding_Attribute           |
                 A_Safe_First_Attribute         |
                 A_Safe_Last_Attribute          |
                 A_Scaling_Attribute            |
                 A_Signed_Zeros_Attribute       |
                 A_Size_Attribute               |
                 A_Small_Attribute              |
                 A_Succ_Attribute               |
                 A_Truncation_Attribute         |
                 An_Unbiased_Rounding_Attribute |
                 A_Val_Attribute                |
                 A_Valid_Attribute              =>

               --  Spark subset, this is allowed
               null;
            when others =>
               State.Detected := True;
         end case;

      end if;

   end Rule_Check_Pre_Op;

   ------------------------------
   -- Non_Tagged_Derived_Types --
   ------------------------------

   procedure Init_Rule (Rule : in out Non_Tagged_Derived_Types_Rule_Type) is
   begin
      Init_Rule (Rule_Template (Rule));

      Rule.Name        := new String'("Non_Tagged_Derived_Types");
      Rule.Synonym     := new String'("Derived_Types");
      Rule.Rule_Status := Fully_Implemented;
      Rule.Help_Info   := new String'("derived types that are not " &
                                       "type extensions");
      Rule.Diagnosis   := new String'("derived type that is not a type " &
                                      "extension");
   end Init_Rule;

   procedure Rule_Check_Pre_Op
     (Rule    : in out Non_Tagged_Derived_Types_Rule_Type;
      Element :        Asis.Element;
      Control : in out Traverse_Control;
      State   : in out Rule_Traversal_State)
   is
      pragma Unreferenced (Rule);
      pragma Unreferenced (Control);
   begin

      if Type_Kind (Element) = A_Derived_Type_Definition then
         State.Detected := True;
      end if;

   end Rule_Check_Pre_Op;

   ----------------------
   -- Outer_Loop_Exits --
   ----------------------

   procedure Init_Rule (Rule : in out Outer_Loop_Exits_Rule_Type) is
   begin
      Init_Rule (Rule_Template (Rule));

      Rule.Name        := new String'("Outer_Loop_Exits");
      Rule.Synonym     := new String'("Global_Loop_Exit");
      Rule.Rule_Status := Fully_Implemented;
      Rule.Help_Info   := new String'("exiting more than one loop at once");
      Rule.Diagnosis   := new String'("exit out of the nesting loop");
   end Init_Rule;

   procedure Rule_Check_Pre_Op
     (Rule    : in out Outer_Loop_Exits_Rule_Type;
      Element :        Asis.Element;
      Control : in out Traverse_Control;
      State   : in out Rule_Traversal_State)
   is
      pragma Unreferenced (Rule);
      pragma Unreferenced (Control);
      Loop_Name      : Asis.Element;
      Enclosing_Loop : Asis.Element;
      Steps_Up       : Elmt_Idx := 0;
   begin

      if Statement_Kind (Element) = An_Exit_Statement then
         Loop_Name := Exit_Loop_Name (Element);

         if not Is_Nil (Loop_Name) then

            if Expression_Kind (Loop_Name) = A_Selected_Component then
               Loop_Name := Selector (Loop_Name);
            end if;

            loop
               Enclosing_Loop := Get_Enclosing_Element (Steps_Up);

               exit when Statement_Kind (Enclosing_Loop) in
                  A_Loop_Statement .. A_For_Loop_Statement;

               Steps_Up := Steps_Up + 1;
            end loop;

            if not Is_Equal
                 (Corresponding_Name_Definition (Loop_Name),
                  Statement_Identifier (Enclosing_Loop))
            then
               State.Detected := True;
            end if;

         end if;

      end if;

   end Rule_Check_Pre_Op;

   --------------------------
   -- Overloaded_Operators --
   --------------------------

   procedure Init_Rule (Rule : in out Overloaded_Operators_Rule_Type) is
   begin
      Init_Rule (Rule_Template (Rule));

      Rule.Name        := new String'("Overloaded_Operators");
      Rule.Rule_Status := Fully_Implemented;
      Rule.Help_Info   := new String'("operator overloading");
      Rule.Diagnosis   := new String'("overloading of an operator symbol");
   end Init_Rule;

   procedure Rule_Check_Pre_Op
     (Rule    : in out Overloaded_Operators_Rule_Type;
      Element :        Asis.Element;
      Control : in out Traverse_Control;
      State   : in out Rule_Traversal_State)
   is
      pragma Unreferenced (Rule);
      pragma Unreferenced (Control);

      Enclosing_Decl : Asis.Element;
   begin

      if Defining_Name_Kind (Element) = A_Defining_Operator_Symbol then
         Enclosing_Decl := Get_Enclosing_Element;

         if Defining_Name_Kind (Enclosing_Decl) = A_Defining_Expanded_Name then
            Enclosing_Decl := Get_Enclosing_Element (Steps_Up => 1);
         end if;

         case Declaration_Kind (Enclosing_Decl) is
            when A_Function_Body_Declaration =>

               if Is_Nil (Corresponding_Declaration (Enclosing_Decl)) then
                  State.Detected := True;
               end if;

            when A_Function_Renaming_Declaration =>

               if not Is_Renaming_As_Body (Enclosing_Decl) then
                  State.Detected := True;
               end if;

            when others =>
               State.Detected := True;
         end case;

      end if;

   end Rule_Check_Pre_Op;

   -----------
   -- Slices --
   -----------

   procedure Init_Rule (Rule : in out Slices_Rule_Type) is
   begin
      Init_Rule (Rule_Template (Rule));

      Rule.Name        := new String'("Slices");
      Rule.Rule_Status := Fully_Implemented;
      Rule.Help_Info   := new String'("slices");
      Rule.Diagnosis   := new String'("slice");
   end Init_Rule;

   procedure Rule_Check_Pre_Op
     (Rule    : in out Slices_Rule_Type;
      Element :        Asis.Element;
      Control : in out Traverse_Control;
      State   : in out Rule_Traversal_State)
   is
      pragma Unreferenced (Rule);
      pragma Unreferenced (Control);
   begin

      if Expression_Kind (Element) = A_Slice then
         State.Detected := True;
      end if;

   end Rule_Check_Pre_Op;

   ---------------------------------
   -- Unconstrained_Array_Returns --
   ---------------------------------

   ---------------------------------------------------------
   -- Activate_In_Test_Mode (Unconstrained_Array_Returns) --
   ---------------------------------------------------------

   overriding procedure Activate_In_Test_Mode
     (Rule : in out Unconstrained_Array_Returns_Rule_Type)
   is
   begin
      Process_Rule_Parameter
        (Rule       => Rule,
         Param      => "Except_String",
         Enable     => True,
         Defined_At => "");
   end Activate_In_Test_Mode;

   function Exception_Name
     (Rule      : Unconstrained_Array_Returns_Rule_Type;
      Exc_Index : Exception_Index)
      return      String
   is
      pragma Unreferenced (Rule);
   begin
      case Exc_Index is
         when 1 =>
            return "Except_String";
         when others =>
            return "";
      end case;
   end Exception_Name;

   function Exception_Number
     (Rule     : Unconstrained_Array_Returns_Rule_Type;
      Exc_Name : String)
      return     Exception_Numbers
   is
      pragma Unreferenced (Rule);
      Result : Exception_Numbers := Not_An_Exception;
      Normalized_Exc_Name : constant String := To_Lower (Exc_Name);
   begin
      if Normalized_Exc_Name = "except_string" then
         Result := 1;
      end if;

      return Result;
   end Exception_Number;

   procedure Init_Rule (Rule : in out Unconstrained_Array_Returns_Rule_Type) is
   begin
      Init_Rule (Rule_With_Exceptions_Template (Rule));

      Rule.Name        := new String'("Unconstrained_Array_Returns");
      Rule.Synonym     := new String'("Unconstr_Array_Return");
      Rule.Rule_Status := Fully_Implemented;
      Rule.Help_Info   := new String'("functions returning unconstrained " &
                                       "arrays");
      Rule.Diagnosis   := new String'("function returns unconstrained array");

      Rule.Check_In_Expanded_Generics := True;
   end Init_Rule;

   procedure Rule_Check_Pre_Op
     (Rule    : in out Unconstrained_Array_Returns_Rule_Type;
      Element :        Asis.Element;
      Control : in out Traverse_Control;
      State   : in out Rule_Traversal_State)
   is
      Arg_Element  : Asis.Element := Element;
      Return_Type  : Asis.Element;

      pragma Unreferenced (Control);
   begin

      if In_Executable_Code (State)
        and then
         (Declaration_Kind (Element) = A_Function_Declaration
         or else
          ((Declaration_Kind (Element) in A_Function_Body_Declaration        |
                                          An_Expression_Function_Declaration |
                                          A_Function_Body_Stub)
            and then
           Acts_As_Spec (Element)))
      then
         if Declaration_Kind (Element) = A_Function_Instantiation then
            Arg_Element := Corresponding_Declaration (Arg_Element);
         end if;

         Return_Type := Result_Profile (Arg_Element);

         if Expression_Kind (Return_Type) = A_Selected_Component then
            Return_Type := Asis.Expressions.Selector (Return_Type);
         end if;

         if  Expression_Kind (Return_Type) = An_Identifier then

            Return_Type := Corresponding_Name_Declaration (Return_Type);

            if Is_Unconstrained_Array (Return_Type) then

               if Rule.Exceptions (1) then
                  State.Detected := not Is_Predefined_String (Return_Type);
               else
                  State.Detected := True;
               end if;
            end if;

         end if;

      end if;

   end Rule_Check_Pre_Op;

   ----------------------
   -- Universal_Ranges --
   ----------------------

   procedure Init_Rule (Rule : in out Universal_Ranges_Rule_Type) is
   begin
      Init_Rule (Rule_Template (Rule));

      Rule.Name        := new String'("Universal_Ranges");
      Rule.Synonym     := new String'("Universl_Ranges");
      Rule.Rule_Status := Fully_Implemented;
      Rule.Help_Info   := new String'("ranges with universal integer bounds");
      Rule.Diagnosis   := new String'("range with universal integer bounds");
   end Init_Rule;

   procedure Rule_Check_Pre_Op
     (Rule    : in out Universal_Ranges_Rule_Type;
      Element :        Asis.Element;
      Control : in out Traverse_Control;
      State   : in out Rule_Traversal_State)
   is
      Encl_Element : Asis.Element;
      Left_Bound   : Asis.Element;
      Right_Bound  : Asis.Element;
      Type_Def_El  : Asis.Element;

      pragma Unreferenced (Rule);
      pragma Unreferenced (Control);
   begin

      if Discrete_Range_Kind (Element) =
           A_Discrete_Simple_Expression_Range
      then
         Encl_Element := Get_Enclosing_Element;

         if Constraint_Kind (Encl_Element) = An_Index_Constraint
           or else
            Definition_Kind (Encl_Element) = A_Discrete_Subtype_Definition
           or else
            Declaration_Kind (Encl_Element) = A_Loop_Parameter_Specification
         then
            Left_Bound := Lower_Bound (Element);

            Type_Def_El :=
              Type_Declaration_View (Corresponding_Expression_Type
                (Left_Bound));

            if Root_Type_Kind (Type_Def_El) =
               A_Universal_Integer_Definition
            then

               Right_Bound := Upper_Bound (Element);

               Type_Def_El :=
                 Type_Declaration_View (Corresponding_Expression_Type
                   (Right_Bound));

               if Root_Type_Kind (Type_Def_El) =
                  A_Universal_Integer_Definition
               then
                  State.Detected := True;
               end if;

            end if;

         end if;

      end if;

   end Rule_Check_Pre_Op;

   ------------------------
   -- Unused_Subprograms --
   ------------------------

--  procedure Analyze_Global_Structure (Rule : Unused_Subprograms_Rule_Type) is
--      pragma Unreferenced (Rule);
--   begin
--      Gnatcheck.Global_State.CG.Set_Used_Entities;
--   end Analyze_Global_Structure;

--   procedure Check_Global_Structure_Node
--     (Rule     :     Unused_Subprograms_Rule_Type;
--      N        :     Gnatcheck.Global_State.GS_Node_Id;
--      Detected : out Boolean)
--   is
--      pragma Unreferenced (Rule);
--   begin
--      Detected :=
--         Is_Subprogram (N)
--        and then
--         GS_Node_Renaming_Kind (N) /= Pass_Actual_Subprogram
--        and then
--         not Gnatcheck.Global_State.CG.Is_Called (N);
--   end Check_Global_Structure_Node;

--   procedure Init_Rule (Rule : in out Unused_Subprograms_Rule_Type) is
--   begin
--      Rule.Name       := new String'("Unused_Subprograms");
--      Rule.Rule_State := Disabled;
--      Rule.Help_Info  := new String'("unused subprograms");
--      Rule.Diagnosis  := new String'("unused subprogram");
--   end Init_Rule;

   -------------------------
   -- USE_PACKAGE_Clauses --
   -------------------------

   procedure Init_Rule (Rule : in out USE_PACKAGE_Clauses_Rule_Type) is
   begin
      Init_Rule (Rule_Template (Rule));

      Rule.Name        := new String'("USE_PACKAGE_Clauses");
      Rule.Synonym     := new String'("Use_Pckg_Clauses");
      Rule.Rule_Status := Fully_Implemented;
      Rule.Help_Info   := new String'("use clause for packages");
      Rule.Diagnosis   := new String'("use clause for package");
   end Init_Rule;

   procedure Rule_Check_Pre_Op
     (Rule    : in out USE_PACKAGE_Clauses_Rule_Type;
      Element :        Asis.Element;
      Control : in out Traverse_Control;
      State   : in out Rule_Traversal_State)
   is
      pragma Unreferenced (Rule);
      pragma Unreferenced (Control);
   begin

      if Clause_Kind (Element) = A_Use_Package_Clause then
         State.Detected := True;
      end if;

   end Rule_Check_Pre_Op;

   ----------------------------------------------
   -- Volatile_Objects_Without_Address_Clauses --
   ----------------------------------------------

   procedure Init_Rule
     (Rule : in out Volatile_Objects_Without_Address_Clauses_Rule_Type)
   is
   begin
      Init_Rule (Rule_Template (Rule));

      Rule.Name := new String'("Volatile_Objects_Without_Address_Clauses");
      Rule.Synonym     := new String'("Volatile_Requires_Addr_Clause");
      Rule.Rule_Status := Fully_Implemented;
      Rule.Help_Info   := new String'("volatile objects with no " &
                                      "address clause");
      Rule.Diagnosis  := new String'("volatile object with no address clause");
   end Init_Rule;

   procedure Rule_Check_Pre_Op
     (Rule    : in out Volatile_Objects_Without_Address_Clauses_Rule_Type;
      Element :        Asis.Element;
      Control : in out Traverse_Control;
      State   : in out Rule_Traversal_State)
   is
      Enclosing_Decl : Asis.Element;
      pragma Unreferenced (Rule);
      pragma Unreferenced (Control);
   begin

      if Defining_Name_Kind (Element) = A_Defining_Identifier then

         Enclosing_Decl := Get_Enclosing_Element;

         if Declaration_Kind (Enclosing_Decl) = A_Variable_Declaration
           and then
            Is_Volatile (Element)
           and then
            not Has_Address_Clause (Element)
         then
            State.Detected  := True;
         end if;

      end if;

   end Rule_Check_Pre_Op;

end Gnatcheck.Rules.Default;
