------------------------------------------------------------------------------
--                                                                          --
--                          GNATCHECK COMPONENTS                            --
--                                                                          --
--             G N A T C H E C K . R U L E S . C U S T O M _ 3              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                     Copyright (C) 2016-2017, AdaCore                     --
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

with Ada.Characters.Handling;      use Ada.Characters.Handling;
with Ada.Wide_Characters.Handling; use Ada.Wide_Characters.Handling;

with Asis;                         use Asis;
with Asis.Compilation_Units;       use Asis.Compilation_Units;
with Asis.Declarations;            use Asis.Declarations;
with Asis.Definitions;             use Asis.Definitions;
with Asis.Elements;                use Asis.Elements;
with Asis.Expressions;             use Asis.Expressions;
with Asis.Statements;              use Asis.Statements;
with Asis.Text;                    use Asis.Text;

with ASIS_UL.Utilities;            use ASIS_UL.Utilities;

with Gnatcheck.ASIS_Utilities;     use Gnatcheck.ASIS_Utilities;
with Gnatcheck.Rules.Traversing;   use Gnatcheck.Rules.Traversing;
with Gnatcheck.Traversal_Stack;    use Gnatcheck.Traversal_Stack;

package body Gnatcheck.Rules.Custom_3 is

   ----------------------------
   -- Binary_Case_Statements --
   ----------------------------

   ----------------------------------------
   -- Init_Rule (Binary_Case_Statements) --
   ----------------------------------------

   procedure Init_Rule (Rule : in out Binary_Case_Statements_Rule_Type) is
   begin
      Init_Rule (Rule_Template (Rule));

      Rule.Name        := new String'("Binary_Case_Statements");
      Rule.Rule_Status := Fully_Implemented;
      Rule.Help_Info   := new String'("CASE statements tnat can be " &
                                      "replaced with IF statements");
      Rule.Diagnosis   := new String'("CASE statement can be replaced with " &
                                      "IF statement");
   end Init_Rule;

   ------------------------------------------------
   -- Rule_Check_Pre_Op (Binary_Case_Statements) --
   ------------------------------------------------

   procedure Rule_Check_Pre_Op
     (Rule    : in out Binary_Case_Statements_Rule_Type;
      Element :        Asis.Element;
      Control : in out Traverse_Control;
      State   : in out Rule_Traversal_State)
   is
      pragma Unreferenced (Rule, Control);
   begin

      if Statement_Kind (Element) = A_Case_Statement then

         declare
            Case_Paths : constant Asis.Element_List :=
              Statement_Paths (Element);
            First_Idx  : constant List_Index := Case_Paths'First;
         begin
            if Case_Paths'Length = 2
             and then
               Case_Path_Alternative_Choices
                 (Case_Paths (First_Idx))'Length = 1
             and then
               Case_Path_Alternative_Choices
                 (Case_Paths (First_Idx + 1))'Length = 1
            then
               State.Detected := True;
            end if;
         end;
      end if;

   end Rule_Check_Pre_Op;

   ------------------------------------------
   -- Default_Values_For_Record_Components --
   ------------------------------------------

   ------------------------------------------------------
   -- Init_Rule (Default_Values_For_Record_Components) --
   ------------------------------------------------------

   procedure
     Init_Rule (Rule : in out Default_Values_For_Record_Components_Rule_Type)
   is
   begin
      Init_Rule (Rule_Template (Rule));

      Rule.Name        := new String'("Default_Values_For_Record_Components");
      Rule.Rule_Status := Fully_Implemented;
      Rule.Help_Info   := new String'("default values for record components");
      Rule.Diagnosis   := new String'("default value for record component");
   end Init_Rule;

   --------------------------------------------------------------
   -- Rule_Check_Pre_Op (Default_Values_For_Record_Components) --
   --------------------------------------------------------------

   procedure Rule_Check_Pre_Op
     (Rule    : in out Default_Values_For_Record_Components_Rule_Type;
      Element :        Asis.Element;
      Control : in out Traverse_Control;
      State   : in out Rule_Traversal_State)
   is
      pragma Unreferenced (Rule, Control);
   begin

      if Declaration_Kind (Element) = A_Component_Declaration
        and then
         Definition_Kind (Get_Enclosing_Element) /= A_Protected_Definition
        and then
         not Is_Nil (Initialization_Expression (Element))
      then
         State.Detected := True;
      end if;

   end Rule_Check_Pre_Op;

   -----------------------------------
   -- Deriving_From_Predefined_Type --
   -----------------------------------

   -----------------------------------------------
   -- Init_Rule (Deriving_From_Predefined_Type) --
   -----------------------------------------------

   procedure
     Init_Rule (Rule : in out Deriving_From_Predefined_Type_Rule_Type)
   is
   begin
      Init_Rule (Rule_Template (Rule));

      Rule.Name        := new String'("Deriving_From_Predefined_Type");
      Rule.Rule_Status := Fully_Implemented;
      Rule.Help_Info   := new String'("types derived from predefined types");
      Rule.Diagnosis   := new String'("deriving from predefined type");

      Rule.Check_In_Expanded_Generics := True;

   end Init_Rule;

   -------------------------------------------------------
   -- Rule_Check_Pre_Op (Deriving_From_Predefined_Type) --
   -------------------------------------------------------

   procedure Rule_Check_Pre_Op
     (Rule    : in out Deriving_From_Predefined_Type_Rule_Type;
      Element :        Asis.Element;
      Control : in out Traverse_Control;
      State   : in out Rule_Traversal_State)
   is
      pragma Unreferenced (Rule, Control);
      Ancestor     : Asis.Element;
   begin

      if Declaration_Kind (Element) = An_Ordinary_Type_Declaration then
         Ancestor := Type_Declaration_View (Element);

         if Type_Kind (Ancestor) = A_Derived_Type_Definition then
            Ancestor := Corresponding_Root_Type (Ancestor);
            Ancestor := Unwind_Type (Ancestor);

            pragma Assert (not Is_Nil (Ancestor));

            if Unit_Origin (Enclosing_Compilation_Unit (Ancestor)) =
                 A_Predefined_Unit
            then
               State.Detected := True;
            end if;
         end if;
      end if;

   end Rule_Check_Pre_Op;

   ----------------------------------------
   -- Enumeration_Representation_Clauses --
   ----------------------------------------

   ----------------------------------------------------
   -- Init_Rule (Enumeration_Representation_Clauses) --
   ----------------------------------------------------

   procedure Init_Rule
     (Rule : in out Enumeration_Representation_Clauses_Rule_Type)
   is
   begin
      Init_Rule (Rule_Template (Rule));

      Rule.Name        := new String'("Enumeration_Representation_Clauses");
      Rule.Rule_Status := Fully_Implemented;
      Rule.Help_Info   := new String'("enumeration representation clauses");
      Rule.Diagnosis   := new String'("enumeration representation clause");
   end Init_Rule;

   ------------------------------------------------------------
   -- Rule_Check_Pre_Op (Enumeration_Representation_Clauses) --
   ------------------------------------------------------------

   procedure Rule_Check_Pre_Op
     (Rule    : in out Enumeration_Representation_Clauses_Rule_Type;
      Element :        Asis.Element;
      Control : in out Traverse_Control;
      State   : in out Rule_Traversal_State)
   is
      pragma Unreferenced (Rule, Control);
   begin
      if Representation_Clause_Kind (Element) =
           An_Enumeration_Representation_Clause
      then
         State.Detected := True;
      end if;
   end Rule_Check_Pre_Op;

   --------------------------
   -- Expression_Functions --
   --------------------------

   --------------------------------------
   -- Init_Rule (Expression_Functions) --
   --------------------------------------

   procedure Init_Rule (Rule : in out Expression_Functions_Rule_Type) is
   begin
      Init_Rule (Rule_Template (Rule));

      Rule.Name        := new String'("Expression_Functions");
      Rule.Rule_Status := Fully_Implemented;
      Rule.Help_Info   := new String'("expression function in package " &
                                      "specifications");
      Rule.Diagnosis   := new String'("expression function");
   end Init_Rule;

   ----------------------------------------------
   -- Rule_Check_Pre_Op (Expression_Functions) --
   ----------------------------------------------

   procedure Rule_Check_Pre_Op
     (Rule    : in out Expression_Functions_Rule_Type;
      Element :        Asis.Element;
      Control : in out Traverse_Control;
      State   : in out Rule_Traversal_State)
   is
      EE      : Asis.Element;
      Step_Up : Elmt_Idx := 0;
      pragma Unreferenced (Rule, Control);
   begin
      if Declaration_Kind (Element) = An_Expression_Function_Declaration then

         loop
            EE := Get_Enclosing_Element (Step_Up);
            exit when Is_Program_Unit (EE);
            Step_Up := Step_Up + 1;
         end loop;

         State.Detected :=
           Declaration_Kind (EE) in A_Package_Declaration |
                                    A_Generic_Package_Declaration;
      end if;
   end Rule_Check_Pre_Op;

   ---------------------------
   -- Fixed_Equality_Checks --
   ---------------------------

   ---------------------------------------
   -- Init_Rule (Fixed_Equality_Checks) --
   ---------------------------------------

   procedure Init_Rule (Rule : in out Fixed_Equality_Checks_Rule_Type) is
   begin
      Init_Rule (Rule_Template (Rule));
      Rule.Name        := new String'("Fixed_Equality_Checks");
      Rule.Rule_Status := Fully_Implemented;
      Rule.Help_Info   := new String'("equality for fixed values");
      Rule.Diagnosis   := new String'("use of equality operation " &
                                      "for fixed values");
   end Init_Rule;

   -----------------------------------------------
   -- Rule_Check_Pre_Op (Fixed_Equality_Checks) --
   -----------------------------------------------

   procedure Rule_Check_Pre_Op
     (Rule    : in out Fixed_Equality_Checks_Rule_Type;
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
            Is_Fixed
              (Actual_Parameter (Function_Call_Parameters (Call_Element) (1)))
         then
            State.Detected := True;
         end if;

      end if;

   end Rule_Check_Pre_Op;

   ------------------------
   -- Nested_Subprograms --
   ------------------------

   ------------------------------------
   -- Init_Rule (Nested_Subprograms) --
   ------------------------------------

   procedure Init_Rule (Rule : in out Nested_Subprograms_Rule_Type) is
   begin
      Init_Rule (Rule_Template (Rule));
      Rule.Name        := new String'("Nested_Subprograms");
      Rule.Rule_Status := Fully_Implemented;
      Rule.Help_Info   := new String'("nested subprograms");
      Rule.Diagnosis   := new String'("subprogram declared in " &
                                      "executable body");
   end Init_Rule;

   --------------------------------------------
   -- Rule_Check_Pre_Op (Nested_Subprograms) --
   --------------------------------------------

   procedure Rule_Check_Pre_Op
     (Rule    : in out Nested_Subprograms_Rule_Type;
      Element :        Asis.Element;
      Control : in out Traverse_Control;
      State   : in out Rule_Traversal_State)
   is
      pragma Unreferenced (Rule);
      pragma Unreferenced (Control);

      EE : Asis.Element;
   begin
      if (Declaration_Kind (Element) = A_Function_Declaration
          and then
           Definition_Kind (Enclosing_Element (Element)) /=
             A_Protected_Definition)
        or else
         (Declaration_Kind (Element) = A_Procedure_Declaration
          and then
           Definition_Kind (Enclosing_Element (Element)) /=
             A_Protected_Definition
          and then
           Declaration_Kind (Corresponding_Body (Element)) /=
             A_Null_Procedure_Declaration)
        or else
         Declaration_Kind (Element) in
           A_Procedure_Instantiation |
           A_Function_Instantiation
        or else
         (Declaration_Kind (Element) in
           A_Procedure_Body_Declaration       |
           A_Function_Body_Declaration        |
           An_Expression_Function_Declaration |
           A_Procedure_Body_Stub              |
           A_Function_Body_Stub
        and then
         Acts_As_Spec (Element))
      then
         EE := Enclosing_Element (Element);

         while not Is_Nil (EE) loop
            if Declaration_Kind (EE) in
                 A_Procedure_Body_Declaration |
                 A_Function_Body_Declaration  |
                 A_Task_Body_Declaration      |
                 An_Entry_Body_Declaration
            then
               State.Detected := True;
               exit;
            end if;

            if Is_Subunit (EE) then
               EE := Corresponding_Body_Stub (EE);
            else
               EE := Enclosing_Element (EE);
            end if;
         end loop;
      end if;

   end Rule_Check_Pre_Op;

   ----------------
   -- Null_Paths --
   ----------------

   ----------------------------
   -- Init_Rule (Null_Paths) --
   ----------------------------

   procedure Init_Rule (Rule : in out Null_Paths_Rule_Type) is
   begin
      Init_Rule (Rule_Template (Rule));
      Rule.Name        := new String'("Null_Paths");
      Rule.Rule_Status := Fully_Implemented;
      Rule.Help_Info   := new String'("null paths in control statements");
      Rule.Diagnosis   := new String'("null path");
   end Init_Rule;

   ------------------------------------
   -- Rule_Check_Pre_Op (Null_Paths) --
   ------------------------------------

   procedure Rule_Check_Pre_Op
     (Rule    : in out Null_Paths_Rule_Type;
      Element :        Asis.Element;
      Control : in out Traverse_Control;
      State   : in out Rule_Traversal_State)
   is
      pragma Unreferenced (Rule);
      pragma Unreferenced (Control);

   begin
      if Statement_Kind (Element) in
           A_Loop_Statement       |
           A_While_Loop_Statement |
           A_For_Loop_Statement
        or else
         Path_Kind (Element) in
           An_If_Path    |
           An_Elsif_Path |
           An_Else_Path  |
           A_Case_Path
      then
         declare
            Stmts : constant Asis.Element_List :=
              (if Statement_Kind (Element) in
                    A_Loop_Statement       |
                    A_While_Loop_Statement |
                    A_For_Loop_Statement
               then
                 Loop_Statements (Element, Include_Pragmas => True)
               else
                 Sequence_Of_Statements (Element, Include_Pragmas => True));

            First_Stmt_Span : Span;
         begin
            State.Detected := True;

            for J in Stmts'Range loop
               if Statement_Kind (Stmts (J)) /= A_Null_Statement then
                  Reset_State (State);
                  exit;
               end if;
            end loop;

            if State.Detected then
               First_Stmt_Span := Element_Span (Stmts (Stmts'First));
               State.Line   := First_Stmt_Span.First_Line;
               State.Column := First_Stmt_Span.First_Column;
            end if;
         end;
      end if;

   end Rule_Check_Pre_Op;

   --------------------------------
   -- Objects_Of_Anonymous_Types --
   --------------------------------

   --------------------------------------------
   -- Init_Rule (Objects_Of_Anonymous_Types) --
   --------------------------------------------

   procedure Init_Rule
     (Rule : in out Objects_Of_Anonymous_Types_Rule_Type)
   is
   begin
      Init_Rule (Rule_Template (Rule));

      Rule.Name        := new String'("Objects_Of_Anonymous_Types");
      Rule.Rule_Status := Fully_Implemented;
      Rule.Help_Info   := new String'("data objects of anonymous types");
      Rule.Diagnosis   := new String'("object of anonymous type");
   end Init_Rule;

   ----------------------------------------------------
   -- Rule_Check_Pre_Op (Objects_Of_Anonymous_Types) --
   ----------------------------------------------------

   procedure Rule_Check_Pre_Op
     (Rule    : in out Objects_Of_Anonymous_Types_Rule_Type;
      Element :        Asis.Element;
      Control : in out Traverse_Control;
      State   : in out Rule_Traversal_State)
   is
      pragma Unreferenced (Rule, Control);
      Def : Asis.Element;
   begin

      if (Declaration_Kind (Element) in
            A_Variable_Declaration | A_Constant_Declaration
        and then
          Declaration_Kind (Get_Enclosing_Element) in
            A_Package_Declaration | A_Package_Body_Declaration)
       or else
        Declaration_Kind (Element) = A_Formal_Object_Declaration
      then
         Def := Object_Declaration_View (Element);

         if Type_Kind (Def) = A_Constrained_Array_Definition
           or else
            Definition_Kind (Def) = An_Access_Definition
         then
            State.Detected  := True;
         end if;
      end if;

   end Rule_Check_Pre_Op;

   ------------------------------
   -- POS_On_Enumeration_Types --
   ------------------------------

   ------------------------------------------
   -- Init_Rule (POS_On_Enumeration_Types) --
   ------------------------------------------

   procedure Init_Rule
     (Rule : in out POS_On_Enumeration_Types_Rule_Type)
   is
   begin
      Init_Rule (Rule_Template (Rule));

      Rule.Name        := new String'("POS_On_Enumeration_Types");
      Rule.Rule_Status := Fully_Implemented;
      Rule.Help_Info   := new String'("attribute 'POS applied to " &
                                      "enumeration types");
      Rule.Diagnosis   := new String'("prefix of 'POS attribute has " &
                                      "enumeration type");
   end Init_Rule;

   --------------------------------------------------
   -- Rule_Check_Pre_Op (POS_On_Enumeration_Types) --
   --------------------------------------------------

   procedure Rule_Check_Pre_Op
     (Rule    : in out POS_On_Enumeration_Types_Rule_Type;
      Element :        Asis.Element;
      Control : in out Traverse_Control;
      State   : in out Rule_Traversal_State)
   is
      pragma Unreferenced (Rule, Control);
      Pref : Asis.Element;
   begin

      if Attribute_Kind (Element) = A_Pos_Attribute then
         Pref := Prefix (Element);

         if Has_Enumeration_Type (Pref) then
            State.Detected  := True;
         end if;
      end if;

   end Rule_Check_Pre_Op;

   -----------------------------------
   -- Representation_Specifications --
   -----------------------------------

   -----------------------------------------------
   -- Init_Rule (Representation_Specifications) --
   -----------------------------------------------

   procedure Init_Rule
     (Rule : in out Representation_Specifications_Rule_Type)
   is
   begin
      Init_Rule (Rule_Template (Rule));

      Rule.Name        := new String'("Representation_Specifications");
      Rule.Rule_Status := Fully_Implemented;
      Rule.Help_Info   := new String'("representation specification " &
                                      "(clause or aspect)");
      Rule.Diagnosis   := new String'("representation specification");
   end Init_Rule;

   -------------------------------------------------------
   -- Rule_Check_Pre_Op (Representation_Specifications) --
   -------------------------------------------------------

   procedure Rule_Check_Pre_Op
     (Rule    : in out Representation_Specifications_Rule_Type;
      Element :        Asis.Element;
      Control : in out Traverse_Control;
      State   : in out Rule_Traversal_State)
   is
      A_Mark : Asis.Element;
      pragma Unreferenced (Rule, Control);
   begin
      if Clause_Kind (Element) = A_Representation_Clause then
         State.Detected  := True;
      elsif Definition_Kind (Element) = An_Aspect_Specification then
         A_Mark := Aspect_Mark (Element);

         if Expression_Kind (A_Mark) = An_Identifier then

            declare
               A_Mark_Img : constant String :=
                 To_Lower (To_String (Name_Image (A_Mark)));
            begin
               --  The following aspects are checked as representation aspects
               --  (see ARM 13.3):
               --
               --    Address
               --    Alignment
               --    Size
               --    Component_Size
               --    External_Tag

               if A_Mark_Img = "address"
                 or else
                  A_Mark_Img = "alignment"
                 or else
                  A_Mark_Img = "size"
                 or else
                  A_Mark_Img = "component_size"
                 or else
                  A_Mark_Img = "external_tag"
               then
                  State.Detected  := True;
               end if;
            end;

         end if;
      end if;

   end Rule_Check_Pre_Op;

   -----------------------
   -- Subprogram_Access --
   -----------------------

   -----------------------------------
   -- Init_Rule (Subprogram_Access) --
   -----------------------------------

   procedure Init_Rule (Rule : in out Subprogram_Access_Rule_Type) is
   begin
      Init_Rule (Rule_Template (Rule));

      Rule.Name        := new String'("Subprogram_Access");
      Rule.Rule_Status := Fully_Implemented;
      Rule.Help_Info   := new String'("access to subprogram definition");
      Rule.Diagnosis   := new String'("access to subprogram definition");
   end Init_Rule;

   -------------------------------------------
   -- Rule_Check_Pre_Op (Subprogram_Access) --
   -------------------------------------------

   procedure Rule_Check_Pre_Op
     (Rule    : in out Subprogram_Access_Rule_Type;
      Element :        Asis.Element;
      Control : in out Traverse_Control;
      State   : in out Rule_Traversal_State)
   is
      pragma Unreferenced (Rule, Control);
   begin
      if Access_Type_Kind (Element) in Access_To_Subprogram_Definition
       or else
         Access_Definition_Kind (Element) in
           An_Anonymous_Access_To_Procedure           |
           An_Anonymous_Access_To_Protected_Procedure |
           An_Anonymous_Access_To_Function            |
           An_Anonymous_Access_To_Protected_Function
      then
         State.Detected  := True;
      end if;

   end Rule_Check_Pre_Op;

   ------------------------------------
   -- Unchecked_Address_Conversions --
   ------------------------------------

   ------------------------------------------------
   -- Init_Rule (Unchecked_Address_Conversions) --
   ------------------------------------------------

   procedure Init_Rule
     (Rule : in out Unchecked_Address_Conversions_Rule_Type)
   is
   begin
      Init_Rule (Rule_Template (Rule));

      Rule.Name        := new String'("Unchecked_Address_Conversions");
      Rule.Rule_Status := Fully_Implemented;
      Rule.Help_Info   := new String'("unchecked conversions from " &
                                      "System.Address into access type");
      Rule.Diagnosis   := new String'("address-to-access conversion");

      Rule.Check_In_Expanded_Generics := True;

   end Init_Rule;

   -------------------------------------------------------
   -- Rule_Check_Pre_Op (Unchecked_Address_Conversions) --
   -------------------------------------------------------

   procedure Rule_Check_Pre_Op
     (Rule    : in out Unchecked_Address_Conversions_Rule_Type;
      Element :        Asis.Element;
      Control : in out Traverse_Control;
      State   : in out Rule_Traversal_State)
   is
      pragma Unreferenced (Rule, Control);

      Is_UC_Instance    : Boolean := False;
      Is_Address_Source : Boolean := False;
      Is_Access_Target  : Boolean := False;

      Tmp           : Asis.Element;
      Encl_CU       : Asis.Compilation_Unit;
      Act_Pars      : Asis.Element_List (1 .. 2);
      Reverse_Order : Boolean := False;
      --  Set to True in case if the instantiation of UC uses named
      --  associations and they are in reverse order:
      --
      --   ... is new Unchecked_Conversion (Tagget => T1, Source => T2)
      --
      --  very unlikely but possible.

   begin

      --  Check that we have an instantiation of Unchecked_Conversion

      if Declaration_Kind (Element) = A_Function_Instantiation then
         Tmp := Generic_Unit_Name (Element);

         if Expression_Kind (Tmp) = A_Selected_Component then
            Tmp := Selector (Tmp);
         end if;

         Tmp := Corresponding_Name_Declaration (Tmp);

         if Declaration_Kind (Tmp) =
              A_Generic_Function_Renaming_Declaration
         then
            Tmp := Corresponding_Base_Entity (Tmp);

            if Expression_Kind (Tmp) = A_Selected_Component then
               Tmp := Selector (Tmp);
            end if;

            if Expression_Kind (Tmp) = An_Identifier then
               Tmp := Corresponding_Name_Declaration (Tmp);
            end if;

         end if;

         Encl_CU := Enclosing_Compilation_Unit (Tmp);

         if Unit_Origin (Encl_CU) = A_Predefined_Unit
           and then
            (To_Lower (Unit_Full_Name (Encl_CU)) =
                "ada.unchecked_conversion"
            or else
             --  GNAT does not literally follow section J.1 of ARM 12
             To_Lower (Unit_Full_Name (Encl_CU)) =
                "unchecked_conversion")
         then
            Is_UC_Instance := True;
         else
            goto Fin;
         end if;
      else
         return;
      end if;

      Act_Pars := Generic_Actual_Part (Element);

      Tmp := Formal_Parameter (Act_Pars (1));

      if not Is_Nil (Tmp)
        and then
         To_Lower (Name_Image (Tmp)) = "target"
      then
         Reverse_Order := True;
         Tmp           := Actual_Parameter (Act_Pars (2));
      else
         Tmp := Actual_Parameter (Act_Pars (1));
      end if;

      --  Check that the actual for Source is System.Address

      Tmp := Get_Underlying_Type (Tmp);

      Encl_CU := Enclosing_Compilation_Unit (Tmp);

      if Unit_Origin (Encl_CU) = A_Predefined_Unit
        and then
         To_Lower (Unit_Full_Name (Encl_CU)) = "system"
      then
         Tmp := First_Name (Tmp);

         if To_Lower (Defining_Name_Image (Tmp)) = "address" then
            Is_Address_Source := True;
         else
            goto Fin;
         end if;
      else
         goto Fin;
      end if;

      --  Check that the actual for Target is an access type

      if Reverse_Order then
         Tmp := Actual_Parameter (Act_Pars (1));
      else
         Tmp := Actual_Parameter (Act_Pars (2));
      end if;

      Tmp := Get_Underlying_Type (Tmp);

      case Declaration_Kind (Tmp) is
         when A_Formal_Type_Declaration =>
            Is_Access_Target :=
              Formal_Type_Kind (Type_Declaration_View (Tmp)) =
                A_Formal_Access_Type_Definition;

         when An_Ordinary_Type_Declaration =>
            Is_Access_Target :=
              Type_Kind (Type_Declaration_View (Tmp)) =
                An_Access_Type_Definition;
         when others =>
            null;
      end case;

      <<Fin>> State.Detected :=
        Is_UC_Instance and then Is_Address_Source and then Is_Access_Target;

   end Rule_Check_Pre_Op;

   --------------------------------------
   -- Unchecked_Conversions_As_Actuals --
   --------------------------------------

   --------------------------------------------------
   -- Init_Rule (Unchecked_Conversions_As_Actuals) --
   --------------------------------------------------

   procedure Init_Rule
     (Rule : in out Unchecked_Conversions_As_Actuals_Rule_Type)
   is
   begin
      Init_Rule (Rule_Template (Rule));

      Rule.Name        := new String'("Unchecked_Conversions_As_Actuals");
      Rule.Rule_Status := Fully_Implemented;
      Rule.Help_Info   := new String'("passing instance of " &
                                      "Unchecked_Conversion as actual");
      Rule.Diagnosis   := new String'("#1#instance of Unchecked_Conversion " &
                                      " as actual parameter" &
                                      "#2#instance of Unchecked_Conversion " &
                                      " as default parameter value");
   end Init_Rule;

   ----------------------------------------------------------
   -- Rule_Check_Pre_Op (Unchecked_Conversions_As_Actuals) --
   ----------------------------------------------------------

   procedure Rule_Check_Pre_Op
     (Rule    : in out Unchecked_Conversions_As_Actuals_Rule_Type;
      Element :        Asis.Element;
      Control : in out Traverse_Control;
      State   : in out Rule_Traversal_State)
   is
      pragma Unreferenced (Rule, Control);
      Called_Fun  : Asis.Element;
      Encl        : Asis.Element;
      Template    : Asis.Element;
      Template_CU : Asis.Compilation_Unit;
   begin

      if Expression_Kind (Element) = A_Function_Call then
         Encl := Get_Enclosing_Element;

         if Association_Kind (Encl) = A_Parameter_Association
           or else
            Declaration_Kind (Encl) = A_Parameter_Specification
         then
            Called_Fun := Corresponding_Called_Function (Element);

            if Declaration_Kind (Called_Fun) =
                 A_Function_Renaming_Declaration
            then
               Called_Fun := Corresponding_Base_Entity (Called_Fun);

               if Expression_Kind (Called_Fun) = A_Selected_Component then
                  Called_Fun := Selector (Called_Fun);
               end if;

               if Expression_Kind (Called_Fun) = An_Identifier then
                  Called_Fun := Corresponding_Name_Declaration (Called_Fun);
               end if;

            end if;

            if Declaration_Kind (Called_Fun) = A_Function_Instantiation then
               Template := Generic_Unit_Name (Called_Fun);

               if Expression_Kind (Template) = A_Selected_Component then
                  Template := Selector (Template);
               end if;

               Template := Corresponding_Name_Declaration (Template);

               if Declaration_Kind (Template) =
                    A_Generic_Function_Renaming_Declaration
               then
                  Template := Corresponding_Base_Entity (Template);

                  if Expression_Kind (Template) = A_Selected_Component then
                     Template := Selector (Template);
                  end if;

                  if Expression_Kind (Template) = An_Identifier then
                     Template := Corresponding_Name_Declaration (Template);
                  end if;

               end if;

               Template_CU := Enclosing_Compilation_Unit (Template);

               if Unit_Origin (Template_CU) = A_Predefined_Unit
                 and then
                  (To_Lower (Unit_Full_Name (Template_CU)) =
                      "ada.unchecked_conversion"
                  or else
                   --  GNAT does not literally follow section J.1 of ARM 12
                   To_Lower (Unit_Full_Name (Template_CU)) =
                      "unchecked_conversion")
               then
                  State.Detected  := True;
                  State.Diagnosis :=
                    (if Association_Kind (Encl) = A_Parameter_Association then
                        1
                     else
                        2);
               end if;

            end if;
         end if;
      end if;

   end Rule_Check_Pre_Op;

   ------------------------------------
   -- Uninitialized_Global_Variables --
   ------------------------------------

   ------------------------------------------------
   -- Init_Rule (Uninitialized_Global_Variables) --
   ------------------------------------------------

   procedure Init_Rule
     (Rule : in out Uninitialized_Global_Variables_Rule_Type)
   is
   begin
      Init_Rule (Rule_Template (Rule));

      Rule.Name        := new String'("Uninitialized_Global_Variables");
      Rule.Rule_Status := Fully_Implemented;
      Rule.Help_Info   := new String'("uninitialized variables in packages");
      Rule.Diagnosis   := new String'("uninitalized global variable");
   end Init_Rule;

   --------------------------------------------------------
   -- Rule_Check_Pre_Op (Uninitialized_Global_Variables) --
   --------------------------------------------------------

   procedure Rule_Check_Pre_Op
     (Rule    : in out Uninitialized_Global_Variables_Rule_Type;
      Element :        Asis.Element;
      Control : in out Traverse_Control;
      State   : in out Rule_Traversal_State)
   is
      pragma Unreferenced (Rule, Control);
      SM : Asis.Element;
   begin

      if Declaration_Kind (Element) = A_Variable_Declaration
        and then
         Is_Nil (Initialization_Expression (Element))
        and then
         Declaration_Kind (Get_Enclosing_Element) in
            A_Package_Declaration      |
            A_Package_Body_Declaration |
            A_Generic_Package_Declaration
      then
         SM := Object_Declaration_View (Element);
         SM := Asis.Definitions.Subtype_Mark (SM);

         if not Is_Limited (SM) then
            State.Detected  := True;
         end if;
      end if;

   end Rule_Check_Pre_Op;

end Gnatcheck.Rules.Custom_3;
