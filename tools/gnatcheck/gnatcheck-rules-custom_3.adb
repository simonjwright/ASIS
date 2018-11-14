------------------------------------------------------------------------------
--                                                                          --
--                          GNATCHECK COMPONENTS                            --
--                                                                          --
--             G N A T C H E C K . R U L E S . C U S T O M _ 3              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                     Copyright (C) 2016-2018, AdaCore                     --
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
with Ada.Containers.Indefinite_Ordered_Sets;
with Ada.Strings;                  use Ada.Strings;
with Ada.Strings.Wide_Fixed;       use Ada.Strings.Wide_Fixed;
with Ada.Wide_Characters.Handling; use Ada.Wide_Characters.Handling;

with Asis;                         use Asis;
with Asis.Clauses;                 use Asis.Clauses;
with Asis.Compilation_Units;       use Asis.Compilation_Units;
with Asis.Declarations;            use Asis.Declarations;
with Asis.Definitions;             use Asis.Definitions;
with Asis.Elements;                use Asis.Elements;
with Asis.Expressions;             use Asis.Expressions;
with Asis.Extensions;              use Asis.Extensions;
with Asis.Extensions.Flat_Kinds;   use Asis.Extensions.Flat_Kinds;
with Asis.Statements;              use Asis.Statements;
with Asis.Text;                    use Asis.Text;

with ASIS_UL.Misc;
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
      Rule.Help_Info   := new String'("CASE statements that can be " &
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

   ------------------
   -- Constructors --
   ------------------

   ------------------------------
   -- Init_Rule (Constructors) --
   ------------------------------

   procedure Init_Rule (Rule : in out Constructors_Rule_Type) is
   begin
      Init_Rule (Rule_Template (Rule));

      Rule.Name        := new String'("Constructors");
      Rule.Rule_Status := Fully_Implemented;
      Rule.Help_Info   := new String'("constructors");
      Rule.Diagnosis   := new String'("declaration of constructor function");
   end Init_Rule;

   --------------------------------------
   -- Rule_Check_Pre_Op (Constructors) --
   --------------------------------------

   procedure Rule_Check_Pre_Op
     (Rule    : in out Constructors_Rule_Type;
      Element :        Asis.Element;
      Control : in out Traverse_Control;
      State   : in out Rule_Traversal_State)
   is

      pragma Unreferenced (Rule, Control);
   begin
      State.Detected := Is_Constructor (Element);
   end Rule_Check_Pre_Op;

   ----------------------------
   -- Deep_Library_Hierarchy --
   ----------------------------

   ----------------------------------------
   -- Init_Rule (Deep_Library_Hierarchy) --
   ----------------------------------------

   procedure Init_Rule (Rule : in out Deep_Library_Hierarchy_Rule_Type) is
   begin
      Init_Rule (Rule_Template (Rule));

      Rule.Name        := new String'("Deep_Library_Hierarchy");
      Rule.Rule_Status := Fully_Implemented;
      Rule.Help_Info   := new String'("deep library hierarchy");
      Rule.Diagnosis   := new String'("unit has too many ancestors (%1%)");
   end Init_Rule;

   ------------------------------------------------
   -- Rule_Check_Pre_Op (Deep_Library_Hierarchy) --
   ------------------------------------------------

   procedure Rule_Check_Pre_Op
     (Rule    : in out Deep_Library_Hierarchy_Rule_Type;
      Element :        Asis.Element;
      Control : in out Traverse_Control;
      State   : in out Rule_Traversal_State)
   is
      Def_Name  : Asis.Element;
      Ansestors : Natural := 0;

      pragma Unreferenced (Control);
   begin
      if Declaration_Kind (Element) in
           A_Package_Declaration         |
           A_Generic_Package_Declaration |
           A_Package_Instantiation
        and then
         Is_Nil (Get_Enclosing_Element)
      then
         Def_Name := First_Name (Element);

         declare
            Def_Name_Img : constant Program_Text :=
              Defining_Name_Image (Def_Name);
         begin
            for J in Def_Name_Img'Range loop
               if Def_Name_Img  (J) = '.' then
                  Ansestors := Ansestors + 1;

               end if;
            end loop;
         end;

         if Ansestors > Rule.Rule_Limit then
            State.Detected := True;
            State.Diag_Params :=
              Enter_String ("%1%" & ASIS_UL.Misc.Image (Ansestors));
         end if;
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

   -------------------------------
   -- Downward_View_Conversions --
   -------------------------------

   -------------------------------------------
   -- Init_Rule (Downward_View_Conversions) --
   -------------------------------------------

   procedure Init_Rule (Rule : in out Downward_View_Conversions_Rule_Type) is
   begin
      Init_Rule (Rule_Template (Rule));

      Rule.Name        := new String'("Downward_View_Conversions");
      Rule.Rule_Status := Fully_Implemented;
      Rule.Help_Info   := new String'("downward view conversions");
      Rule.Diagnosis   := new String'("downward view conversion");
   end Init_Rule;

   ---------------------------------------------------
   -- Rule_Check_Pre_Op (Downward_View_Conversions) --
   ---------------------------------------------------

   procedure Rule_Check_Pre_Op
     (Rule    : in out Downward_View_Conversions_Rule_Type;
      Element :        Asis.Element;
      Control : in out Traverse_Control;
      State   : in out Rule_Traversal_State)
   is

      pragma Unreferenced (Rule, Control);
   begin
      State.Detected := Is_Downward_View_Conversion (Element);
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

   --------------------------------
   -- No_Inherited_Classwide_Pre --
   --------------------------------

   --------------------------------------------
   -- Init_Rule (No_Inherited_Classwide_Pre) --
   --------------------------------------------

   procedure Init_Rule (Rule : in out No_Inherited_Classwide_Pre_Rule_Type) is
   begin
      Init_Rule (Rule_Template (Rule));
      Rule.Name        := new String'("No_Inherited_Classwide_Pre");
      Rule.Rule_Status := Fully_Implemented;
      Rule.Help_Info   := new String'("overridden operation has no Pre'Class");
      Rule.Diagnosis   := new String'("overriding operation that does not " &
                                      "inherit Pre'Class (%1%)");
   end Init_Rule;

   ----------------------------------------------------
   -- Rule_Check_Pre_Op (No_Inherited_Classwide_Pre) --
   ----------------------------------------------------

   procedure Rule_Check_Pre_Op
     (Rule    : in out No_Inherited_Classwide_Pre_Rule_Type;
      Element :        Asis.Element;
      Control : in out Traverse_Control;
      State   : in out Rule_Traversal_State)
   is
      pragma Unreferenced (Rule);
      pragma Unreferenced (Control);

      LSP_Violation_Detected : Boolean;
      LSP_Violation_SLOC     : String_Loc;
   begin
      if Is_Dispatching_Operation (Element)
        and then
         not Is_Not_Overriding_Declaration (Element)
        and then
         (Is_Overriding_Declaration (Element)
         or else
          Is_Overriding_Operation (Element))
      then
         --  All the processing in the implementation of this rule is based on
         --  direct tree traversing and analysis, so it is encapsulated in
         --  an utility procedure
         Check_Classwide_Pre_Vioaltion
           (Element,
            LSP_Violation_Detected,
            LSP_Violation_SLOC);

         if LSP_Violation_Detected then
            State.Detected    := True;
            State.Diag_Params := LSP_Violation_SLOC;
         end if;
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
                                      "(clause, aspect or pragma)");
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
               --
               --  We have also flag aspects that corresponds to representation
               --  pragmas:
               --   Asynchronous   J.15.13(1/3)
               --   Atomic   J.15.8(9/3)
               --   Atomic_Components   J.15.8(9/3)
               --   Convention   J.15.5(1/3)
               --   Discard_Names   C.5(6)
               --   Export   J.15.5(1/3)
               --   Import   J.15.5(1/3)
               --   Independent   J.15.8(9/3)
               --   Independent_Components   J.15.8(9/3)
               --   No_Return   J.15.2(1/3)
               --   Pack   J.15.3(1/3)
               --   Unchecked_Union   J.15.6(1/3)
               --   Volatile   J.15.8(9/3)
               --   Volatile_Components   J.15.8(9/3)

               if A_Mark_Img = "address"
                 or else
                  A_Mark_Img = "alignment"
                 or else
                  A_Mark_Img = "size"
                 or else
                  A_Mark_Img = "component_size"
                 or else
                  A_Mark_Img = "external_tag"
                 or else
                  A_Mark_Img = "asynchronous"
                 or else
                  A_Mark_Img = "atomic"
                 or else
                  A_Mark_Img = "atomic_components"
                 or else
                  A_Mark_Img = "convention"
                 or else
                  A_Mark_Img = "import"
                 or else
                  A_Mark_Img = "discard_names"
                 or else
                  A_Mark_Img = "export"
                 or else
                  A_Mark_Img = "independent"
                 or else
                  A_Mark_Img = "independent_components"
                 or else
                  A_Mark_Img = "no_return"
                 or else
                  A_Mark_Img = "pack"
                 or else
                  A_Mark_Img = "unchecked_union"
                 or else
                  A_Mark_Img = "volatile"
                 or else
                  A_Mark_Img = "volatile_components"
               then
                  State.Detected  := True;
               end if;
            end;

         end if;
      elsif Element_Kind (Element) = A_Pragma then
         declare
            Pragme_Name_Img : constant String :=
              To_Lower (To_String (Pragma_Name_Image (Element)));
         begin
            if Pragme_Name_Img = "asynchronous"
              or else
               Pragme_Name_Img =  "atomic"
              or else
               Pragme_Name_Img = "atomic_components"
              or else
               Pragme_Name_Img = "convention"
              or else
               Pragme_Name_Img = "import"
              or else
               Pragme_Name_Img = "discard_names"
              or else
               Pragme_Name_Img = "export"
              or else
               Pragme_Name_Img = "independent"
              or else
               Pragme_Name_Img = "independent_components"
              or else
               Pragme_Name_Img = "no_return"
              or else
               Pragme_Name_Img = "pack"
              or else
               Pragme_Name_Img = "unchecked_union"
              or else
               Pragme_Name_Img = "volatile"
              or else
               Pragme_Name_Img = "volatile_components"
            then
               State.Detected  := True;
            end if;
         end;
      end if;

   end Rule_Check_Pre_Op;

   ------------------------------------
   -- Specific_Parent_Type_Invariant --
   ------------------------------------

   function Has_Specific_Type_Invariant
     (Type_Decl : Asis.Element)
      return      Boolean;
   --  Checks if Type_Decl has a specification of Type_Invariant aspect.
   --  Returns False for any unexpected element.
   --
   --  Expected Declaration_Kinds
   --    A_Private_Type_Declaration
   --    A_Private_Extension_Declaration
   --    An_Ordinary_Type_Declaration

   ------------------------------------------------------------------
   -- Has_Specific_Type_Invariant (Specific_Parent_Type_Invariant) --
   ------------------------------------------------------------------

   function Has_Specific_Type_Invariant
     (Type_Decl : Asis.Element)
      return      Boolean
   is
      Result : Boolean := False;
   begin
      if Declaration_Kind (Type_Decl) in
           A_Private_Type_Declaration      |
           A_Private_Extension_Declaration |
           An_Ordinary_Type_Declaration
      then

         declare
            Asp_Defs : constant Asis.Element_List :=
              Aspect_Specifications (Type_Decl);

            Asp_Mark : Asis.Element;
         begin

            for J in Asp_Defs'Range loop
               Asp_Mark := Aspect_Mark (Asp_Defs (J));

               if Expression_Kind (Asp_Mark) = An_Identifier
                 and then
                  To_Lower (Name_Image (Asp_Mark)) = "type_invariant"
               then
                  Result := True;
                  exit;
               end if;

            end loop;

         end;

      end if;

      return Result;
   end Has_Specific_Type_Invariant;

   ------------------------------------------------
   -- Init_Rule (Specific_Parent_Type_Invariant) --
   ------------------------------------------------

   procedure Init_Rule
     (Rule : in out Specific_Parent_Type_Invariant_Rule_Type)
   is
   begin
      Init_Rule (Rule_Template (Rule));

      Rule.Name        := new String'("Specific_Parent_Type_Invariant");
      Rule.Rule_Status := Fully_Implemented;
      Rule.Help_Info   := new String'("derivation from a type with " &
                                      "specific Type_Invariant aspect");
      Rule.Diagnosis   := new String'("parent type has specific " &
                                      "Type_Invariant aspect (%1%)");
   end Init_Rule;

   --------------------------------------------------------
   -- Rule_Check_Pre_Op (Specific_Parent_Type_Invariant) --
   --------------------------------------------------------

   procedure Rule_Check_Pre_Op
     (Rule    : in out Specific_Parent_Type_Invariant_Rule_Type;
      Element :        Asis.Element;
      Control : in out Traverse_Control;
      State   : in out Rule_Traversal_State)
   is
      pragma Unreferenced (Rule, Control);
      Parent : Asis.Element;
   begin
      if Definition_Kind (Element) = A_Private_Extension_Definition then
         Parent := Ancestor_Subtype_Indication (Element);
      elsif Type_Kind (Element) = A_Derived_Record_Extension_Definition
          and then
            Declaration_Kind
              (Corresponding_Type_Partial_View (Get_Enclosing_Element)) /=
              A_Private_Extension_Declaration
      then
         Parent := Parent_Subtype_Indication (Element);
      else
         return;
      end if;

      Parent := Asis.Definitions.Subtype_Mark (Parent);
      Parent := Normalize_Reference (Parent);
      Parent := Corresponding_Name_Declaration (Parent);
      Parent := Corresponding_First_Subtype (Parent);

      if Has_Specific_Type_Invariant (Parent) then
         State.Detected := True;
         State.Diag_Params :=
           Enter_String ("%1%" & Build_GNAT_Location (Parent));
      else
         if Declaration_Kind (Parent) in
              A_Private_Type_Declaration |
              A_Private_Extension_Declaration
         then
            Parent := Corresponding_Type_Completion (Parent);
         elsif Declaration_Kind (Parent) = An_Ordinary_Type_Declaration then
            Parent := Corresponding_Type_Partial_View (Parent);
         else
            return;
         end if;

         if Declaration_Kind (Parent) not in
              An_Incomplete_Type_Declaration |
              A_Tagged_Incomplete_Type_Declaration
           and then
            Has_Specific_Type_Invariant (Parent)
         then
            State.Detected := True;
            State.Diag_Params :=
              Enter_String ("%1%" & Build_GNAT_Location (Parent));
         end if;

      end if;

   end Rule_Check_Pre_Op;

   -----------------------
   -- Specific_Pre_Post --
   -----------------------

   -----------------------------------
   -- Init_Rule (Specific_Pre_Post) --
   -----------------------------------

   procedure Init_Rule (Rule : in out Specific_Pre_Post_Rule_Type) is
   begin
      Init_Rule (Rule_Template (Rule));

      Rule.Name        := new String'("Specific_Pre_Post");
      Rule.Rule_Status := Fully_Implemented;
      Rule.Help_Info   := new String'("definition of non class-wide Pre " &
                                      "and Post aspects for tagged type " &
                                      "primitives");
      Rule.Diagnosis   := new String'("#1#definition of non class-wide " &
                                      "Pre aspect"                       &
                                      "#2#definition of non class-wide " &
                                      "Post aspect"                      &
                                      "#3#definition of non class-wide " &
                                      "Pre and Post aspects");
   end Init_Rule;

   -------------------------------------------
   -- Rule_Check_Pre_Op (Specific_Pre_Post) --
   -------------------------------------------

   procedure Rule_Check_Pre_Op
     (Rule    : in out Specific_Pre_Post_Rule_Type;
      Element :        Asis.Element;
      Control : in out Traverse_Control;
      State   : in out Rule_Traversal_State)
   is
      pragma Unreferenced (Rule, Control);
   begin
      if Is_Dispatching_Operation (Element) then
         declare
            Asp_Defs : constant Asis.Element_List :=
              Aspect_Specifications (Element);

            Pre_Detected  : Boolean := False;
            Post_Detected : Boolean := False;
            Asp_Mark      : Asis.Element;
         begin
            for J in Asp_Defs'Range loop
               Asp_Mark := Aspect_Mark (Asp_Defs (J));

               if Expression_Kind (Asp_Mark) = An_Identifier then
                  if To_Lower (Name_Image (Asp_Mark)) = "pre" then
                     Pre_Detected := True;
                  elsif To_Lower (Name_Image (Asp_Mark)) = "post" then
                     Post_Detected := True;
                  end if;
               end if;

               exit when Pre_Detected and then Post_Detected;

            end loop;

            if Pre_Detected or else Post_Detected then
               State.Detected  := True;

               State.Diagnosis :=
                 (if Pre_Detected and then Post_Detected then 3
                  elsif Pre_Detected                     then 1
                  else                                        2);

            end if;
         end;
      end if;

   end Rule_Check_Pre_Op;

   ------------------------------------------
   -- Init_Rule (Specific_Type_Invariants) --
   ------------------------------------------

   procedure Init_Rule (Rule : in out Specific_Type_Invariants_Rule_Type) is
   begin
      Init_Rule (Rule_Template (Rule));

      Rule.Name        := new String'("Specific_Type_Invariants");
      Rule.Rule_Status := Fully_Implemented;
      Rule.Help_Info   := new String'("non class-wide Type_Invariant aspect");
      Rule.Diagnosis   := new String'("definition of non class-wide " &
                                      "Type_Invariant aspect");
   end Init_Rule;

   --------------------------------------------------
   -- Rule_Check_Pre_Op (Specific_Type_Invariants) --
   --------------------------------------------------

   procedure Rule_Check_Pre_Op
     (Rule    : in out Specific_Type_Invariants_Rule_Type;
      Element :        Asis.Element;
      Control : in out Traverse_Control;
      State   : in out Rule_Traversal_State)
   is
      A_Mark : Asis.Element;
      pragma Unreferenced (Rule, Control);
   begin
      if Definition_Kind (Element) = An_Aspect_Specification then

         if Declaration_Kind (Get_Enclosing_Element) =
              A_Private_Extension_Declaration
           or else
             (Declaration_Kind (Get_Enclosing_Element) =
                     A_Private_Type_Declaration
              and then
                Flat_Element_Kind (Type_Declaration_View
                                    (Get_Enclosing_Element)) =
                  A_Tagged_Private_Type_Definition)
           or else
             (Declaration_Kind (Get_Enclosing_Element) =
                An_Ordinary_Type_Declaration
              and then
                Flat_Element_Kind (Type_Declaration_View
                                    (Get_Enclosing_Element)) in
                  A_Tagged_Record_Type_Definition  |
                  A_Derived_Record_Extension_Definition)
         then

            A_Mark := Aspect_Mark (Element);

            if Expression_Kind (A_Mark) = An_Identifier then
               declare
                  A_Mark_Img : constant String :=
                    To_Lower (To_String (Name_Image (A_Mark)));
               begin
                  State.Detected  := A_Mark_Img = "type_invariant";
               end;
            end if;
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

   ---------------------------
   -- Too_Many_Dependencies --
   ---------------------------

   package Dependency_Sets is new
     Ada.Containers.Indefinite_Ordered_Sets (Element_Type => Program_Text);

   use Dependency_Sets;

   Dependencies : Dependency_Sets.Set;

   procedure Collect_Dependencies (CU : Asis.Compilation_Unit);
   --  Collect all the dependencies for CU in a way as it is required by the
   --  rule. That is, in case of a body, the dependencies on the corresponding
   --  spec unit is not collected, but the dependencies introduced by the with
   --  clauses applied to the spec are collected. In case of a subunit, the
   --  dependencies on proper body (proper bodies in case of nested subunits)
   --  and the corresponding spec (if any) are not collected, but the
   --  dependencies introduced by the with clauses applied to the proper body
   --  (bodies) and spec are collected.

   procedure Store_Dependencies (CU : Asis.Compilation_Unit);
   --  Stores in Dependencies the names of the rule (folded to lower case) the
   --  unit under test depends upon it a way needed for this rule. That is, if
   --  a child unit is stored that all its parent and all the grandparent
   --  units are removed, if any

   --------------------------------------------------
   -- Collect_Dependencies (Too_Many_Dependencies) --
   --------------------------------------------------

   procedure Collect_Dependencies (CU : Asis.Compilation_Unit) is
      Next_CU : Asis.Compilation_Unit;
   begin
      Store_Dependencies (CU);

      case Unit_Kind (CU) is
         when A_Library_Unit_Body =>
            Next_CU := Corresponding_Declaration (CU);

            if not Is_Nil (Next_CU) then
               Collect_Dependencies (Next_CU);
            end if;

         when A_Subunit =>
            Next_CU := Corresponding_Subunit_Parent_Body (CU);
            Collect_Dependencies (Next_CU);
         when others =>
            null;
      end case;

   end Collect_Dependencies;

   --------------------------------------------------
   -- Init_Rule (Too_Many_Dependencies) --
   --------------------------------------------------

   procedure Init_Rule (Rule : in out Too_Many_Dependencies_Rule_Type) is
   begin
      Init_Rule (Rule_Template (Rule));

      Rule.Name        := new String'("Too_Many_Dependencies");
      Rule.Rule_Status := Fully_Implemented;
      Rule.Help_Info   := new String'("compilation unit has too many " &
                                      "dependencies");
      Rule.Diagnosis   := new String'("unit has too many dependencies (%1%)");
   end Init_Rule;

   -----------------------------------------------
   -- Rule_Check_Pre_Op (Too_Many_Dependencies) --
   -----------------------------------------------

   procedure Rule_Check_Pre_Op
     (Rule    : in out Too_Many_Dependencies_Rule_Type;
      Element :        Asis.Element;
      Control : in out Traverse_Control;
      State   : in out Rule_Traversal_State)
   is
      CU                : Asis.Compilation_Unit;
      N_Of_Dependencies : Natural;

      pragma Unreferenced (Control);
   begin
      if Element_Kind (Element) = A_Declaration
        and then
         Is_Nil (Get_Enclosing_Element)
      then
         CU := Enclosing_Compilation_Unit (Element);
      else
         return;
      end if;

      Dependencies.Clear;

      Collect_Dependencies (CU);

      N_Of_Dependencies := Natural (Dependencies.Length);

      if N_Of_Dependencies > Rule.Rule_Limit then
            State.Detected := True;
            State.Diag_Params :=
              Enter_String ("%1%" & ASIS_UL.Misc.Image (N_Of_Dependencies));
      end if;

   end Rule_Check_Pre_Op;

   ------------------------------------------------
   -- Store_Dependencies (Too_Many_Dependencies) --
   ------------------------------------------------

   procedure Store_Dependencies (CU : Asis.Compilation_Unit) is
      Cont_Clauses : constant Asis.Element_List :=
        Context_Clause_Elements (CU);

      Name_Img        : Program_Text_Access;
      First_Idx       : Positive;
      Dot_Idx         : Natural;
      Inserted        : Boolean;
      C               : Cursor;
   begin

      for J in Cont_Clauses'Range loop

         if Clause_Kind (Cont_Clauses (J)) = A_With_Clause then

            declare
               Nms : constant Asis.Element_List :=
                 Clause_Names (Cont_Clauses (J));
            begin

               for K in Nms'Range loop
                  Name_Img := new Program_Text'
                                    (To_Lower (Full_Name_Image (Nms (K))));

                  --  Check if we have already collected some child of this
                  --  unit:

                  Inserted := False;

                  C := Dependencies.First;

                  while Has_Element (C) loop
                     if Index (Dependency_Sets.Element (C), Name_Img.all) > 0
                     then
                        Inserted := True;
                        exit;
                     end if;

                     C := Next (C);
                  end loop;

                  if Inserted then
                     goto Next_Iteration;
                  end if;

                  Insert (Container => Dependencies,
                          New_Item  => Name_Img.all,
                          Position  => C,
                          Inserted  => Inserted);

                  if Inserted then
                     First_Idx := Name_Img'First;
                     Dot_Idx   := Index (Name_Img.all, ".", Backward);

                     while Dot_Idx > 0 loop
                        C := Find
                          (Dependencies, Name_Img (First_Idx .. Dot_Idx - 1));

                        if Has_Element (C) then
                           Delete (Dependencies, C);
                        end if;

                        Dot_Idx := Index (Name_Img (First_Idx .. Dot_Idx - 1),
                                          ".",
                                          Backward);
                     end loop;

                  end if;

                  <<Next_Iteration>> null;
               end loop;

            end;

         end if;

      end loop;

      Free (Name_Img);

   end Store_Dependencies;

   -------------------------
   -- Too_Many_Primitives --
   -------------------------

   -------------------------------------
   -- Init_Rule (Too_Many_Primitives) --
   -------------------------------------

   procedure Init_Rule (Rule : in out Too_Many_Primitives_Rule_Type) is
   begin
      Init_Rule (Rule_Template (Rule));

      Rule.Name        := new String'("Too_Many_Primitives");
      Rule.Rule_Status := Fully_Implemented;
      Rule.Help_Info   := new String'("tagged type has too many primitives");
      Rule.Diagnosis   := new String'("tagged type has too many " &
                                      "primitives (%1%)");
   end Init_Rule;

   ---------------------------------------------
   -- Rule_Check_Pre_Op (Too_Many_Primitives) --
   ---------------------------------------------

   procedure Rule_Check_Pre_Op
     (Rule    : in out Too_Many_Primitives_Rule_Type;
      Element :        Asis.Element;
      Control : in out Traverse_Control;
      State   : in out Rule_Traversal_State)
   is
      Arg_Kind : constant Declaration_Kinds := Declaration_Kind (Element);
      Type_Def :          Asis.Element;
      EE       :          Asis.Element;

      Num_Of_Primitives : Natural := 0;

      pragma Unreferenced (Control);
   begin
      if Arg_Kind in
           An_Ordinary_Type_Declaration    |
           A_Private_Type_Declaration      |
           A_Private_Extension_Declaration
      then
         Type_Def := Type_Declaration_View (Element);

         --  First, check if we have a tagged type
         if Arg_Kind = A_Private_Type_Declaration
           and then
            Definition_Kind (Type_Def) /= A_Tagged_Private_Type_Definition
         then
            return;
         end if;

         if Arg_Kind = An_Ordinary_Type_Declaration
           and then
            Type_Kind (Type_Def) not in
              A_Derived_Record_Extension_Definition |
              A_Tagged_Record_Type_Definition       |
              An_Interface_Type_Definition
         then
            return;
         end if;

         --  Then, skip full views of private types.
         if Arg_Kind = An_Ordinary_Type_Declaration
           and then
            Declaration_Kind (Corresponding_Type_Partial_View (Element)) in
              A_Private_Type_Declaration |
              A_Private_Extension_Declaration
         then
            return;
         end if;

         if Arg_Kind = A_Private_Extension_Declaration
           or else
            Type_Kind (Type_Def) in
              A_Derived_Record_Extension_Definition |
              An_Interface_Type_Definition
         then
            Num_Of_Primitives :=
              Implicit_Inherited_Subprograms (Type_Def)'Length;
         end if;

         --  Now, counting explicit primitives

         EE := Get_Enclosing_Element;

         if Flat_Element_Kind (EE) in
              A_Procedure_Body_Declaration |
              A_Function_Body_Declaration  |
              A_Package_Body_Declaration   |
              A_Task_Body_Declaration      |
              An_Entry_Body_Declaration    |
              A_Block_Statement
         then
            --  New explicit primitives may be added to a type in
            --  a package spec only, here we can only override an existing
            --  primitive
            return;
         end if;

         declare
            Dcls : constant Asis.Element_List :=
              (case Flat_Element_Kind (EE) is
                  when A_Package_Declaration         |
                       A_Generic_Package_Declaration =>
                     Visible_Part_Declarative_Items (EE),
                  when others => Nil_Element_List);

            Idx      :          Natural := 0;
            Last_Idx : constant Natural := Dcls'Last;
         begin
            if Is_Nil (Dcls) then
               --  A visible part may be empty
               return;
            end if;

            for J in Dcls'Range loop
               if Is_Equal (Element, Dcls (J)) then
                  Idx := J;
                  exit;
               end if;
            end loop;

            if Idx = 0 then
               --  Type is declared in a private part
               return;
            end if;

            for J in Idx + 1 .. Last_Idx loop
               if Is_Dispatching_Operation (Dcls (J))
                 and then
                  Is_Equal (Primitive_Owner (Dcls (J)), Type_Def)
               then
                  Num_Of_Primitives := Num_Of_Primitives + 1;
               end if;

            end loop;

         end;

         if Num_Of_Primitives > Rule.Rule_Limit then
            State.Detected := True;
            State.Diag_Params :=
              Enter_String ("%1%" & ASIS_UL.Misc.Image (Num_Of_Primitives));
         end if;

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
      --   ... is new Unchecked_Conversion (Target => T1, Source => T2)
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
      Rule.Diagnosis   := new String'("uninitialized global variable");
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

         if Definition_Kind (SM) = A_Type_Definition then
            --  Variable declared with constrained array definition
            State.Detected  := True;
         elsif Definition_Kind (SM) = A_Subtype_Indication then
            SM := Asis.Definitions.Subtype_Mark (SM);

            if not Is_Limited (SM) then
               State.Detected  := True;
            end if;
         else
            State.Detected  := True;
         end if;
      end if;

   end Rule_Check_Pre_Op;

end Gnatcheck.Rules.Custom_3;
------------------------------------------------------------------------------
