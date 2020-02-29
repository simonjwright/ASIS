------------------------------------------------------------------------------
--                                                                          --
--                          GNATCHECK COMPONENTS                            --
--                                                                          --
--             G N A T C H E C K . R U L E S . C U S T O M _ 2              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                     Copyright (C) 2008-2019, AdaCore                     --
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

with Ada.Characters.Handling;        use Ada.Characters.Handling;

with Asis.Clauses;                   use Asis.Clauses;
with Asis.Compilation_Units;         use Asis.Compilation_Units;
with Asis.Declarations;              use Asis.Declarations;
with Asis.Definitions;               use Asis.Definitions;
with Asis.Elements;                  use Asis.Elements;
with Asis.Expressions;               use Asis.Expressions;
with Asis.Extensions;                use Asis.Extensions;
with Asis.Extensions.Flat_Kinds;     use Asis.Extensions.Flat_Kinds;
with Asis.Iterator;                  use Asis.Iterator;
with Asis.Statements;                use Asis.Statements;
with Asis.Text;                      use Asis.Text;

with ASIS_UL.Global_State.Utilities; use ASIS_UL.Global_State.Utilities;
with ASIS_UL.Misc;                   use ASIS_UL.Misc;
with ASIS_UL.Utilities;              use ASIS_UL.Utilities;

with Gnatcheck.ASIS_Utilities;       use Gnatcheck.ASIS_Utilities;
with Gnatcheck.Rules.Traversing;     use Gnatcheck.Rules.Traversing;
with Gnatcheck.Traversal_Stack;  use Gnatcheck.Traversal_Stack;

package body Gnatcheck.Rules.Custom_2 is

   ---------------------------------
   -- Complex_Inlined_Subprograms --
   ---------------------------------

   ---------------------------------------------
   -- Init_Rule (Complex_Inlined_Subprograms) --
   ---------------------------------------------

   procedure Init_Rule (Rule : in out Complex_Inlined_Subprograms_Rule_Type)
   is
   begin
      Init_Rule (One_Integer_Parameter_Rule_Template (Rule));

      Rule.Name        := new String'("Complex_Inlined_Subprograms");
      Rule.Rule_Status := Fully_Implemented;
      Rule.Help_Info   := new String'("complex inlined subprograms");
      Rule.Diagnosis   := new String'
        ("#1#too many statements in inlined subprogram"           &
         "#2#branching in inlined subprogram (%1%)"          &
         "#3#complex declaration in inlined subprogram (%1%)");
   end Init_Rule;

   -----------------------------------------------------
   -- Rule_Check_Pre_Op (Complex_Inlined_Subprograms) --
   -----------------------------------------------------

   procedure Rule_Check_Pre_Op
     (Rule    : in out Complex_Inlined_Subprograms_Rule_Type;
      Element :        Asis.Element;
      Control : in out Traverse_Control;
      State   : in out Rule_Traversal_State)
   is
      Corr_Generic : Asis.Element;
      Exp_Body     : Asis.Element;

      Local_State   : Natural          := 0;
      Local_Control : Traverse_Control := Continue;

      procedure Check_Statement
        (Element       :        Asis.Element;
         Local_Control : in out Traverse_Control;
         Local_State   : in out Natural);
      --  Check if the argument is a branching construct, that is, an IF, LOOP
      --  or CASE statement or a short-circuit control form. Also counts the
      --  number of statements. If a branching construct is encountered or
      --  if the number of statements exceeds the rule limit, terminates the
      --  traversing and sets the diagnosis accordingly.

      procedure No_Operation
        (Element       :        Asis.Element;
         Local_Control : in out Traverse_Control;
         Local_State   : in out Natural);
      --  Does nothing

      procedure Check_Statements is new Traverse_Element
       (State_Information => Natural,
        Pre_Operation     => Check_Statement,
        Post_Operation    => No_Operation);

      ---------------------
      -- Check_Statement --
      ---------------------

      procedure Check_Statement
        (Element       :        Asis.Element;
         Local_Control : in out Traverse_Control;
         Local_State   : in out Natural)
      is
      begin

         if Element_Kind (Element) = A_Statement then
            Local_State := Local_State + 1;

            if Local_State > Rule.Rule_Limit then
               State.Detected  := True;
               State.Diagnosis := 1;
               Local_Control := Terminate_Immediately;
               return;
            end if;
         end if;

         if Statement_Kind (Element) in
               An_If_Statement .. A_For_Loop_Statement
         then
            State.Detected  := True;
            State.Diagnosis := 2;

            if Is_Part_Of_Instance (Element) then
               State.Diag_Params := Enter_String
                 ("%1%" & Build_GNAT_Location (Element));
            else
               State.Diag_Params := Enter_String
                 ("%1%" & "line " &
                  ASIS_UL.Misc.Image (Element_Span (Element).First_Line));
            end if;

            Local_Control := Terminate_Immediately;
         end if;
      end Check_Statement;

      ------------------
      -- No_Operation --
      ------------------

      procedure No_Operation
        (Element       :        Asis.Element;
         Local_Control : in out Traverse_Control;
         Local_State   : in out Natural)
      is
      begin
         null;
      end No_Operation;

   begin --  Rule_Check_Pre_Op (Complex_Inlined_Subprograms)

      if Declaration_Kind (Element) in
        A_Procedure_Body_Declaration .. A_Function_Body_Declaration
      and then
         Has_Pragma_Inline (Element)
      then

         declare
            Dcls : constant Asis.Element_List :=
              Body_Declarative_Items (Element);
         begin

            --  First, check if we have "bad" declarations:

            for J in Dcls'Range loop
               case Declaration_Kind (Dcls (J)) is
                  when A_Task_Type_Declaration         |
                       A_Protected_Type_Declaration    |
                       A_Single_Task_Declaration       |
                       A_Single_Protected_Declaration  |
                       A_Procedure_Body_Declaration    |
                       A_Function_Body_Declaration     |
                       A_Package_Declaration           |
                       A_Package_Body_Declaration      |
                       A_Task_Body_Declaration         |
                       A_Protected_Body_Declaration    |
                       A_Procedure_Body_Stub           |
                       A_Function_Body_Stub            |
                       A_Package_Body_Stub             |
                       A_Task_Body_Stub                |
                       A_Protected_Body_Stub           |
                       A_Generic_Procedure_Declaration |
                       A_Generic_Function_Declaration  |
                       A_Generic_Package_Declaration   |
                       A_Package_Instantiation         |
                       A_Procedure_Instantiation       =>

                     State.Detected  := True;
                     State.Diagnosis := 3;

                     if Is_Part_Of_Instance (Dcls (J)) then
                        State.Diag_Params := Enter_String
                          ("%1%" & Build_GNAT_Location (Dcls (J)));
                     else
                        State.Diag_Params := Enter_String
                          ("%1%" & "line " &
                           ASIS_UL.Misc.Image
                             (Element_Span (Dcls (J)).First_Line));
                     end if;

                     return;

                  when A_Function_Instantiation =>
                     --  check if this is not an instantiation of
                     --  Ada.Unchecked_Convertion

                     if not Is_Unchecked_Convertion_Instance (Dcls (J)) then
                        State.Detected  := True;
                        State.Diagnosis := 3;

                        if Is_Part_Of_Instance (Dcls (J)) then
                           State.Diag_Params := Enter_String
                             ("%1%" & Build_GNAT_Location (Dcls (J)));
                        else
                           State.Diag_Params := Enter_String
                             ("%1%" & "line " &
                              ASIS_UL.Misc.Image
                                (Element_Span (Dcls (J)).First_Line));
                        end if;

                        return;
                     end if;

                  when others =>
                     null;
               end case;
            end loop;

            --  Now we know that there are no bad local declarations, so -
            --  checking the statements.

         end;

         Check_Statements (Element, Local_Control, Local_State);

      elsif Declaration_Kind (Element) in
         A_Procedure_Instantiation .. A_Function_Instantiation
        and then
         Has_Pragma_Inline (Element)
      then
         Corr_Generic := Generic_Unit_Name (Element);
         Corr_Generic := Normalize_Reference (Corr_Generic);
         Corr_Generic := Corresponding_Name_Declaration (Corr_Generic);

         if Declaration_Kind (Corr_Generic) in
           A_Generic_Procedure_Renaming_Declaration ..
           A_Generic_Function_Renaming_Declaration
         then
            Corr_Generic := Corresponding_Base_Entity (Corr_Generic);
            Corr_Generic := Normalize_Reference (Corr_Generic);
            Corr_Generic := Corresponding_Name_Declaration (Corr_Generic);
         end if;

         if not Has_Pragma_Inline (Corr_Generic) then
            --  Otherwise we will report the same problem for the body of the
            --  generic and for each of its instantiations!

            Exp_Body := Corresponding_Body (Element);

            if not Is_Nil (Exp_Body) then
               Rule_Check_Pre_Op
                 (Rule    => Rule,
                  Element => Exp_Body,
                  Control => Control,
                  State   => State);
            end if;

         end if;

      end if;
   end Rule_Check_Pre_Op;

   -----------------------------
   -- Conditional_Expressions --
   -----------------------------

   -----------------------------------------------------
   -- Activate_In_Test_Mode (Conditional_Expressions) --
   -----------------------------------------------------

   overriding procedure Activate_In_Test_Mode
     (Rule : in out Conditional_Expressions_Rule_Type)
   is
   begin
      Process_Rule_Parameter
        (Rule       => Rule,
         Param      => "Except_Assertions",
         Enable     => True,
         Defined_At => "");
   end Activate_In_Test_Mode;

   ----------------------------------------------
   -- Exception_Name (Conditional_Expressions) --
   ----------------------------------------------

   overriding function Exception_Name
     (Rule      : Conditional_Expressions_Rule_Type;
      Exc_Index : Exception_Index)
      return      String
   is
      pragma Unreferenced (Rule);
   begin
      case Exc_Index is
         when 1 =>
            return "Except_Assertions";
         when others =>
            return "";
      end case;
   end Exception_Name;

   ------------------------------------------------
   -- Exception_Number (Conditional_Expressions) --
   ------------------------------------------------

   overriding function Exception_Number
     (Rule     : Conditional_Expressions_Rule_Type;
      Exc_Name : String)
      return     Exception_Numbers
   is
      pragma Unreferenced (Rule);
      Result : Exception_Numbers := Not_An_Exception;
      Normalized_Exc_Name : constant String := To_Lower (Exc_Name);
   begin
      if Normalized_Exc_Name = "except_assertions" then
         Result := 1;
      end if;

      return Result;
   end Exception_Number;

   ------------------------------------------
   -- Init_Rule (Conditional_Expressions) --
   ------------------------------------------

   overriding procedure Init_Rule
     (Rule : in out Conditional_Expressions_Rule_Type)
   is
   begin
      Init_Rule (Rule_With_Exceptions_Template (Rule));

      Rule.Name        := new String'("Conditional_Expressions");
      Rule.Rule_Status := Fully_Implemented;
      Rule.Help_Info   := new String'("Ada 2012 conditional expressions");
      Rule.Diagnosis   := new String'("conditional expression");
   end Init_Rule;

   -------------------------------------------------
   -- Rule_Check_Pre_Op (Conditional_Expressions) --
   -------------------------------------------------

   overriding procedure Rule_Check_Pre_Op
     (Rule    : in out Conditional_Expressions_Rule_Type;
      Element :        Asis.Element;
      Control : in out Traverse_Control;
      State   : in out Rule_Traversal_State)
   is
      Enclosing_El : Asis.Element;
      Steps_Up     : Elmt_Idx;

      pragma Unreferenced (Control);
   begin
      if Expression_Kind (Element) in
           A_Case_Expression |
           An_If_Expression
      then
         if Rule.Exceptions (1) then
            Enclosing_El := Get_Enclosing_Element;
            Steps_Up     := 1;

            while Element_Kind (Enclosing_El) not in
               A_Pragma      |
               A_Declaration |
               A_Definition  |
               A_Statement
            loop
               Enclosing_El := Get_Enclosing_Element (Steps_Up);
               Steps_Up     := Steps_Up + 1;
            end loop;

            if not Is_Assertion (Enclosing_El) then
               State.Detected  := True;
            end if;
         else
            State.Detected  := True;
         end if;

      end if;
   end Rule_Check_Pre_Op;

   ----------------------------------
   -- Deep_Inheritance_Hierarchies --
   ----------------------------------

   ----------------------------------------------
   -- Init_Rule (Deep_Inheritance_Hierarchies) --
   ----------------------------------------------

   procedure Init_Rule (Rule : in out Deep_Inheritance_Hierarchies_Rule_Type)
   is
   begin
      Init_Rule (One_Integer_Parameter_Rule_Template (Rule));
      Rule.Rule_Bound := -1;

      Rule.Name        := new String'("Deep_Inheritance_Hierarchies");
      Rule.Rule_Status := Fully_Implemented;
      Rule.Help_Info   := new String'("derivation tree is too deep");
      Rule.Diagnosis   := new String'("derivation tree is too deep (%1%)");

      Rule.Check_In_Expanded_Generics := True;

   end Init_Rule;

   ------------------------------------------------------
   -- Rule_Check_Pre_Op (Deep_Inheritance_Hierarchies) --
   ------------------------------------------------------

   procedure Rule_Check_Pre_Op
     (Rule    : in out Deep_Inheritance_Hierarchies_Rule_Type;
      Element :        Asis.Element;
      Control : in out Traverse_Control;
      State   : in out Rule_Traversal_State)
   is
      pragma Unreferenced (Control);
      Type_Def : Asis.Element := Nil_Element;
      Depth    : Natural;
   begin

      if Declaration_Kind (Element) = An_Ordinary_Type_Declaration then
         Type_Def := Type_Declaration_View (Element);
      end if;

      if Type_Kind (Type_Def) = A_Derived_Record_Extension_Definition
        or else
         Type_Kind (Type_Def) = A_Tagged_Record_Type_Definition
        or else
         Type_Kind (Type_Def) = An_Interface_Type_Definition
      then
         Depth := Inheritance_Depth (Type_Def);

         if Depth > Rule.Rule_Limit then
            State.Detected    := True;
            State.Diag_Params :=
              Enter_String ("%1%" & ASIS_UL.Misc.Image (Depth));
         end if;

      end if;

   end Rule_Check_Pre_Op;

   ----------------------------
   -- Deeply_Nested_Generics --
   ----------------------------

   ----------------------------------------
   -- Init_Rule (Deeply_Nested_Generics) --
   ----------------------------------------

   procedure Init_Rule (Rule : in out Deeply_Nested_Generics_Rule_Type)
   is
   begin
      Init_Rule (One_Integer_Parameter_Rule_Template (Rule));
      Rule.Rule_Bound := 0;

      Rule.Name        := new String'("Deeply_Nested_Generics");
      Rule.Rule_Status := Fully_Implemented;
      Rule.Help_Info   := new String'("deeply nested generic declarations");
      Rule.Diagnosis   := new String'("deeply nested generic (%1%)");
   end Init_Rule;

   ------------------------------------------------
   -- Rule_Check_Pre_Op (Deeply_Nested_Generics) --
   ------------------------------------------------

   procedure Rule_Check_Pre_Op
     (Rule    : in out Deeply_Nested_Generics_Rule_Type;
      Element :        Asis.Element;
      Control : in out Traverse_Control;
      State   : in out Rule_Traversal_State)
   is
      pragma Unreferenced (Control);
      Generic_Nesting_Level : Natural := 0;
      Encl_El               : Asis.Element;
   begin

      if Declaration_Kind (Element) in A_Generic_Declaration then

         Encl_El := Enclosing_Element (Element);

         while not Is_Nil (Encl_El) loop

            case Declaration_Kind (Encl_El) is
               when A_Generic_Declaration =>
                  Generic_Nesting_Level := Generic_Nesting_Level + 1;

               when Not_A_Declaration            |
                    A_Procedure_Body_Declaration |
                    A_Function_Body_Declaration  |
                    A_Package_Body_Declaration   |
                    A_Task_Body_Declaration      |
                    A_Protected_Body_Declaration |
                    An_Entry_Body_Declaration    =>
                  exit;

               when others =>
                  null;
            end case;

            Encl_El := Enclosing_Element (Encl_El);
         end loop;

         if Generic_Nesting_Level > Rule.Rule_Limit then
            State.Detected := True;
            State.Diag_Params :=
              Enter_String ("%1%" &
                            ASIS_UL.Misc.Image (Generic_Nesting_Level));
         end if;

      end if;

   end Rule_Check_Pre_Op;

   ----------------------------------
   -- Deeply_Nested_Local_Inlining --
   ----------------------------------

   ----------------------------------------------
   -- Init_Rule (Deeply_Nested_Local_Inlining) --
   ----------------------------------------------

   procedure Init_Rule (Rule : in out Deeply_Nested_Local_Inlining_Rule_Type)
   is
   begin
      Init_Rule (One_Integer_Parameter_Rule_Template (Rule));

      Rule.Name        := new String'("Deeply_Nested_Local_Inlining");
      Rule.Rule_Status := Non_Documented;
      Rule.Help_Info   := new String'("deeply nested inlining (local check)");
      Rule.Diagnosis   := new String'("deeply nested inlining (local check)");
   end Init_Rule;

   ------------------------------------------------------
   -- Rule_Check_Pre_Op (Deeply_Nested_Local_Inlining) --
   ------------------------------------------------------

   procedure Rule_Check_Pre_Op
     (Rule    : in out Deeply_Nested_Local_Inlining_Rule_Type;
      Element :        Asis.Element;
      Control : in out Traverse_Control;
      State   : in out Rule_Traversal_State)
   is
      pragma Unreferenced (Control);
      Local_State   : Natural          := 0;
      Local_Control : Traverse_Control := Continue;

      procedure Check_Inlined_Body_Pre_Op
        (Element       :        Asis.Element;
         Local_Control : in out Traverse_Control;
         Local_State   : in out Natural);
      --  Checks nesting inlining. That is, if Element is a call to a
      --  subprogram that is inlined  (has a pragma Inline applied to it and
      --  located in the same body as Rule_Check_Pre_Op.Element), adds 1 to
      --  Local_State and processes the body of the called inlined subprogram.
      --  If Local_State exceeds the rule limit, sets Control

      procedure Check_Inlined_Body_Post_Op
        (Element :        Asis.Element;
         Local_Control : in out Traverse_Control;
         Local_State   : in out Natural);
      --  Does nothing

      procedure Check_Local_Inlinings is new Traverse_Element
        (State_Information => Natural,
         Pre_Operation     => Check_Inlined_Body_Pre_Op,
         Post_Operation    => Check_Inlined_Body_Post_Op);

      procedure Check_Inlined_Body_Pre_Op
        (Element       :        Asis.Element;
         Local_Control : in out Traverse_Control;
         Local_State   : in out Natural)
      is
         Tmp : Asis.Element;
      begin

         if Statement_Kind (Element) = A_Procedure_Call_Statement
          or else
            Expression_Kind (Element) = A_Function_Call
         then
            Tmp := Get_Called_Element (Element);

            if Has_Pragma_Inline (Tmp) then
               --  This means that Tmp is not Nil_Element
               Tmp :=
                 ASIS_UL.Global_State.Utilities.Corresponding_Element (Tmp);

               if not Is_Nil (Tmp) then
                  Tmp := Corresponding_Body (Tmp);

                  if not Is_Nil (Tmp) then
                     Local_State := Local_State + 1;

                     if Local_State > Rule.Rule_Limit then
                        State.Detected := True;
                        Local_Control  := Terminate_Immediately;
                     else
                        Check_Local_Inlinings
                          (Tmp, Local_Control, Local_State);
                     end if;

                     Local_State := Local_State - 1;
                  end if;

               end if;

            end if;
         end if;

      end Check_Inlined_Body_Pre_Op;

      procedure Check_Inlined_Body_Post_Op
        (Element :        Asis.Element;
         Local_Control : in out Traverse_Control;
         Local_State   : in out Natural)
      is
      begin
         null;
      end Check_Inlined_Body_Post_Op;

   begin

      if Declaration_Kind (Element) in
        A_Procedure_Body_Declaration .. A_Function_Body_Declaration
      and then
         Has_Pragma_Inline (Element)
      then
         Check_Local_Inlinings (Element, Local_Control, Local_State);
      end if;

   end Rule_Check_Pre_Op;

   --------------------------------
   -- Direct_Calls_To_Primitives --
   --------------------------------

   --------------------------------------------------------
   -- Activate_In_Test_Mode (Direct_Calls_To_Primitives) --
   --------------------------------------------------------

   overriding procedure Activate_In_Test_Mode
     (Rule : in out Direct_Calls_To_Primitives_Rule_Type)
   is
   begin
      Process_Rule_Parameter
        (Rule       => Rule,
         Param      => "Except_Constructors",
         Enable     => True,
         Defined_At => "");
   end Activate_In_Test_Mode;

   -------------------------------------------------
   -- Exception_Name (Direct_Calls_To_Primitives) --
   -------------------------------------------------

   overriding function Exception_Name
     (Rule      : Direct_Calls_To_Primitives_Rule_Type;
      Exc_Index : Exception_Index)
      return      String
   is
      pragma Unreferenced (Rule);
   begin
      case Exc_Index is
         when 1 =>
            return "Except_Constructors";
         when others =>
            return "";
      end case;
   end Exception_Name;

   ---------------------------------------------------
   -- Exception_Number (Direct_Calls_To_Primitives) --
   ---------------------------------------------------

   overriding function Exception_Number
     (Rule     : Direct_Calls_To_Primitives_Rule_Type;
      Exc_Name : String)
      return     Exception_Numbers
   is
      pragma Unreferenced (Rule);
      Result : Exception_Numbers := Not_An_Exception;
      Normalized_Exc_Name : constant String := To_Lower (Exc_Name);
   begin
      if Normalized_Exc_Name = "except_constructors" then
         Result := 1;
      end if;

      return Result;
   end Exception_Number;

   --------------------------------------------
   -- Init_Rule (Direct_Calls_To_Primitives) --
   --------------------------------------------

   overriding procedure Init_Rule
     (Rule : in out Direct_Calls_To_Primitives_Rule_Type)
   is
   begin
      Init_Rule (Rule_With_Exceptions_Template (Rule));

      Rule.Name        := new String'("Direct_Calls_To_Primitives");
      Rule.Rule_Status := Fully_Implemented;
      Rule.Help_Info   := new String'("non-dispatching calls to primitives");
      Rule.Diagnosis   :=
        new String'("non-dispatching call to primitive operation");
   end Init_Rule;

   ------------------------------------------------
   -- Rule_Check_Pre_Op (Direct_Calls_To_Primitives) --
   ------------------------------------------------

   overriding procedure Rule_Check_Pre_Op
     (Rule    : in out Direct_Calls_To_Primitives_Rule_Type;
      Element :        Asis.Element;
      Control : in out Traverse_Control;
      State   : in out Rule_Traversal_State)
   is
      pragma Unreferenced (Control);
      Called_Routine  : Asis.Element;
      Calling_Context : Asis.Element;
      Owner           : Asis.Element;
      Steps_Up        : Elmt_Idx;
   begin

      if (Statement_Kind (Element) = A_Procedure_Call_Statement
        or else
         Expression_Kind (Element) = A_Function_Call)
        and then
         not Is_Dispatching_Call (Element)
      then
         Called_Routine := Get_Called_Element (Element);

         if Declaration_Kind (Called_Routine) in
           A_Procedure_Renaming_Declaration .. A_Function_Renaming_Declaration
         then
            Called_Routine := Get_Renamed_Subprogram (Called_Routine);
         end if;

         if Is_Dispatching_Operation (Called_Routine) then

            State.Detected := True;

            if Is_Part_Of_Inherited (Called_Routine) then
               Owner := Corresponding_Type (Called_Routine);
            else
               Owner := Primitive_Owner (Called_Routine);
            end if;

            if Definition_Kind (Owner) = A_Private_Type_Definition
              and then
               not Full_View_Visible
                     (Type_Decl => Enclosing_Element (Owner),
                      At_Place  => Element)
            then
               --  At the place of the call we have an operation of non-tagged
               --  private type, so this call should not be considered as
               --  a dispatching call
               Reset_State (State);
            end if;

            --  And now we have to check if we have one of the possible
            --  exception cases.

            --  1. We have the exception Except_Constructors ON, and we have
            --     a call to a constructor function.

            if State.Detected
             and then
               Rule.Exceptions (1)
             and then
               Expression_Kind (Element) = A_Function_Call
             and then
               Is_Constructor_Function (Called_Routine)
            then
               Reset_State (State);
            end if;

            --  2. We are in the situation when (grand)parent's primitive is
            --     called in the body of overriding child's primitive.

            if State.Detected then
               Calling_Context := Get_Enclosing_Element;
               Steps_Up        := 1;

               while not (Is_Nil (Calling_Context)
                   or else
                     Is_Body (Calling_Context))
               loop
                  Calling_Context := Get_Enclosing_Element (Steps_Up);
                  Steps_Up        := Steps_Up + 1;
               end loop;

               if Declaration_Kind (Calling_Context) in
                  A_Procedure_Body_Declaration .. A_Function_Body_Declaration
               then
                  Calling_Context :=
                    Corresponding_Declaration (Calling_Context);

                  if Is_Overriding_Operation (Calling_Context) then
                     Calling_Context :=
                       Corresponding_Overridden_Operation (Calling_Context);

                     if Is_Equal (Calling_Context, Called_Routine) then
                        Reset_State (State);
                     end if;
                  end if;

               end if;
            end if;

         end if;
      end if;

   end Rule_Check_Pre_Op;

   ----------------------------------
   -- Exits_From_Conditional_Loops --
   ----------------------------------

   ----------------------------------------------
   -- Init_Rule (Exits_From_Conditional_Loops) --
   ----------------------------------------------

   procedure Init_Rule (Rule : in out Exits_From_Conditional_Loops_Rule_Type)
   is
   begin
      Init_Rule (Rule_Template (Rule));

      Rule.Name        := new String'("Exits_From_Conditional_Loops");
      Rule.Rule_Status := Fully_Implemented;
      Rule.Help_Info   := new String'("exit from conditional loops");
      Rule.Diagnosis   := new String'("exit from conditional loop");
   end Init_Rule;

   ---------------------------------------------------------
   -- Rule_Check_Pre_Op (Exits_From_Conditional_Loops) --
   ---------------------------------------------------------

   procedure Rule_Check_Pre_Op
     (Rule    : in out Exits_From_Conditional_Loops_Rule_Type;
      Element :        Asis.Element;
      Control : in out Traverse_Control;
      State   : in out Rule_Traversal_State)
   is
      Loop_Exited : Asis.Element;
      Tmp         : Asis.Element;
      pragma Unreferenced (Rule, Control);
   begin

      if Statement_Kind (Element) = An_Exit_Statement then

         Loop_Exited := Corresponding_Loop_Exited (Element);

         if Statement_Kind (Loop_Exited) in
           A_While_Loop_Statement .. A_For_Loop_Statement
         then
            State.Detected  := True;
         else
            Tmp := Enclosing_Element (Element);

            while not Is_Equal (Tmp, Loop_Exited) loop

               if Statement_Kind (Tmp) in
                 A_While_Loop_Statement .. A_For_Loop_Statement
               then
                  State.Detected := True;
                  exit;
               end if;

               Tmp := Enclosing_Element (Tmp);
            end loop;

         end if;

      end if;

   end Rule_Check_Pre_Op;

   ----------------------
   -- Global_Variables --
   ----------------------

   ----------------------------------------------
   -- Activate_In_Test_Mode (Global_Variables) --
   ----------------------------------------------

   overriding procedure Activate_In_Test_Mode
     (Rule : in out Global_Variables_Rule_Type)
   is
   begin
      Process_Rule_Parameter
        (Rule       => Rule,
         Param      => "Only_Public",
         Enable     => True,
         Defined_At => "");
   end Activate_In_Test_Mode;

   ---------------------------------------
   -- Exception_Name (Global_Variables) --
   ---------------------------------------

   overriding function Exception_Name
     (Rule      : Global_Variables_Rule_Type;
      Exc_Index : Exception_Index)
      return      String
   is
      pragma Unreferenced (Rule);
   begin
      case Exc_Index is
         when 1 =>
            return "Only_Public";
         when others =>
            return "";
      end case;
   end Exception_Name;

   -----------------------------------------
   -- Exception_Number (Global_Variables) --
   -----------------------------------------

   overriding function Exception_Number
     (Rule     : Global_Variables_Rule_Type;
      Exc_Name : String)
      return     Exception_Numbers
   is
      pragma Unreferenced (Rule);
      Result : Exception_Numbers := Not_An_Exception;
      Normalized_Exc_Name : constant String := To_Lower (Exc_Name);
   begin
      if Normalized_Exc_Name = "only_public" then
         Result := 1;
      end if;

      return Result;
   end Exception_Number;

   ----------------------------------
   -- Init_Rule (Global_Variables) --
   ----------------------------------

   overriding procedure Init_Rule
     (Rule : in out Global_Variables_Rule_Type)
   is
   begin
      Init_Rule (Rule_With_Exceptions_Template (Rule));

      Rule.Name        := new String'("Global_Variables");
      Rule.Rule_Status := Fully_Implemented;
      Rule.Help_Info   := new String'("declarations of global variables");
      Rule.Diagnosis   := new String'("declaration of global variable");
   end Init_Rule;

   ------------------------------------------
   -- Rule_Check_Pre_Op (Global_Variables) --
   ------------------------------------------

   overriding procedure Rule_Check_Pre_Op
     (Rule    : in out Global_Variables_Rule_Type;
      Element :        Asis.Element;
      Control : in out Traverse_Control;
      State   : in out Rule_Traversal_State)
   is
      Encl_Unit : Asis.Element;
      pragma Unreferenced (Control);
   begin

      if Declaration_Kind (Element) = A_Variable_Declaration then

         Encl_Unit := Get_Enclosing_Element;

         if Declaration_Kind (Encl_Unit) in
              A_Package_Declaration | A_Generic_Package_Declaration
            and then
              Is_Nil (Enclosing_Element (Encl_Unit))
         then

            declare
               Encl_CUnit : constant Asis.Compilation_Unit :=
                 Enclosing_Compilation_Unit (Element);
            begin

               if Rule.Exceptions (1) then

                  if Unit_Class (Encl_CUnit) /= A_Private_Declaration then
                     declare
                        Public_Dcls : constant Asis.Element_List :=
                          Visible_Part_Declarative_Items (Encl_Unit);
                        Last_Public_Dcl  : Asis.Element;
                        Last_Public_Span : Span;
                        Arg_Span         : Span;
                     begin

                        if not Is_Nil (Public_Dcls) then
                           Last_Public_Dcl  := Public_Dcls (Public_Dcls'Last);
                           Last_Public_Span := Element_Span (Last_Public_Dcl);
                           Arg_Span         := Element_Span (Element);

                           State.Detected :=
                             Arg_Span.Last_Line < Last_Public_Span.First_Line
                            or else
                             -- pathological case
                             (Arg_Span.Last_Line = Last_Public_Span.First_Line
                              and then
                              Arg_Span.First_Column <=
                                Last_Public_Span.Last_Column);
                        end if;
                     end;
                  end if;

               else
                  State.Detected := True;
               end if;

            end;
         end if;
      end if;

   end Rule_Check_Pre_Op;

   ------------------------
   -- Maximum_Parameters --
   ------------------------

   ------------------------------------
   -- Init_Rule (Maximum_Parameters) --
   ------------------------------------

   overriding procedure Init_Rule (Rule : in out Maximum_Parameters_Rule_Type)
   is
   begin
      Init_Rule (One_Integer_Parameter_Rule_Template (Rule));

      Rule.Name        := new String'("Maximum_Parameters");
      Rule.Rule_Status := Fully_Implemented;
      Rule.Help_Info   := new String
       '("maximum number of subprogram parameters");
      Rule.Diagnosis   := new String'("too many formal parameters (%1%)");
   end Init_Rule;

   --------------------------------------------
   -- Rule_Check_Pre_Op (Maximum_Parameters) --
   --------------------------------------------

   overriding procedure Rule_Check_Pre_Op
     (Rule    : in out Maximum_Parameters_Rule_Type;
      Element :        Asis.Element;
      Control : in out Traverse_Control;
      State   : in out Rule_Traversal_State)
   is
      pragma Unreferenced (Control);
   begin
      if not Is_Subunit (Element)
       and then
         (Declaration_Kind (Element) in
          A_Procedure_Declaration         |
          A_Function_Declaration          |
          A_Generic_Procedure_Declaration |
          A_Generic_Function_Declaration
         or else
          (Declaration_Kind (Element) in
           A_Procedure_Body_Declaration       |
           A_Function_Body_Declaration        |
           A_Null_Procedure_Declaration       |
           An_Expression_Function_Declaration |
           A_Procedure_Body_Stub              |
           A_Function_Body_Stub
          and then
           Is_Nil (Corresponding_Declaration (Element))))
      then
         declare
            Params : constant Asis.Element_List := Parameter_Profile (Element);
            Par_N  : Natural := 0;
         begin
            for J in Params'Range loop
               Par_N := Par_N + Names (Params (J))'Length;
            end loop;

            if Par_N > Rule.Rule_Limit then
               State.Detected  := True;
               State.Diag_Params :=
                 Enter_String ("%1%" & ASIS_UL.Misc.Image (Par_N));
            end if;
         end;
      end if;

   end Rule_Check_Pre_Op;

   ----------------------
   -- Membership_Tests --
   ----------------------

   ----------------------------------------------
   -- Activate_In_Test_Mode (Membership_Tests) --
   ----------------------------------------------

   overriding procedure Activate_In_Test_Mode
     (Rule : in out Membership_Tests_Rule_Type)
   is
   begin
      Process_Rule_Parameter
        (Rule       => Rule,
         Param      => "Except_Assertions",
         Enable     => True,
         Defined_At => "");
      Process_Rule_Parameter
        (Rule       => Rule,
         Param      => "Multi_Alternative_Only",
         Enable     => True,
         Defined_At => "");
   end Activate_In_Test_Mode;

   ---------------------------------------
   -- Exception_Name (Membership_Tests) --
   ---------------------------------------

   overriding function Exception_Name
     (Rule      : Membership_Tests_Rule_Type;
      Exc_Index : Exception_Index)
      return      String
   is
      pragma Unreferenced (Rule);
   begin
      case Exc_Index is
         when 1 =>
            return "Except_Assertions";
         when 2 =>
            return "Multi_Alternative_Only";
         when 3 =>
            return "Float_Types_Only";
         when others =>
            return "";
      end case;
   end Exception_Name;

   -----------------------------------------
   -- Exception_Number (Membership_Tests) --
   -----------------------------------------

   overriding function Exception_Number
     (Rule     : Membership_Tests_Rule_Type;
      Exc_Name : String)
      return     Exception_Numbers
   is
      pragma Unreferenced (Rule);
      Result : Exception_Numbers := Not_An_Exception;
      Normalized_Exc_Name : constant String := To_Lower (Exc_Name);
   begin
      if Normalized_Exc_Name = "except_assertions" then
         Result := 1;
      elsif Normalized_Exc_Name = "multi_alternative_only" then
         Result := 2;
      elsif Normalized_Exc_Name = "float_types_only" then
         Result := 3;
      end if;

      return Result;
   end Exception_Number;

   ----------------------------------
   -- Init_Rule (Membership_Tests) --
   ----------------------------------

   overriding procedure Init_Rule
     (Rule : in out Membership_Tests_Rule_Type)
   is
   begin
      Init_Rule (Rule_With_Exceptions_Template (Rule));

      Rule.Name        := new String'("Membership_Tests");
      Rule.Rule_Status := Fully_Implemented;
      Rule.Help_Info   := new String'("membership test");
      Rule.Diagnosis   := new String'("membership test");
   end Init_Rule;

   ------------------------------------------
   -- Rule_Check_Pre_Op (Membership_Tests) --
   ------------------------------------------

   overriding procedure Rule_Check_Pre_Op
     (Rule    : in out Membership_Tests_Rule_Type;
      Element :        Asis.Element;
      Control : in out Traverse_Control;
      State   : in out Rule_Traversal_State)
   is
      Detected     : Boolean := False;
      Enclosing_El : Asis.Element;
      Steps_Up     : Elmt_Idx;
      Tmp          : Asis.Element;

      pragma Unreferenced (Control);

   begin

      Detected := Expression_Kind (Element) in
        An_In_Membership_Test | A_Not_In_Membership_Test;

      if Detected and then Rule.Exceptions (2) then
         Detected := Membership_Test_Choices (Element)'Length > 1;
      end if;

      if Detected then
         if Rule.Exceptions (1) then
            Enclosing_El := Get_Enclosing_Element;
            Steps_Up     := 1;

            while Element_Kind (Enclosing_El) not in
               A_Pragma      |
               A_Declaration |
               A_Definition  |
               A_Statement
            loop
               Enclosing_El := Get_Enclosing_Element (Steps_Up);
               Steps_Up     := Steps_Up + 1;
            end loop;

            if not Is_Assertion (Enclosing_El) then
               State.Detected  := True;
            end if;
         else
            State.Detected  := True;
         end if;

         if State.Detected and then Rule.Exceptions (3) then
            Tmp := Membership_Test_Expression (Element);
            State.Detected := Is_Float (Tmp);
         end if;
      end if;
   end Rule_Check_Pre_Op;

   ------------------------------------
   -- Misnamed_Controlling_Parameter --
   ------------------------------------

   -------------------------------------------------
   -- Init_Rule (Misnamed_Controlling_Parameters) --
   -------------------------------------------------

   procedure Init_Rule
     (Rule : in out Misnamed_Controlling_Parameters_Rule_Type)
   is
   begin
      Init_Rule (Rule_Template (Rule));

      Rule.Name        := new String'("Misnamed_Controlling_Parameters");
      Rule.Rule_Status := Fully_Implemented;
      Rule.Help_Info   := new String'("Badly formatted profile of "     &
                                      "a primitive operation");
      Rule.Diagnosis   := new String'("#1#first parameter should be "   &
                                       "of type %1%"                    &
                                      "#2#first parameter should have " &
                                       "name 'This'");
   end Init_Rule;

   ---------------------------------------------------------
   -- Rule_Check_Pre_Op (Misnamed_Controlling_Parameters) --
   ---------------------------------------------------------

   procedure Rule_Check_Pre_Op
     (Rule    : in out Misnamed_Controlling_Parameters_Rule_Type;
      Element :        Asis.Element;
      Control : in out Traverse_Control;
      State   : in out Rule_Traversal_State)
   is
      pragma Unreferenced (Rule, Control);
   begin

      if Is_Dispatching_Operation (Element) then

            --  Filter out the cases we are not interested in:
            case Declaration_Kind (Element) is
               when A_Procedure_Body_Declaration |
                    A_Function_Body_Declaration  |
                    A_Procedure_Body_Stub        |
                    A_Function_Body_Stub         =>

                  if not Is_Nil (Corresponding_Declaration (Element)) then
                     return;
                  end if;

               when others =>
                  null;
            end case;

         declare
            This   : Asis.Element;
            Params : constant Asis.Element_List := Parameter_Profile (Element);
            Owner  : constant Asis.Element      :=
              First_Name (Enclosing_Element (Primitive_Owner (Element)));

            No_Control_Par : Boolean := True;
            --  Used to check if there is a case of a function with a
            --  controlling result and no control parameter
         begin

            if Is_Nil (Params) then
               --  The only possibility is a parameterless function with
               --  controlling result, no rule violation in this case
               return;
            end if;

            This := First_Name (Params (Params'First));

            if To_Lower (To_String (Defining_Name_Image (This))) /= "this" then
               State.Detected  := True;
               State.Diagnosis := 2;
            else
               This := Object_Declaration_View (Params (Params'First));

               if Definition_Kind (This) = An_Access_Definition then
                  This := Anonymous_Access_To_Object_Subtype_Mark (This);
               end if;

               This := Normalize_Reference (This);
               This := Corresponding_Name_Declaration (This);

               if Declaration_Kind (This) = A_Subtype_Declaration then
                  This := Corresponding_First_Subtype (This);
               end if;

               This := First_Name (This);

               if not (Defining_Name_Kind (This) = A_Defining_Identifier
                   and then
                       Defining_Name_Image (This) =
                         Defining_Name_Image (Owner))
               then
                  State.Detected  := True;
                  State.Diagnosis := 1;
                  State.Diag_Params :=
                    Enter_String ("%1%" &
                                  To_String (Defining_Name_Image (Owner)));
               end if;

            end if;

            --  Filter out the case of a function with controlling result and
            --  no parameter of tagged type

            if State.Detected
              and then
               Declaration_Kind (Element) in A_Function_Declaration          |
                                             A_Function_Body_Declaration     |
                                             A_Function_Renaming_Declaration |
                                             A_Function_Body_Stub            |
                                             An_Expression_Function_Declaration
            then

               for J in  Params'Range loop
                  This := Object_Declaration_View (Params (J));

                  if Definition_Kind (This) = An_Access_Definition then
                     This := Anonymous_Access_To_Object_Subtype_Mark (This);
                  end if;

                  if Expression_Kind (This) /= An_Attribute_Reference then
                     This := Normalize_Reference (This);
                     This := Corresponding_Name_Declaration (This);
                     This := Corresponding_First_Subtype (This);
                     This := First_Name (This);

                     if Is_Equal (This, Owner) then
                        No_Control_Par := False;
                        exit;
                     end if;
                  end if;

               end loop;

               if No_Control_Par then
                  --  Not a rule violation, function with no controlling
                  --  parameters

                  Reset_State (State);
               end if;

            end if;

         end;

      end if;

   end Rule_Check_Pre_Op;

   ----------------------------------------
   --  No_Scalar_Storage_Order_Specified --
   ----------------------------------------

   ---------------------------------------------------
   -- Init_Rule (No_Scalar_Storage_Order_Specified) --
   ---------------------------------------------------

   overriding procedure Init_Rule
     (Rule : in out No_Scalar_Storage_Order_Specified_Rule_Type)
   is
   begin
      Init_Rule (Rule_Template (Rule));

      Rule.Name        := new String'("No_Scalar_Storage_Order_Specified");
      Rule.Rule_Status := Fully_Implemented;
      Rule.Help_Info   := new String'("a record/record extension type has " &
                                      "record representation clause but no " &
                                      "Scalar_Storage_Order attribute clause");
      Rule.Diagnosis   := new String'
       ("#1#Scalar_Storage_Order is not specified"  &
        "#2#Scalar_Storage_Order is not specified " &
        "(Bit_Order is Low_Order_First)"           &
        "#3#Scalar_Storage_Order is not specified " &
        "(Bit_Order is High_Order_First)");
   end Init_Rule;

   -----------------------------------------------------------
   -- Rule_Check_Pre_Op (No_Scalar_Storage_Order_Specified) --
   -----------------------------------------------------------

   overriding procedure Rule_Check_Pre_Op
     (Rule    : in out No_Scalar_Storage_Order_Specified_Rule_Type;
      Element :        Asis.Element;
      Control : in out Traverse_Control;
      State   : in out Rule_Traversal_State)
   is
      pragma Unreferenced (Control, Rule);
      Type_Def       : Asis.Element;
      Is_Record_Type : Boolean := False;
      Asp_Mark       : Asis.Element;
   begin
      if Declaration_Kind (Element) = An_Ordinary_Type_Declaration then
         Type_Def := Type_Declaration_View (Element);

         case Type_Kind (Type_Def) is
            when A_Derived_Record_Extension_Definition |
                 A_Record_Type_Definition              |
                 A_Tagged_Record_Type_Definition       =>
               Is_Record_Type := True;
            when A_Derived_Type_Definition =>
               Is_Record_Type :=
                 Type_Kind (Type_Declaration_View
                   (Corresponding_Root_Type (Type_Def))) =
                   A_Record_Type_Definition;
            when others =>
               null;
         end case;

         if Is_Record_Type then
            declare
               CRC : constant Asis.Element_List :=
                 Corresponding_Representation_Items (First_Name (Element));
               Rec_Rep_Clause_Found              : Boolean := False;
               Scalar_Storage_Order_Clause_Found : Boolean := False;
               Bit_Order_Low                     : Boolean := False;
               Bit_Order_High                    : Boolean := False;
               Attr                              : Asis.Element;
            begin
               for J in CRC'Range loop
                  case Flat_Element_Kind (CRC (J)) is
                     when A_Record_Representation_Clause =>

                        if  Component_Clauses
                              (CRC (J), Include_Pragmas => False)'Length > 0
                        then
                           Rec_Rep_Clause_Found := True;
                           exit when Scalar_Storage_Order_Clause_Found
                                    and then
                                     (Bit_Order_Low or else Bit_Order_High);
                        end if;
                     when An_Attribute_Definition_Clause =>
                        Attr := Representation_Clause_Name (CRC (J));

                        if Attribute_Kind (Attr) =
                           An_Implementation_Defined_Attribute
                        then
                           Attr := Attribute_Designator_Identifier (Attr);

                           if To_Lower_Case (Name_Image (Attr)) =
                              "scalar_storage_order"
                           then
                              Scalar_Storage_Order_Clause_Found := True;
                              exit when Rec_Rep_Clause_Found
                                       and then
                                       (Bit_Order_Low or else Bit_Order_High);
                           end if;
                        elsif Attribute_Kind (Attr) =
                              A_Bit_Order_Attribute
                        then
                           Attr := Representation_Clause_Expression (CRC (J));
                           Attr := Normalize_Reference (Attr);

                           if To_Lower_Case (Name_Image (Attr)) =
                              "high_order_first"
                           then
                              Bit_Order_High := True;
                           elsif To_Lower_Case (Name_Image (Attr)) =
                              "low_order_first"
                           then
                              Bit_Order_Low := True;
                           end if;
                        end if;
                     when An_Aspect_Specification =>
                        Asp_Mark := Aspect_Mark (CRC (J));

                        if To_Lower_Case (Name_Image (Asp_Mark)) =
                             "scalar_storage_order"
                        then
                           Scalar_Storage_Order_Clause_Found := True;
                           exit when Rec_Rep_Clause_Found
                                    and then
                                    (Bit_Order_Low or else Bit_Order_High);

                        elsif To_Lower_Case (Name_Image (Asp_Mark)) =
                             "bit_order"
                        then
                           Attr := Aspect_Definition (CRC (J));
                           Attr := Normalize_Reference (Attr);

                           if To_Lower_Case (Name_Image (Attr)) =
                              "high_order_first"
                           then
                              Bit_Order_High := True;
                           elsif To_Lower_Case (Name_Image (Attr)) =
                              "low_order_first"
                           then
                              Bit_Order_Low := True;
                           end if;
                        end if;

                     when others =>
                        null;
                  end case;
               end loop;

               if Rec_Rep_Clause_Found
                 and then
                  not Scalar_Storage_Order_Clause_Found
               then
                  Scalar_Storage_Order_Clause_Found :=
                    Storage_Order_Defined_By_Pragma (Element);
               end if;

               if Rec_Rep_Clause_Found
                 and then
                  not Scalar_Storage_Order_Clause_Found
               then
                  State.Detected  := True;

                  if Bit_Order_Low then
                     State.Diagnosis := 2;
                  elsif Bit_Order_High then
                     State.Diagnosis := 3;
                  else
                     State.Diagnosis := 1;
                  end if;

               end if;
            end;
         end if;
      end if;
   end Rule_Check_Pre_Op;

   ------------------------
   -- Predicate_Testing --
   ------------------------

   -----------------------------------------------
   -- Activate_In_Test_Mode (Predicate_Testing) --
   -----------------------------------------------

   overriding procedure Activate_In_Test_Mode
     (Rule : in out Predicate_Testing_Rule_Type)
   is
   begin
      Process_Rule_Parameter
        (Rule       => Rule,
         Param      => "Except_Assertions",
         Enable     => True,
         Defined_At => "");
   end Activate_In_Test_Mode;

   ----------------------------------------
   -- Exception_Name (Predicate_Testing) --
   ----------------------------------------

   overriding function Exception_Name
     (Rule      : Predicate_Testing_Rule_Type;
      Exc_Index : Exception_Index)
      return      String
   is
      pragma Unreferenced (Rule);
   begin
      case Exc_Index is
         when 1 =>
            return "Except_Assertions";
         when others =>
            return "";
      end case;
   end Exception_Name;

   ------------------------------------------
   -- Exception_Number (Predicate_Testing) --
   ------------------------------------------

   overriding function Exception_Number
     (Rule     : Predicate_Testing_Rule_Type;
      Exc_Name : String)
      return     Exception_Numbers
   is
      pragma Unreferenced (Rule);
      Result : Exception_Numbers := Not_An_Exception;
      Normalized_Exc_Name : constant String := To_Lower (Exc_Name);
   begin
      if Normalized_Exc_Name = "except_assertions" then
         Result := 1;
      end if;

      return Result;
   end Exception_Number;

   -----------------------------------
   -- Init_Rule (Predicate_Testing) --
   -----------------------------------

   overriding procedure Init_Rule
     (Rule : in out Predicate_Testing_Rule_Type)
   is
   begin
      Init_Rule (Rule_With_Exceptions_Template (Rule));

      Rule.Name        := new String'("Predicate_Testing");
      Rule.Rule_Status := Fully_Implemented;
      Rule.Help_Info   := new String'("expressions need subtype predicate"  &
                                      "evaluation");
      Rule.Diagnosis   := new String'("#1#membership choice needs subtype " &
                                      "predicate evaluation"                &
                                      "#2#attribute 'Valid needs subtype "  &
                                      "predicate evaluation");
   end Init_Rule;

   -------------------------------------------
   -- Rule_Check_Pre_Op (Predicate_Testing) --
   -------------------------------------------

   overriding procedure Rule_Check_Pre_Op
     (Rule    : in out Predicate_Testing_Rule_Type;
      Element :        Asis.Element;
      Control : in out Traverse_Control;
      State   : in out Rule_Traversal_State)
   is
      Detected : Boolean := False;
      Diag_Var : Diagnosis_Variant range 1 .. 2;

      Enclosing_El : Asis.Element;
      Steps_Up     : Elmt_Idx;

      pragma Unreferenced (Control);

   begin

      if Expression_Kind (Element) in An_Identifier | A_Selected_Component
        and then
         Expression_Kind (Get_Enclosing_Element) in
           An_In_Membership_Test | A_Not_In_Membership_Test
        and then
           Denotes_Subtype_With_Predicate (Element)
      then
         Detected := True;
         Diag_Var := 1;
      elsif Attribute_Kind (Element) = A_Valid_Attribute
        and then
           From_Subtype_With_Predicate (Prefix (Element))
      then
         Detected := True;
         Diag_Var := 2;
      end if;

      if Detected then
         if Rule.Exceptions (1) then
            Enclosing_El := Get_Enclosing_Element;
            Steps_Up     := 1;

            while Element_Kind (Enclosing_El) not in
               A_Pragma      |
               A_Declaration |
               A_Definition  |
               A_Statement
            loop
               Enclosing_El := Get_Enclosing_Element (Steps_Up);
               Steps_Up     := Steps_Up + 1;
            end loop;

            if not Is_Assertion (Enclosing_El) then
               State.Detected  := True;
            end if;
         else
            State.Detected  := True;
         end if;

         if State.Detected then
            State.Diagnosis := Diag_Var;
         end if;

      end if;
   end Rule_Check_Pre_Op;

   ----------------------------
   -- Quantified_Expressions --
   ----------------------------

   ----------------------------------------------------
   -- Activate_In_Test_Mode (Quantified_Expressions) --
   ----------------------------------------------------

   overriding procedure Activate_In_Test_Mode
     (Rule : in out Quantified_Expressions_Rule_Type)
   is
   begin
      Process_Rule_Parameter
        (Rule       => Rule,
         Param      => "Except_Assertions",
         Enable     => True,
         Defined_At => "");
   end Activate_In_Test_Mode;

   ---------------------------------------------
   -- Exception_Name (Quantified_Expressions) --
   ---------------------------------------------

   overriding function Exception_Name
     (Rule      : Quantified_Expressions_Rule_Type;
      Exc_Index : Exception_Index)
      return      String
   is
      pragma Unreferenced (Rule);
   begin
      case Exc_Index is
         when 1 =>
            return "Except_Assertions";
         when others =>
            return "";
      end case;
   end Exception_Name;

   -----------------------------------------------
   -- Exception_Number (Quantified_Expressions) --
   -----------------------------------------------

   overriding function Exception_Number
     (Rule     : Quantified_Expressions_Rule_Type;
      Exc_Name : String)
      return     Exception_Numbers
   is
      pragma Unreferenced (Rule);
      Result : Exception_Numbers := Not_An_Exception;
      Normalized_Exc_Name : constant String := To_Lower (Exc_Name);
   begin
      if Normalized_Exc_Name = "except_assertions" then
         Result := 1;
      end if;

      return Result;
   end Exception_Number;

   ----------------------------------------
   -- Init_Rule (Quantified_Expressions) --
   ----------------------------------------

   overriding procedure Init_Rule
     (Rule : in out Quantified_Expressions_Rule_Type)
   is
   begin
      Init_Rule (Rule_With_Exceptions_Template (Rule));

      Rule.Name        := new String'("Quantified_Expressions");
      Rule.Rule_Status := Fully_Implemented;
      Rule.Help_Info   := new String'("Ada 2012 quantified expressions");
      Rule.Diagnosis   := new String'("quantified expression");
   end Init_Rule;

   ------------------------------------------------
   -- Rule_Check_Pre_Op (Quantified_Expressions) --
   ------------------------------------------------

   overriding procedure Rule_Check_Pre_Op
     (Rule    : in out Quantified_Expressions_Rule_Type;
      Element :        Asis.Element;
      Control : in out Traverse_Control;
      State   : in out Rule_Traversal_State)
   is
      Enclosing_El : Asis.Element;
      Steps_Up     : Elmt_Idx;

      pragma Unreferenced (Control);
   begin
      if Expression_Kind (Element) in
           A_For_All_Quantified_Expression |
           A_For_Some_Quantified_Expression
      then
         if Rule.Exceptions (1) then
            Enclosing_El := Get_Enclosing_Element;
            Steps_Up     := 1;

            while Element_Kind (Enclosing_El) not in
               A_Pragma      |
               A_Declaration |
               A_Definition  |
               A_Statement
            loop
               Enclosing_El := Get_Enclosing_Element (Steps_Up);
               Steps_Up     := Steps_Up + 1;
            end loop;

            if not Is_Assertion (Enclosing_El) then
               State.Detected  := True;
            end if;
         else
            State.Detected  := True;
         end if;

      end if;

      null;
   end Rule_Check_Pre_Op;

   -------------------------------------
   -- Separate_Numeric_Error_Handlers --
   -------------------------------------

   -------------------------------------------------
   -- Init_Rule (Separate_Numeric_Error_Handlers) --
   -------------------------------------------------

   procedure Init_Rule
     (Rule : in out Separate_Numeric_Error_Handlers_Rule_Type)
   is
   begin
      Init_Rule (Rule_Template (Rule));

      Rule.Name        := new String'("Separate_Numeric_Error_Handlers");
      Rule.Rule_Status := Fully_Implemented;
      Rule.Help_Info   := new String'("Numeric_Error and Constraint error " &
                                      "are not handled together");
      Rule.Diagnosis   := new String'("#1#Numeric_Error is handled "      &
                                       "separately from Constraint_Error" &
                                      "#2#Constraint_Error is handled "   &
                                       "separately from Numeric_Error");
   end Init_Rule;

   ---------------------------------------------------------
   -- Rule_Check_Pre_Op (Separate_Numeric_Error_Handlers) --
   ---------------------------------------------------------

   procedure Rule_Check_Pre_Op
     (Rule    : in out Separate_Numeric_Error_Handlers_Rule_Type;
      Element :        Asis.Element;
      Control : in out Traverse_Control;
      State   : in out Rule_Traversal_State)
   is
      pragma Unreferenced (Rule, Control);
   begin

      if Element_Kind (Element) = An_Exception_Handler then

         declare
            Choices : constant Asis.Element_List :=
              Exception_Choices (Element);

            Next_Choice : Asis.Element;

            Constraint_Error_Present : Boolean := False;
            Numeric_Error_Present    : Boolean := False;

         begin

            for J in Choices'Range loop
               Next_Choice := Normalize_Reference (Choices (J));

               exit when Definition_Kind (Next_Choice) = An_Others_Choice;

               if not Constraint_Error_Present then
                  Constraint_Error_Present :=
                    Is_Constraint_Error (Next_Choice);
               end if;

               if not Numeric_Error_Present then
                  Numeric_Error_Present := Is_Numeric_Error (Next_Choice);
               end if;

               exit when Constraint_Error_Present
                       and then
                         Numeric_Error_Present;
            end loop;

            if Constraint_Error_Present and then not Numeric_Error_Present then
               State.Detected  := True;
               State.Diagnosis := 2;
            elsif Numeric_Error_Present
               and then
                  not Constraint_Error_Present
            then
               State.Detected  := True;
               State.Diagnosis := 1;
            end if;
         end;

      end if;

   end Rule_Check_Pre_Op;

   ----------------------
   -- Too_Many_Parents --
   ----------------------

   ----------------------------------
   -- Init_Rule (Too_Many_Parents) --
   ----------------------------------

   procedure Init_Rule (Rule : in out Too_Many_Parents_Rule_Type) is
   begin
      Init_Rule (One_Integer_Parameter_Rule_Template (Rule));

      Rule.Name        := new String'("Too_Many_Parents");
      Rule.Rule_Status := Fully_Implemented;
      Rule.Help_Info   := new String'("type/object has too many parents");
      Rule.Diagnosis   :=
        new String'("#1#type has too many parents (%1%)" &
                    "#2#task object has too many parents (%1%)" &
                    "#3#protected object has too many parents (%1%)");
   end Init_Rule;

   ------------------------------------------
   -- Rule_Check_Pre_Op (Too_Many_Parents) --
   ------------------------------------------

   procedure Rule_Check_Pre_Op
     (Rule    : in out Too_Many_Parents_Rule_Type;
      Element :        Asis.Element;
      Control : in out Traverse_Control;
      State   : in out Rule_Traversal_State)
   is
      pragma Unreferenced (Control);
   begin

      if May_Have_Interface_List (Element) then

         declare
            Int_List :  constant Asis.Element_List := Interface_List (Element);
         begin

            case Declaration_Kind (Element) is
               when  An_Ordinary_Type_Declaration    |
                     A_Formal_Type_Declaration       |
                     A_Private_Extension_Declaration =>

                  --  In case of an interface type declaration we do not have
                  --  a parent subtype, so we have to handle this case
                  --  separately:
                  if Type_Kind (Type_Declaration_View (Element)) =
                        An_Interface_Type_Definition
                  then
                     State.Detected := Int_List'Length > Rule.Rule_Limit;
                     State.Diagnosis   := 1;
                     State.Diag_Params := Enter_String ("%1%" &
                       ASIS_UL.Misc.Image (Int_List'Length));
                  else
                     State.Detected := Int_List'Length + 1 > Rule.Rule_Limit;
                     State.Diagnosis   := 1;
                     State.Diag_Params := Enter_String ("%1%" &
                       ASIS_UL.Misc.Image (Int_List'Length + 1));
                  end if;

               when A_Task_Type_Declaration        |
                    A_Protected_Type_Declaration   |
                    A_Single_Task_Declaration      |
                    A_Single_Protected_Declaration =>

                  if Int_List'Length > Rule.Rule_Limit then
                     State.Detected    := True;
                     State.Diag_Params := Enter_String ("%1%" &
                       ASIS_UL.Misc.Image (Int_List'Length));

                     if Declaration_Kind (Element) =
                        A_Single_Task_Declaration
                     then
                        State.Diagnosis := 2;
                     elsif Declaration_Kind (Element) =
                           A_Single_Protected_Declaration
                     then
                        State.Diagnosis := 3;
                     else
                        State.Diagnosis := 1;
                     end if;

                  end if;

               when others =>
                  pragma Assert (False);
                  null;
            end case;

         end;

      end if;

   end Rule_Check_Pre_Op;

   -------------------------
   -- Unconditional_Exits --
   -------------------------

   -------------------------------------
   -- Init_Rule (Unconditional_Exits) --
   -------------------------------------

   procedure Init_Rule (Rule : in out Unconditional_Exits_Rule_Type) is
   begin
      Init_Rule (Rule_Template (Rule));

      Rule.Name        := new String'("Unconditional_Exits");
      Rule.Rule_Status := Fully_Implemented;
      Rule.Help_Info   := new String'("Exit statement with no condition");
      Rule.Diagnosis   := new String'("exit statement does not contain " &
                                      "condition");
   end Init_Rule;

   ---------------------------------------------
   -- Rule_Check_Pre_Op (Unconditional_Exits) --
   ---------------------------------------------

   procedure Rule_Check_Pre_Op
     (Rule    : in out Unconditional_Exits_Rule_Type;
      Element :        Asis.Element;
      Control : in out Traverse_Control;
      State   : in out Rule_Traversal_State)
   is
      pragma Unreferenced (Rule, Control);
   begin

      if Statement_Kind (Element) = An_Exit_Statement
        and then
         Is_Nil (Exit_Condition (Element))
      then
         State.Detected  := True;
      end if;

   end Rule_Check_Pre_Op;

   ------------------------
   -- Visible_Components --
   ------------------------

   -----------------------------------------
   -- Exception_Name (Visible_Components) --
   -----------------------------------------

   overriding function Exception_Name
     (Rule      : Visible_Components_Rule_Type;
      Exc_Index : Exception_Index)
      return      String
   is
      pragma Unreferenced (Rule);
   begin
      case Exc_Index is
         when 1 =>
            return "Tagged_Only";
         when others =>
            return "";
      end case;
   end Exception_Name;

   -------------------------------------------
   -- Exception_Number (Visible_Components) --
   -------------------------------------------

   overriding function Exception_Number
     (Rule     : Visible_Components_Rule_Type;
      Exc_Name : String)
      return     Exception_Numbers
   is
      pragma Unreferenced (Rule);
      Result : Exception_Numbers := Not_An_Exception;
      Normalized_Exc_Name : constant String := To_Lower (Exc_Name);
   begin
      if Normalized_Exc_Name = "tagged_only" then
         Result := 1;
      end if;

      return Result;
   end Exception_Number;

   ------------------------------------
   -- Init_Rule (Visible_Components) --
   ------------------------------------

   procedure Init_Rule (Rule : in out Visible_Components_Rule_Type) is
   begin
      Init_Rule (Rule_Template (Rule));

      Rule.Name        := new String'("Visible_Components");
      Rule.Rule_Status := Fully_Implemented;
      Rule.Help_Info   := new String'("Types with publicly accessible " &
                                      "components");
      Rule.Diagnosis   := new String'("type defines publicly accessible " &
                                      "components");
   end Init_Rule;

   --------------------------------------------
   -- Rule_Check_Pre_Op (Visible_Components) --
   --------------------------------------------

   procedure Rule_Check_Pre_Op
     (Rule    : in out Visible_Components_Rule_Type;
      Element :        Asis.Element;
      Control : in out Traverse_Control;
      State   : in out Rule_Traversal_State)
   is
      pragma Unreferenced (Control);
   begin

      if Defines_Components (Element)
        and then
         Is_Publically_Accessible (Element)
      then
         State.Detected := not Rule.Exceptions (1) or else Is_Tagged (Element);
      end if;

   end Rule_Check_Pre_Op;

end Gnatcheck.Rules.Custom_2;
