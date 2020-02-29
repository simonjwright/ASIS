------------------------------------------------------------------------------
--                                                                          --
--                          GNATCHECK COMPONENTS                            --
--                                                                          --
--             G N A T C H E C K . R U L E S . C U S T O M _ 4              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                    Copyright (C) 2018-2019, AdaCore                      --
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

with Ada.Characters.Handling;    use Ada.Characters.Handling;

with Asis.Compilation_Units;     use Asis.Compilation_Units;
with Asis.Clauses;               use Asis.Clauses;
with Asis.Declarations;          use Asis.Declarations;
with Asis.Definitions;           use Asis.Definitions;
with Asis.Elements;              use Asis.Elements;
with Asis.Expressions;           use Asis.Expressions;
with Asis.Extensions;            use Asis.Extensions;
with Asis.Extensions.Flat_Kinds; use Asis.Extensions.Flat_Kinds;
with Asis.Statements;            use Asis.Statements;
with Asis.Text;                  use Asis.Text;

with ASIS_UL.Misc;               use ASIS_UL.Misc;

with Gnatcheck.Traversal_Stack;  use Gnatcheck.Traversal_Stack;
with Gnatcheck.ASIS_Utilities;   use Gnatcheck.ASIS_Utilities;

package body Gnatcheck.Rules.Custom_4 is

   -----------------------
   --  Abort_Statements --
   -----------------------

   ----------------------------------
   -- Init_Rule (Abort_Statements) --
   ----------------------------------

   procedure Init_Rule (Rule : in out Abort_Statements_Rule_Type) is
   begin
      Init_Rule (Rule_Template (Rule));

      Rule.Name        := new String'("Abort_Statements");
      Rule.Rule_Status := Fully_Implemented;
      Rule.Help_Info   := new String'("abort statements");
      Rule.Diagnosis   := new String'("abort statement");
   end Init_Rule;

   ----------------------------------------------
   -- Rule_Check_Pre_Op (Abort_Statements) --
   ----------------------------------------------

   procedure Rule_Check_Pre_Op
     (Rule    : in out Abort_Statements_Rule_Type;
      Element :        Asis.Element;
      Control : in out Traverse_Control;
      State   : in out Rule_Traversal_State)
   is
      pragma Unreferenced (Rule, Control);
   begin
      State.Detected := Statement_Kind (Element) = An_Abort_Statement;
   end Rule_Check_Pre_Op;

   ----------------------------------------------------
   -- Address_Specifications_For_Initialized_Objects --
   ----------------------------------------------------

   ----------------------------------------------------------------
   -- Init_Rule (Address_Specifications_For_Initialized_Objects) --
   ----------------------------------------------------------------

   procedure Init_Rule
     (Rule : in out Address_Specifications_For_Initialized_Objects_Rule_Type)
   is
   begin
      Init_Rule (Rule_Template (Rule));

      Rule.Name :=
        new String'("Address_Specifications_For_Initialized_Objects");
      Rule.Rule_Status := Fully_Implemented;
      Rule.Help_Info   := new String'("address specifications for " &
                                      "initialized objects");
      Rule.Diagnosis   := new String'("address specifications for " &
                                      "initialized object");
   end Init_Rule;

   ------------------------------------------------------------------------
   -- Rule_Check_Pre_Op (Address_Specifications_For_Initialized_Objects) --
   ------------------------------------------------------------------------

   procedure Rule_Check_Pre_Op
     (Rule    : in out
        Address_Specifications_For_Initialized_Objects_Rule_Type;
      Element :        Asis.Element;
      Control : in out Traverse_Control;
      State   : in out Rule_Traversal_State)
   is
      Obj_Declaration : Asis.Element;

      pragma Unreferenced (Rule, Control);
   begin

      if Is_Object_Address_Specification (Element) then

         if Definition_Kind (Element) = An_Aspect_Specification then
            Obj_Declaration := Get_Enclosing_Element;
         else
            Obj_Declaration := Entity_From_Rep_Item (Element);
         end if;

         State.Detected :=
           not Is_Nil (Initialization_Expression (Obj_Declaration));
      end if;

   end Rule_Check_Pre_Op;

   ----------------------------------------------
   -- Address_Specifications_For_Local_Objects --
   ----------------------------------------------

   ----------------------------------------------------------
   -- Init_Rule (Address_Specifications_For_Local_Objects) --
   ----------------------------------------------------------

   procedure Init_Rule
     (Rule : in out Address_Specifications_For_Local_Objects_Rule_Type)
   is
   begin
      Init_Rule (Rule_Template (Rule));

      Rule.Name :=
        new String'("Address_Specifications_For_Local_Objects");
      Rule.Rule_Status := Fully_Implemented;
      Rule.Help_Info   := new String'("address specifications for " &
                                      "local objects");
      Rule.Diagnosis   := new String'("address specifications for " &
                                      "local object");
   end Init_Rule;

   ------------------------------------------------------------------
   -- Rule_Check_Pre_Op (Address_Specifications_For_Local_Objects) --
   ------------------------------------------------------------------

   procedure Rule_Check_Pre_Op
     (Rule    : in out Address_Specifications_For_Local_Objects_Rule_Type;
      Element :        Asis.Element;
      Control : in out Traverse_Control;
      State   : in out Rule_Traversal_State)
   is
      Encl_Body : Asis.Element;
      Steps_Up  : Elmt_Idx := 0;

      pragma Unreferenced (Rule, Control);
   begin

      if Is_Object_Address_Specification (Element) then

         Encl_Body := Get_Enclosing_Element (Steps_Up);

         while not (Is_Nil (Encl_Body)
                   or else
                     Declaration_Kind (Encl_Body) in
                       A_Procedure_Body_Declaration |
                       A_Function_Body_Declaration)
          loop
            Steps_Up  := Steps_Up + 1;
            Encl_Body := Get_Enclosing_Element (Steps_Up);
         end loop;

         if Declaration_Kind (Encl_Body) in A_Procedure_Body_Declaration |
                                             A_Function_Body_Declaration
         then
            State.Detected :=
               Is_Subunit (Encl_Body)
             or else
               not Is_Nil (Get_Enclosing_Element (Steps_Up + 1));
         end if;

      end if;

   end Rule_Check_Pre_Op;

   -------------------------------------------
   -- Bit_Records_Without_Layout_Definition --
   -------------------------------------------

   -------------------------------------------------------
   -- Init_Rule (Bit_Records_Without_Layout_Definition) --
   -------------------------------------------------------

   procedure Init_Rule
     (Rule : in out Bit_Records_Without_Layout_Definition_Rule_Type)
   is
   begin
      Init_Rule (Rule_Template (Rule));

      Rule.Name        := new String'("Bit_Records_Without_Layout_Definition");
      Rule.Rule_Status := Fully_Implemented;
      Rule.Help_Info   := new String'("record with modular component " &
                                      "without representation clause");
      Rule.Diagnosis   := new String'("bit record without layout definition");
   end Init_Rule;

   ---------------------------------------------------------------
   -- Rule_Check_Pre_Op (Bit_Records_Without_Layout_Definition) --
   ---------------------------------------------------------------

   procedure Rule_Check_Pre_Op
     (Rule    : in out Bit_Records_Without_Layout_Definition_Rule_Type;
      Element :        Asis.Element;
      Control : in out Traverse_Control;
      State   : in out Rule_Traversal_State)
   is
      pragma Unreferenced (Rule, Control);
      Rec_Rep_Clause_Not_Found : Boolean := True;
   begin
      if Declaration_Kind (Element) = An_Ordinary_Type_Declaration
         and then
            Contains_Modular_Component (Element)
      then
         declare
            Rep_Cls : constant Asis.Element_List :=
              Corresponding_Representation_Clauses (Element);
         begin
            for J in Rep_Cls'Range loop
               if Representation_Clause_Kind (Rep_Cls (J)) =
                    A_Record_Representation_Clause
               then
                  Rec_Rep_Clause_Not_Found := False;
                  exit;
               end if;
            end loop;
         end;

         State.Detected := Rec_Rep_Clause_Not_Found;

      end if;
   end Rule_Check_Pre_Op;

   ----------------------------------------------
   -- Incomplete_Representation_Specifications --
   ----------------------------------------------

   ----------------------------------------------------------
   -- Init_Rule (Incomplete_Representation_Specifications) --
   ----------------------------------------------------------

   procedure Init_Rule
     (Rule : in out Incomplete_Representation_Specifications_Rule_Type)
   is
   begin
      Init_Rule (Rule_Template (Rule));

      Rule.Name := new String'("Incomplete_Representation_Specifications");
      Rule.Rule_Status := Fully_Implemented;
      Rule.Help_Info   := new String'("record types with layout "           &
                                      "specification but without size and " &
                                      "pack specification");
      Rule.Diagnosis   := new String'("record type with incomplete "  &
                                      "representation specification " &
                                      "(%1% missing)");
   end Init_Rule;

   ------------------------------------------------------------------
   -- Rule_Check_Pre_Op (Incomplete_Representation_Specifications) --
   ------------------------------------------------------------------

   procedure Rule_Check_Pre_Op
     (Rule    : in out Incomplete_Representation_Specifications_Rule_Type;
      Element :        Asis.Element;
      Control : in out Traverse_Control;
      State   : in out Rule_Traversal_State)
   is
      pragma Unreferenced (Rule, Control);
      Layout_Clause_Present   : Boolean := False;
      Size_Definition_Present : Boolean := False;
      Pack_Present            : Boolean := False;
      Tmp                     : Asis.Element;
   begin
      if Declaration_Kind (Element) = An_Ordinary_Type_Declaration
         and then
            Type_Kind (Type_Declaration_View (Element)) =
              A_Record_Type_Definition
      then
         declare
            Rep_Items : constant Asis.Element_List :=
              Corresponding_Representation_Items (First_Name (Element));
         begin

            for J in Rep_Items'Range loop

               case Flat_Element_Kind (Rep_Items (J)) is
                  when A_Record_Representation_Clause =>
                     Layout_Clause_Present := True;
                  when An_Attribute_Definition_Clause =>
                     Tmp := Representation_Clause_Name (Rep_Items (J));
                     Tmp := Attribute_Designator_Identifier (Tmp);

                     if To_Lower_Case (Asis.Expressions.Name_Image (Tmp)) =
                           "size"
                     then
                        Size_Definition_Present := True;
                     end if;

                  when A_Pack_Pragma =>
                     Pack_Present := True;
                  when others =>
                     null;
               end case;

               exit when Size_Definition_Present and then Pack_Present;

            end loop;
         end;

         if not Layout_Clause_Present
             or else
                (Size_Definition_Present and then Pack_Present)
         then
            return;
         end if;

         --  The last thing to do is to check aspect definitions in Element
         declare
            Asp_Specs : constant Asis.Element_List :=
              Aspect_Specifications (Element);
         begin

            for J in Asp_Specs'Range loop
               Tmp := Aspect_Mark (Asp_Specs (J));

               if To_Lower_Case (Asis.Expressions.Name_Image (Tmp)) =
                           "size"
               then
                  Size_Definition_Present := True;
               elsif To_Lower_Case (Asis.Expressions.Name_Image (Tmp)) =
                           "pack"
               then
                  Pack_Present := True;
               end if;

               exit when Size_Definition_Present and then Pack_Present;
            end loop;

         end;

         State.Detected := not (Size_Definition_Present and then Pack_Present);
         State.Diag_Params :=
           Enter_String ("%1%" &
                         (if not Size_Definition_Present
                           and then
                             not  Pack_Present
                          then
                             "Pack and Size are"
                          elsif not Size_Definition_Present then
                             "Size is"
                          else
                             "Pack is"));

      end if;
   end Rule_Check_Pre_Op;

   -----------------------
   -- Local_USE_Clauses --
   -----------------------

   ----------------------------------------
   -- Exception_Name (Local_USE_Clauses) --
   ----------------------------------------

   overriding function Exception_Name
     (Rule      : Local_USE_Clauses_Rule_Type;
      Exc_Index : Exception_Index)
      return      String
   is
      pragma Unreferenced (Rule);
   begin
      case Exc_Index is
         when 1 =>
            return "Except_USE_TYPE_Clauses";
         when others =>
            return "";
      end case;
   end Exception_Name;

   ------------------------------------------
   -- Exception_Number (Local_USE_Clauses) --
   ------------------------------------------

   overriding function Exception_Number
     (Rule     : Local_USE_Clauses_Rule_Type;
      Exc_Name : String)
      return     Exception_Numbers
   is
      pragma Unreferenced (Rule);
      Result : Exception_Numbers := Not_An_Exception;
      Normalized_Exc_Name : constant String := To_Lower (Exc_Name);
   begin
      if Normalized_Exc_Name = "except_use_type_clauses" then
         Result := 1;
      end if;

      return Result;
   end Exception_Number;

   -----------------------------------
   -- Init_Rule (Local_USE_Clauses) --
   -----------------------------------

   overriding procedure Init_Rule
     (Rule : in out Local_USE_Clauses_Rule_Type)
   is
   begin
      Init_Rule (Rule_With_Exceptions_Template (Rule));

      Rule.Name        := new String'("Local_USE_Clauses");
      Rule.Rule_Status := Fully_Implemented;
      Rule.Help_Info   := new String'("local use clauses");
      Rule.Diagnosis   := new String'("local use clause");
   end Init_Rule;

   -------------------------------------------
   -- Rule_Check_Pre_Op (Local_USE_Clauses) --
   -------------------------------------------

   overriding procedure Rule_Check_Pre_Op
     (Rule    : in out Local_USE_Clauses_Rule_Type;
      Element :        Asis.Element;
      Control : in out Traverse_Control;
      State   : in out Rule_Traversal_State)
   is
      Arg_Kind : constant Clause_Kinds := Clause_Kind (Element);

      pragma Unreferenced (Control);
   begin

      if Arg_Kind in
           A_Use_Package_Clause |
           A_Use_Type_Clause    |
           A_Use_All_Type_Clause
        and then
           not Is_Nil (Get_Enclosing_Element)
      then
         State.Detected :=
           Arg_Kind = A_Use_Package_Clause
         or else
           not Rule.Exceptions (1);
      end if;

   end Rule_Check_Pre_Op;

   ----------------------------
   --  Max_Identifier_Length --
   ----------------------------

   ---------------------------------------
   -- Init_Rule (Max_Identifier_Length) --
   ---------------------------------------

   procedure Init_Rule (Rule : in out Max_Identifier_Length_Rule_Type) is
   begin
      Init_Rule (One_Integer_Parameter_Rule_Template (Rule));

      Rule.Name        := new String'("Max_Identifier_Length");
      Rule.Rule_Status := Fully_Implemented;
      Rule.Help_Info   := new String'("maximal identifier length");
      Rule.Diagnosis   := new String'("identifier too long");
   end Init_Rule;

   -----------------------------------------------
   -- Rule_Check_Pre_Op (Max_Identifier_Length) --
   -----------------------------------------------

   procedure Rule_Check_Pre_Op
     (Rule    : in out Max_Identifier_Length_Rule_Type;
      Element :        Asis.Element;
      Control : in out Traverse_Control;
      State   : in out Rule_Traversal_State)
   is
      pragma Unreferenced (Control);
   begin
      State.Detected :=
        Defining_Name_Kind (Element) = A_Defining_Identifier
       and then
        Defining_Name_Image (Element)'Length > Rule.Rule_Limit;
   end Rule_Check_Pre_Op;

   ------------------------------------
   -- Misplaced_Representation_Items --
   ------------------------------------

   ------------------------------------------------
   -- Init_Rule (Misplaced_Representation_Items) --
   ------------------------------------------------

   procedure Init_Rule
     (Rule : in out Misplaced_Representation_Items_Rule_Type)
   is
   begin
      Init_Rule (Rule_Template (Rule));

      Rule.Name        := new String'("Misplaced_Representation_Items");
      Rule.Rule_Status := Fully_Implemented;
      Rule.Help_Info   := new String'("representation items out of order");
      Rule.Diagnosis   := new String'("misplaced representation item");
   end Init_Rule;

   --------------------------------------------------------
   -- Rule_Check_Pre_Op (Misplaced_Representation_Items) --
   --------------------------------------------------------

   procedure Rule_Check_Pre_Op
     (Rule    : in out Misplaced_Representation_Items_Rule_Type;
      Element :        Asis.Element;
      Control : in out Traverse_Control;
      State   : in out Rule_Traversal_State)
   is
      pragma Unreferenced (Rule, Control);
   begin

      if not Is_Representation_Item (Element) then
         return;
      end if;

      declare
         Encl_List    : constant Element_List := Enclosing_List;

         Rep_Item_Idx : constant ASIS_Natural :=
           Pos_In_List (Element, Encl_List);

         Dcl        : constant Asis.Element      :=
           Entity_From_Rep_Item (Element);

         Ent_Idx    : constant ASIS_Natural :=
           Pos_In_List (Dcl, Encl_List);

      begin
         pragma Assert (Rep_Item_Idx /= 0);
         pragma Assert (Ent_Idx /= 0);

         for J in Ent_Idx + 1 .. Rep_Item_Idx - 1 loop
            if Clause_Kind (Encl_List (J)) /= A_Representation_Clause
              or else
               not Is_Equal (Dcl, (Entity_From_Rep_Item (Encl_List (J))))
            then
               State.Detected := True;
               exit;
            end if;
         end loop;
      end;

   end Rule_Check_Pre_Op;

   -----------------------------
   --  No_Explicit_Real_Range --
   -----------------------------

   ----------------------------------------
   -- Init_Rule (No_Explicit_Real_Range) --
   ----------------------------------------

   procedure Init_Rule (Rule : in out No_Explicit_Real_Range_Rule_Type) is
   begin
      Init_Rule (Rule_Template (Rule));

      Rule.Name        := new String'("No_Explicit_Real_Range");
      Rule.Rule_Status := Fully_Implemented;
      Rule.Help_Info   := new String'("real types with no range definition");
      Rule.Diagnosis   := new String'("real type with no range definition");
   end Init_Rule;

   ------------------------------------------------
   -- Rule_Check_Pre_Op (No_Explicit_Real_Range) --
   ------------------------------------------------

   procedure Rule_Check_Pre_Op
     (Rule    : in out No_Explicit_Real_Range_Rule_Type;
      Element :        Asis.Element;
      Control : in out Traverse_Control;
      State   : in out Rule_Traversal_State)
   is
      pragma Unreferenced (Rule, Control);
   begin

      if not Needs_Real_Range_Definition (Element) then
         return;
      end if;

      State.Detected := not Has_Range_Specification (Element);

   end Rule_Check_Pre_Op;

   --------------------------
   --  Number_Declarations --
   --------------------------

   -------------------------------------
   -- Init_Rule (Number_Declarations) --
   -------------------------------------

   procedure Init_Rule (Rule : in out Number_Declarations_Rule_Type) is
   begin
      Init_Rule (Rule_Template (Rule));

      Rule.Name        := new String'("Number_Declarations");
      Rule.Rule_Status := Fully_Implemented;
      Rule.Help_Info   := new String'("number declarations");
      Rule.Diagnosis   := new String'("number declaration");
   end Init_Rule;

   ---------------------------------------------
   -- Rule_Check_Pre_Op (Number_Declarations) --
   ---------------------------------------------

   procedure Rule_Check_Pre_Op
     (Rule    : in out Number_Declarations_Rule_Type;
      Element :        Asis.Element;
      Control : in out Traverse_Control;
      State   : in out Rule_Traversal_State)
   is
      pragma Unreferenced (Rule, Control);
   begin
      State.Detected := Declaration_Kind (Element) in A_Number_Declaration;
   end Rule_Check_Pre_Op;

   --------------------------------------
   -- Object_Declarations_Out_Of_Order --
   --------------------------------------

   --------------------------------------------------
   -- Init_Rule (Object_Declarations_Out_Of_Order) --
   --------------------------------------------------

   procedure Init_Rule
     (Rule : in out Object_Declarations_Out_Of_Order_Rule_Type)
   is
   begin
      Init_Rule (Rule_Template (Rule));

      Rule.Name        := new String'("Object_Declarations_Out_Of_Order");
      Rule.Rule_Status := Fully_Implemented;
      Rule.Help_Info   := new String'("object declarations should precede " &
                                      "program unit declarations");
      Rule.Diagnosis   := new String'("object declaration after program " &
                                      "unit declaration (%1%)");
   end Init_Rule;

   ----------------------------------------------------------
   -- Rule_Check_Pre_Op (Object_Declarations_Out_Of_Order) --
   -----------------------------------------------------------

   procedure Rule_Check_Pre_Op
     (Rule    : in out Object_Declarations_Out_Of_Order_Rule_Type;
      Element :        Asis.Element;
      Control : in out Traverse_Control;
      State   : in out Rule_Traversal_State)
   is
      pragma Unreferenced (Rule, Control);
      Arg_Kind : constant Flat_Element_Kinds := Flat_Element_Kind (Element);
      CU       : constant Asis.Compilation_Unit :=
        Enclosing_Compilation_Unit (Element);
   begin

      if Unit_Kind (CU) in
           A_Procedure_Body |
           A_Function_Body  |
           A_Package_Body
        and then
         Arg_Kind in
           A_Variable_Declaration |
           A_Constant_Declaration
        and then
          Is_Nil (Get_Enclosing_Element (Steps_Up => 1))
      then
         declare
            Encl_List : constant Element_List := Enclosing_List;
            Idx       : constant ASIS_Natural :=
              Pos_In_List (Element, Encl_List);
         begin
            for J in Encl_List'First .. Idx - 1 loop
               if Declaration_Kind (Encl_List (J)) in
                    A_Task_Type_Declaration         |
                    A_Protected_Type_Declaration    |
                    A_Single_Task_Declaration       |
                    A_Single_Protected_Declaration  |
                    A_Procedure_Declaration         |
                    A_Function_Declaration          |
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
                    A_Procedure_Instantiation       |
                    A_Function_Instantiation
               then
                  State.Detected := True;
                  State.Diag_Params :=
                    Enter_String ("%1%" & Build_GNAT_Location (Encl_List (J)));
               end if;
            end loop;
         end;
      end if;

   end Rule_Check_Pre_Op;

   -----------------------------
   --  One_Construct_Per_Line --
   -----------------------------

   ----------------------------------------
   -- Init_Rule (One_Construct_Per_Line) --
   ----------------------------------------

   procedure Init_Rule (Rule : in out One_Construct_Per_Line_Rule_Type) is
   begin
      Init_Rule (Rule_Template (Rule));

      Rule.Name        := new String'("One_Construct_Per_Line");
      Rule.Rule_Status := Fully_Implemented;
      Rule.Help_Info   := new String'("each statement or declaration " &
                                      "should use separate code line");
      Rule.Diagnosis   := new String'("#1#statement is not a single "    &
                                      "construct on a code line"         &
                                      "#2#declaration is not a single "  &
                                      "construct on a code line"         &
                                      "#3#representation clause is not " &
                                      "a single construct on a code line");
   end Init_Rule;

   ------------------------------------------------
   -- Rule_Check_Pre_Op (One_Construct_Per_Line) --
   ------------------------------------------------

   procedure Rule_Check_Pre_Op
     (Rule    : in out One_Construct_Per_Line_Rule_Type;
      Element :        Asis.Element;
      Control : in out Traverse_Control;
      State   : in out Rule_Traversal_State)
   is
      Arg_Kind : constant Flat_Element_Kinds := Flat_Element_Kind (Element);
      Diag     :           Diagnosis_Variant;
      pragma Unreferenced (Rule, Control);
   begin
      if Is_Nil (Get_Enclosing_Element) then
         --  Library-level declaration
         return;
      end if;

      case Arg_Kind is
         when Flat_Statement_Kinds =>
            Diag := 1;
         when Flat_Declaration_Kinds =>
            if Arg_Kind in
              An_Enumeration_Literal_Specification |
              A_Discriminant_Specification         |
              A_Parameter_Specification            |
              A_Loop_Parameter_Specification       |
              An_Entry_Index_Specification         |
              A_Choice_Parameter_Specification
            then
               return;
            end if;

            Diag := 2;
         when Flat_Clause_Kinds =>
            if Arg_Kind = An_At_Clause then
               return;
            end if;

            Diag := 3;
         when others =>
            return;
      end case;

      --  If we are here, we should check the rule.

      declare
         Encl_List : constant Element_List := Enclosing_List;
         Idx       : constant ASIS_Natural := Pos_In_List (Element, Encl_List);
         Arg_Span  : constant Span         := Element_Span (Element);
         Prev_Span :          Span;
         pragma Assert (not Is_Nil (Encl_List));
         pragma Assert (Idx /= 0);
      begin
         if Idx > 1 then
            --  Check if there is another construct on the same line
            Prev_Span := Element_Span (Encl_List (Idx - 1));

            if Prev_Span.Last_Line = Arg_Span.First_Line then
               State.Detected  := True;
               State.Diagnosis := Diag;
            end if;
         end if;

         if not State.Detected then
            --  Check cases like:
            --
            --     begin I := I + 1;
            --
            --  or
            --
            --     I := I + 1; end

            if Idx = 1 then
               declare
                  Line_Num : constant Line_Number :=
                    First_Line_Number (Element);
                  Line_Img : constant Program_Text :=
                    Line_Image
                      (Lines (Element, Line_Num, Line_Num) (Line_Num));
               begin
                  for J in reverse Line_Img'First .. Arg_Span.First_Column - 1
                  loop
                     if not Is_White_Space (Line_Img (J)) then
                        State.Detected  := True;
                        State.Diagnosis := Diag;
                        exit;
                     end if;
                  end loop;
               end;
            end if;

            if not State.Detected
              and then
               Idx = Encl_List'Last
            then
               declare
                  Line_Num : constant Line_Number :=
                    Last_Line_Number (Element);
                  Line_Img : constant Program_Text :=
                    Non_Comment_Image
                      (Lines (Element, Line_Num, Line_Num) (Line_Num));
               begin
                  for J in Arg_Span.Last_Column + 1 .. Line_Img'Last loop
                     if not Is_White_Space (Line_Img (J)) then
                        State.Detected  := True;
                        State.Diagnosis := Diag;
                        exit;
                     end if;
                  end loop;
               end;
            end if;

         end if;
      end;

   end Rule_Check_Pre_Op;

   -------------------------------------
   --  Outbound_Protected_Assignments --
   -------------------------------------

   ------------------------------------------------
   -- Init_Rule (Outbound_Protected_Assignments) --
   ------------------------------------------------

   procedure Init_Rule
     (Rule : in out Outbound_Protected_Assignments_Rule_Type)
   is
   begin
      Init_Rule (Rule_Template (Rule));

      Rule.Name        := new String'("Outbound_Protected_Assignments");
      Rule.Rule_Status := Fully_Implemented;
      Rule.Help_Info   := new String'("assignments from protected bodies" &
                                      "to outside objects");
      Rule.Diagnosis   := new String'("assignment from protected body" &
                                      "to outside object");
   end Init_Rule;

   --------------------------------------------------------
   -- Rule_Check_Pre_Op (Outbound_Protected_Assignments) --
   --------------------------------------------------------

   procedure Rule_Check_Pre_Op
     (Rule    : in out Outbound_Protected_Assignments_Rule_Type;
      Element :        Asis.Element;
      Control : in out Traverse_Control;
      State   : in out Rule_Traversal_State)
   is
      Encl_Protected_Body : Asis.Element;
      Obj_Decl            : Asis.Element;
      pragma Unreferenced (Rule, Control);
   begin
      if Statement_Kind (Element) = An_Assignment_Statement then
         Encl_Protected_Body := Get_Encl_Protected_Body;

         if not Is_Nil (Encl_Protected_Body) then
            Obj_Decl := Get_Obj_Dcl (Assignment_Variable_Name (Element));

            if not Is_Nil (Obj_Decl) then
               State.Detected := not Is_Local (Obj_Decl, Encl_Protected_Body);
            end if;
         end if;

      end if;

   end Rule_Check_Pre_Op;

   --------------------------------
   --  Relative_Delay_Statements --
   --------------------------------

   -------------------------------------------
   -- Init_Rule (Relative_Delay_Statements) --
   -------------------------------------------

   procedure Init_Rule (Rule : in out Relative_Delay_Statements_Rule_Type) is
   begin
      Init_Rule (Rule_Template (Rule));

      Rule.Name        := new String'("Relative_Delay_Statements");
      Rule.Rule_Status := Fully_Implemented;
      Rule.Help_Info   := new String'("relative delay statements");
      Rule.Diagnosis   := new String'("relative delay statement");
   end Init_Rule;

   ---------------------------------------------------
   -- Rule_Check_Pre_Op (Relative_Delay_Statements) --
   ---------------------------------------------------

   procedure Rule_Check_Pre_Op
     (Rule    : in out Relative_Delay_Statements_Rule_Type;
      Element :        Asis.Element;
      Control : in out Traverse_Control;
      State   : in out Rule_Traversal_State)
   is
      pragma Unreferenced (Rule, Control);
   begin
      State.Detected := Statement_Kind (Element) = A_Delay_Relative_Statement;
   end Rule_Check_Pre_Op;

   -------------------------------------
   --  Single_Value_Enumeration_Types --
   -------------------------------------

   ------------------------------------------------
   -- Init_Rule (Single_Value_Enumeration_Types) --
   ------------------------------------------------

   procedure Init_Rule
     (Rule : in out Single_Value_Enumeration_Types_Rule_Type)
   is
   begin
      Init_Rule (Rule_Template (Rule));

      Rule.Name        := new String'("Single_Value_Enumeration_Types");
      Rule.Rule_Status := Fully_Implemented;
      Rule.Help_Info   := new String'("enumeration type definitions with " &
                                      "a single enumeration literal");
      Rule.Diagnosis   := new String'("enumeration type definition with " &
                                      "a single enumeration literal");
   end Init_Rule;

   --------------------------------------------------------
   -- Rule_Check_Pre_Op (Single_Value_Enumeration_Types) --
   --------------------------------------------------------

   procedure Rule_Check_Pre_Op
     (Rule    : in out Single_Value_Enumeration_Types_Rule_Type;
      Element :        Asis.Element;
      Control : in out Traverse_Control;
      State   : in out Rule_Traversal_State)
   is
      pragma Unreferenced (Rule, Control);
   begin
      State.Detected :=
        Type_Kind (Element) = An_Enumeration_Type_Definition
       and then
        Enumeration_Literal_Declarations (Element)'Length = 1;
   end Rule_Check_Pre_Op;

   --------------------------
   -- Unconstrained_Arrays --
   --------------------------

   --------------------------------------
   -- Init_Rule (Unconstrained_Arrays) --
   --------------------------------------

   procedure Init_Rule (Rule : in out Unconstrained_Arrays_Rule_Type) is
   begin
      Init_Rule (Rule_Template (Rule));

      Rule.Name        := new String'("Unconstrained_Arrays");
      Rule.Rule_Status := Fully_Implemented;
      Rule.Help_Info   := new String'("unconstrained array definitions");
      Rule.Diagnosis   := new String'("unconstrained array definition");
   end Init_Rule;

   ----------------------------------------------
   -- Rule_Check_Pre_Op (Unconstrained_Arrays) --
   ----------------------------------------------

   procedure Rule_Check_Pre_Op
     (Rule    : in out Unconstrained_Arrays_Rule_Type;
      Element :        Asis.Element;
      Control : in out Traverse_Control;
      State   : in out Rule_Traversal_State)
   is
      pragma Unreferenced (Rule, Control);
   begin
      State.Detected :=
        Type_Kind (Element) = An_Unconstrained_Array_Definition;
   end Rule_Check_Pre_Op;

end Gnatcheck.Rules.Custom_4;
