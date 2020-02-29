------------------------------------------------------------------------------
--                                                                          --
--                          GNATCHECK COMPONENTS                            --
--                                                                          --
--             G N A T C H E C K . R U L E S . C U S T O M _ 1              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                     Copyright (C) 2006-2019, AdaCore                     --
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

with Ada.Characters.Handling;     use Ada.Characters.Handling;
with Ada.Strings;                 use Ada.Strings;
with Ada.Strings.Fixed;           use Ada.Strings.Fixed;
with Ada.Strings.Wide_Fixed;      use Ada.Strings.Wide_Fixed;
with Ada.Text_IO;                 use Ada.Text_IO;
with Ada.Wide_Characters.Unicode;

with Asis.Clauses;                use Asis.Clauses;
with Asis.Compilation_Units;      use Asis.Compilation_Units;
with Asis.Declarations;           use Asis.Declarations;
with Asis.Definitions;            use Asis.Definitions;
with Asis.Elements;               use Asis.Elements;
with Asis.Expressions;            use Asis.Expressions;
with Asis.Extensions.Flat_Kinds;  use Asis.Extensions.Flat_Kinds;
with Asis.Extensions;             use Asis.Extensions;
with Asis.Iterator;
with Asis.Statements;             use Asis.Statements;
with Asis.Text;                   use Asis.Text;

with Namet;
with Snames;
with GNAT.Table;

with ASIS_UL.Misc;                use ASIS_UL.Misc;
with ASIS_UL.Output;              use ASIS_UL.Output;
with ASIS_UL.Utilities;           use ASIS_UL.Utilities;

with Gnatcheck.ASIS_Utilities;    use Gnatcheck.ASIS_Utilities;
with Gnatcheck.Traversal_Stack;   use Gnatcheck.Traversal_Stack;

package body Gnatcheck.Rules.Custom_1 is

   -------------------------------------
   -- General-purpose local functions --
   -------------------------------------

   -------------------------------------
   -- Rule parameter parsing routines --
   -------------------------------------

   procedure Parse_Par
     (First_Par_Id : out Natural;
      Last_Par_Id  : out Positive;
      First_Str_Id : out Natural;
      Last_Str_Id  : out Positive;
      Par_String   :     String);
   --  This function parses its Par_String parameter that is supposed to be a
   --  slice of the rule parameter obtained by
   --  Gnatcheck.Rules.Rule_Table.Process_Rule_Option (see also the
   --  documentation of Process_Rule_Parameter for Rule_Template type in
   --  Gnatcheck.Rules). If Par_String  contains a '=' character, it sets
   --  First_Par_Id and Last_Par_Id to point to the part of Par_String that
   --  precedes the (leftmost) '=' character (cutting out the leading and
   --  trailing white spaces if any), and First_Str_Id and Last_Str_Id are
   --  set to point to the part of Par_String that follows the (leftmost) '='
   --  character (cutting out the leading and trailing white spaces if any).
   --  If Par_String does not contain a '=' character, First_Str_Id is set to
   --  0 (Last_Str_Id is undefined), and First_Par_Id and Last_Par_Id point to
   --  the leftmost and rightmost non-blank characters of Par_String. If
   --  Par_String does not contain any non-blank character, First_Par_Id and
   --  First_Str_Id are set to 0, Last_Par_Id and Last_Str_Id are indefinite.
   --  If Par_String has '=' as it first (non-blank) character (most probably
   --  this means a bug in the parameter structure), First_Par_Id is set to 0,
   --  Last_Par_Id is indefinite, Last_Str_Id and Par_String are set to point
   --  to (non-blank) part of Par_String after '='.

   -------------------------------------
   -- Rule-specific local subprograms --
   -------------------------------------

   function Is_Access_Suffix (S : String) return Boolean;
   --  For Identifier_Suffixes rule.
   --  S is supposed to be the 'string' part from the +R parameter option, and
   --  it is known that Is_Identifier_Suffix (S) = False. The function checks
   --  if S has the structure Suffix1 (Suffix2), where
   --  Is_Identifier_Suffix (Suffix1) AND Is_Identifier_Suffix (Suffix2) = True

   function Is_Access_To_Access
     (Def  : Asis.Element;
      Arg  : Asis.Element)
      return Boolean;
   function Is_Access_To_Class (Def : Asis.Element) return Boolean;
   --  For Identifier_Prefixes and Identifier_Suffixes rules.
   --  Def is supposed to be of An_Access_Type_Definition kind. Checks if it
   --  defines an access to access type/access to class-wide type.
   --  In case of checking for access-to-access type we need a second argument
   --  to represent the place of the check. Consider:
   --
   --  package Pack1 is
   --     type PT1 is private;
   --     ...
   --  end Pack1;
   --
   --  package Pack2 is
   --     type PT2 is private;
   --  private
   --     type PT2 is access Integer;
   --
   --     type A1 is access Pack1.PT1;
   --     type A2 is access PT2;
   --
   --  A1 is not access-to-access, but A2 is, because at the place where A2 is
   --  defined the full view of PT2 is visible and PT2 is an access type but
   --  not a private type.

   -----------------------------------------------
   -- Bodies of general-purpose local functions --
   -----------------------------------------------

   ---------------
   -- Parse_Par --
   ---------------

   procedure Parse_Par
     (First_Par_Id : out Natural;
      Last_Par_Id  : out Positive;
      First_Str_Id : out Natural;
      Last_Str_Id  : out Positive;
      Par_String   :     String)
   is
      Eq_Pos : Natural := 0;
      Tmp    : Natural;
   begin

      for J in Par_String'Range loop

         if Par_String (J) = '=' then
            Eq_Pos := J;
            exit;
         end if;

      end loop;

      if Eq_Pos = 0 then
         Tmp := Par_String'Last;
      else
         Tmp := Eq_Pos - 1;
      end if;

      First_Par_Id := 0;

      for J in Par_String'First .. Tmp loop

         if not Is_White_Space (Par_String (J)) then
            First_Par_Id := J;
            exit;
         end if;

      end loop;

      if First_Par_Id > 0 then

         for J in reverse First_Par_Id .. Tmp loop

            if not Is_White_Space (Par_String (J)) then
               Last_Par_Id := J;
               exit;
            end if;

         end loop;

      end if;

      First_Str_Id := 0;

      if Eq_Pos > 0 then

         for J in Eq_Pos + 1 .. Par_String'Last loop

            if not Is_White_Space (Par_String (J)) then
               First_Str_Id := J;
               exit;
            end if;

         end loop;

         if First_Str_Id > 0 then

            for J in reverse First_Str_Id .. Par_String'Last loop

               if not Is_White_Space (Par_String (J)) then
                  Last_Str_Id := J;
                  exit;
               end if;

            end loop;

         end if;

      end if;

   end Parse_Par;

   -----------------------------------------------
   -- Bodies of rule-specific local subprograms --
   -----------------------------------------------

   -------------------------
   -- Is_Access_To_Access --
   -------------------------

   function Is_Access_To_Access
     (Def  : Asis.Element;
      Arg  : Asis.Element)
      return Boolean
   is
      Tmp, Tmp1 : Asis.Element;
      Result    : Boolean := False;
   begin

      if Access_Type_Kind (Def) in
        A_Pool_Specific_Access_To_Variable .. An_Access_To_Constant
      then
         Tmp := Asis.Definitions.Access_To_Object_Definition (Def);
         Tmp := Asis.Definitions.Subtype_Mark (Tmp);

         Tmp := Normalize_Reference (Tmp);
         Tmp := Corresponding_Name_Declaration (Tmp);

         if Declaration_Kind (Tmp) in
            An_Incomplete_Type_Declaration ..
            A_Tagged_Incomplete_Type_Declaration
         then
            Tmp1 := Corresponding_Type_Completion (Tmp);

            if not Is_Nil (Tmp1)
              and then
               Is_Equal (Enclosing_Compilation_Unit (Tmp),
                         Enclosing_Compilation_Unit (Tmp1))
            then
               --  For this check, we consider the full declaration instead of
               --  incomplete type declaration only if both of them are in the
               --  same unit.
               Tmp := Tmp1;
            end if;
         else
            Tmp := Corresponding_First_Subtype (Tmp);
         end if;

         if Declaration_Kind (Tmp) = A_Private_Type_Declaration
           and then
            Full_View_Visible (Tmp, At_Place => Arg)
         then
            Tmp := Corresponding_Type_Completion (Tmp);
         end if;

         Tmp := First_Name (Tmp);

         Result := Denotes_Access_Subtype (Tmp);

      end if;

      return Result;
   end Is_Access_To_Access;

   ------------------------
   -- Is_Access_To_Class --
   ------------------------

   function Is_Access_To_Class (Def : Asis.Element) return Boolean is
      Tmp      :          Asis.Element;
      Arg_Kind : constant Flat_Element_Kinds := Flat_Element_Kind (Def);
      Result   :          Boolean            := False;
   begin

      if Arg_Kind = A_Pool_Specific_Access_To_Variable        or else
         Arg_Kind = An_Access_To_Variable                     or else
         Arg_Kind = An_Access_To_Constant                     or else
         Arg_Kind = A_Formal_Pool_Specific_Access_To_Variable or else
         Arg_Kind = A_Formal_Access_To_Variable               or else
         Arg_Kind = A_Formal_Access_To_Constant
      then
         Tmp := Asis.Definitions.Access_To_Object_Definition (Def);
         Tmp := Asis.Definitions.Subtype_Mark (Tmp);

         if Attribute_Kind (Tmp) = A_Class_Attribute then
            Result := True;
         else

            if Expression_Kind (Tmp) = An_Identifier
              or else
               Expression_Kind (Tmp) = A_Selected_Component
            then
               Result := Denotes_Class_Wide_Subtype (Tmp);
            end if;

         end if;
      end if;

      return Result;
   end Is_Access_To_Class;

   ----------------------
   -- Is_Access_Suffix --
   ----------------------

   function Is_Access_Suffix (S : String) return Boolean is
      Parameter           : constant String  := Trim (S, Both);
      First_Idx           : constant Natural := Parameter'First;
      Last_Idx            : constant Natural := Parameter'Last - 1;
      Bracket_Idx         :          Natural;
      Result              :          Boolean := False;
   begin
      if Parameter (Last_Idx + 1) = ')' then

         Bracket_Idx := Index (Parameter, "(");

         if Bracket_Idx > 0 then

            if Is_Identifier_Suffix (To_Wide_String
                 (Trim (Parameter (First_Idx .. Bracket_Idx - 1), Right)))
            then
               Result :=
                 Is_Identifier_Suffix (To_Wide_String
                   (Trim (Parameter (Bracket_Idx + 1 .. Last_Idx), Left)));
            end if;

         end if;

      end if;

      return Result;
   end Is_Access_Suffix;

   --------------------------------------------
   -- Bodies of rule implementation routines --
   --------------------------------------------

   ----------------------
   -- Anonymous_Arrays --
   ----------------------

   ----------------------------------
   -- Init_Rule (Anonymous_Arrays) --
   ----------------------------------

   procedure Init_Rule (Rule : in out Anonymous_Arrays_Rule_Type) is
   begin
      Init_Rule (Rule_Template (Rule));

      Rule.Name        := new String'("Anonymous_Arrays");
      Rule.Rule_Status := Fully_Implemented;
      Rule.Help_Info   := new String'("anonymous array types");
      Rule.Diagnosis   := new String'("anonymous array type");
   end Init_Rule;

   ------------------------------------------
   -- Rule_Check_Pre_Op (Anonymous_Arrays) --
   ------------------------------------------

   procedure Rule_Check_Pre_Op
     (Rule    : in out Anonymous_Arrays_Rule_Type;
      Element :        Asis.Element;
      Control : in out Traverse_Control;
      State   : in out Rule_Traversal_State)
   is
      pragma Unreferenced (Rule, Control);
   begin

      if Type_Kind (Element) in
           An_Unconstrained_Array_Definition .. A_Constrained_Array_Definition
        and then
         Declaration_Kind (Get_Enclosing_Element) in
           A_Variable_Declaration .. A_Constant_Declaration
      then
         State.Detected  := True;
      end if;

   end Rule_Check_Pre_Op;

   -------------------------------------------
   -- Enumeration_Ranges_In_CASE_Statements --
   -------------------------------------------

   -------------------------------------------------------
   -- Init_Rule (Enumeration_Ranges_In_CASE_Statements) --
   -------------------------------------------------------

   procedure Init_Rule
     (Rule : in out Enumeration_Ranges_In_CASE_Statements_Rule_Type)
   is
   begin
      Init_Rule (Rule_Template (Rule));

      Rule.Name        := new String'("Enumeration_Ranges_In_CASE_Statements");
      Rule.Rule_Status := Fully_Implemented;
      Rule.Help_Info   := new String'("enumeration ranges as choices in case "
                                    & "statements");
      Rule.Diagnosis   := new String'("enumeration range as a choice in a "
                                    & "case statement");
   end Init_Rule;

   ---------------------------------------------------------------
   -- Rule_Check_Pre_Op (Enumeration_Ranges_In_CASE_Statements) --
   ---------------------------------------------------------------

   procedure Rule_Check_Pre_Op
     (Rule    : in out Enumeration_Ranges_In_CASE_Statements_Rule_Type;
      Element :        Asis.Element;
      Control : in out Traverse_Control;
      State   : in out Rule_Traversal_State)
   is
      pragma Unreferenced (Rule, Control);
      Case_Var : Asis.Element;
   begin

      if Definition_Kind (Element) = A_Discrete_Range
       and then
         Path_Kind (Get_Enclosing_Element) = A_Case_Path
      then
         Case_Var := Case_Expression (Get_Enclosing_Element (Steps_Up => 1));

         if Has_Enumeration_Type (Case_Var) then
            State.Detected := True;
         end if;

      end if;

   end Rule_Check_Pre_Op;

   --------------------------------
   -- Exceptions_As_Control_Flow --
   --------------------------------

   --------------------------------------------
   -- Init_Rule (Exceptions_As_Control_Flow) --
   --------------------------------------------

   procedure Init_Rule (Rule : in out Exceptions_As_Control_Flow_Rule_Type) is
   begin
      Init_Rule (Rule_Template (Rule));

      Rule.Name        := new String'("Exceptions_As_Control_Flow");
      Rule.Rule_Status := Fully_Implemented;
      Rule.Help_Info   := new String'("exceptions for control flow");
      Rule.Diagnosis   := new String'("this exception will be handled in " &
                                      "the same body, line%1%");
   end Init_Rule;

   ----------------------------------------------------
   -- Rule_Check_Pre_Op (Exceptions_As_Control_Flow) --
   ----------------------------------------------------

   procedure Rule_Check_Pre_Op
     (Rule    : in out Exceptions_As_Control_Flow_Rule_Type;
      Element :        Asis.Element;
      Control : in out Traverse_Control;
      State   : in out Rule_Traversal_State)
   is
      pragma Unreferenced (Rule, Control);

      Raised_Exc : Asis.Element;
      Encl_Body  : Asis.Element;
      Next_Frame : Asis.Element := Nil_Element;
      --  Construct that can contain exception handlers

      Step_Up    : Elmt_Idx := 0;
   begin

      if Statement_Kind (Element) = A_Raise_Statement then
         Raised_Exc := Raised_Exception (Element);

         if not Is_Nil (Raised_Exc) then
            --  First, get the enclosing body:

            Encl_Body := Get_Enclosing_Element (Step_Up);

            while Element_Kind (Encl_Body) in A_Statement .. A_Path loop
               Step_Up   := Step_Up + 1;
               Encl_Body := Get_Enclosing_Element (Step_Up);
            end loop;

            if Declaration_Kind (Encl_Body) not in
              A_Procedure_Body_Declaration .. A_Function_Body_Declaration
            then
               return;
            end if;

            Raised_Exc := Get_Name_Definition (Raised_Exc);

            Step_Up    := 0;
            Next_Frame := Get_Enclosing_Element (Step_Up);

            Check_Frames : loop
               --  Computing the next frame

               while not Is_Frame (Next_Frame) loop
                  Step_Up    := Step_Up + 1;
                  Next_Frame := Get_Enclosing_Element (Step_Up);
               end loop;

               --  Processing the next frame

               declare
                  Handlers : constant Asis.Element_List :=
                    Get_Handlers (Next_Frame);

                  Handler     : Asis.Element := Nil_Element;
                  Handled_Exc : Asis.Element;
               begin

                  if Handlers'Length = 0 then
                     return;
                  end if;

                  Check_Handlers : for J in Handlers'Range loop

                     declare
                        Exc_Choices : constant Asis.Element_List :=
                          Exception_Choices (Handlers (J));
                     begin

                        for K in Exc_Choices'Range loop

                           if Definition_Kind (Exc_Choices (K)) =
                              An_Others_Choice
                           then
                              State.Detected := True;
                           else
                              Handled_Exc :=
                                Get_Name_Definition (Exc_Choices (K));

                              State.Detected :=
                                Is_Equal (Raised_Exc, Handled_Exc);
                           end if;

                           if State.Detected then
                              Handler := Handlers (J);
                              State.Diag_Params := Enter_String ("%1%" &
                                 Element_Span (Handler).First_Line'Img);

                              exit Check_Frames;
                           end if;

                        end loop;

                     end;

                  end loop Check_Handlers;

               end;

               exit Check_Frames when Is_Equal (Next_Frame, Encl_Body);

               --  Go to the next frame

               Step_Up    := Step_Up + 1;
               Next_Frame := Get_Enclosing_Element (Step_Up);

            end loop Check_Frames;

         end if;

      end if;

   end Rule_Check_Pre_Op;

   ---------------------------------------
   -- EXIT_Statements_With_No_Loop_Name --
   ---------------------------------------

   ---------------------------------------------------
   -- Init_Rule (EXIT_Statements_With_No_Loop_Name) --
   ---------------------------------------------------

   procedure Init_Rule
     (Rule : in out EXIT_Statements_With_No_Loop_Name_Rule_Type)
   is
   begin
      Init_Rule (Rule_Template (Rule));

      Rule.Name        := new String'("EXIT_Statements_With_No_Loop_Name");
      Rule.Rule_Status := Fully_Implemented;
      Rule.Help_Info   := new String'("exit statements with no loop name");
      Rule.Diagnosis   := new String'("exit statement with no loop name");
   end Init_Rule;

   ---------------------------------------------------
   -- Init_Rule (EXIT_Statements_With_No_Loop_Name) --
   ---------------------------------------------------

   procedure Rule_Check_Pre_Op
     (Rule    : in out EXIT_Statements_With_No_Loop_Name_Rule_Type;
      Element :        Asis.Element;
      Control : in out Traverse_Control;
      State   : in out Rule_Traversal_State)
   is
      pragma Unreferenced (Rule, Control);
   begin

      if Statement_Kind (Element) = An_Exit_Statement
        and then
         Is_Nil (Exit_Loop_Name (Element))
      then
         State.Detected := True;
      end if;

   end Rule_Check_Pre_Op;

   -----------------------------------
   -- Explicit_Full_Discrete_Ranges --
   -----------------------------------

   ------------------------------------------------
   --  Init_Rule (Explicit_Full_Discrete_Ranges) --
   ------------------------------------------------

   procedure Init_Rule (Rule : in out Explicit_Full_Discrete_Ranges_Rule_Type)
   is
   begin
      Init_Rule (Rule_Template (Rule));

      Rule.Name        := new String'("Explicit_Full_Discrete_Ranges");
      Rule.Synonym     := new String'("Explicit_Discrete_Ranges");
      Rule.Rule_Status := Fully_Implemented;
      Rule.Help_Info   := new String'("explicit discrete ranges");
      Rule.Diagnosis   :=
        new String'("#1#bad discrete range, consider replacement " &
                       "with subtype mark"                         &
                    "#2#bad discrete range, consider replacement " &
                       "with 'Range attribute");
   end Init_Rule;

   --------------------------------------------------------
   --  Rule_Check_Pre_Op (Explicit_Full_Discrete_Ranges) --
   --------------------------------------------------------

   procedure Rule_Check_Pre_Op
     (Rule    : in out Explicit_Full_Discrete_Ranges_Rule_Type;
      Element :        Asis.Element;
      Control : in out Traverse_Control;
      State   : in out Rule_Traversal_State)
   is
      pragma Unreferenced (Rule, Control);
      L, R : Asis.Element;
   begin

      if Discrete_Range_Kind (Element) =
           A_Discrete_Simple_Expression_Range
      then
         L := Lower_Bound (Element);

         if Attribute_Kind (L) = A_First_Attribute then
            R := Upper_Bound (Element);

            if Attribute_Kind (R) = A_Last_Attribute then
               --  The argument discrete range is to be detected only if
               --  L and R are or ends with the same identifier

               L := Prefix (L);
               R := Prefix (R);

               if Expression_Kind (L) = A_Selected_Component then
                  L := Selector (L);
               end if;

               if Expression_Kind (R) = A_Selected_Component then
                  L := Selector (R);
               end if;

               if Expression_Kind (L) = An_Identifier
                 and then
                  Expression_Kind (R) = An_Identifier
                 and then
                  To_Lower (To_String (Name_Image (L))) =
                  To_Lower (To_String (Name_Image (R)))
               then
                  --  Now we have to check that L (and, therefore R) is
                  --  either a subtype mark of a discrete (sub)type or a
                  --  reference to an array data object

                  L := Corresponding_Name_Declaration (L);

                  case Declaration_Kind (L) is
                     when An_Ordinary_Type_Declaration |
                          A_Subtype_Declaration        =>
                        --  It must be a discrete (sub)type!
                        State.Detected  := True;
                        State.Diagnosis := 1;
                     when A_Variable_Declaration          |
                          A_Constant_Declaration          |
                          A_Component_Declaration         |
                          A_Parameter_Specification       |
                          A_Return_Variable_Specification |
                          A_Return_Constant_Specification |
                          An_Object_Renaming_Declaration  |
                          A_Formal_Object_Declaration     =>

                        --  It must be a declaration of an array object or an
                        --  access object that points to an array object!
                        State.Detected  := True;
                        State.Diagnosis := 2;
                     when others =>
                        null;
                  end case;

               end if;

            end if;

         end if;

      end if;

   end Rule_Check_Pre_Op;

   --------------------------
   -- Forbidden_Attributes --
   --------------------------

   --------------------------------------------------------
   -- Data structures and local subprograms for the rule --
   --------------------------------------------------------

   type Check_Status is (Off, On, Selective);
   --  The values of this type say if a given attribute/pragma should be
   --  detected. The Selective value is used only for
   --  An_Implementation_Defined_Attribute/An_Implementation_Defined_Pragma
   --  kinds, it means that only some of the GNAT-specific attributes/pragmas
   --  should be detected.

   Attribute_Check_Switch :
     array (Asis.Attribute_Kinds'(An_Access_Attribute) ..
             Asis.Attribute_Kinds'(An_Unknown_Attribute)) of Check_Status :=
               (others => Off);
   --  Specifies which pragmas should be detected.

   GNAT_Attribute_Check_Switch :
     array (Snames.Attribute_Id) of Boolean := (others => False);
   --  Specifies which GNAT-specific attributes should be detected. Note, that
   --  the index range covers all the attribute IDs, both standard and
   --  GNAT-specific, but only those components that correspond to
   --  GNAT-specific attributes are referenced

   function Get_Attribute_Kind (S : String) return Attribute_Kinds;
   --  Tries to get from its argument (that is treated as an (an identifier
   --  from) attribute designator and is supposed to be obtained from the rule
   --  parameter) the corresponding ASIS Attribute_Kinds value. If S does not
   --  have a structure of an identifier, returns Not_An_Attribute

   procedure Get_GNAT_Attribute_Id
     (S       :     String;
      Id      : out Snames.Attribute_Id;
      Success : out Boolean);
   --  Supposing that S is a name of a GNAT attribute, computes its
   --  Attribute_Id. Sets Success OFF if the argument is not a name of a
   --  GNAT-specific attribute, otherwise Success is set ON.

   --------------------------------------------------
   -- Activate_In_Test_Mode (Forbidden_Attributes) --
   --------------------------------------------------

   overriding procedure Activate_In_Test_Mode
     (Rule : in out Forbidden_Attributes_Rule_Type)
   is
   begin
      Process_Rule_Parameter
        (Rule       => Rule,
         Param      => "Range",
         Enable     => True,
         Defined_At => "");

      Process_Rule_Parameter
        (Rule       => Rule,
         Param      => "Access",
         Enable     => True,
         Defined_At => "");

      Process_Rule_Parameter
        (Rule       => Rule,
         Param      => "Img",
         Enable     => True,
         Defined_At => "");

      Process_Rule_Parameter
        (Rule       => Rule,
         Param      => "Object_Size",
         Enable     => True,
         Defined_At => "");

   end Activate_In_Test_Mode;

   -----------------------------------------------------------
   -- Allowed_As_Exemption_Parameter (Forbidden_Attributes) --
   -----------------------------------------------------------

   overriding function Allowed_As_Exemption_Parameter
     (Rule      : Forbidden_Attributes_Rule_Type;
      Parameter : String)
      return  Boolean
   is
      pragma Unreferenced (Rule);
   begin
      return Get_Attribute_Kind (Parameter) in
        Attribute_Kinds'Succ (Not_An_Attribute) ..
        An_Implementation_Defined_Attribute;
   end Allowed_As_Exemption_Parameter;

   --------------------------------------------------
   -- Get_GNAT_Attribute_Id (Forbidden_Attributes) --
   --------------------------------------------------

   procedure Get_GNAT_Attribute_Id
     (S       :     String;
      Id      : out Snames.Attribute_Id;
      Success : out Boolean)
   is
      use Namet;
      Attribute_Name_Id : Namet.Name_Id;
   begin

      if Is_Identifier (To_Wide_String (S)) then
         Name_Len                    := S'Length;
         Name_Buffer (1 .. Name_Len) := To_Lower (S);
         Attribute_Name_Id           := Name_Find;

         if Attribute_Name_Id in
              Snames.First_Attribute_Name .. Snames.Last_Attribute_Name
         then
            Id      := Snames.Get_Attribute_Id (Attribute_Name_Id);
            Success := True;
         else
            Success := False;
         end if;

      end if;

   end Get_GNAT_Attribute_Id;

   -----------------------------------------------
   -- Get_Attribute_Kind (Forbidden_Attributes) --
   -----------------------------------------------

   function Get_Attribute_Kind (S : String) return Attribute_Kinds is
      Result  : Attribute_Kinds := Not_An_Attribute;
      Attr_Id : Snames.Attribute_Id;
      pragma Warnings (Off, Attr_Id);
      --  We need Attr_Id only as a placeholder in the call to
      --  Get_GNAT_Attribute_Id
      Success : Boolean;
   begin

      if Is_Identifier (To_Wide_String (S)) then

         begin

            case To_Lower (S (S'First)) is
               when 'a' | 'e' | 'i' | 'o' | 'u'  =>
                  Result := Attribute_Kinds'Value ("an_" & S & "_attribute");
               when others =>
                  Result := Attribute_Kinds'Value ("a_" & S & "_attribute");
            end case;

         exception
            when Constraint_Error =>
               Result := An_Unknown_Attribute;
         end;

      end if;

      if Result = An_Unknown_Attribute then
         --  We can have a GNAT-specific pragma here!
         Get_GNAT_Attribute_Id (S, Attr_Id, Success);

         if Success then
            Result := An_Implementation_Defined_Attribute;
         end if;

      end if;

      return Result;
   end Get_Attribute_Kind;

   --------------------------------------
   -- Init_Rule (Forbidden_Attributes) --
   --------------------------------------

   procedure Init_Rule (Rule : in out Forbidden_Attributes_Rule_Type) is
   begin
      Init_Rule (Rule_Template (Rule));

      Rule.Name        := new String'("Forbidden_Attributes");
      Rule.Rule_Status := Fully_Implemented;
      Rule.Help_Info   := new String'("usage of specified attributes");
      Rule.Diagnosis   := new String'("use of attribute %1%");
   end Init_Rule;

   ---------------------------------------
   -- Print_Rule (Forbidden_Attributes) --
   ---------------------------------------

   procedure Print_Rule
     (Rule         : Forbidden_Attributes_Rule_Type;
      Indent_Level : Natural := 0)
   is
      All_On        : Boolean := True;
      First_Param   : Boolean := True;
      Rule_Name_Pad : constant String (1 .. Rule_Name (Rule)'Length + 2) :=
        (others => ' ');
   begin
      Print_Rule (Rule_Template (Rule), Indent_Level);

      --  Special case: all attributes are ON
      for J in Asis.Attribute_Kinds'(An_Access_Attribute) ..
               Asis.Attribute_Kinds'(An_Implementation_Defined_Attribute)
      loop
         if Attribute_Check_Switch (J) /= On then
            All_On := False;
            exit;
         end if;
      end loop;

      if All_On then
         Report_No_EOL (": ALL");
         return;
      end if;

      --  Standard Ada attributes
      for J in Asis.Attribute_Kinds'(An_Access_Attribute) ..
               Asis.Attribute_Kinds'(A_Wide_Wide_Width_Attribute)
      loop

         if Attribute_Check_Switch (J) = On then

            if First_Param then
               Report_No_EOL (": " & Ada_Attribute_Designator (J));
               First_Param := False;
            else
               Report (",");
               Report_No_EOL
                 (Rule_Name_Pad &
                  Ada_Attribute_Designator (J),
                  Indent_Level);
            end if;

         end if;

      end loop;

      case Attribute_Check_Switch (An_Implementation_Defined_Attribute) is
         when Off =>
            null;
         when On =>
            if First_Param then
               Report_No_EOL (": GNAT");
            else
               Report (",");
               Report_No_EOL (Rule_Name_Pad & "GNAT", Indent_Level);
            end if;
         when Selective =>
            for J in GNAT_Attribute_Check_Switch'Range loop

               if GNAT_Attribute_Check_Switch (J) then
                  if First_Param then
                     Report_No_EOL (": " & GNAT_Attribute_Designator (J));
                     First_Param := False;
                  else
                     Report (",");
                     Report_No_EOL
                       (Rule_Name_Pad &
                        GNAT_Attribute_Designator (J),
                        Indent_Level);
                  end if;
               end if;

            end loop;

      end case;

   end Print_Rule;

   -----------------------------------------------
   -- Print_Rule_To_File (Forbidden_Attributes) --
   -----------------------------------------------

   overriding procedure Print_Rule_To_File
     (Rule         : Forbidden_Attributes_Rule_Type;
      Rule_File    : File_Type;
      Indent_Level : Natural := 0)
   is
      All_On        : Boolean := True;
      First_Param   : Boolean := True;
      Rule_Name_Pad : constant String (1 .. Rule_Name (Rule)'Length + 2) :=
        (others => ' ');
   begin
      Print_Rule_To_File (Rule_Template (Rule), Rule_File, Indent_Level);

      --  Special case: all attributes are ON
      for J in Asis.Attribute_Kinds'(An_Access_Attribute) ..
               Asis.Attribute_Kinds'(An_Implementation_Defined_Attribute)
      loop
         if Attribute_Check_Switch (J) /= On then
            All_On := False;
            exit;
         end if;
      end loop;

      if All_On then
         Report_No_EOL (": ALL");
         return;
      end if;

      --  Standard Ada attributes
      for J in Asis.Attribute_Kinds'(An_Access_Attribute) ..
               Asis.Attribute_Kinds'(A_Wide_Wide_Width_Attribute)
      loop

         if Attribute_Check_Switch (J) = On then

            if First_Param then
               Put (Rule_File, ": " & Ada_Attribute_Designator (J));
               First_Param := False;
            else
               Put_Line (Rule_File, ",");

               for J in 1 .. Indent_Level loop
                  Put (Rule_File, Get_Indent_String);
               end loop;

               Put (Rule_File, Rule_Name_Pad & Ada_Attribute_Designator (J));
            end if;

         end if;

      end loop;

      case Attribute_Check_Switch (An_Implementation_Defined_Attribute) is
         when Off =>
            null;
         when On =>
            if First_Param then
               Put (Rule_File, ": GNAT");
            else
               Put_Line (Rule_File, ",");

               for J in 1 .. Indent_Level loop
                  Put (Rule_File, Get_Indent_String);
               end loop;

               Put (Rule_File, Rule_Name_Pad & "GNAT");
            end if;
         when Selective =>
            for J in GNAT_Attribute_Check_Switch'Range loop

               if GNAT_Attribute_Check_Switch (J) then
                  if First_Param then
                     Put (Rule_File, ": " & GNAT_Attribute_Designator (J));
                     First_Param := False;
                  else
                     Put_Line (Rule_File, ",");

                     for J in 1 .. Indent_Level loop
                        Put (Rule_File, Get_Indent_String);
                     end loop;

                     Put (Rule_File,
                          Rule_Name_Pad & GNAT_Attribute_Designator (J));
                  end if;
               end if;

            end loop;

      end case;

   end Print_Rule_To_File;

   ---------------------------------------------------
   -- Process_Rule_Parameter (Forbidden_Attributes) --
   ---------------------------------------------------

   procedure Process_Rule_Parameter
     (Rule       : in out Forbidden_Attributes_Rule_Type;
      Param      :        String;
      Enable     :        Boolean;
      Defined_At : String)
   is
      pragma Unreferenced (Defined_At);
      Arg_Kind       : Attribute_Kinds;
      GNAT_Attribute : Snames.Attribute_Id;
      Success        : Boolean;
   begin

      if Param = "" then
         if Enable then
            Rule.Rule_State := Enabled;
         else
            Rule.Rule_State := Disabled;
         end if;

         return;
      end if;

      if To_Lower (Param) = "gnat" then

         if Enable then
            Attribute_Check_Switch (An_Implementation_Defined_Attribute) := On;
            GNAT_Attribute_Check_Switch := (others => True);

            Rule.Rule_State := Enabled;
         else
            Attribute_Check_Switch (An_Implementation_Defined_Attribute) :=
              Off;
         end if;

         return;
      end if;

      if To_Lower (Param) = "all" then

         if Enable then
            Attribute_Check_Switch      := (others => On);
            GNAT_Attribute_Check_Switch := (others => True);
            Rule.Rule_State := Enabled;

--            Attribute_Check_Switch (An_Implementation_Defined_Attribute) :=
--              Selective;
         else
            Attribute_Check_Switch      := (others => Off);
            GNAT_Attribute_Check_Switch := (others => False);
            Rule.Rule_State             := Disabled;
         end if;

         return;
      end if;

      Arg_Kind := Get_Attribute_Kind (Param);

      case Arg_Kind is

         when Not_An_Attribute =>
            Error ("(" & Rule.Name.all & ") wrong attribute designator : " &
                   Param);

         when An_Implementation_Defined_Attribute =>

            Get_GNAT_Attribute_Id (Param, GNAT_Attribute, Success);

            if Enable then

               if Attribute_Check_Switch (Arg_Kind) = Off then
                  Attribute_Check_Switch (Arg_Kind) := Selective;
               end if;

               if Success then
                  GNAT_Attribute_Check_Switch (GNAT_Attribute) := True;
               end if;

               Rule.Rule_State := Enabled;

            else
               GNAT_Attribute_Check_Switch (GNAT_Attribute) := False;

               if Attribute_Check_Switch (Arg_Kind) = On then
                  Attribute_Check_Switch (Arg_Kind) := Selective;
               end if;
            end if;

         when others =>

            --  Only specific attribute kinds and An_Unknown_Attribute are
            --  possible
            if Enable then
               Attribute_Check_Switch (Arg_Kind) := On;
               Rule.Rule_State := Enabled;
            else
               Attribute_Check_Switch (Arg_Kind) := Off;
            end if;

      end case;

   end Process_Rule_Parameter;

   ----------------------------------------------
   -- Rule_Check_Pre_Op (Forbidden_Attributes) --
   ----------------------------------------------

   procedure Rule_Check_Pre_Op
     (Rule    : in out Forbidden_Attributes_Rule_Type;
      Element :        Asis.Element;
      Control : in out Traverse_Control;
      State   : in out Rule_Traversal_State)
   is
      El_Kind : constant Attribute_Kinds := Attribute_Kind (Element);
      pragma Unreferenced (Control);
      pragma Unreferenced (Rule);
   begin

      if Expression_Kind (Element) = An_Attribute_Reference then

         if Attribute_Check_Switch (El_Kind) = On then
            State.Detected := True;

         elsif Attribute_Check_Switch (El_Kind) = Selective then

            declare
               Attr_Designator : constant String :=
                 To_String
                  (Name_Image (Attribute_Designator_Identifier (Element)));

               Attr_Id : Snames.Attribute_Id;
               Success : Boolean;
            begin
               Get_GNAT_Attribute_Id (Attr_Designator, Attr_Id, Success);

               State.Detected :=
                 GNAT_Attribute_Check_Switch (Attr_Id);
            end;

         end if;

         if State.Detected then
            State.Diag_Params := Enter_String ("%1%" &
              To_String (Name_Image (
                Attribute_Designator_Identifier (Element))));
         end if;

      end if;

   end Rule_Check_Pre_Op;

   -------------------------------------------
   -- Rule_Parameter (Forbidden_Attributes) --
   -------------------------------------------

   overriding function Rule_Parameter
     (Rule : Forbidden_Attributes_Rule_Type;
      Diag : String)
      return String
   is
      pragma Unreferenced (Rule);
      First_Idx : constant Natural := Index (Diag, " ", Going => Backward) + 1;
   begin
      return To_Lower (Diag (First_Idx .. Diag'Last));
   end Rule_Parameter;

   -------------------------------------------
   -- XML_Print_Rule (Forbidden_Attributes) --
   -------------------------------------------

   overriding procedure XML_Print_Rule
     (Rule         : Forbidden_Attributes_Rule_Type;
      Indent_Level : Natural := 0)
   is
      All_On : Boolean := True;
   begin
      XML_Report
        ("<rule id=""" & Rule_Name (Rule) & """>",
         Indent_Level);

      --  Special case: all attributes are ON
      for J in Asis.Attribute_Kinds'(An_Access_Attribute) ..
               Asis.Attribute_Kinds'(An_Implementation_Defined_Attribute)
      loop
         if Attribute_Check_Switch (J) /= On then
            All_On := False;
            exit;
         end if;
      end loop;

      if All_On then

         XML_Report
           ("<parameter>ALL</parameter>",
            Indent_Level + 1);
         goto Done;
      end if;

      --  Standard Ada attributes
      for J in Asis.Attribute_Kinds'(An_Access_Attribute) ..
               Asis.Attribute_Kinds'(A_Wide_Wide_Width_Attribute)
      loop

         if Attribute_Check_Switch (J) = On then

            XML_Report
              ("<parameter>" & Ada_Attribute_Designator (J) & "</parameter>",
               Indent_Level + 1);
         end if;

      end loop;

      case Attribute_Check_Switch (An_Implementation_Defined_Attribute) is
         when Off =>
            null;
         when On =>
            XML_Report
              ("<parameter>GNAT</parameter>",
               Indent_Level + 1);
         when Selective =>
            for J in GNAT_Attribute_Check_Switch'Range loop

               if GNAT_Attribute_Check_Switch (J) then
                  XML_Report
                    ("<parameter>" & GNAT_Attribute_Designator (J) &
                     "</parameter>",
                     Indent_Level + 1);
               end if;

            end loop;

      end case;

      <<Done>>
      XML_Report ("</rule>", Indent_Level);
   end XML_Print_Rule;

   ------------------------------------------
   -- XML_Rule_Help (Forbidden_Attributes) --
   ------------------------------------------

   procedure XML_Rule_Help
     (Rule  : Forbidden_Attributes_Rule_Type;
      Level : Natural)
   is
   begin
      Info (Level * Ident_String                               &
            "<check  switch=""+R"                              &
            Rule.Name.all                                      &
            ":ALL"""                                           &
            " label="""                                        &
            "detect all attributes except explicitly disabled""/>");

      Info (Level * Ident_String                                &
            "<check  switch=""+R"                               &
            Rule.Name.all                                       &
            ":GNAT"""                                           &
            " label="""                                         &
            "detect all GNAT attributes except explicitly disabled""/>");

      Info (Level * Ident_String                                   &
            "<field switch=""+R"                                   &
            Rule.Name.all                                          &
            """ label="""                                          &
            "detect specified attributes (use ',' as separator)""" &
            " separator="":"""                                     &
            "/>");

      Info (Level * Ident_String                                          &
            "<field switch=""-R"                                          &
            Rule.Name.all                                                 &
            """ label="""                                                 &
            "do not detect specified attributes (use ',' as separator)""" &
            " separator="":"""                                            &
            "/>");
   end XML_Rule_Help;

   -----------------------
   -- Forbidden_Pragmas --
   -----------------------

   --------------------------------------------------------
   -- Data structures and local subprograms for the rule --
   --------------------------------------------------------

   Pragma_Check_Switch :
     array (Asis.Pragma_Kinds'(An_All_Calls_Remote_Pragma) ..
            Asis.Pragma_Kinds'(An_Unknown_Pragma)) of Check_Status :=
              (others => Off);
   --  Specifies which pragma should be detected.

   GNAT_Pragma_Check_Switch :
     array (Snames.Pragma_Id) of Boolean := (others => False);
   --  Specifies which GNAT-specific pragmas should be detected. Note, that
   --  the index range covers all the pragma IDs, both standard and
   --  GNAT-specific, but only those components that correspond to
   --  GNAT-specific pragmas are referenced

   function Get_Pragma_Kind (S : String) return Pragma_Kinds;
   --  Tries to get from its argument (that is treated as a pragma name and is
   --  supposed to be obtained from the rule parameter) the corresponding
   --  ASIS Pragma_Kinds value. If S does not have a structure of an
   --  identifier, returns Not_A_Pragma

   function Get_GNAT_Pragma_Id (S : String) return Snames.Pragma_Id;
   --  Supposing that S is a name of a GNAT pragma, computes its Pragma_Id.
   --  Returns Unknown_Pragma if the argument is not a name of a GNAT-specific
   --  pragma.

   -----------------------------------------------
   -- Activate_In_Test_Mode (Forbidden_Pragmas) --
   -----------------------------------------------

   overriding procedure Activate_In_Test_Mode
     (Rule : in out Forbidden_Pragmas_Rule_Type)
   is
   begin
      Process_Rule_Parameter
        (Rule       => Rule,
         Param      => "Inline",
         Enable     => True,
         Defined_At => "");

      Process_Rule_Parameter
        (Rule       => Rule,
         Param      => "Suppress",
         Enable     => True,
         Defined_At => "");

      Process_Rule_Parameter
        (Rule       => Rule,
         Param      => "Initialize_Scalars",
         Enable     => True,
         Defined_At => "");

      Process_Rule_Parameter
        (Rule       => Rule,
         Param      => "Keep_Names",
         Enable     => True,
         Defined_At => "");

   end Activate_In_Test_Mode;

   --------------------------------------------------------
   -- Allowed_As_Exemption_Parameter (Forbidden_Pragmas) --
   --------------------------------------------------------

   overriding function Allowed_As_Exemption_Parameter
     (Rule      : Forbidden_Pragmas_Rule_Type;
      Parameter : String)
      return  Boolean
   is
      pragma Unreferenced (Rule);
   begin
      return Get_Pragma_Kind (Parameter) in
        Pragma_Kinds'Succ (Not_A_Pragma) ..
        An_Implementation_Defined_Pragma;
   end Allowed_As_Exemption_Parameter;

   --------------------------------------------
   -- Get_GNAT_Pragma_Id (Forbidden_Pragmas) --
   --------------------------------------------

   function Get_GNAT_Pragma_Id (S : String) return Snames.Pragma_Id is
      use Namet;
      Result : Snames.Pragma_Id := Snames.Unknown_Pragma;
      Pragma_Name_Id : Namet.Name_Id;
   begin

      if Is_Identifier (To_Wide_String (S)) then
         Name_Len                    := S'Length;
         Name_Buffer (1 .. Name_Len) := To_Lower (S);
         Pragma_Name_Id              := Name_Find;
         Result                      := Snames.Get_Pragma_Id (Pragma_Name_Id);
      end if;

      return Result;
   end Get_GNAT_Pragma_Id;

   -----------------------------------------
   -- Get_Pragma_Kind (Forbidden_Pragmas) --
   -----------------------------------------

   function Get_Pragma_Kind (S : String) return Pragma_Kinds is
      use  type Snames.Pragma_Id;
      Result : Pragma_Kinds := Not_A_Pragma;
   begin

      if Is_Identifier (To_Wide_String (S)) then

         begin

            case To_Lower (S (S'First)) is
               when 'a' | 'e' | 'i' | 'o' | 'u'  =>
                  Result := Pragma_Kinds'Value ("an_" & S & "_pragma");
               when others =>
                  Result := Pragma_Kinds'Value ("a_" & S & "_pragma");
            end case;

         exception
            when Constraint_Error =>
               Result := An_Unknown_Pragma;
         end;

      end if;

      if Result = An_Unknown_Pragma then
         --  We can have a GNAT-specific pragma here!
         if Get_GNAT_Pragma_Id (S) /= Snames.Unknown_Pragma then
            Result := An_Implementation_Defined_Pragma;
         end if;

      end if;

      return Result;
   end Get_Pragma_Kind;

   -----------------------------------
   -- Init_Rule (Forbidden_Pragmas) --
   -----------------------------------

   procedure Init_Rule (Rule : in out Forbidden_Pragmas_Rule_Type) is
   begin
      Init_Rule (Rule_Template (Rule));

      Rule.Name        := new String'("Forbidden_Pragmas");
      Rule.Synonym     := new String'("Pragma_Usage");
      Rule.Rule_Status := Fully_Implemented;
      Rule.Help_Info   := new String'("usage of specified pragmas");
      Rule.Diagnosis   := new String'("use of pragma %1%");
   end Init_Rule;

   ------------------------------------
   -- Print_Rule (Forbidden_Pragmas) --
   ------------------------------------

   procedure Print_Rule
     (Rule         : Forbidden_Pragmas_Rule_Type;
      Indent_Level : Natural := 0)
   is
      All_On        : Boolean := True;
      First_Param   : Boolean := True;
      Rule_Name_Pad : constant String (1 .. Rule_Name (Rule)'Length + 2) :=
        (others => ' ');
   begin
      Print_Rule (Rule_Template (Rule), Indent_Level);

      --  Special case: all pragmas are ON
      for J in Asis.Pragma_Kinds'(An_All_Calls_Remote_Pragma) ..
               Asis.Pragma_Kinds'(An_Implementation_Defined_Pragma)
      loop
         if Pragma_Check_Switch (J) /= On then
            All_On := False;
            exit;
         end if;
      end loop;

      if All_On then
         Report_No_EOL (": ALL");
         return;
      end if;

      --  Standard Ada pragmas
      for J in Asis.Pragma_Kinds'(An_All_Calls_Remote_Pragma) ..
               Asis.Pragma_Kinds'(An_Unsuppress_Pragma)
      loop

         if Pragma_Check_Switch (J) = On then

            if First_Param then
               Report_No_EOL (": " & Ada_Pragma_Identifier (J));
               First_Param := False;
            else
               Report (",");
               Report_No_EOL
                 (Rule_Name_Pad &
                  Ada_Pragma_Identifier (J),
                  Indent_Level);
            end if;

         end if;

      end loop;

      case Pragma_Check_Switch (An_Implementation_Defined_Pragma) is
         when Off =>
            null;
         when On =>
            if First_Param then
               Report_No_EOL (": GNAT");
            else
               Report (",");
               Report_No_EOL (Rule_Name_Pad & "GNAT", Indent_Level);
            end if;
         when Selective =>
            for J in GNAT_Pragma_Check_Switch'Range loop

               if GNAT_Pragma_Check_Switch (J) then
                  if First_Param then
                     Report_No_EOL (": " & GNAT_Pragma_Identifier (J));
                     First_Param := False;
                  else
                     Report (",");
                     Report_No_EOL
                       (Rule_Name_Pad &
                        GNAT_Pragma_Identifier (J),
                        Indent_Level);
                  end if;
               end if;

            end loop;

      end case;

   end Print_Rule;

   --------------------------------------------
   -- Print_Rule_To_File (Forbidden_Pragmas) --
   --------------------------------------------

   overriding procedure Print_Rule_To_File
     (Rule         : Forbidden_Pragmas_Rule_Type;
      Rule_File    : File_Type;
      Indent_Level : Natural := 0)
   is
      All_On        : Boolean := True;
      First_Param   : Boolean := True;
      Rule_Name_Pad : constant String (1 .. Rule_Name (Rule)'Length + 2) :=
        (others => ' ');
   begin
      Print_Rule_To_File (Rule_Template (Rule), Rule_File, Indent_Level);

      --  Special case: all pragmas are ON
      for J in Asis.Pragma_Kinds'(An_All_Calls_Remote_Pragma) ..
               Asis.Pragma_Kinds'(An_Implementation_Defined_Pragma)
      loop
         if Pragma_Check_Switch (J) /= On then
            All_On := False;
            exit;
         end if;
      end loop;

      if All_On then
         Put (Rule_File, ": ALL");
         return;
      end if;

      --  Standard Ada pragmas
      for J in Asis.Pragma_Kinds'(An_All_Calls_Remote_Pragma) ..
               Asis.Pragma_Kinds'(An_Unsuppress_Pragma)
      loop

         if Pragma_Check_Switch (J) = On then

            if First_Param then
               Put (Rule_File, ": " & Ada_Pragma_Identifier (J));
               First_Param := False;
            else
               Put_Line (Rule_File, ",");

               for J in 1 .. Indent_Level loop
                  Put (Rule_File, Get_Indent_String);
               end loop;

               Put (Rule_File, Rule_Name_Pad & Ada_Pragma_Identifier (J));
            end if;

         end if;

      end loop;

      case Pragma_Check_Switch (An_Implementation_Defined_Pragma) is
         when Off =>
            null;
         when On =>
            if First_Param then
               Put (Rule_File, ": GNAT");
            else
               Put (Rule_File, ",");

               for J in 1 .. Indent_Level loop
                  Put (Rule_File, Get_Indent_String);
               end loop;

               Put (Rule_File, Rule_Name_Pad & "GNAT");
            end if;
         when Selective =>
            for J in GNAT_Pragma_Check_Switch'Range loop

               if GNAT_Pragma_Check_Switch (J) then
                  if First_Param then
                     Put (Rule_File, ": " & GNAT_Pragma_Identifier (J));
                     First_Param := False;
                  else
                     Put_Line (Rule_File, ",");

                     for J in 1 .. Indent_Level loop
                        Put (Rule_File, Get_Indent_String);
                     end loop;

                     Put (Rule_File,
                          Rule_Name_Pad & GNAT_Pragma_Identifier (J));
                  end if;
               end if;

            end loop;

      end case;

   end Print_Rule_To_File;

   ------------------------------------------------
   -- Process_Rule_Parameter (Forbidden_Pragmas) --
   ------------------------------------------------

   procedure Process_Rule_Parameter
     (Rule       : in out Forbidden_Pragmas_Rule_Type;
      Param      :        String;
      Enable     :        Boolean;
      Defined_At : String)
   is
      pragma Unreferenced (Defined_At);
      Arg_Kind    : Pragma_Kinds;
      GNAT_Pragma : Snames.Pragma_Id;
   begin

      if Param = "" then
         if Enable then
            Rule.Rule_State := Enabled;
         else
            Rule.Rule_State := Disabled;
         end if;

         return;
      end if;

      if To_Lower (Param) = "gnat" then

         if Enable then
            Pragma_Check_Switch (An_Implementation_Defined_Pragma) := On;
            Rule.Rule_State := Enabled;
         else
            Pragma_Check_Switch (An_Implementation_Defined_Pragma) := Off;
         end if;

         return;
      end if;

      if To_Lower (Param) = "all" then

         if Enable then
            Pragma_Check_Switch      := (others => On);
            GNAT_Pragma_Check_Switch := (others => True);
            Rule.Rule_State          := Enabled;

--            Pragma_Check_Switch (An_Implementation_Defined_Pragma) :=
--              Selective;
         else
            Pragma_Check_Switch      := (others => Off);
            GNAT_Pragma_Check_Switch := (others => False);
            Rule.Rule_State          := Disabled;
         end if;

         return;
      end if;

      Arg_Kind := Get_Pragma_Kind (Param);

      case Arg_Kind is

         when Not_A_Pragma =>
            Error ("(" & Rule.Name.all & ") wrong pragma name : " & Param);

         when An_Implementation_Defined_Pragma =>

            GNAT_Pragma := Get_GNAT_Pragma_Id (Param);

            if Enable then

               if Pragma_Check_Switch (Arg_Kind) = Off then
                  Pragma_Check_Switch (Arg_Kind) := Selective;
               end if;

               GNAT_Pragma_Check_Switch (GNAT_Pragma) := True;
               Rule.Rule_State                        := Enabled;

            else
               GNAT_Pragma_Check_Switch (GNAT_Pragma) := False;

               if Pragma_Check_Switch (An_Implementation_Defined_Pragma) =
                    On
               then
                  Pragma_Check_Switch (An_Implementation_Defined_Pragma) :=
                     Selective;
               end if;

            end if;

         when others =>

            --  Only specific pragma kinds and An_Unknown_Pragma are possible
            if Enable then
               Pragma_Check_Switch (Arg_Kind) := On;
               Rule.Rule_State := Enabled;
            else
               Pragma_Check_Switch (Arg_Kind) := Off;
            end if;

      end case;

   end Process_Rule_Parameter;

   -------------------------------------------
   -- Rule_Check_Pre_Op (Forbidden_Pragmas) --
   -------------------------------------------

   procedure Rule_Check_Pre_Op
     (Rule    : in out Forbidden_Pragmas_Rule_Type;
      Element :        Asis.Element;
      Control : in out Traverse_Control;
      State   : in out Rule_Traversal_State)
   is
      El_Kind : constant Pragma_Kinds := Pragma_Kind (Element);
      pragma Unreferenced (Control);
      pragma Unreferenced (Rule);
   begin

      if Element_Kind (Element) = A_Pragma then

         if Pragma_Check_Switch (El_Kind) = On then
            State.Detected := True;
         elsif Pragma_Check_Switch (El_Kind) = Selective then
            State.Detected :=
              GNAT_Pragma_Check_Switch (Get_GNAT_Pragma_Id
                (To_String (Pragma_Name_Image (Element))));
         end if;

         if State.Detected then
            State.Diag_Params :=
              Enter_String ("%1%" & To_String (Pragma_Name_Image (Element)));
         end if;

      end if;

   end Rule_Check_Pre_Op;

   ----------------------------------------
   -- Rule_Parameter (Forbidden_Pragmas) --
   ----------------------------------------

   overriding function Rule_Parameter
     (Rule : Forbidden_Pragmas_Rule_Type;
      Diag : String)
      return String
   is
      pragma Unreferenced (Rule);
      First_Idx : constant Natural := Index (Diag, " ", Going => Backward) + 1;
   begin
      return To_Lower (Diag (First_Idx .. Diag'Last));
   end Rule_Parameter;

   ----------------------------------------
   -- XML_Print_Rule (Forbidden_Pragmas) --
   ----------------------------------------

   overriding procedure XML_Print_Rule
     (Rule         : Forbidden_Pragmas_Rule_Type;
      Indent_Level : Natural := 0)
   is
      All_On : Boolean := True;
   begin
      XML_Report
        ("<rule id=""" & Rule_Name (Rule) & """>",
         Indent_Level);

      --  Special case: all pragmas are ON
      for J in Asis.Pragma_Kinds'(An_All_Calls_Remote_Pragma) ..
               Asis.Pragma_Kinds'(An_Implementation_Defined_Pragma)
      loop
         if Pragma_Check_Switch (J) /= On then
            All_On := False;
            exit;
         end if;
      end loop;

      if All_On then

         XML_Report
           ("<parameter>ALL</parameter>",
            Indent_Level + 1);
         goto Done;
      end if;

      --  Standard Ada pragmas
      for J in Asis.Pragma_Kinds'(An_All_Calls_Remote_Pragma) ..
               Asis.Pragma_Kinds'(An_Unsuppress_Pragma)
      loop

         if Pragma_Check_Switch (J) = On then

            XML_Report
              ("<parameter>" & Ada_Pragma_Identifier (J) & "</parameter>",
               Indent_Level + 1);
         end if;

      end loop;

      case Pragma_Check_Switch (An_Implementation_Defined_Pragma) is
         when Off =>
            null;
         when On =>
            XML_Report
              ("<parameter>GNAT</parameter>",
               Indent_Level + 1);
         when Selective =>
            for J in GNAT_Pragma_Check_Switch'Range loop

               if GNAT_Pragma_Check_Switch (J) then
                  XML_Report
                    ("<parameter>" & GNAT_Pragma_Identifier (J) &
                     "</parameter>",
                     Indent_Level + 1);
               end if;

            end loop;

      end case;

      <<Done>>
      XML_Report ("</rule>", Indent_Level);
   end XML_Print_Rule;

   ---------------------------------------
   -- XML_Rule_Help (Forbidden_Pragmas) --
   ---------------------------------------

   procedure XML_Rule_Help
     (Rule  : Forbidden_Pragmas_Rule_Type;
      Level : Natural)
   is
   begin
      Info (Level * Ident_String                               &
            "<check  switch=""+R"                              &
            Rule.Name.all                                      &
            ":ALL"""                                           &
            " label="""                                        &
            "detect all pragmas except explicitly disabled""/>");

      Info (Level * Ident_String                                &
            "<check  switch=""+R"                               &
            Rule.Name.all                                       &
            ":GNAT"""                                           &
            " label="""                                         &
            "detect all GNAT pragmas except explicitly disabled""/>");

      Info (Level * Ident_String                                &
            "<field switch=""+R"                                &
            Rule.Name.all                                       &
            """ label="""                                       &
            "detect specified pragmas (use ',' as separator)""" &
            " separator="":"""                                  &
            "/>");

      Info (Level * Ident_String                                       &
            "<field switch=""-R"                                       &
            Rule.Name.all                                              &
            """ label="""                                              &
            "do not detect specified pragmas (use ',' as separator)""" &
            " separator="":"""                                         &
            "/>");
   end XML_Rule_Help;

   -----------------------------
   -- Function_Style_Procedures --
   -----------------------------

   -------------------------------------------
   -- Init_Rule (Function_Style_Procedures) --
   -------------------------------------------

   procedure Init_Rule (Rule : in out Function_Style_Procedures_Rule_Type) is
   begin
      Init_Rule (Rule_Template (Rule));

      Rule.Name        := new String'("Function_Style_Procedures");
      Rule.Synonym     := new String'("Functionlike_Procedures");
      Rule.Rule_Status := Fully_Implemented;
      Rule.Help_Info   := new String'("procedures looking like functions");
      Rule.Diagnosis  := new String'("procedure can be rewritten as function");
   end Init_Rule;

   ---------------------------------------------------
   -- Rule_Check_Pre_Op (Function_Style_Procedures) --
   ---------------------------------------------------

   procedure Rule_Check_Pre_Op
     (Rule    : in out Function_Style_Procedures_Rule_Type;
      Element :        Asis.Element;
      Control : in out Traverse_Control;
      State   : in out Rule_Traversal_State)
   is
      pragma Unreferenced (Rule, Control);
      Consider_Replacement_With_Function : Boolean := False;
   begin

      case Declaration_Kind (Element) is

         when A_Procedure_Declaration =>
            Consider_Replacement_With_Function :=
              Definition_Kind (Get_Enclosing_Element) /=
                A_Protected_Definition;

         when A_Generic_Procedure_Declaration |
              A_Formal_Procedure_Declaration  =>
            Consider_Replacement_With_Function := True;

         when A_Procedure_Body_Declaration |
              A_Procedure_Body_Stub        =>
            Consider_Replacement_With_Function := Acts_As_Spec (Element);

         when others =>
            null;
      end case;

      if Consider_Replacement_With_Function then
         State.Detected := Can_Be_Replaced_With_Function (Element);
      end if;

   end Rule_Check_Pre_Op;

   -----------------------------
   -- Generics_In_Subprograms --
   -----------------------------

   -----------------------------------------
   -- Init_Rule (Generics_In_Subprograms) --
   -----------------------------------------

   procedure Init_Rule (Rule : in out Generics_In_Subprograms_Rule_Type) is
   begin
      Init_Rule (Rule_Template (Rule));

      Rule.Name        := new String'("Generics_In_Subprograms");
      Rule.Rule_Status := Fully_Implemented;
      Rule.Help_Info   := new String'("definitions of generic units in " &
                                      " subprogram bodies");
      Rule.Diagnosis   := new String'("generic definition in subprogram " &
                                      "body starting at line %1%");
   end Init_Rule;

   -------------------------------------------------
   -- Rule_Check_Pre_Op (Generics_In_Subprograms) --
   -------------------------------------------------

   procedure Rule_Check_Pre_Op
     (Rule    : in out Generics_In_Subprograms_Rule_Type;
      Element :        Asis.Element;
      Control : in out Traverse_Control;
      State   : in out Rule_Traversal_State)
   is
      pragma Unreferenced (Rule, Control);
      Encl_Body : Asis.Element;
      Step_Up   : Elmt_Idx := 0;
   begin

      if Declaration_Kind (Element) in A_Generic_Declaration then
         Encl_Body := Get_Enclosing_Element;

         while not Is_Nil (Encl_Body) loop

            case Declaration_Kind (Encl_Body) is
               when A_Procedure_Body_Declaration |
                    A_Function_Body_Declaration  =>
                  State.Detected := True;
                  exit;
               when A_Generic_Package_Declaration =>
                  exit;
               when others =>
                  Step_Up   := Step_Up + 1;
                  Encl_Body := Get_Enclosing_Element (Step_Up);
            end case;
         end loop;

         if State.Detected then
            State.Diag_Params := Enter_String ("%1%" &
                                 Element_Span (Encl_Body).First_Line'Img);
         end if;

      end if;

   end Rule_Check_Pre_Op;

   -----------------------
   -- Identifier_Casing --
   -----------------------

   --------------------------------------------------------
   -- Data structures and local subprograms for the rule --
   --------------------------------------------------------

   type Identifier_Casing_Parameter_Kinds is
     (Not_A_Parameter,
      Type_Par,
      Constant_Par,
      Exception_Par,
      Enum_Par,
      Others_Par,
      Exclude_Par);

   type Wildcard_Kinds is
     (Not_A_Wildcard,
      Left,    --  ABC*
      Right,   --  *ABC
      Both);   --  *ABC*
   --  ???

   function Get_Pattern (W : String; WK : Wildcard_Kinds) return String;
   --  W is supposed to be a wildcard with '*' stripped away. The function
   --  returns a pattern that is stored for the given wildcard. If 'ABC' is the
   --  actual for W the result is:
   --
   --  WK = Not_A_Wildcard   -> ABC
   --  WK = Left             -> ABC_
   --  WK = Right            -> _ABC
   --  WK = Both             -> _ABC_

   procedure Check_With_Word_Dictionary
     (Name        :        Program_Text_Access;
      Dict        :        String_Access_Sets.Set;
      State       : in out Rule_Traversal_State;
      Not_In_Dict :    out Boolean);
   --  Check Name against dictionary Dict, State is set according to the
   --  results of the check. Dict is the dictionary that contains only whole
   --  words but not wildcards. Not_In_Dict is set to False if Name is found in
   --  Dict and True otherwise

   function Get_Diag_Variant (E : Asis.Element) return Diagnosis_Variant;
   --  Detects the diagnosis variant from the argument.

   function Get_Identifier_Casing_Parameter_Kind
     (S    : String)
      return Identifier_Casing_Parameter_Kinds;
   --  If S denotes one of the rule parameters, returns the corresponding
   --  parameter kind, otherwise Not_A_Parameter is returned

   function Get_Casing_Scheme (S : String) return Casing_Schemes;
   --  If S represents one of the casing schemes, returns the corresponding
   --  literal of Casing_Schemes, and Not_A_Casing_Scheme otherwise.

   procedure Scan_Dictionary_File
     (Stored_Exceptions : in out String_Access_Sets.Set;
      Stored_Wildcards  : in out Wildcard_Sets.Set;
      D_File_Name       :        String_Access);
   --  If D_File_Name is the name of an existing file, scans it as a dictionary
   --  file and places all the valid casing exceptions into Stored_Exceptions.

   procedure Check_Casing
     (Name      : Program_Text_Access;
      Wildcards : Wildcard_Sets.Set;
      Diag_Var  : Diagnosis_Variant;
      Rule      : Identifier_Casing_Rule_Type;
      State     : in out Rule_Traversal_State);
   --  Checks Name against specified casing scheme and the wildcards exceptions
   --  specified. In case if the argument correspond to some wildcard, the
   --  check is made that the parts specified by wildcard have the same casing
   --  as in wildcard, and the rest - casings specified by the Casing parameter
   --  (this check is skipped if Casing is equal to Not_A_Casing_Scheme). State
   --  is set according to the check results

   procedure Find_Next_Pattern
     (Name          :     String;
      Wildcards     :     Wildcard_Sets.Set;
      Success       : out Boolean;
      Pattern_Start : out Natural;
      Pattern       : out String_Access;
      Orig_Wilcard  : out String_Access);
   --  Checks if Name contains any pattern contained in wildcard dictionary.
   --  If it does not, sets Success OFF, and all the other out parameters are
   --  undefined. Otherwise sets Success ON, Pattern_Start is the index of the
   --  start of the pattern in Name, Pattern is the corresponding wildcard with
   --  '*' cut off, and Orig_Wilcard is the corresponding wildcard in the
   --  dictionary file.

   function Follow_Casing_Scheme
     (Str        : Program_Text;
      Casing     : Casing_Schemes;
      Word_Start : Boolean)
      return       Boolean;
   --  Checks if Str that is treated as a part of an identifier satisfies the
   --  Casing. If Word_Start is ON Str is considered as the start of the name
   --  or a part of the name immediately following the underscore (important
   --  for mixed case scheme)

   function Required_Casing_Scheme
     (Diag_Var : Diagnosis_Variant;
      Rule     : Identifier_Casing_Rule_Type)
      return     Casing_Schemes;
   --  Using the variant of the diagnosis as the way to detect the
   --  corresponding kind of the entities to check, gets from Rule the
   --  corresponding casing scheme. If for the entity kind that corresponds to
   --  Diag_Var no casing scheme is set, tries the casing defined for others
   --  entities.

   -----------------------------
   -- "<" (Identifier_Casing) --
   -----------------------------

   function "<" (Left, Right : Wildcard_Rec) return Boolean is
   begin
      return Left.Img < Right.Img;
   end "<";

   -----------------------------------------------
   -- Activate_In_Test_Mode (Identifier_Casing) --
   -----------------------------------------------

   overriding procedure Activate_In_Test_Mode
     (Rule : in out Identifier_Casing_Rule_Type)
   is
   begin
      Process_Rule_Parameter
        (Rule       => Rule,
         Param      => "Type=upper",
         Enable     => True,
         Defined_At => "");

      Process_Rule_Parameter
        (Rule       => Rule,
         Param      => "Enum=mixed",
         Enable     => True,
         Defined_At => "");

      Process_Rule_Parameter
        (Rule       => Rule,
         Param      => "Constant=lower",
         Enable     => True,
         Defined_At => "");

      Process_Rule_Parameter
        (Rule       => Rule,
         Param      => "Exception=upper",
         Enable     => True,
         Defined_At => "");

      Process_Rule_Parameter
        (Rule       => Rule,
         Param      => "Others=mixed",
         Enable     => True,
         Defined_At => "");

      --  Now manually define some exceptions:
      String_Access_Sets.Insert (Rule.Exclude,  new String'("UNIT"));
      String_Access_Sets.Insert (Rule.Exclude,  new String'("ASIS"));
      String_Access_Sets.Insert (Rule.Exclude,  new String'("bits1"));

      Wildcard_Sets.Insert
        (Rule.Wilcards,
         (Img      => new String'(Get_Pattern ("_IO", Right)),
          Orig_Img => new String'("*_IO")));

      Wildcard_Sets.Insert
        (Rule.Wilcards,
         (Img      => new String'(Get_Pattern ("_IO", Not_A_Wildcard)),
          Orig_Img => new String'("*_IO")));

   end Activate_In_Test_Mode;

   --------------------------------------------------------
   -- Allowed_As_Exemption_Parameter (Identifier_Casing) --
   --------------------------------------------------------

   overriding function Allowed_As_Exemption_Parameter
     (Rule      : Identifier_Casing_Rule_Type;
      Parameter : String)
      return  Boolean
   is
      pragma Unreferenced (Rule);
      Par : constant String := To_Lower (Parameter);
   begin
      return Par = "type"
           or else Par = "constant"
           or else Par = "enum"
           or else Par = "exception"
           or else Par = "others"
           or else Par = "exclude";
   end Allowed_As_Exemption_Parameter;

   ---------------------------------------
   -- Annotate_Rule (Identifier_Casing) --
   ---------------------------------------

   overriding function Annotate_Rule
     (Rule : Identifier_Casing_Rule_Type;
      Var  : Diagnosis_Variant := 0)
      return String
   is
   begin
      if not Gnatcheck.Options.Mapping_Mode then
         return "";
      else
         if Var = 1 and then Rule.Type_Casing_Synonym /= null then
            return " [" & Rule.Type_Casing_Synonym.all & "]";
         elsif Var = 2 and then Rule.Constant_Casing_Synonym /= null then
            return " [" & Rule.Constant_Casing_Synonym.all & "]";
         elsif Var = 3 and then Rule.Enum_Casing_Synonym /= null then
            return " [" & Rule.Enum_Casing_Synonym.all & "]";
         elsif Var = 4 and then Rule.Exception_Casing_Synonym /= null then
            return " [" & Rule.Exception_Casing_Synonym.all & "]";
         elsif Var = 5 and then Rule.Others_Casing_Synonym /= null then
            return " [" & Rule.Others_Casing_Synonym.all & "]";
         elsif Var = 6 and then Rule.Exclude_Synonym /= null then
            return " [" & Rule.Exclude_Synonym.all & "]";

         else

            return " [" & Rule_Name (Rule) & ':' &
                   (case Var is
                       when 1 => "Type",
                       when 2 => "Constant",
                       when 3 => "Enum",
                       when 4 => "Exception",
                       when 5 => "Others",
                       when 6 => "Exclude",
                       when others => "")
                   & "]";
         end if;
      end if;

   end Annotate_Rule;

   --------------------------------------
   -- Check_Casing (Identifier_Casing) --
   --------------------------------------

   procedure Check_Casing
     (Name      : Program_Text_Access;
      Wildcards : Wildcard_Sets.Set;
      Diag_Var  : Diagnosis_Variant;
      Rule      : Identifier_Casing_Rule_Type;
      State     : in out Rule_Traversal_State)
   is
      Success                 : Boolean;
      Tmp                     : String_Access;
      Tmp_Lowercase           : String_Access;
      Wildcard                : String_Access;
      Orig_Wildcard           : String_Access;
      First_N_Idx             : Natural;
      Name_Last               : Natural;
      Casing                  : constant Casing_Schemes :=
        Required_Casing_Scheme (Diag_Var, Rule);

      Pattern_Start : Natural;
      Pattern_End   : Natural;
   begin

      Tmp := new String'(To_String (Name.all));

      if Wildcard_Sets.Is_Empty (Wildcards) then
         --  A simple case, no wildcard involved
         if not Follow_Casing_Scheme
                  (Name.all, Casing, Word_Start => True)
         then
            State.Detected    := True;
            State.Diagnosis   := Diag_Var;
            State.Diag_Params :=
               Enter_String
                ("%1%" & Tmp.all
                 &
                 "%2%" & To_Lower
                    (Required_Casing_Scheme (Diag_Var, Rule)'Img));
         end if;

         Free (Tmp);

         return;
      end if;

      Tmp_Lowercase := new String'(To_Lower (To_String (Name.all)));

      First_N_Idx := Tmp_Lowercase'First;
      Name_Last   := Tmp_Lowercase'Last;

      Traverse_Name : while First_N_Idx <= Tmp'Last loop

         Find_Next_Pattern
           (Name          => Tmp_Lowercase (First_N_Idx .. Name_Last),
            Wildcards     => Wildcards,
            Success       => Success,
            Pattern_Start => Pattern_Start,
            Pattern       => Wildcard,
            Orig_Wilcard  => Orig_Wildcard);

         if Success then

            --  Check if the part of the name before pattern follows the casing
            --  scheme:

            if not Follow_Casing_Scheme
                     (Str        => Name (First_N_Idx .. Pattern_Start - 1),
                      Casing     => Casing,
                      Word_Start => True)
            then
               State.Detected    := True;
               State.Diagnosis   := Diag_Var;
               State.Diag_Params :=
                  Enter_String
                   ("%1%" & Tmp.all
                    &
                    "%2%" & To_Lower (Required_Casing_Scheme
                       (Diag_Var, Rule)'Img));

               exit Traverse_Name;
            end if;

            --  Check if the pattern has the correct casing

            Pattern_End :=
              Pattern_Start + Wildcard.all'Length - 1;

            if Tmp (Pattern_Start .. Pattern_End) /= Wildcard.all then
               State.Detected    := True;
               State.Diagnosis   := 6;
               State.Diag_Params :=
                  Enter_String
                   ("%1%" & Tmp.all
                    &
                    "%2%" & Orig_Wildcard.all);
               exit Traverse_Name;
            end if;

            --  Corner case of 'A' or abcd_D
            exit Traverse_Name when Pattern_End = Name_Last;

            First_N_Idx := Pattern_End;

         else
            --  Check if the rest of the word follows the casing scheme.
            if Name (First_N_Idx) = '_' then
               First_N_Idx := First_N_Idx + 1;
            end if;

            if not Follow_Casing_Scheme
                     (Str        => Name (First_N_Idx .. Name_Last),
                      Casing     => Casing,
                      Word_Start => True)
            then
               State.Detected    := True;
               State.Diagnosis   := Diag_Var;
               State.Diag_Params :=
                  Enter_String
                   ("%1%" & Tmp.all
                    &
                    "%2%" & To_Lower (Required_Casing_Scheme
                       (Diag_Var, Rule)'Img));

            end if;

            exit Traverse_Name;

         end if;

      end loop Traverse_Name;

      Free (Tmp);
      Free (Tmp_Lowercase);
   end Check_Casing;

   ----------------------------------------------------
   -- Check_With_Word_Dictionary (Identifier_Casing) --
   ----------------------------------------------------

   procedure Check_With_Word_Dictionary
     (Name        :        Program_Text_Access;
      Dict        :        String_Access_Sets.Set;
      State       : in out Rule_Traversal_State;
      Not_In_Dict :    out Boolean)
   is
      C   : String_Access_Sets.Cursor;
      Tmp : String_Access;

   begin
      Not_In_Dict := True;

      Tmp := new String'(To_String (Name.all));

      if not String_Access_Sets.Is_Empty (Dict) then

         C := String_Access_Sets.Find (Container => Dict, Item => Tmp);

         if String_Access_Sets.Has_Element (C) then
            Not_In_Dict := False;

            if String_Access_Sets.Element (C).all /= Tmp.all then
               State.Detected  := True;
               State.Diagnosis := 6;
               State.Diag_Params :=
                 Enter_String
                   ("%1%" & Tmp.all
                    &
                    "%2%" & String_Access_Sets.Element (C).all);
            end if;
         end if;
      end if;

      Free (Tmp);

   end Check_With_Word_Dictionary;

   --------
   -- Eq --
   --------

   function Eq  (Left, Right : Wildcard_Rec) return Boolean is
   begin
      return Left.Img = Right.Img;
   end Eq;

   -------------------------------------------
   -- Find_Next_Pattern (Identifier_Casing) --
   -------------------------------------------

   procedure Find_Next_Pattern
     (Name          :     String;
      Wildcards     :     Wildcard_Sets.Set;
      Success       : out Boolean;
      Pattern_Start : out Natural;
      Pattern       : out String_Access;
      Orig_Wilcard  : out String_Access)
   is
      Pattern_C     :          Wildcard_Sets.Cursor;
      Pattern_W     :          Wildcard_Rec;
      Pattern_End   :          Natural;
      Name_End      : constant Natural := Name'Last;
   begin
      Success       := False;
      Pattern_Start := Name'First;

      Traverse_Name : while Pattern_Start <= Name_End loop
         Pattern_End := 0;

         Find_Subword_End : for J in Pattern_Start + 1 .. Name_End loop
            if Name (J) = '_' then
               Pattern_End := J;
               exit Find_Subword_End;
            end if;
         end loop Find_Subword_End;

         if Pattern_End = 0 then
            Pattern_End := Name_End;
         end if;

         Pattern_W.Img := new String'(Name (Pattern_Start .. Pattern_End));
         Pattern_C     := Wildcard_Sets.Find (Wildcards, Pattern_W);
         Free (Pattern_W.Img);

         if Wildcard_Sets.Has_Element (Pattern_C) then
            Success       := True;
            Pattern       := Wildcard_Sets.Element (Pattern_C).Img;
            Orig_Wilcard  := Wildcard_Sets.Element (Pattern_C).Orig_Img;
            exit Traverse_Name;
         else
            --  Corner case of 'A' or abcd_D
            exit Traverse_Name when Pattern_Start = Pattern_End;

            Pattern_Start := Pattern_End;
         end if;

      end loop Traverse_Name;

   end Find_Next_Pattern;

   ----------------------------------------------
   -- Follow_Casing_Scheme (Identifier_Casing) --
   ----------------------------------------------

   function Follow_Casing_Scheme
     (Str        : Program_Text;
      Casing     : Casing_Schemes;
      Word_Start : Boolean)
      return       Boolean
   is
      Result         : Boolean := True;
      Word_Start_Tmp : Boolean := Word_Start;
   begin
      case Casing is
         when Lower =>
            if Str /= To_Lower_Case (Str) then
               Result := False;
            end if;
         when Upper =>
            if Str /= To_Upper_Case (Str) then
               Result :=  False;
            end if;
         when Mixed =>
            for J in Str'Range loop

               if Word_Start_Tmp then
                  if Ada.Wide_Characters.Unicode.Is_Letter (Str (J))
                    and then
                     Str (J) /=
                     Ada.Wide_Characters.Unicode.To_Upper_Case (Str (J))
                  then
                     Result := False;
                     exit;
                  elsif Str (J) /= '_' then
                     Word_Start_Tmp := False;
                  end if;
               elsif Str (J) = '_' then
                  Word_Start_Tmp := True;
               else
                  if Ada.Wide_Characters.Unicode.Is_Letter (Str (J))
                    and then
                     Str (J) /=
                     Ada.Wide_Characters.Unicode.To_Lower_Case (Str (J))
                  then
                     Result := False;
                     exit;
                  end if;
               end if;
            end loop;
         when Not_A_Casing_Scheme => null;
      end case;

      return Result;
   end Follow_Casing_Scheme;

   -------------------------------------------
   -- Get_Casing_Scheme (Identifier_Casing) --
   -------------------------------------------

   function Get_Casing_Scheme (S : String) return Casing_Schemes is
   begin
      return Casing_Schemes'Value (S);
   exception
      when Constraint_Error =>
         return Not_A_Casing_Scheme;
   end Get_Casing_Scheme;

   ------------------------------------------
   -- Get_Diag_Variant (Identifier_Casing) --
   ------------------------------------------

   function Get_Diag_Variant (E : Asis.Element) return Diagnosis_Variant is
      Result : Diagnosis_Variant;
      Tmp    : Asis.Element;
   begin
      case Flat_Element_Kind (E) is
         when A_Defining_Enumeration_Literal =>
            Result := 3;

         when A_Constant_Declaration          |
              A_Deferred_Constant_Declaration |
              An_Integer_Number_Declaration   |
              A_Real_Number_Declaration       =>
            Result := 2;

         when An_Exception_Declaration          |
              An_Exception_Renaming_Declaration =>
            Result := 4;

         when An_Ordinary_Type_Declaration         |
              A_Task_Type_Declaration              |
              A_Protected_Type_Declaration         |
              A_Private_Type_Declaration           |
              A_Private_Extension_Declaration      |
              A_Formal_Type_Declaration            |
              An_Incomplete_Type_Declaration       |
              A_Tagged_Incomplete_Type_Declaration |
              A_Subtype_Declaration                =>
            Result := 1;

         when A_Task_Body_Declaration      |
              A_Protected_Body_Declaration =>

            if Is_Subunit (E) then
               Tmp := Corresponding_Body_Stub (E);
            else
               Tmp := E;
            end if;

            if Declaration_Kind (Corresponding_Declaration (Tmp)) in
               A_Task_Type_Declaration .. A_Protected_Type_Declaration
            then
               Result := 1;
            else
               Result := 5;
            end if;

         when A_Task_Body_Stub      |
              A_Protected_Body_Stub =>
            if Declaration_Kind (Corresponding_Declaration (E)) in
               A_Task_Type_Declaration .. A_Protected_Type_Declaration
            then
               Result := 1;
            else
               Result := 5;
            end if;

         when An_Object_Renaming_Declaration =>

            if Is_Constant (First_Name (E)) then
               Result := 2;
            else
               Result := 5;
            end if;

         when A_Function_Renaming_Declaration =>
            Tmp := Corresponding_Base_Entity (E);

            if Expression_Kind (Tmp) = A_Selected_Component then
               Tmp := Selector (Tmp);
            end if;

            if Expression_Kind (Tmp) = An_Enumeration_Literal then
               Result := 3;
            else
               Result := 5;
            end if;

         when others =>
            Result := 5;

      end case;

      return Result;
   end Get_Diag_Variant;

   --------------------------------------------------------------
   -- Get_Identifier_Casing_Parameter_Kind (Identifier_Casing) --
   --------------------------------------------------------------

   function Get_Identifier_Casing_Parameter_Kind
     (S    : String)
      return Identifier_Casing_Parameter_Kinds
   is
   begin
      return Identifier_Casing_Parameter_Kinds'Value (S & "_Par");
   exception
      when Constraint_Error =>
         return Not_A_Parameter;
   end Get_Identifier_Casing_Parameter_Kind;

   --------------------------------------
   -- Get_Pattern (Identifier_Casing) --
   --------------------------------------

   function Get_Pattern (W : String; WK : Wildcard_Kinds) return String is
   begin
      case WK is
         when Not_A_Wildcard =>
            return W;
         when Left =>
            return W & '_';
         when Right =>
            return '_' &  W;
         when Both =>
            return '_' & W & '_';
      end case;
   end Get_Pattern;

   -----------------------------------
   -- Init_Rule (Identifier_Casing) --
   -----------------------------------

   overriding procedure Init_Rule
     (Rule : in out Identifier_Casing_Rule_Type)
   is
   begin
      Init_Rule (Rule_Template (Rule));

      Rule.Name             := new String'("Identifier_Casing");
      Rule.Rule_Status      := Fully_Implemented;
      Rule.Help_Info        := new String'("casing of defining names");
      Rule.Type_Casing      := Not_A_Casing_Scheme;
      Rule.Enum_Casing      := Not_A_Casing_Scheme;
      Rule.Constant_Casing  := Not_A_Casing_Scheme;
      Rule.Exception_Casing := Not_A_Casing_Scheme;
      Rule.Others_Casing    := Not_A_Casing_Scheme;

      Rule.Type_Casing_Def_At      := Nil_String_Loc;
      Rule.Enum_Casing_Def_At      := Nil_String_Loc;
      Rule.Constant_Casing_Def_At  := Nil_String_Loc;
      Rule.Exception_Casing_Def_At := Nil_String_Loc;
      Rule.Others_Casing_Def_At    := Nil_String_Loc;

      Rule.Exclude          := String_Access_Sets.Empty_Set;
      Rule.Dictionaries     := String_Access_Sets.Empty_Set;

      Rule.Diagnosis :=
        new String'("#1#%1% does not have casing specified for subtype "     &
                    "names (%2%)"                                            &

                    "#2#%1% does not have casing specified for constant "    &
                    "names (%2%)"                                            &

                    "#3#%1% does not have casing specified for enumeration " &
                    "literals (%2%)"                                         &

                    "#4#%1% does not have casing specified for exception "   &
                    "names  (%2%)"                                           &

                    "#5#%1% does not have casing specified (%2%)"            &

                    "#6#%1% does not have casing specified in the "         &
                    "dictionary (%2%)");
   end Init_Rule;

   ------------------------------------
   -- Print_Rule (Identifier_Casing) --
   ------------------------------------

   overriding procedure Print_Rule
     (Rule         : Identifier_Casing_Rule_Type;
      Indent_Level : Natural := 0)
   is
      First_Param       : Boolean         := True;
      Rule_Name_Padding : constant String :=
        (1 .. Rule.Name'Length + 2 => ' ');
      C : String_Access_Sets.Cursor;
   begin
      Print_Rule (Rule_Template (Rule), Indent_Level);

      if Rule.Type_Casing /= Not_A_Casing_Scheme then
         Report_No_EOL (": Type = " & To_Lower (Rule.Type_Casing'Img));
         First_Param := False;
      end if;

      if Rule.Enum_Casing /= Not_A_Casing_Scheme then
         if First_Param then
            Report_No_EOL (": Enum = " & To_Lower (Rule.Enum_Casing'Img));
            First_Param := False;
         else
            Report (", ");
            Report_No_EOL
              (Rule_Name_Padding &
               "Enum = " & To_Lower (Rule.Enum_Casing'Img),
              Indent_Level);
         end if;
      end if;

      if Rule.Constant_Casing /= Not_A_Casing_Scheme then
         if First_Param then
            Report_No_EOL (": Constant = " &
                           To_Lower (Rule.Constant_Casing'Img));
            First_Param := False;
         else
            Report (", ");
            Report_No_EOL
              (Rule_Name_Padding &
               "Constant = " & To_Lower (Rule.Constant_Casing'Img),
              Indent_Level);
         end if;
      end if;

      if Rule.Exception_Casing /= Not_A_Casing_Scheme then
         if First_Param then
            Report_No_EOL (": Exception = " &
                           To_Lower (Rule.Exception_Casing'Img));
            First_Param := False;
         else
            Report (", ");
            Report_No_EOL
              (Rule_Name_Padding &
               "Exception = " & To_Lower (Rule.Exception_Casing'Img),
              Indent_Level);
         end if;
      end if;

      if Rule.Others_Casing /= Not_A_Casing_Scheme then
         if First_Param then
            Report_No_EOL (": Others = " &
                           To_Lower (Rule.Others_Casing'Img));
            First_Param := False;
         else
            Report (", ");
            Report_No_EOL
              (Rule_Name_Padding &
               "Others = " & To_Lower (Rule.Others_Casing'Img),
              Indent_Level);
         end if;
      end if;

      if not String_Access_Sets.Is_Empty (Rule.Dictionaries) then
         C := String_Access_Sets.First (Rule.Dictionaries);

         while C /= String_Access_Sets.No_Element loop

            if First_Param then
               Report_No_EOL (": Exclude  = " &
                              String_Access_Sets.Element (C).all);
               First_Param := False;
            else
               Report (", ");
               Report_No_EOL
                 (Rule_Name_Padding &
                  "Exclude  = " & String_Access_Sets.Element (C).all,
                 Indent_Level);
            end if;

            C := Next (C);
         end loop;
      end if;
   end Print_Rule;

   --------------------------------------------
   -- Print_Rule_To_File (Identifier_Casing) --
   --------------------------------------------

   overriding procedure Print_Rule_To_File
     (Rule         : Identifier_Casing_Rule_Type;
      Rule_File    : File_Type;
      Indent_Level : Natural := 0)
   is
      First_Param       : Boolean         := True;
      Rule_Name_Padding : constant String :=
        (1 .. Rule.Name'Length + 4 => ' ');
      C : String_Access_Sets.Cursor;
   begin
      Print_Rule_To_File (Rule_Template (Rule), Rule_File, Indent_Level);

      if Rule.Type_Casing /= Not_A_Casing_Scheme then
         Put (Rule_File, ": Type = " & To_Lower (Rule.Type_Casing'Img));
         First_Param := False;
      end if;

      if Rule.Enum_Casing /= Not_A_Casing_Scheme then
         if First_Param then
            Put (Rule_File, ": Enum = " & To_Lower (Rule.Enum_Casing'Img));
            First_Param := False;
         else
            Put_Line (Rule_File, ", ");

            for J in 1 .. Indent_Level loop
               Put (Rule_File, Get_Indent_String);
            end loop;

            Put (Rule_File,
                 Rule_Name_Padding &
                 "Enum = " & To_Lower (Rule.Enum_Casing'Img));
         end if;
      end if;

      if Rule.Constant_Casing /= Not_A_Casing_Scheme then
         if First_Param then
            Put (Rule_File, ": Constant = " &
                            To_Lower (Rule.Constant_Casing'Img));
            First_Param := False;
         else
            Put_Line (Rule_File, ", ");

            for J in 1 .. Indent_Level loop
               Put (Rule_File, Get_Indent_String);
            end loop;

            Put (Rule_File,
                 Rule_Name_Padding &
                 "Constant = " & To_Lower (Rule.Constant_Casing'Img));
         end if;
      end if;

      if Rule.Exception_Casing /= Not_A_Casing_Scheme then
         if First_Param then
            Put (Rule_File, ": Exception = " &
                            To_Lower (Rule.Exception_Casing'Img));
            First_Param := False;
         else
            Put_Line (Rule_File, ", ");

            for J in 1 .. Indent_Level loop
               Put (Rule_File, Get_Indent_String);
            end loop;

            Put (Rule_File,
                 Rule_Name_Padding &
                 "Exception = " & To_Lower (Rule.Exception_Casing'Img));
         end if;
      end if;

      if Rule.Others_Casing /= Not_A_Casing_Scheme then
         if First_Param then
            Put (Rule_File, ": Others = " &
                           To_Lower (Rule.Others_Casing'Img));
            First_Param := False;
         else
            Put_Line (Rule_File, ", ");

            for J in 1 .. Indent_Level loop
               Put (Rule_File, Get_Indent_String);
            end loop;

            Put (Rule_File,
                 Rule_Name_Padding &
                 "Others = " & To_Lower (Rule.Others_Casing'Img));
         end if;
      end if;

      if not String_Access_Sets.Is_Empty (Rule.Dictionaries) then
         C := String_Access_Sets.First (Rule.Dictionaries);

         while C /= String_Access_Sets.No_Element loop

            if First_Param then
               Put (Rule_File, ": Exclude  = " &
                              String_Access_Sets.Element (C).all);
               First_Param := False;
            else
               Put_Line (Rule_File, ", ");

               for J in 1 .. Indent_Level loop
                  Put (Rule_File, Get_Indent_String);
               end loop;

               Put (Rule_File,
                    Rule_Name_Padding &
                    "Exclude  = " & String_Access_Sets.Element (C).all);
            end if;

            C := Next (C);
         end loop;
      end if;
   end Print_Rule_To_File;

   ------------------------------------------------
   -- Process_Rule_Parameter (Identifier_Casing) --
   ------------------------------------------------

   overriding procedure Process_Rule_Parameter
     (Rule       : in out Identifier_Casing_Rule_Type;
      Param      :        String;
      Enable     :        Boolean;
      Defined_At : String)
   is
      First_Str_Idx, Last_Str_Idx : Natural;
      --  Beginning and end of the 'string' part of the parameter, see the
      --  rule parameter description in the spec. First_Str_Idx is set to 0 if
      --  the parameter does not contain a '=' character.

      First_Par_Idx, Last_Par_Idx : Natural;
      --  If the parameter contains a '=' character, set to point to the
      --  beginning and the end of the part of the parameter that precedes '='.
      --  Otherwise First_Par_Idx points to the first, and Last_Par_Idx - to
      --  the last non-blank character in Param (First_Idx .. Last_Idx)

      Parameter_Kind : Identifier_Casing_Parameter_Kinds;
      Casing_Scheme  : Casing_Schemes;

      C        : String_Access_Sets.Cursor;
      Inserted : Boolean := False;
      Tmp_Str  : String_Access;
   begin
      if Param = "" then
         if Enable then
            Error ("(" & Rule.Name.all & ") +R option must have a parameter");
         else
            Rule.Rule_State := Disabled;
         end if;

         return;
      elsif not Enable then
         Error ("(" & Rule.Name.all & ") -R option should not " &
                "have a parameter");
      end if;

      Parse_Par
        (First_Par_Idx, Last_Par_Idx, First_Str_Idx, Last_Str_Idx, Param);

      Parameter_Kind :=
        Get_Identifier_Casing_Parameter_Kind
          (Param (First_Par_Idx .. Last_Par_Idx));

      if Parameter_Kind = Not_A_Parameter or else First_Str_Idx = 0 then
         Error ("(" & Rule.Name.all & ") wrong parameter: " &
                Param & ", ignored");
         return;
      end if;

      --  If we are here, we have "+R<valid_par>=string"
      if Parameter_Kind in Type_Par .. Others_Par then
         Casing_Scheme :=
           Get_Casing_Scheme (Param (First_Str_Idx .. Last_Str_Idx));
         if Casing_Scheme = Not_A_Casing_Scheme then
            Error ("(" & Rule.Name.all & ") wrong casing scheme: " &
                   Param & ", ignored");
            return;
         end if;
      end if;

      case Parameter_Kind is
         when Type_Par =>
            if Gnatcheck.Options.Check_Param_Redefinition
              and then
               Rule.Type_Casing /= Not_A_Casing_Scheme
              and then
               Rule.Type_Casing /= Casing_Scheme
               --  We do not check if Rule.Rule_State = Enabled because the
               --  disabled rule remembers all the previous settings
            then
               Error
                ("redefining at " &
                 (if Defined_At = "" then
                     "command line"
                  else
                     Defined_At) &
                 " type casing for rule " & Rule.Name.all &
                 " defined at "  &
                 (if Rule.Type_Casing_Def_At = Nil_String_Loc then
                     "command line"
                  else
                     Get_String (Rule.Type_Casing_Def_At)));
            end if;

            Rule.Type_Casing := Casing_Scheme;
            Rule.Type_Casing_Def_At := Enter_String (Defined_At);

            if Has_Synonym (Rule) then
               Free (Rule.Type_Casing_Synonym);
               Rule.Type_Casing_Synonym :=
                 new String'(Rule_Synonym (Rule));
            end if;

         when Constant_Par =>
            if Gnatcheck.Options.Check_Param_Redefinition
              and then
               Rule.Constant_Casing /= Not_A_Casing_Scheme
              and then
               Rule.Constant_Casing /= Casing_Scheme
               --  We do not check if Rule.Rule_State = Enabled because the
               --  disabled rule remembers all the previous settings
            then
               Error
                ("redefining at " &
                 (if Defined_At = "" then
                     "command line"
                  else
                     Defined_At) &
                 " constant casing for rule " & Rule.Name.all &
                 " defined at "  &
                 (if Rule.Constant_Casing_Def_At = Nil_String_Loc then
                     "command line"
                  else
                     Get_String (Rule.Constant_Casing_Def_At)));
            end if;

            Rule.Constant_Casing := Casing_Scheme;
            Rule.Constant_Casing_Def_At := Enter_String (Defined_At);

            if Has_Synonym (Rule) then
               Free (Rule.Constant_Casing_Synonym);
               Rule.Constant_Casing_Synonym :=
                 new String'(Rule_Synonym (Rule));
            end if;

         when Exception_Par =>
            if Gnatcheck.Options.Check_Param_Redefinition
              and then
               Rule.Exception_Casing /= Not_A_Casing_Scheme
              and then
               Rule.Exception_Casing /= Casing_Scheme
               --  We do not check if Rule.Rule_State = Enabled because the
               --  disabled rule remembers all the previous settings
            then
               Error
                ("redefining at " &
                 (if Defined_At = "" then
                     "command line"
                  else
                     Defined_At) &
                 " exception casing for rule " & Rule.Name.all &
                 " defined at "  &
                 (if Rule.Exception_Casing_Def_At = Nil_String_Loc then
                     "command line"
                  else
                     Get_String (Rule.Exception_Casing_Def_At)));
            end if;

            Rule.Exception_Casing := Casing_Scheme;
            Rule.Exception_Casing_Def_At := Enter_String (Defined_At);

            if Has_Synonym (Rule) then
               Free (Rule.Exception_Casing_Synonym);
               Rule.Exception_Casing_Synonym :=
                 new String'(Rule_Synonym (Rule));
            end if;

         when Enum_Par =>
            if Gnatcheck.Options.Check_Param_Redefinition
              and then
               Rule.Enum_Casing /= Not_A_Casing_Scheme
              and then
               Rule.Enum_Casing /= Casing_Scheme
               --  We do not check if Rule.Rule_State = Enabled because the
               --  disabled rule remembers all the previous settings
            then
               Error
                ("redefining at " &
                 (if Defined_At = "" then
                     "command line"
                  else
                     Defined_At) &
                 " enumeration literal casing for rule " & Rule.Name.all &
                 " defined at "  &
                 (if Rule.Enum_Casing_Def_At = Nil_String_Loc then
                     "command line"
                  else
                     Get_String (Rule.Enum_Casing_Def_At)));
            end if;

            Rule.Enum_Casing := Casing_Scheme;
            Rule.Enum_Casing_Def_At := Enter_String (Defined_At);

            if Has_Synonym (Rule) then
               Free (Rule.Enum_Casing_Synonym);
               Rule.Enum_Casing_Synonym :=
                 new String'(Rule_Synonym (Rule));
            end if;

         when Others_Par =>
            if Gnatcheck.Options.Check_Param_Redefinition
              and then
               Rule.Others_Casing /= Not_A_Casing_Scheme
              and then
               Rule.Others_Casing /= Casing_Scheme
               --  We do not check if Rule.Rule_State = Enabled because the
               --  disabled rule remembers all the previous settings
            then
               Error
                ("redefining at " &
                 (if Defined_At = "" then
                     "command line"
                  else
                     Defined_At) &
                 " name casing for rule " & Rule.Name.all &
                 " defined at "  &
                 (if Rule.Others_Casing_Def_At = Nil_String_Loc then
                     "command line"
                  else
                     Get_String (Rule.Others_Casing_Def_At)));
            end if;

            Rule.Others_Casing := Casing_Scheme;
            Rule.Others_Casing_Def_At := Enter_String (Defined_At);

            if Has_Synonym (Rule) then
               Free (Rule.Others_Casing_Synonym);
               Rule.Others_Casing_Synonym :=
                 new String'(Rule_Synonym (Rule));

               if Rule.Type_Casing = Not_A_Casing_Scheme then
                  Rule.Type_Casing_Synonym :=
                    new String'(Rule_Synonym (Rule));
               end if;

               if Rule.Constant_Casing = Not_A_Casing_Scheme then
                  Rule.Constant_Casing_Synonym :=
                    new String'(Rule_Synonym (Rule));
               end if;

               if Rule.Enum_Casing = Not_A_Casing_Scheme then
                  Rule.Enum_Casing_Synonym :=
                    new String'(Rule_Synonym (Rule));
               end if;

               if Rule.Exception_Casing = Not_A_Casing_Scheme then
                  Rule.Exception_Casing_Synonym :=
                    new String'(Rule_Synonym (Rule));
               end if;

            end if;

         when Exclude_Par =>
            Tmp_Str := new String'(Param (First_Str_Idx .. Last_Str_Idx));

            String_Access_Sets.Insert
              (Container => Rule.Dictionaries,
               New_Item  => Tmp_Str,
               Position  => C,
               Inserted  => Inserted);

            if not Inserted then
               Error ("(" & Rule.Name.all & ") dictionary " &
                      Tmp_Str.all & ", specified more than once, " &
                      "all but first ignored");
               Free (Tmp_Str);
               return;
            end if;

            if not Is_Regular_File  (Tmp_Str.all) then
               Error ("(" & Rule.Name.all & ") dictionary " &
                      Tmp_Str.all & " does not exist");
               return;
            end if;

            Scan_Dictionary_File (Rule.Exclude, Rule.Wilcards, Tmp_Str);

            if Has_Synonym (Rule) then
               Free (Rule.Exclude_Synonym);
               Rule.Exclude_Synonym :=
                 new String'(Rule_Synonym (Rule));
            end if;

         when Not_A_Parameter => null;
      end case;

      Rule.Rule_State := Enabled;

   end Process_Rule_Parameter;

   ------------------------------------------------
   -- Required_Casing_Scheme (Identifier_Casing) --
   ------------------------------------------------

   function Required_Casing_Scheme
     (Diag_Var : Diagnosis_Variant;
      Rule     : Identifier_Casing_Rule_Type)
      return     Casing_Schemes
   is
      Result : Casing_Schemes := Not_A_Casing_Scheme;
   begin
      case Diag_Var is
         when 1 =>
            Result := Rule.Type_Casing;
         when 2 =>
            Result := Rule.Constant_Casing;
         when 3 =>
            Result := Rule.Enum_Casing;
         when 4 =>
            Result := Rule.Exception_Casing;
         when 5 =>
            Result := Rule.Others_Casing;
         when others =>
            pragma Assert (False);
            null;
      end case;

      if Result = Not_A_Casing_Scheme then
         Result := Rule.Others_Casing;
      end if;

      return Result;
   end Required_Casing_Scheme;

   -------------------------------------------
   -- Rule_Check_Pre_Op (Identifier_Casing) --
   -------------------------------------------

   overriding procedure Rule_Check_Pre_Op
     (Rule    : in out Identifier_Casing_Rule_Type;
      Element :        Asis.Element;
      Control : in out Traverse_Control;
      State   : in out Rule_Traversal_State)
   is
      pragma Unreferenced (Control);
      Tmp                    : Asis.Element := Element;
      Name_Img               : Program_Text_Access;
      Not_In_Word_Dictionary : Boolean;
   begin

      case Defining_Name_Kind (Element) is
         when A_Defining_Identifier =>
            Tmp := Get_Enclosing_Element;
         when A_Defining_Enumeration_Literal =>
            null;
         when others =>
            return;
      end case;

      Name_Img := new Program_Text'(Defining_Name_Image (Element));

      Check_With_Word_Dictionary
        (Name        => Name_Img,
         Dict        => Rule.Exclude,
         State       => State,
         Not_In_Dict => Not_In_Word_Dictionary);

      if Not_In_Word_Dictionary then
         Check_Casing
           (Name      => Name_Img,
            Wildcards => Rule.Wilcards,
            Diag_Var  => Get_Diag_Variant (Tmp),
            Rule      => Rule,
            State     => State);
      end if;

      Free (Name_Img);
   end Rule_Check_Pre_Op;

   ----------------------------------------
   -- Rule_Parameter (Identifier_Casing) --
   ----------------------------------------

   overriding function Rule_Parameter
     (Rule : Identifier_Casing_Rule_Type;
      Diag : String)
      return String
   is
      pragma Unreferenced (Rule);
      First_Idx : Natural := Index (Diag, " for ");
   begin
      if First_Idx > 0 then
         First_Idx := First_Idx + 5;

         case Diag (First_Idx) is
            when 'c' =>
               return "constant";
            when 'e' =>
               if Diag (First_Idx + 1) = 'n' then
                  return "enum";
               else
                  return "exception";
               end if;

            when 's' =>
               return "type";
            when others =>
               raise Constraint_Error with
                 "Identifier_Casing: bug in exemption parameter processing";
         end case;

      end if;

      First_Idx := Index (Diag, " in ");

      if First_Idx > 0 then
         return "exclude";
      else
         return "others";
      end if;
   end Rule_Parameter;

   ----------------------------------------------
   -- Scan_Dictionary_File (Identifier_Casing) --
   ----------------------------------------------

   procedure Scan_Dictionary_File
     (Stored_Exceptions : in out String_Access_Sets.Set;
      Stored_Wildcards  : in out Wildcard_Sets.Set;
      D_File_Name       :        String_Access)
   is
      Dictionary_File : File_Type;

      String_Buffer_Max_Len : constant Natural := 1024;
      --  Should be enough, I hope...

      String_Buffer : String (1 .. String_Buffer_Max_Len);

      Len : Natural range 0 .. String_Buffer_Max_Len := 0;
      --  The length of the dictionary file line which is being processed

      Line_Num : Natural := 0;
      --  The number of the currently processed line

      Start_Word : Natural := 0;
      End_Word   : Natural := 0;

      Start_Wildcard : Natural := 0;
      End_Wildcard   : Natural := 0;
      Wildcard_Kind  : Wildcard_Kinds;

      C       : String_Access_Sets.Cursor;
      C_W     : Wildcard_Sets.Cursor;
      Tmp_Str : String_Access;

      New_Wildcard : Wildcard_Rec;
   begin
      begin
         Open (File => Dictionary_File,
               Mode => In_File,
               Name => D_File_Name.all);
      exception
         when others =>
            Error ("cannot open dictionary file " & D_File_Name.all);
            return;
      end;

      while not End_Of_File (Dictionary_File) loop
         Line_Num := Line_Num + 1;
         Get_Line (Dictionary_File, String_Buffer, Len);

         Start_Word := 1;

         Scan_Line : while Start_Word <= Len loop
            while Is_White_Space (String_Buffer (Start_Word)) loop
               Start_Word := Start_Word + 1;

               exit Scan_Line when Start_Word > Len;
            end loop;

            if Start_Word < Len
              and then
               String_Buffer (Start_Word .. Start_Word + 1) = "--"
            then
               --  Skip comment
               exit Scan_Line;
            end if;

            End_Word := Len;

            for J in Start_Word + 1 .. Len loop
               if Is_White_Space (String_Buffer (J)) then
                  End_Word := J - 1;
                  exit;
               end if;
            end loop;

            if Is_Identifier (To_Wide_String
                 (String_Buffer (Start_Word .. End_Word)))
            then
               Tmp_Str := new String'(String_Buffer (Start_Word .. End_Word));
               C       := String_Access_Sets.Find (Stored_Exceptions, Tmp_Str);

               if Has_Element (C) then
                  if String_Access_Sets.Element (C).all /= Tmp_Str.all then
                     String_Access_Sets.Replace_Element
                       (Container => Stored_Exceptions,
                        Position  => C,
                        New_Item  => Tmp_Str);
                  else
                     Free (Tmp_Str);
                  end if;
               else
                  String_Access_Sets.Insert (Stored_Exceptions, Tmp_Str);
               end if;
            else
               --  In case of a correctly formatted wildcard we do the
               --  following. For each wildcard we store a set of patterns that
               --  can be moved onto this wildcard. Then, when analyzing a
               --  defining name, we select subwords in it and check if a
               --  subword can be mapped onto some pattern. For each pattern
               --  we store the original wildcard to be used in the diagnosis.
               --
               --  For *ABC* we store ABC, _ABC, _ABC_ and ABC_,
               --  for *ABC  we store ABC and _ABC
               --  for ABC*  we store ABC and ABC_

               Wildcard_Kind := Both;

               if String_Buffer (Start_Word) = '*' then
                  Start_Wildcard := Start_Word + 1;
               else
                  Start_Wildcard := Start_Word;
                  Wildcard_Kind  := Left;
               end if;

               if String_Buffer (End_Word) = '*' then
                  End_Wildcard := End_Word - 1;
               else
                  if Wildcard_Kind = Both then
                     Wildcard_Kind := Right;
                     End_Wildcard  := End_Word;
                  else
                     Wildcard_Kind := Not_A_Wildcard;
                  end if;
               end if;

               if Wildcard_Kind = Not_A_Wildcard
                 or else
                  not Is_Identifier (To_Wide_String
                        (String_Buffer (Start_Wildcard .. End_Wildcard)))
                 or else
                  Index (String_Buffer (Start_Wildcard .. End_Wildcard), "_")
                  /= 0
               then
                  Error (D_File_Name.all & ':' & Image (Line_Num) & ':' &
                         Image (Start_Word) &
                         ": wrong syntax of a casing exception");
               else
                  New_Wildcard.Orig_Img :=
                    new String'(String_Buffer (Start_Word .. End_Word));

                  for W_Kind in Wildcard_Kinds loop

                     if W_Kind = Not_A_Wildcard
                       or else
                        Wildcard_Kind = Both
                       or else
                        Wildcard_Kind = W_Kind
                     then

                        New_Wildcard.Img := new String'(Get_Pattern
                          (String_Buffer (Start_Wildcard .. End_Wildcard),
                           W_Kind));

                        C_W := Wildcard_Sets.Find
                                 (Stored_Wildcards, New_Wildcard);

                        if Wildcard_Sets.Has_Element (C_W) then
                           if Wildcard_Sets.Element (C_W).Img.all /=
                              New_Wildcard.Img.all
                           then
                              Wildcard_Sets.Replace_Element
                                (Container => Stored_Wildcards,
                                 Position  => C_W,
                                 New_Item  => New_Wildcard);
                           else
                              Free (New_Wildcard.Img);
                           end if;
                        else
                           Wildcard_Sets.Insert
                             (Stored_Wildcards, New_Wildcard);
                        end if;

                     end if;
                  end loop;
               end if;
            end if;

            Start_Word := End_Word + 2;
         end loop Scan_Line;
      end loop;

      if Is_Open (Dictionary_File) then
         Close (Dictionary_File);
      end if;

   end Scan_Dictionary_File;

   ----------------------------------------
   -- XML_Print_Rule (Identifier_Casing) --
   ----------------------------------------

   overriding procedure XML_Print_Rule
     (Rule         : Identifier_Casing_Rule_Type;
      Indent_Level : Natural := 0)
   is
      C : String_Access_Sets.Cursor;
   begin
      XML_Report
        ("<rule id=""" & Rule_Name (Rule) & """>",
         Indent_Level);

      if Rule.Type_Casing /= Not_A_Casing_Scheme then
         XML_Report
           ("<parameter>Type=" & To_Lower (Rule.Type_Casing'Img) &
            "</parameter>",
            Indent_Level + 1);
      end if;

      if Rule.Enum_Casing /= Not_A_Casing_Scheme then
         XML_Report
           ("<parameter>Enum=" & To_Lower (Rule.Enum_Casing'Img) &
            "</parameter>",
            Indent_Level + 1);
      end if;

      if Rule.Constant_Casing /= Not_A_Casing_Scheme then
         XML_Report
           ("<parameter>Constant=" & To_Lower (Rule.Constant_Casing'Img) &
            "</parameter>",
            Indent_Level + 1);
      end if;

      if Rule.Exception_Casing /= Not_A_Casing_Scheme then
         XML_Report
           ("<parameter>Exception=" & To_Lower (Rule.Exception_Casing'Img) &
            "</parameter>",
            Indent_Level + 1);
      end if;

      if Rule.Others_Casing /= Not_A_Casing_Scheme then
         XML_Report
           ("<parameter>Others=" & To_Lower (Rule.Others_Casing'Img) &
            "</parameter>",
            Indent_Level + 1);
      end if;

      if not String_Access_Sets.Is_Empty (Rule.Dictionaries) then
         C := String_Access_Sets.First (Rule.Dictionaries);

         while C /= String_Access_Sets.No_Element loop

            XML_Report
              ("<parameter>Exclude=" & String_Access_Sets.Element (C).all &
               "</parameter>",
               Indent_Level + 1);

            C := Next (C);
         end loop;
      end if;

      XML_Report ("</rule>", Indent_Level);
   end XML_Print_Rule;

   ---------------------------------------
   -- XML_Rule_Help (Identifier_Casing) --
   ---------------------------------------

   overriding procedure XML_Rule_Help
     (Rule  : Identifier_Casing_Rule_Type;
      Level : Natural)
   is
   begin
      Info (Level * Ident_String                &
            "<field switch=""+R"                &
            Rule.Name.all                       &
            ":Type"""                           &
            " label="""                         &
            "type name casing"""                &
            " separator=""="""                  &
            "/>");

      Info (Level * Ident_String                &
            "<field switch=""+R"                &
            Rule.Name.all                       &
            ":Enum"""                           &
            " label="""                         &
            "enumeration literal casing"""      &
            " separator=""="""                  &
            "/>");

      Info (Level * Ident_String                &
            "<field switch=""+R"                &
            Rule.Name.all                       &
            ":Constant"""                       &
            " label="""                         &
            "constant name casing"""            &
            " separator=""="""                  &
            "/>");

      Info (Level * Ident_String                &
            "<field switch=""+R"                &
            Rule.Name.all                       &
            ":Exception"""                      &
            " label="""                         &
            "exception name casing"""           &
            " separator=""="""                  &
            "/>");

      Info (Level * Ident_String                &
            "<field switch=""+R"                &
            Rule.Name.all                       &
            ":Others"""                         &
            " label="""                         &
            "other name casing"""               &
            " separator=""="""                  &
            "/>");

      Info (Level * Ident_String                &
            "<field switch=""+R"                &
            Rule.Name.all                       &
            ":Exclude"""                        &
            " label="""                         &
            "dictionary of casing exceptions""" &
            " separator=""="""                  &
            "/>");

   end XML_Rule_Help;

   -------------------------
   -- Identifier_Suffixes --
   -------------------------

   --------------------------------------------------------
   -- Data structures and local subprograms for the rule --
   --------------------------------------------------------
   Identifier_Suffixes_Exemption_Parameters : Exemption_Parameters.Set;

   procedure Free_All_Suffixes (Rule : in out Identifier_Suffixes_Rule_Type);
   --  Cleans all the name suffixes to check

   function Has_Suffix
     (El     : Asis.Element;
      Suffix : Wide_String)
      return   Boolean;
   --  Checks if the string image of El ends with Suffix.

   -------------------------------------------------
   -- Activate_In_Test_Mode (Identifier_Suffixes) --
   -------------------------------------------------

   overriding procedure Activate_In_Test_Mode
     (Rule : in out Identifier_Suffixes_Rule_Type)
   is
   begin
      Process_Rule_Parameter
        (Rule       => Rule,
         Param      => "",
         Enable     => True,
         Defined_At => "");

      Process_Rule_Parameter
        (Rule       => Rule,
         Param      => "Type_Suffix=_T",
         Enable     => True,
         Defined_At => "");

      Process_Rule_Parameter
        (Rule       => Rule,
         Param      => "Access_Suffix=_Access(_Access)",
         Enable     => True,
         Defined_At => "");

      Process_Rule_Parameter
        (Rule       => Rule,
         Param      => "Class_Access_Suffix=_Class_Access",
         Enable     => True,
         Defined_At => "");

      Process_Rule_Parameter
        (Rule       => Rule,
         Param      => "Class_Subtype_Suffix=_Class",
         Enable     => True,
         Defined_At => "");

      Process_Rule_Parameter
        (Rule       => Rule,
         Param      => "Constant_Suffix=_C",
         Enable     => True,
         Defined_At => "");

      Process_Rule_Parameter
        (Rule       => Rule,
         Param      => "Renaming_Suffix=_R",
         Enable     => True,
         Defined_At => "");

      Process_Rule_Parameter
        (Rule       => Rule,
         Param      => "Access_Obj_Suffix=_PTR",
         Enable     => True,
         Defined_At => "");

      Process_Rule_Parameter
        (Rule       => Rule,
         Param      => "Interrupt_Suffix=_Interrupt",
         Enable     => True,
         Defined_At => "");

   end Activate_In_Test_Mode;

   ----------------------------------------------------------
   -- Allowed_As_Exemption_Parameter (Identifier_Suffixes) --
   ----------------------------------------------------------

   overriding function Allowed_As_Exemption_Parameter
     (Rule      : Identifier_Suffixes_Rule_Type;
      Parameter : String)
      return  Boolean
   is
      pragma Unreferenced (Rule);
   begin
      return Exemption_Parameters.Contains
               (Identifier_Suffixes_Exemption_Parameters,
                Parameter);
   end Allowed_As_Exemption_Parameter;

   -----------------------------------------
   -- Annotate_Rule (Identifier_Suffixes) --
   -----------------------------------------

   overriding function Annotate_Rule
     (Rule : Identifier_Suffixes_Rule_Type;
      Var  : Diagnosis_Variant := 0)
      return String
   is
   begin
      if not Gnatcheck.Options.Mapping_Mode then
         return "";
      else
         if Var = 1 and then Rule.Type_Suffix_Synonym /= null then
            return " [" & Rule.Type_Suffix_Synonym.all & "]";
         elsif Var in 2 | 5 and then Rule.Access_Suffix_Synonym /= null then
            return " [" & Rule.Access_Suffix_Synonym.all & "]";
         elsif Var = 3 and then Rule.Constant_Suffix_Synonym /= null then
            return " [" & Rule.Constant_Suffix_Synonym.all & "]";
         elsif Var = 4 and then Rule.Renaming_Suffix_Synonym /= null then
            return " [" & Rule.Renaming_Suffix_Synonym.all & "]";
         elsif Var = 6 and then Rule.Class_Subtype_Suffix_Synonym /= null then
            return " [" & Rule.Class_Subtype_Suffix_Synonym.all & "]";
         elsif Var = 7 and then Rule.Class_Access_Suffix_Synonym /= null then
            return " [" & Rule.Class_Access_Suffix_Synonym.all & "]";
         elsif Var = 8 and then Rule.Access_Obj_Suffix_Synonym /= null then
            return " [" & Rule.Access_Obj_Suffix_Synonym.all & "]";
         elsif Var = 9 and then Rule.Interrupt_Suffix_Synonym /= null then
            return " [" & Rule.Interrupt_Suffix_Synonym.all & "]";

         else
            return " [" & Rule_Name (Rule) & ':' &
                   (case Var is
                       when 1     => "Type_Suffix",
                       when 2 | 5 => "Access_Suffix",
                       when 3     => "Constant_Suffix",
                       when 4     => "Renaming_Suffix",
                       when 6     => "Class_Subtype_Suffix",
                       when 7     => "Class_Access_Suffix",
                       when 8     => "Access_Obj_Suffix",
                       when 9     => "Interrupt_Suffix",
                       when others => "")
                   & "]";
         end if;

      end if;

   end Annotate_Rule;

   ---------------------------------------------
   -- Free_All_Suffixes (Identifier_Suffixes) --
   ---------------------------------------------

   procedure Free_All_Suffixes
     (Rule : in out Identifier_Suffixes_Rule_Type)
   is
   begin
      Free (Rule.Type_Suffix);
      Free (Rule.Access_Suffix);
      Free (Rule.Access_To_Access_Suffix);
      Free (Rule.Class_Subtype_Suffix);
      Free (Rule.Class_Access_Suffix);
      Free (Rule.Constant_Suffix);
      Free (Rule.Renaming_Suffix);
      Free (Rule.Access_Obj_Suffix);
      Free (Rule.Interrupt_Suffix);

      Free (Rule.Type_Suffix_Synonym);
      Free (Rule.Access_Suffix_Synonym);
      Free (Rule.Class_Subtype_Suffix_Synonym);
      Free (Rule.Class_Access_Suffix_Synonym);
      Free (Rule.Constant_Suffix_Synonym);
      Free (Rule.Renaming_Suffix_Synonym);
      Free (Rule.Access_Obj_Suffix_Synonym);
      Free (Rule.Interrupt_Suffix_Synonym);
   end Free_All_Suffixes;

   --------------------------------------
   -- Has_Suffix (Identifier_Suffixes) --
   --------------------------------------

   function Has_Suffix
     (El     : Asis.Element;
      Suffix : Wide_String)
      return   Boolean
   is
      Result : Boolean := False;
   begin
      --  At the moment this function works with A_Defining_Identifier Elements
      --  only

      Result :=
        Suffix =
        Tail (Source => Defining_Name_Image (El), Count  => Suffix'Length);

      return Result;
   end Has_Suffix;

   -------------------------------------
   -- Init_Rule (Identifier_Suffixes) --
   -------------------------------------

   procedure Init_Rule (Rule : in out Identifier_Suffixes_Rule_Type) is
      use Exemption_Parameters;
   begin
      Init_Rule (Rule_Template (Rule));

      Rule.Name        := new String'("Identifier_Suffixes");
      Rule.Synonym     := new String'("Misnamed_Identifiers");
      Rule.Rule_Status := Fully_Implemented;
      Rule.Help_Info   := new String'("suffixes in defining names");
      Rule.Diagnosis   :=
        new String'("#1#wrong suffix in type name"                  &
                    "#2#wrong suffix in access type name"           &
                    "#3#wrong suffix in constant name"              &
                    "#4#wrong suffix in package renaming"           &
                    "#5#wrong suffix in access-to-access type name" &
                    "#6#wrong suffix in class-wide subtype name"    &
                    "#7#wrong suffix in access-to-class type name"  &
                    "#8#wrong suffix in access object name"         &
                    "#9#wrong suffix in interrupt handler name");

      --  Exemption parameters:
      Insert (Identifier_Suffixes_Exemption_Parameters, "type");
      Insert (Identifier_Suffixes_Exemption_Parameters, "access");
      Insert (Identifier_Suffixes_Exemption_Parameters, "access_obj");
      Insert (Identifier_Suffixes_Exemption_Parameters, "class_access");
      Insert (Identifier_Suffixes_Exemption_Parameters, "class_subtype");
      Insert (Identifier_Suffixes_Exemption_Parameters, "constant");
      Insert (Identifier_Suffixes_Exemption_Parameters, "renaming");
      Insert (Identifier_Suffixes_Exemption_Parameters, "interrupt");
   end Init_Rule;

   --------------------------------------
   -- Print_Rule (Identifier_Suffixes) --
   --------------------------------------

   procedure Print_Rule
     (Rule         : Identifier_Suffixes_Rule_Type;
      Indent_Level : Natural := 0)
   is
      First_Param       : Boolean         := True;
      Rule_Name_Padding : constant String :=
        (1 .. Rule.Name'Length + 2 => ' ');
   begin
      Print_Rule (Rule_Template (Rule), Indent_Level);

      if Rule.Type_Suffix /= null then
         Report_No_EOL (": Type_Suffix = " & Rule.Type_Suffix.all);
         First_Param := False;
      end if;

      if Rule.Access_Suffix /= null then

         if First_Param then
            Report_No_EOL (": Access_Suffix = " & Rule.Access_Suffix.all);
            First_Param := False;
         else
            Report (",");
            Report_No_EOL
              (Rule_Name_Padding &
               "Access_Suffix = " & Rule.Access_Suffix.all,
              Indent_Level);
         end if;

      end if;

      if Rule.Access_To_Access_Suffix /= null then

         if First_Param then
            Report_No_EOL
              (": Access_To_Access_Suffix = " &
               Rule.Access_To_Access_Suffix.all);
            First_Param := False;
         else
            Report (",");
            Report_No_EOL
              (Rule_Name_Padding &
               "Access_To_Access_Suffix = " & Rule.Access_To_Access_Suffix.all,
              Indent_Level);
         end if;

      end if;

      if Rule.Class_Subtype_Suffix /= null then

         if First_Param then
            Report_No_EOL
              (": Class_Subtype_Suffix = " & Rule.Class_Subtype_Suffix.all);
            First_Param := False;
         else
            Report (",");
            Report_No_EOL
              (Rule_Name_Padding &
               "Class_Subtype_Suffix = " & Rule.Class_Subtype_Suffix.all,
              Indent_Level);
         end if;

      end if;

      if Rule.Class_Access_Suffix /= null then

         if First_Param then
            Report_No_EOL
              (": Class_Access_Suffix = " & Rule.Class_Access_Suffix.all);
            First_Param := False;
         else
            Report (",");
            Report_No_EOL
              (Rule_Name_Padding &
               "Class_Access_Suffix = " & Rule.Class_Access_Suffix.all,
              Indent_Level);
         end if;

      end if;

      if Rule.Constant_Suffix /= null then

         if First_Param then
            Report_No_EOL (":  = " & Rule.Constant_Suffix.all);
            First_Param := False;
         else
            Report (",");
            Report_No_EOL
              (Rule_Name_Padding &
               "Constant_Suffix = " & Rule.Constant_Suffix.all,
              Indent_Level);
         end if;

      end if;

      if Rule.Renaming_Suffix /= null then

         if First_Param then
            Report_No_EOL (": Renaming_Suffix = " & Rule.Renaming_Suffix.all);
            First_Param := False;
         else
            Report (",");
            Report_No_EOL
              (Rule_Name_Padding &
               "Renaming_Suffix = " & Rule.Renaming_Suffix.all,
              Indent_Level);
         end if;

      end if;

      if Rule.Access_Obj_Suffix /= null then

         if First_Param then
            Report_No_EOL
              (": Access_Obj_Suffix = " & Rule.Access_Obj_Suffix.all);
            First_Param := False;
         else
            Report (",");
            Report_No_EOL
              (Rule_Name_Padding &
               "Access_Obj_Suffix = " & Rule.Access_Obj_Suffix.all,
              Indent_Level);
         end if;

      end if;

      if Rule.Interrupt_Suffix /= null then

         if First_Param then
            Report_No_EOL
              (": Interrupt_Suffix = " & Rule.Interrupt_Suffix.all);
            First_Param := False;
         else
            Report (",");
            Report_No_EOL
              (Rule_Name_Padding &
               "Interrupt_Suffix = " & Rule.Interrupt_Suffix.all,
              Indent_Level);
         end if;

      end if;

   end Print_Rule;

   ----------------------------------------------
   -- Print_Rule_To_File (Identifier_Suffixes) --
   ----------------------------------------------

   procedure Print_Rule_To_File
     (Rule         : Identifier_Suffixes_Rule_Type;
      Rule_File    : File_Type;
      Indent_Level : Natural := 0)
   is
      First_Param       : Boolean         := True;
      Rule_Name_Padding : constant String :=
        (1 .. Rule.Name'Length + 4 => ' ');
   begin
      Print_Rule_To_File (Rule_Template (Rule), Rule_File, Indent_Level);

      if Rule.Type_Suffix /= null then
         Put (Rule_File, ": Type_Suffix = " & Rule.Type_Suffix.all);
         First_Param := False;
      end if;

      if Rule.Access_Suffix /= null then

         if First_Param then
            Put (Rule_File, ": Access_Suffix = " & Rule.Access_Suffix.all);
            First_Param := False;
         else
            Put_Line (Rule_File, ",");

            for J in 1 .. Indent_Level loop
               Put (Rule_File, Get_Indent_String);
            end loop;

            Put (Rule_File,
                 Rule_Name_Padding &
                 "Access_Suffix = " & Rule.Access_Suffix.all);
         end if;

         if Rule.Access_To_Access_Suffix /= null then
            Put (Rule_File, "(" & Rule.Access_To_Access_Suffix.all & ")");
         end if;

      end if;

      if Rule.Class_Subtype_Suffix /= null then

         if First_Param then
            Put (Rule_File,
                 ": Class_Subtype_Suffix = " & Rule.Class_Subtype_Suffix.all);
            First_Param := False;
         else
            Put_Line (Rule_File, ",");

            for J in 1 .. Indent_Level loop
               Put (Rule_File, Get_Indent_String);
            end loop;

            Put (Rule_File,
                 Rule_Name_Padding &
                "Class_Subtype_Suffix = " & Rule.Class_Subtype_Suffix.all);
         end if;

      end if;

      if Rule.Class_Access_Suffix /= null then

         if First_Param then
            Put (Rule_File,
                 ": Class_Access_Suffix = " & Rule.Class_Access_Suffix.all);
            First_Param := False;
         else
            Put_Line (Rule_File, ",");

            for J in 1 .. Indent_Level loop
               Put (Rule_File, Get_Indent_String);
            end loop;

            Put (Rule_File,
                 Rule_Name_Padding &
                 "Class_Access_Suffix = " & Rule.Class_Access_Suffix.all);
         end if;

      end if;

      if Rule.Constant_Suffix /= null then

         if First_Param then
            Put (Rule_File, ": Constant_Suffix = " & Rule.Constant_Suffix.all);
            First_Param := False;
         else
            Put_Line (Rule_File, ",");

            for J in 1 .. Indent_Level loop
               Put (Rule_File, Get_Indent_String);
            end loop;

            Put (Rule_File,
                 Rule_Name_Padding &
                 "Constant_Suffix = " & Rule.Constant_Suffix.all);
         end if;

      end if;

      if Rule.Renaming_Suffix /= null then

         if First_Param then
            Put (Rule_File, ": Renaming_Suffix = " & Rule.Renaming_Suffix.all);
            First_Param := False;
         else
            Put_Line (Rule_File, ",");

            for J in 1 .. Indent_Level loop
               Put (Rule_File, Get_Indent_String);
            end loop;

            Put (Rule_File,
                 Rule_Name_Padding &
                 "Renaming_Suffix = " & Rule.Renaming_Suffix.all);
         end if;

      end if;

      if Rule.Access_Obj_Suffix /= null then

         if First_Param then
            Put (Rule_File,
                 ": Access_Obj_Suffix = " & Rule.Access_Obj_Suffix.all);
            First_Param := False;
         else
            Put_Line (Rule_File, ",");

            for J in 1 .. Indent_Level loop
               Put (Rule_File, Get_Indent_String);
            end loop;

            Put (Rule_File,
                 Rule_Name_Padding &
                 "Access_Obj_Suffix = " & Rule.Access_Obj_Suffix.all);
         end if;

      end if;

      if Rule.Interrupt_Suffix /= null then

         if First_Param then
            Put (Rule_File,
                 ": Interrupt_Suffix = " & Rule.Interrupt_Suffix.all);
            First_Param := False;
         else
            Put_Line (Rule_File, ",");

            for J in 1 .. Indent_Level loop
               Put (Rule_File, Get_Indent_String);
            end loop;

            Put (Rule_File,
                 Rule_Name_Padding &
                 "Interrupt_Suffix = " & Rule.Interrupt_Suffix.all);
         end if;

      end if;

   end Print_Rule_To_File;

   --------------------------------------------------
   -- Process_Rule_Parameter (Identifier_Suffixes) --
   --------------------------------------------------

   procedure Process_Rule_Parameter
     (Rule       : in out Identifier_Suffixes_Rule_Type;
      Param      :        String;
      Enable     :        Boolean;
      Defined_At : String)
   is
      pragma Unreferenced (Defined_At);

      First_Str_Idx, Last_Str_Idx : Natural;
      --  Beginning and end of the 'string' part of the parameter, see the
      --  rule parameter description in the spec. First_Str_Idx is set to 0 if
      --  the parameter does not contain a '=' character.

      Last_Str_Idx_Original : Natural;

      First_Par_Idx, Last_Par_Idx : Natural;
      --  If the parameter contains a '=' character, set to point to the
      --  beginning and the end of the part of the parameter that precedes '='.
      --  Otherwise First_Par_Idx points to the first, and Last_Par_Idx - to
      --  the last non-blank character in Param (First_Idx .. Last_Idx)

      Is_Legal_Suffix        : Boolean := False;
      Is_Legal_Access_Suffix : Boolean := False;

   begin

      if Param = "" then
         if Enable then
            Rule.Rule_State := Enabled;
         else
            Rule.Rule_State := Disabled;
         end if;

         return;
      end if;

      Parse_Par
        (First_Par_Idx, Last_Par_Idx, First_Str_Idx, Last_Str_Idx, Param);

      if First_Str_Idx = 0 then

         if Enable then
            if To_Lower (Param (First_Par_Idx .. Last_Par_Idx)) =
               "default"
            then
               Set_Rule_Defaults (Rule);
               Rule.Rule_State := Enabled;
            else
               Error
                ("(" & Rule.Name.all & ") wrong parameter : " &
                 Param & ", ignored");
            end if;
         else

            if To_Lower (Param (First_Par_Idx .. Last_Par_Idx)) =
               "all_suffixes"
            then
               Free_All_Suffixes (Rule);
               Rule.Rule_State := Disabled;

            elsif To_Lower (Param (First_Par_Idx .. Last_Par_Idx)) =
               "type_suffix"
            then
               Free (Rule.Type_Suffix);
               Free (Rule.Type_Suffix_Synonym);

            elsif To_Lower (Param (First_Par_Idx .. Last_Par_Idx)) =
               "access_suffix"
            then
               Free (Rule.Access_Suffix);
               Free (Rule.Access_To_Access_Suffix);

               Free (Rule.Access_Suffix_Synonym);
            elsif To_Lower (Param (First_Par_Idx .. Last_Par_Idx)) =
               "constant_suffix"
            then
               Free (Rule.Constant_Suffix);
               Free (Rule.Constant_Suffix_Synonym);
            elsif To_Lower (Param (First_Par_Idx .. Last_Par_Idx)) =
               "class_subtype_suffix"
            then
               Free (Rule.Class_Subtype_Suffix);
               Free (Rule.Class_Subtype_Suffix_Synonym);
            elsif To_Lower (Param (First_Par_Idx .. Last_Par_Idx)) =
               "class_access_suffix"
            then
               Free (Rule.Class_Access_Suffix);
               Free (Rule.Class_Access_Suffix_Synonym);
            elsif To_Lower (Param (First_Par_Idx .. Last_Par_Idx)) =
               "renaming_suffix"
            then
               Free (Rule.Renaming_Suffix);
               Free (Rule.Renaming_Suffix_Synonym);
            elsif To_Lower (Param (First_Par_Idx .. Last_Par_Idx)) =
               "access_obj_suffix"
            then
               Free (Rule.Access_Obj_Suffix);
               Free (Rule.Access_Obj_Suffix_Synonym);
            elsif To_Lower (Param (First_Par_Idx .. Last_Par_Idx)) =
               "interrupt_suffix"
            then
               Free (Rule.Interrupt_Suffix);
               Free (Rule.Interrupt_Suffix_Synonym);
            else
               Error
                ("(" & Rule.Name.all & ") wrong parameter : " &
                 Param & ", ignored");
            end if;

         end if;

      else
         if Enable then

            Is_Legal_Suffix := Is_Identifier_Suffix
              (To_Wide_String (Param (First_Str_Idx .. Last_Str_Idx)));

            if not Is_Legal_Suffix
              and then
               To_Lower (Param (First_Par_Idx .. Last_Par_Idx)) =
                  "access_suffix"
            then
               Is_Legal_Access_Suffix :=
                 Is_Access_Suffix (Param (First_Str_Idx .. Last_Str_Idx));
            end if;

            if Is_Legal_Suffix then

               if To_Lower (Param (First_Par_Idx .. Last_Par_Idx)) =
                  "type_suffix"
               then

                  Rule.Type_Suffix :=
                    new String'(Param (First_Str_Idx .. Last_Str_Idx));

                  if Has_Synonym (Rule) then
                     Free (Rule.Type_Suffix_Synonym);
                     Rule.Type_Suffix_Synonym :=
                       new String'(Rule_Synonym (Rule));
                  end if;

                  Rule.Rule_State := Enabled;

               elsif To_Lower (Param (First_Par_Idx .. Last_Par_Idx)) =
                  "access_suffix"
               then

                  Rule.Access_Suffix :=
                    new String'(Param (First_Str_Idx .. Last_Str_Idx));

                  Rule.Access_To_Access_Suffix := null;

                  if Has_Synonym (Rule) then
                     Free (Rule.Access_Suffix_Synonym);
                     Rule.Access_Suffix_Synonym :=
                       new String'(Rule_Synonym (Rule));
                  end if;

                  Rule.Rule_State := Enabled;

               elsif To_Lower (Param (First_Par_Idx .. Last_Par_Idx)) =
                  "class_subtype_suffix"
               then
                  Rule.Class_Subtype_Suffix :=
                    new String'(Param (First_Str_Idx .. Last_Str_Idx));

                  if Has_Synonym (Rule) then
                     Free (Rule.Class_Subtype_Suffix_Synonym);
                     Rule.Class_Subtype_Suffix_Synonym :=
                       new String'(Rule_Synonym (Rule));
                  end if;

                  Rule.Rule_State := Enabled;

               elsif To_Lower (Param (First_Par_Idx .. Last_Par_Idx)) =
                 "class_access_suffix"
               then
                  Rule.Class_Access_Suffix :=
                    new String'(Param (First_Str_Idx .. Last_Str_Idx));

                  if Has_Synonym (Rule) then
                     Free (Rule.Class_Access_Suffix_Synonym);
                     Rule.Class_Access_Suffix_Synonym :=
                       new String'(Rule_Synonym (Rule));
                  end if;

                  Rule.Rule_State := Enabled;

               elsif To_Lower (Param (First_Par_Idx .. Last_Par_Idx)) =
                  "constant_suffix"
               then

                  Rule.Constant_Suffix :=
                    new String'(Param (First_Str_Idx .. Last_Str_Idx));

                  if Has_Synonym (Rule) then
                     Free (Rule.Constant_Suffix_Synonym);
                     Rule.Constant_Suffix_Synonym :=
                       new String'(Rule_Synonym (Rule));
                  end if;

                  Rule.Rule_State := Enabled;

               elsif To_Lower (Param (First_Par_Idx .. Last_Par_Idx)) =
                  "renaming_suffix"
               then

                  Rule.Renaming_Suffix :=
                    new String'(Param (First_Str_Idx .. Last_Str_Idx));

                  if Has_Synonym (Rule) then
                     Free (Rule.Renaming_Suffix_Synonym);
                     Rule.Renaming_Suffix_Synonym :=
                       new String'(Rule_Synonym (Rule));
                  end if;

                  Rule.Rule_State := Enabled;

               elsif To_Lower (Param (First_Par_Idx .. Last_Par_Idx)) =
                  "access_obj_suffix"
               then

                  Rule.Access_Obj_Suffix :=
                    new String'(Param (First_Str_Idx .. Last_Str_Idx));

                  if Has_Synonym (Rule) then
                     Free (Rule.Access_Obj_Suffix_Synonym);
                     Rule.Access_Obj_Suffix_Synonym :=
                       new String'(Rule_Synonym (Rule));
                  end if;

                  Rule.Rule_State := Enabled;

               elsif To_Lower (Param (First_Par_Idx .. Last_Par_Idx)) =
                  "interrupt_suffix"
               then

                  Rule.Interrupt_Suffix :=
                    new String'(Param (First_Str_Idx .. Last_Str_Idx));

                  if Has_Synonym (Rule) then
                     Free (Rule.Interrupt_Suffix_Synonym);
                     Rule.Interrupt_Suffix_Synonym :=
                       new String'(Rule_Synonym (Rule));
                  end if;

                  Rule.Rule_State := Enabled;

               else
                  Error
                   ("(" & Rule.Name.all & ") wrong parameter name : " &
                    Param & ", ignored");
               end if;

            elsif Is_Legal_Access_Suffix then
               --  In this case we already know that
               --    To_Lower (Param (First_Par_Idx .. Last_Par_Idx)) =
               --      "access_suffix"
               Last_Str_Idx_Original := Last_Str_Idx;

               Last_Str_Idx :=
                 Index (Param (First_Str_Idx .. Last_Str_Idx), "(") - 1;

               Rule.Access_Suffix := new String'(Trim
                 (Param (First_Str_Idx .. Last_Str_Idx), Right));

               First_Str_Idx := Last_Str_Idx + 2;
               Last_Str_Idx  := Last_Str_Idx_Original - 1;

               Rule.Access_To_Access_Suffix := new String'(Trim
                 (Param (First_Str_Idx .. Last_Str_Idx), Both));

                  if Has_Synonym (Rule) then
                     Free (Rule.Access_Suffix_Synonym);
                     Rule.Access_Suffix_Synonym :=
                       new String'(Rule_Synonym (Rule));
                  end if;

               Rule.Rule_State := Enabled;
            else
               Error
                ("(" & Rule.Name.all & ") " &
                 Param (First_Str_Idx .. Last_Str_Idx) &
                 " is not a legal name suffix, ignored");
            end if;

         else
            Error
             ("(" & Rule.Name.all & ") wrong parameter : " &
              Param & ", ignored");
         end if;
      end if;

   end Process_Rule_Parameter;

   ---------------------------------------------
   -- Rule_Check_Pre_Op (Identifier_Suffixes) --
   ---------------------------------------------

   procedure Rule_Check_Pre_Op
     (Rule    : in out Identifier_Suffixes_Rule_Type;
      Element :        Asis.Element;
      Control : in out Traverse_Control;
      State   : in out Rule_Traversal_State)
   is
      pragma Unreferenced (Control);
      pragma Unmodified  (Rule);

      Tmp                     : Asis.Element;
      In_Constant             : Boolean := False;
      Access_Obj_Check_Needed : Boolean := False;
   begin

      if Defining_Name_Kind (Element) = A_Defining_Identifier then
         Tmp := Get_Enclosing_Element;

         if Defining_Name_Kind (Tmp) = A_Defining_Expanded_Name then
            Tmp := Get_Enclosing_Element (Steps_Up => 1);
         end if;

         case Declaration_Kind (Tmp) is
            when An_Ordinary_Type_Declaration ..
                 A_Protected_Type_Declaration =>

               case Declaration_Kind (Corresponding_Type_Declaration (Tmp)) is

                  when Not_A_Declaration                    |
                       An_Incomplete_Type_Declaration       |
                       A_Tagged_Incomplete_Type_Declaration =>

                     if Rule.Type_Suffix /= null
                       or else
                        Rule.Access_Suffix /= null
                       or else
                        Rule.Class_Access_Suffix /= null
                     then
                        --  Here we have to make the difference between access
                        --  and non-access types
                        Tmp := Type_Declaration_View (Tmp);

                        if Type_Kind (Tmp) = An_Access_Type_Definition then

                           --  First, case of access-to-class type
                           if Rule.Class_Access_Suffix /= null
                             and then
                              Is_Access_To_Class (Tmp)
                           then
                              if not Has_Suffix
                                       (Element,
                                        To_Wide_String
                                          (Rule.Class_Access_Suffix.all))
                              then
                                 State.Detected  := True;
                                 State.Diagnosis := 7;
                              end if;

                              return;
                           end if;

                           if Rule.Access_Suffix /= null then

                              if Rule.Access_To_Access_Suffix = null then

                                 if not Has_Suffix
                                          (Element,
                                           To_Wide_String
                                             (Rule.Access_Suffix.all))
                                 then
                                    State.Detected  := True;
                                    State.Diagnosis := 2;
                                 end if;
                              else
                                 if Is_Access_To_Access (Tmp, Element) then

                                    if not Has_Suffix
                                             (Element,
                                              To_Wide_String
                                               (Rule.Access_Suffix.all &
                                                Rule.Access_To_Access_Suffix.
                                                  all))
                                    then
                                       State.Detected  := True;
                                       State.Diagnosis := 5;
                                    end if;

                                 else

                                    if not Has_Suffix
                                             (Element,
                                              To_Wide_String
                                                (Rule.Access_Suffix.all))
                                    then
                                       State.Detected  := True;
                                       State.Diagnosis := 2;
                                    end if;

                                 end if;
                              end if;

                           elsif Rule.Access_Suffix = null
                             and then
                                 Rule.Type_Suffix /= null
                             and then
                                 not Has_Suffix
                                       (Element,
                                       To_Wide_String (Rule.Type_Suffix.all))
                           then
                              --  If the suffix for access types is not set,
                              --  but the suffix for type defining name is set,
                              --  treat the name as an ordinary type name.
                              State.Detected  := True;
                              State.Diagnosis := 1;

                           end if;

                        else

                           if Rule.Type_Suffix /= null
                             and then
                              not Has_Suffix
                                    (Element,
                                     To_Wide_String (Rule.Type_Suffix.all))
                           then
                              State.Detected  := True;
                              State.Diagnosis := 1;
                           end if;

                        end if;

                     end if;

                  when others =>
                     --  The only real possibility is
                     --  A_Private_Type_Declaration or
                     --  A_Private_Extension_Declaration. In both cases we
                     --  do not check the defining identifier of the
                     --  corresponding type declaration
                     null;

               end case;

            when A_Private_Type_Declaration ..
                 A_Private_Extension_Declaration =>

               if Rule.Type_Suffix /= null
                 and then
                  not Has_Suffix
                       (Element, To_Wide_String (Rule.Type_Suffix.all))
               then
                  State.Detected  := True;
                  State.Diagnosis := 1;
               end if;

            when An_Incomplete_Type_Declaration ..
                 A_Tagged_Incomplete_Type_Declaration =>

               null;
            when A_Subtype_Declaration =>
               if Rule.Class_Subtype_Suffix /= null then

                  Tmp := Type_Declaration_View (Tmp);
                  Tmp := Asis.Definitions.Subtype_Mark (Tmp);

                  if (Attribute_Kind (Tmp) = A_Class_Attribute
                     or else
                      Denotes_Class_Wide_Subtype (Tmp))
                    and then
                     not Has_Suffix
                          (Element,
                           To_Wide_String (Rule.Class_Subtype_Suffix.all))
                  then
                     State.Detected  := True;
                     State.Diagnosis := 6;
                  end if;

               end if;

            when A_Variable_Declaration          |
                 A_Constant_Declaration          |
                 A_Discriminant_Specification    |
                 A_Component_Declaration         |
                 A_Parameter_Specification       |
                 A_Return_Variable_Specification |
                 A_Return_Constant_Specification |
                 A_Formal_Object_Declaration     =>

               In_Constant := Declaration_Kind (Tmp) = A_Constant_Declaration;

               if In_Constant
                 and then
                  Rule.Access_Obj_Suffix = null
                 and then
                  Rule.Constant_Suffix /= null
               then
                  --  Check for constant suffix

                  if Is_Nil (Corresponding_Constant_Declaration (Element))
                    and then
                     not Has_Suffix
                          (Element, To_Wide_String (Rule.Constant_Suffix.all))
                  then
                     State.Detected  := True;
                     State.Diagnosis := 3;
                  end if;
               elsif Rule.Access_Obj_Suffix /= null
                  and then
                     not (Declaration_Kind (Tmp) = A_Constant_Declaration
                               and then
                                not Is_Nil (Corresponding_Constant_Declaration
                                              (Element)))
               then
                  --  Check for access suffix. The case of a deferred constant
                  --  and the corresponding full constant declarations is
                  --  filtered out
                  Tmp := Object_Declaration_View (Tmp);

                  if Definition_Kind (Tmp) = A_Component_Definition then
                     Tmp := Component_Definition_View (Tmp);
                  end if;

                  case Flat_Element_Kind (Tmp) is
                     when Flat_Access_Definition_Kinds =>
                        Access_Obj_Check_Needed := True;
                     when A_Subtype_Indication   |
                          A_Component_Definition |
                          An_Identifier          |
                          A_Selected_Component   |
                          A_Base_Attribute       =>

                        if Definition_Kind (Tmp) = A_Subtype_Indication then
                           Tmp := Asis.Definitions.Subtype_Mark (Tmp);
                        end if;

                        Tmp := Get_Underlying_Type (Tmp,
                                                    Stop_At_Private => True);

                        case Declaration_Kind (Tmp) is
                           when A_Formal_Type_Declaration =>
                              if Formal_Type_Kind
                                   (Type_Declaration_View (Tmp)) =
                                      A_Formal_Access_Type_Definition
                              then
                                 Access_Obj_Check_Needed := True;
                              end if;

                           when An_Ordinary_Type_Declaration =>
                              if Type_Kind (Type_Declaration_View (Tmp)) =
                                   An_Access_Type_Definition
                              then
                                 Access_Obj_Check_Needed := True;
                              end if;
                           when A_Private_Type_Declaration =>
                              --  For private type, we do only one step
                              --  attempting to go from private to full view.
                              --  The reason is that for full unwinding of all
                              --  possible subtyping, derivation and privating
                              --  it is very hard to define which information
                              --  is visible at the place of Element

                              if Full_View_Visible
                                   (Type_Decl => Tmp,
                                    At_Place  => Element)
                              then
                                 Tmp := Corresponding_Type_Completion (Tmp);
                                 Tmp := Get_Underlying_Type
                                          (Tmp,
                                           Stop_At_Private => True);

                                 if Declaration_Kind (Tmp) =
                                      An_Ordinary_Type_Declaration
                                  and then
                                    Type_Kind (Type_Declaration_View (Tmp)) =
                                      An_Access_Type_Definition
                                 then
                                    Access_Obj_Check_Needed := True;
                                 end if;
                              end if;
                           when others =>
                              null;
                        end case;

                     when others =>
                        null;
                  end case;
               end if;

               if Access_Obj_Check_Needed
                 and then
                  not Has_Suffix (Element,
                                  To_Wide_String (Rule.Access_Obj_Suffix.all))
               then
                  State.Detected  := True;
                  State.Diagnosis := 8;
               elsif not Access_Obj_Check_Needed
                 and then
                     In_Constant
                 and then
                     Rule.Constant_Suffix /= null
               then
                  if Is_Nil (Corresponding_Constant_Declaration (Element))
                    and then
                     not Has_Suffix
                          (Element, To_Wide_String (Rule.Constant_Suffix.all))
                  then
                     State.Detected  := True;
                     State.Diagnosis := 3;
                  end if;
               end if;

            when A_Deferred_Constant_Declaration =>

               if Rule.Constant_Suffix /= null
                 and then
                  not Has_Suffix
                       (Element, To_Wide_String (Rule.Constant_Suffix.all))
               then
                  State.Detected  := True;
                  State.Diagnosis := 3;
               end if;

            when A_Package_Renaming_Declaration =>

               if Rule.Renaming_Suffix /= null
                 and then
                  not Has_Suffix
                       (Element, To_Wide_String (Rule.Renaming_Suffix.all))
               then
                  State.Detected  := True;
                  State.Diagnosis := 4;
               end if;

            when A_Procedure_Declaration =>

               if Rule.Interrupt_Suffix /= null
                 and then
                  Is_Interrupt_Handler (Tmp)
                 and then
                  not Has_Suffix
                       (Element, To_Wide_String (Rule.Interrupt_Suffix.all))
               then
                  State.Detected  := True;
                  State.Diagnosis := 9;
               end if;

            when others =>
               null;
         end case;

      end if;

   end Rule_Check_Pre_Op;
   ------------------------------------------
   -- Rule_Parameter (Identifier_Suffixes) --
   ------------------------------------------

   overriding function Rule_Parameter
     (Rule : Identifier_Suffixes_Rule_Type;
      Diag : String)
      return String
   is
      pragma Unreferenced (Rule);
   begin
      if Index (Diag, "access-to-class") /= 0 then
         return "class_access";
      elsif Index (Diag, "access object") /= 0 then
         return "access_obj";
      elsif Index (Diag, "access") /= 0 then
         return "access";
      elsif Index (Diag, "class-wide") /= 0 then
         return "class_subtype";
      elsif Index (Diag, "constant") /= 0 then
         return "constant";
      elsif Index (Diag, "type") /= 0 then
         return "type";
      elsif Index (Diag, "renaming") /= 0 then
         return "renaming";
      elsif Index (Diag, "interrupt") /= 0 then
         return "interrupt";
      else
         return "";
      end if;
   end Rule_Parameter;

   ---------------------------------------------
   -- Set_Rule_Defaults (Identifier_Suffixes) --
   ---------------------------------------------

   procedure Set_Rule_Defaults
     (Rule : in out Identifier_Suffixes_Rule_Type)
   is
   begin
      Free_All_Suffixes (Rule);

      Rule.Type_Suffix             := new String'("_T");
      Rule.Access_Suffix           := new String'("_A");
      Rule.Access_To_Access_Suffix := null;
      Rule.Class_Subtype_Suffix    := null;
      Rule.Class_Access_Suffix     := null;
      Rule.Constant_Suffix         := new String'("_C");
      Rule.Renaming_Suffix         := new String'("_R");
      Rule.Interrupt_Suffix        := null;
   end Set_Rule_Defaults;

   ------------------------------------------
   -- XML_Print_Rule (Identifier_Suffixes) --
   ------------------------------------------

   overriding procedure XML_Print_Rule
     (Rule         : Identifier_Suffixes_Rule_Type;
      Indent_Level : Natural := 0)
   is
   begin
      XML_Report
        ("<rule id=""" & Rule_Name (Rule) & """>",
         Indent_Level);

      if Rule.Type_Suffix /= null then
         XML_Report
           ("<parameter>Type_Suffix=" & Rule.Type_Suffix.all & "</parameter>",
            Indent_Level + 1);
      end if;

      if Rule.Access_Suffix /= null then
         XML_Report_No_EOL
           ("<parameter>Access_Suffix=" & Rule.Access_Suffix.all,
            Indent_Level + 1);

         if Rule.Access_To_Access_Suffix /= null then
            XML_Report_No_EOL ("(" & Rule.Access_To_Access_Suffix.all & ")");
         end if;

         XML_Report ("</parameter>");
      end if;

      if Rule.Class_Subtype_Suffix /= null then
         XML_Report
           ("<parameter>Class_Subtype_Suffix=" &
               Rule.Class_Subtype_Suffix.all   & "</parameter>",
            Indent_Level + 1);
      end if;

      if Rule.Class_Access_Suffix /= null then
         XML_Report
           ("<parameter>Class_Access_Suffix=" &
               Rule.Class_Access_Suffix.all   & "</parameter>",
            Indent_Level + 1);
      end if;

      if Rule.Constant_Suffix /= null then
         XML_Report
           ("<parameter>Constant_Suffix=" &
               Rule.Constant_Suffix.all   & "</parameter>",
            Indent_Level + 1);
      end if;

      if Rule.Renaming_Suffix /= null then
         XML_Report
           ("<parameter>Renaming_Suffix=" &
               Rule.Renaming_Suffix.all   & "</parameter>",
            Indent_Level + 1);
      end if;

      if Rule.Access_Obj_Suffix /= null then
         XML_Report
           ("<parameter>Access_Obj_Suffix=" &
               Rule.Access_Obj_Suffix.all   & "</parameter>",
            Indent_Level + 1);
      end if;

      if Rule.Interrupt_Suffix /= null then
         XML_Report
           ("<parameter>Interrupt_Suffix=" &
               Rule.Interrupt_Suffix.all   & "</parameter>",
            Indent_Level + 1);
      end if;

      XML_Report ("</rule>", Indent_Level);
   end XML_Print_Rule;

   ----------------------------------------------------
   -- XML_Rule_Parameters_Help (Identifier_Suffixes) --
   ----------------------------------------------------

   procedure XML_Rule_Help
     (Rule  : Identifier_Suffixes_Rule_Type;
      Level : Natural)
   is
   begin
      Info (Level * Ident_String                  &
            "<check switch=""+R"                  &
            Rule.Name.all                         &
            ":Default"""                          &
            " label="""                           &
            "identifiers use standard suffixes""" &
            "/>");

      Info (Level * Ident_String               &
            "<field switch=""+R"               &
            Rule.Name.all                      &
            ":Type_Suffix"""                   &
            " label="""                        &
            "suffix for type names"            &
            " (empty string disables check)""" &
            " separator=""="""                 &
            " switch-off=""-R"                 &
            Rule.Name.all                      &
            ":Type_Suffix"""                   &
            "/>");

      Info (Level * Ident_String               &
            "<field switch=""+R"               &
            Rule.Name.all                      &
            ":Access_Suffix"""                 &
            " label="""                        &
            "suffix for access type names "    &
            " (empty string disables check)""" &
            " separator=""="""                 &
            " switch-off=""-R"                 &
            Rule.Name.all                      &
            ":Access_Suffix"""                 &
            "/>");

      Info (Level * Ident_String               &
            "<field switch=""+R"               &
            Rule.Name.all                      &
            ":Constant_Suffix"""               &
            " label="""                        &
            "suffix for constant names"        &
            " (empty string disables check)""" &
            " separator=""="""                 &
            " switch-off=""-R"                 &
            Rule.Name.all                      &
            ":Constant_Suffix"""               &
            "/>");

      Info (Level * Ident_String                   &
            "<field switch=""+R"                   &
            Rule.Name.all                          &
            ":Renaming_Suffix"""                   &
            " label="""                            &
            "suffix for package renaming names"    &
            " (empty string disables check)"""     &
            " separator=""="""                     &
            " switch-off=""-R"                     &
            Rule.Name.all                          &
            ":Renaming_Suffix"""                   &
            "/>");

      Info (Level * Ident_String                   &
            "<field switch=""+R"                   &
            Rule.Name.all                          &
            ":Access_Obj_Suffix"""                 &
            " label="""                            &
            "suffix for access object names"       &
            " (empty string disables check)"""     &
            " separator=""="""                     &
            " switch-off=""-R"                     &
            Rule.Name.all                          &
            ":Access_Obj_Suffix"""                 &
            "/>");

      Info (Level * Ident_String                   &
            "<field switch=""+R"                   &
            Rule.Name.all                          &
            ":Interrupt_Suffix"""                  &
            " label="""                            &
            "suffix for interrupt handler names"   &
            " (empty string disables check)"""     &
            " separator=""="""                     &
            " switch-off=""-R"                     &
            Rule.Name.all                          &
            ":Interrupt_Suffix"""                  &
            "/>");

      --  Specifying the dependencies between the default suffixes and the
      --  content of the fields for specific suffixes

      Info (Level * Ident_String                          &
           "<default-value-dependency master-switch=""+R" &
            Rule.Name.all                                 &
            ":Default"""                                  &
            " slave-switch=""+R"                          &
            Rule.Name.all                                 &
            ":Type_Suffix=_T""/>");

      Info (Level * Ident_String                          &
           "<default-value-dependency master-switch=""+R" &
            Rule.Name.all                                 &
            ":Default"""                                  &
            " slave-switch=""+R"                          &
            Rule.Name.all                                 &
            ":Access_Suffix=_A""/>");

      Info (Level * Ident_String                          &
           "<default-value-dependency master-switch=""+R" &
            Rule.Name.all                                 &
            ":Default"""                                  &
            " slave-switch=""+R"                          &
            Rule.Name.all                                 &
            ":Constant_Suffix=_C""/>");

      Info (Level * Ident_String                          &
           "<default-value-dependency master-switch=""+R" &
            Rule.Name.all                                 &
            ":Default"""                                  &
            " slave-switch=""+R"                          &
            Rule.Name.all                                 &
            ":Renaming_Suffix=_R""/>");

   end XML_Rule_Help;

   -------------------------
   -- Identifier_Prefixes --
   -------------------------

   --------------------------------------------------------
   -- Data structures and local subprograms for the rule --
   --------------------------------------------------------

   type Identifier_Prefixes_Parameter_Kinds is
     (Not_A_Parameter,
      All_Prefixes_Par,
      Type_Par,
      Concurrent_Par,
      Access_Par,
      Class_Access_Par,
      Subprogram_Access_Par,
      Derived_Par,
      Constant_Par,
      Exception_Par,
      Enum_Par,
      Exclusive_Par);

   Identifier_Prefixes_Exemption_Parameters : Exemption_Parameters.Set;

   function Get_Identifier_Prefixes_Parameter_Kind
     (S    : String)
      return Identifier_Prefixes_Parameter_Kinds;
   --  If S denotes one of the rule parameters, returns the corresponding
   --  parameter kind, otherwise Not_A_Parameter is returned

   procedure Free_All_Prefixes (Rule : in out Identifier_Prefixes_Rule_Type);
   --  Cleans all the name suffixes to check

   function At_Least_One_Prefix_Set
     (R    : Identifier_Prefixes_Rule_Type)
      return Boolean;
   --  Checks if at least one prefix is specified

   function Has_Prefix
     (El     : Asis.Element;
      Prefix : Wide_String)
      return   Boolean;
   --  Checks if the string image of El ends with Prefix.

   function Get_Full_Parent_Name (Def : Asis.Element) return Program_Text;
   --  Provided that Def is a derived type definition, private extension
   --  definition or the definition of a formal derived type, gives the full
   --  expanded Ada name of its ancestor type (not including the very top
   --  Standard package). See ASIS_UL.Utilities.Full_Expanded_Name_Image
   --  query.

   function Is_Derived_Type_Par (S : String) return Boolean;
   --  Checks if S has the format 'full_expaned_type_name:prefix' where
   --  full_expaned_type_name has the syntax of full expanded type name, and
   --  prefix is a valid identifier prefix. It can be any number of spaces
   --  around ':'

   function Derived_Pref (C : Derived_Prefixes.Cursor) return String;
   --  Returns the parameter of "+RDerived=" option stored under C in the
   --  format "full_expanded_name_of_parent_type:prefix"

   All_Prefixes : String_Access_Sets.Set;

   function Has_Specific_Prefix (E : Asis.Element) return Boolean;
   --  Checks if E has one of the prefixes specified for specific kinds of
   --  entities. (Assumes that E is either of A_Defining_Identifier or
   --  A_Defining_Enumeration_Literal kind). All the prefixes defined for the
   --  names of specific entities are supposed to be stored in the All_Prefixes
   --  set container. When this function is called for the first time, it fills
   --  in this container with the prefixes defined by rule parameters

   --------------------------------
   -- "="  (Identifier_Prefixes) --
   --------------------------------

   function "=" (Left, Right : Derived_Pref_Record) return Boolean is
   begin
      if Left.Parent_Name = null or else Right.Parent_Name = null then
         return True;
      else
         return To_Lower (Left.Parent_Name.all) =
                To_Lower (Right.Parent_Name.all);
      end if;
   end "=";

   -------------------------------
   -- "<" (Identifier_Prefixes) --
   -------------------------------

   function "<" (Left, Right : Derived_Pref_Record) return Boolean is
   begin
      if Left.Parent_Name = null or else Right.Parent_Name = null then
         return True;
      else
         return To_Lower (Left.Parent_Name.all) <
                To_Lower (Right.Parent_Name.all);
      end if;
   end "<";

   function "<" (Left, Right : String_Access) return Boolean is
   begin
      if Left = null or else Right = null then
         return True;
      else
         return To_Lower (Left.all) < To_Lower (Right.all);
      end if;
   end "<";

   -------------------------------------------------
   -- Activate_In_Test_Mode (Identifier_Prefixes) --
   -------------------------------------------------

   overriding procedure Activate_In_Test_Mode
     (Rule : in out Identifier_Prefixes_Rule_Type)
   is
   begin
      Process_Rule_Parameter
        (Rule       => Rule,
         Param      => "Type=T_",
         Enable     => True,
         Defined_At => "");

      Process_Rule_Parameter
        (Rule       => Rule,
         Param      => "Concurrent=J_",
         Enable     => True,
         Defined_At => "");

      Process_Rule_Parameter
        (Rule       => Rule,
         Param      => "Access=P_",
         Enable     => True,
         Defined_At => "");

      Process_Rule_Parameter
        (Rule       => Rule,
         Param      => "Class_Access=CP_",
         Enable     => True,
         Defined_At => "");

      Process_Rule_Parameter
        (Rule       => Rule,
         Param      => "Subprogram_Access=F_",
         Enable     => True,
         Defined_At => "");

      Process_Rule_Parameter
        (Rule       => Rule,
         Param      => "Derived=Ada.Finalization.Controlled:CTRL_",
         Enable     => True,
         Defined_At => "");

      Process_Rule_Parameter
        (Rule       => Rule,
         Param      => "Enum=E_",
         Enable     => True,
         Defined_At => "");

      Process_Rule_Parameter
        (Rule       => Rule,
         Param      => "Constant=C_",
         Enable     => True,
         Defined_At => "");

      Process_Rule_Parameter
        (Rule       => Rule,
         Param      => "Exception= X_",
         Enable     => True,
         Defined_At => "");

      Process_Rule_Parameter
        (Rule       => Rule,
         Param      => "Exclusive",
         Enable     => True,
         Defined_At => "");

   end Activate_In_Test_Mode;

   ----------------------------------------------------------
   -- Allowed_As_Exemption_Parameter (Identifier_Prefixes) --
   ----------------------------------------------------------

   overriding function Allowed_As_Exemption_Parameter
     (Rule      : Identifier_Prefixes_Rule_Type;
      Parameter : String)
      return  Boolean
   is
      pragma Unreferenced (Rule);
   begin
      return Exemption_Parameters.Contains
               (Identifier_Prefixes_Exemption_Parameters,
                Parameter);
   end Allowed_As_Exemption_Parameter;

   -----------------------------------------
   -- Annotate_Rule (Identifier_Prefixes) --
   -----------------------------------------

   overriding function Annotate_Rule
     (Rule : Identifier_Prefixes_Rule_Type;
      Var  : Diagnosis_Variant := 0)
      return String
   is
   begin
      if not Gnatcheck.Options.Mapping_Mode then
         return "";
      else
         if Var = 1 and then Rule.Type_Prefix_Synonym /= null then
            return " [" & Rule.Type_Prefix_Synonym.all & "]";
         elsif Var in 2 | 3
           and then
               Rule.Concurrent_Prefix_Synonym /= null
         then
            return " [" & Rule.Concurrent_Prefix_Synonym.all & "]";
         elsif Var = 4 and then Rule.Access_Prefix_Synonym /= null then
            return " [" & Rule.Access_Prefix_Synonym.all & "]";
         elsif Var = 5 and then Rule.Class_Access_Prefix_Synonym /= null then
            return " [" & Rule.Class_Access_Prefix_Synonym.all & "]";
         elsif Var = 6
           and then
               Rule.Subprogram_Access_Prefix_Synonym /= null
         then
            return " [" & Rule.Subprogram_Access_Prefix_Synonym.all & "]";
         elsif Var = 7 and then Rule.Derived_Prefix_Synonym /= null then
            return " [" & Rule.Derived_Prefix_Synonym.all & "]";
         elsif Var = 8 and then Rule.Constant_Prefix_Synonym /= null then
            return " [" & Rule.Constant_Prefix_Synonym.all & "]";
         elsif Var = 10 and then Rule.Exclusive_Prefix_Synonym /= null then
            return " [" & Rule.Exclusive_Prefix_Synonym.all & "]";
         elsif Var = 9 and then Rule.Enum_Prefix_Synonym /= null then
            return " [" & Rule.Enum_Prefix_Synonym.all & "]";
         elsif Var = 11 and then Rule.Exception_Prefix_Synonym /= null then
            return " [" & Rule.Exception_Prefix_Synonym.all & "]";
         else
            return " [" & Rule_Name (Rule) & ':' &
                   (case Var is
                       when  1     => "Type",
                       when  2 | 3 => "Concurrent",
                       when  4     => "Access",
                       when  5     => "Class_Acces",
                       when  6     => "Subprogram_Access",
                       when  7     => "Derived",
                       when  8     => "Constant",
                       when  9     => "Enum",
                       when 10     => "Exclusive",
                       when 11     => "Exception",
                       when others => "")
                   & "]";
         end if;
      end if;

   end Annotate_Rule;

   ---------------------------------------------------
   -- At_Least_One_Prefix_Set (Identifier_Prefixes) --
   ---------------------------------------------------

   function At_Least_One_Prefix_Set
     (R    : Identifier_Prefixes_Rule_Type)
      return Boolean
   is
   begin
      return
        R.Type_Prefix              /= null or else
        R.Concurrent_Prefix        /= null or else
        R.Access_Prefix            /= null or else
        R.Class_Access_Prefix      /= null or else
        R.Subprogram_Access_Prefix /= null or else
        R.Constant_Prefix          /= null or else
        R.Exception_Prefix         /= null or else
        R.Enum_Prefix              /= null or else
        not Is_Empty (R.Derived_Prefix);
   end At_Least_One_Prefix_Set;

   -------------------------------
   -- Eq  (Identifier_Prefixes) --
   -------------------------------

   function Eq (Left, Right : String_Access) return Boolean is
   begin
      if Left = null or else Right = null then
         return True;
      else
         return To_Lower (Left.all) = To_Lower (Right.all);
      end if;
   end Eq;

   ----------------------------------------
   -- Derived_Pref (Identifier_Prefixes) --
   ----------------------------------------

   function Derived_Pref (C : Derived_Prefixes.Cursor) return String is
      Def_Pref_Rec : constant Derived_Pref_Record :=
        Derived_Prefixes.Element (C);
   begin
      return Def_Pref_Rec.Parent_Name.all & ':' & Def_Pref_Rec.Prefix.all;
   end Derived_Pref;

   ---------------------------------------------
   -- Free_All_Prefixes (Identifier_Prefixes) --
   ---------------------------------------------

   procedure Free_All_Prefixes (Rule : in out Identifier_Prefixes_Rule_Type) is
   begin
      Free (Rule.Type_Prefix);
      Free (Rule.Concurrent_Prefix);
      Free (Rule.Access_Prefix);
      Free (Rule.Class_Access_Prefix);
      Free (Rule.Subprogram_Access_Prefix);
      Free (Rule.Constant_Prefix);
      Free (Rule.Exception_Prefix);
      Free (Rule.Enum_Prefix);

      Free (Rule.Type_Prefix_Synonym);
      Free (Rule.Concurrent_Prefix_Synonym);
      Free (Rule.Access_Prefix_Synonym);
      Free (Rule.Class_Access_Prefix_Synonym);
      Free (Rule.Subprogram_Access_Prefix_Synonym);
      Free (Rule.Constant_Prefix_Synonym);
      Free (Rule.Exception_Prefix_Synonym);
      Free (Rule.Enum_Prefix_Synonym);
      Free (Rule.Exclusive_Prefix_Synonym);

      --  Some memory leak here - we do not free memory for strings.
      Derived_Prefixes.Clear (Rule.Derived_Prefix);
      Free (Rule.Derived_Prefix_Synonym);
   end Free_All_Prefixes;

   ------------------------------------------------
   -- Get_Full_Parent_Name (Identifier_Prefixes) --
   ------------------------------------------------

   function Get_Full_Parent_Name (Def : Asis.Element) return Program_Text is
      Parent_Name : Asis.Element := Def;
   begin
      case Flat_Element_Kind (Def) is
         when A_Derived_Type_Definition |
              A_Derived_Record_Extension_Definition =>
            Parent_Name := Parent_Subtype_Indication (Parent_Name);
         when A_Private_Extension_Definition =>
            Parent_Name := Ancestor_Subtype_Indication (Parent_Name);
         when others => null;
      end case;

      Parent_Name := Asis.Definitions.Subtype_Mark (Parent_Name);
      Parent_Name := Normalize_Reference (Parent_Name);
      Parent_Name := Corresponding_Name_Declaration (Parent_Name);
      Parent_Name := Corresponding_First_Subtype (Parent_Name);
      Parent_Name := First_Name (Parent_Name);

      return Full_Expanded_Name_Image (Parent_Name);

   end Get_Full_Parent_Name;

   ------------------------------------------------------------------
   -- Get_Identifier_Prefixes_Parameter_Kind (Identifier_Prefixes) --
   ------------------------------------------------------------------

   function Get_Identifier_Prefixes_Parameter_Kind
     (S    : String)
      return Identifier_Prefixes_Parameter_Kinds
   is
   begin
      return Identifier_Prefixes_Parameter_Kinds'Value (S & "_Par");
   exception
      when Constraint_Error =>
         return Not_A_Parameter;
   end Get_Identifier_Prefixes_Parameter_Kind;

   --------------------------------------
   -- Has_Prefix (Identifier_Prefixes) --
   --------------------------------------

   function Has_Prefix
     (El     : Asis.Element;
      Prefix : Wide_String)
      return   Boolean
   is
      Result : Boolean := False;
   begin
      --  At the moment this function works with A_Defining_Identifier Elements
      --  only

      Result :=
        Prefix =
        Head (Source => Defining_Name_Image (El), Count  => Prefix'Length);

      return Result;
   end Has_Prefix;

   -----------------------------------------------
   -- Has_Specific_Prefix (Identifier_Prefixes) --
   -----------------------------------------------

   function Has_Specific_Prefix (E : Asis.Element) return Boolean is
      Name_Img  : constant Program_Text := Defining_Name_Image (E);
      C_All     : String_Access_Sets.Cursor;
      C_Derived : Derived_Prefixes.Cursor;
      Result    : Boolean;
   begin
      if String_Access_Sets.Is_Empty (All_Prefixes) then

         if Identifier_Prefixes_Rule.Type_Prefix /= null then
            String_Access_Sets.Insert
              (Container => All_Prefixes,
               New_Item  =>
                 new String'(Identifier_Prefixes_Rule.Type_Prefix.all),
               Position  => C_All,
               Inserted  => Result);
         end if;

         if Identifier_Prefixes_Rule.Concurrent_Prefix /= null then
            String_Access_Sets.Insert
              (Container => All_Prefixes,
               New_Item  =>
                 new String'(Identifier_Prefixes_Rule.Concurrent_Prefix.all),
               Position  => C_All,
               Inserted  => Result);
         end if;

         if Identifier_Prefixes_Rule.Access_Prefix /= null then
            String_Access_Sets.Insert
              (Container => All_Prefixes,
               New_Item  =>
                 new String'(Identifier_Prefixes_Rule.Access_Prefix.all),
               Position  => C_All,
               Inserted  => Result);
         end if;

         if Identifier_Prefixes_Rule.Class_Access_Prefix /= null then
            String_Access_Sets.Insert
              (Container => All_Prefixes,
               New_Item  =>
                 new String'(Identifier_Prefixes_Rule.Class_Access_Prefix.all),
               Position  => C_All,
               Inserted  => Result);
         end if;

         if Identifier_Prefixes_Rule.Subprogram_Access_Prefix /= null then
            String_Access_Sets.Insert
              (Container => All_Prefixes,
               New_Item  =>
                 new String'
                   (Identifier_Prefixes_Rule.Subprogram_Access_Prefix.all),
               Position  => C_All,
               Inserted  => Result);
         end if;

         if Identifier_Prefixes_Rule.Constant_Prefix /= null then
            String_Access_Sets.Insert
              (Container => All_Prefixes,
               New_Item  =>
                 new String'(Identifier_Prefixes_Rule.Constant_Prefix.all),
               Position  => C_All,
               Inserted  => Result);
         end if;

         if Identifier_Prefixes_Rule.Exception_Prefix /= null then
            String_Access_Sets.Insert
              (Container => All_Prefixes,
               New_Item  =>
                 new String'(Identifier_Prefixes_Rule.Exception_Prefix.all),
               Position  => C_All,
               Inserted  => Result);
         end if;

         if Identifier_Prefixes_Rule.Enum_Prefix /= null then
            String_Access_Sets.Insert
              (Container => All_Prefixes,
               New_Item  =>
                 new String'(Identifier_Prefixes_Rule.Enum_Prefix.all),
               Position  => C_All,
               Inserted  => Result);
         end if;

         if not Derived_Prefixes.Is_Empty
           (Identifier_Prefixes_Rule.Derived_Prefix)
         then
            C_Derived :=
              Derived_Prefixes.First (Identifier_Prefixes_Rule.Derived_Prefix);

            while C_Derived /= Derived_Prefixes.No_Element loop
               String_Access_Sets.Insert
                 (Container => All_Prefixes,
                  New_Item  => new String'(Derived_Prefixes.Element
                                 (C_Derived).Prefix.all),
                  Position  => C_All,
                  Inserted  => Result);

               C_Derived := Next (C_Derived);
            end loop;
         end if;

      end if;

      Result := False;

      C_All := String_Access_Sets.First (All_Prefixes);

      while C_All /= String_Access_Sets.No_Element loop
         if To_Wide_String (String_Access_Sets.Element (C_All).all) =
            Head (Name_Img, String_Access_Sets.Element (C_All)'Length)
         then
            Result := True;
            exit;
         end if;

         C_All := Next (C_All);
      end loop;

      return Result;
   end Has_Specific_Prefix;

   -------------------------------------
   -- Init_Rule (Identifier_Prefixes) --
   -------------------------------------

   overriding procedure Init_Rule
     (Rule : in out Identifier_Prefixes_Rule_Type)
   is
      use Exemption_Parameters;
   begin
      Init_Rule (Rule_Template (Rule));

      Rule.Name        := new String'("Identifier_Prefixes");
      Rule.Rule_Status := Fully_Implemented;
      Rule.Help_Info   := new String'("prefixes in defining names");
      Rule.Exclusive   := True;
      Rule.Diagnosis   :=
        new String'("#1#%1% does not start with prefix %2% "       &
                    "required for subtypes"                         &

                    "#2#%1% does not start with prefix %2% "       &
                    "required for task subtypes"                    &

                    "#3#%1% does not start with prefix %2% "       &
                    "required for protected subtypes"               &

                    "#4#%1% does not start with prefix %2% "       &
                    "required for access subtypes"                  &

                    "#5#%1% does not start with prefix %2% "       &
                    "required for access-to-class subtypes"         &

                    "#6#%1% does not start with prefix %2% "       &
                    "required for access-to-subprogram subtypes"    &

                    "#7#%1% does not start with prefix %2% "       &
                    "required for types derived from %3%"           &

                    "#8#%1% does not start with prefix %2% "       &
                    "required for constants"                        &

                    "#9#%1% does not start with prefix %2% "       &
                    "required for enumeration literals"             &

                    "#10#%1% has prefix reserved for a different " &
                    "identifier kind"                              &

                    "#11#%1% does not start with prefix %2% "      &
                    "required for exceptions");

      --  Exemption parameters:
      Insert (Identifier_Prefixes_Exemption_Parameters, "type");
      Insert (Identifier_Prefixes_Exemption_Parameters, "concurrent");
      Insert (Identifier_Prefixes_Exemption_Parameters, "access");
      Insert (Identifier_Prefixes_Exemption_Parameters, "class_access");
      Insert (Identifier_Prefixes_Exemption_Parameters, "subprogram_access");
      Insert (Identifier_Prefixes_Exemption_Parameters, "derived");
      Insert (Identifier_Prefixes_Exemption_Parameters, "constant");
      Insert (Identifier_Prefixes_Exemption_Parameters, "enum");
      Insert (Identifier_Prefixes_Exemption_Parameters, "exception");
      Insert (Identifier_Prefixes_Exemption_Parameters, "exclusive");
   end Init_Rule;

   -----------------------------------------------
   -- Is_Derived_Type_Par (Identifier_Prefixes) --
   -----------------------------------------------

   function Is_Derived_Type_Par (S : String) return Boolean is
      Result    : Boolean;
      First_Idx : constant Natural := S'First;
      Last_Idx  : constant Natural := S'Last;
      Colon_Idx : constant Natural := Index (S, ":");
   begin
      Result :=
        Is_Ada_Name
          (To_Wide_String (Trim (S (First_Idx .. Colon_Idx - 1), Both)));

      if Result then
         Result :=
           Is_Identifier_Prefix
             (To_Wide_String (Trim (S (Colon_Idx + 1 .. Last_Idx), Both)));
      end if;

      return Result;
   end Is_Derived_Type_Par;

   --------------------------------------
   -- Print_Rule (Identifier_Prefixes) --
   --------------------------------------

   overriding procedure Print_Rule
     (Rule         : Identifier_Prefixes_Rule_Type;
      Indent_Level : Natural := 0)
   is
      First_Param       : Boolean         := True;
      Rule_Name_Padding : constant String :=
        (1 .. Rule.Name'Length + 2 => ' ');
      C : Derived_Prefixes.Cursor;

   begin
      Print_Rule (Rule_Template (Rule), Indent_Level);

      if Rule.Type_Prefix /= null then
         Report_No_EOL (": Type = " & Rule.Type_Prefix.all);
         First_Param := False;
      end if;

      if Rule.Concurrent_Prefix /= null then
         if First_Param then
            Report_No_EOL (": Concurrent = " & Rule.Concurrent_Prefix.all);
            First_Param := False;
         else
            Report (", ");
            Report_No_EOL
              (Rule_Name_Padding &
               "Concurrent = " & Rule.Concurrent_Prefix.all,
              Indent_Level);
         end if;
      end if;

      if Rule.Access_Prefix /= null then
         if First_Param then
            Report_No_EOL (": Access = " & Rule.Access_Prefix.all);
            First_Param := False;
         else
            Report (", ");
            Report_No_EOL
              (Rule_Name_Padding &
               "Access = " & Rule.Access_Prefix.all,
              Indent_Level);
         end if;
      end if;

      if Rule.Class_Access_Prefix /= null then
         if First_Param then
            Report_No_EOL (": Class_Access = " & Rule.Class_Access_Prefix.all);
            First_Param := False;
         else
            Report (", ");
            Report_No_EOL
              (Rule_Name_Padding &
               "Class_Access = " & Rule.Class_Access_Prefix.all,
              Indent_Level);
         end if;
      end if;

      if Rule.Subprogram_Access_Prefix /= null then
         if First_Param then
            Report_No_EOL (": Subprogram_Access = " &
                           Rule.Subprogram_Access_Prefix.all);
            First_Param := False;
         else
            Report (", ");
            Report_No_EOL
              (Rule_Name_Padding &
               "Subprogram_Access = " & Rule.Subprogram_Access_Prefix.all,
              Indent_Level);
         end if;
      end if;

      if Rule.Constant_Prefix /= null then
         if First_Param then
            Report_No_EOL (": Constant = " & Rule.Constant_Prefix.all);
            First_Param := False;
         else
            Report (", ");
            Report_No_EOL
              (Rule_Name_Padding &
               "Constant = " & Rule.Constant_Prefix.all,
              Indent_Level);
         end if;
      end if;

      if Rule.Exception_Prefix /= null then
         if First_Param then
            Report_No_EOL (": Exception = " & Rule.Exception_Prefix.all);
            First_Param := False;
         else
            Report (", ");
            Report_No_EOL
              (Rule_Name_Padding &
               "Exception = " & Rule.Exception_Prefix.all,
              Indent_Level);
         end if;
      end if;

      if Rule.Enum_Prefix /= null then
         if First_Param then
            Report_No_EOL (": Enum = " & Rule.Enum_Prefix.all);
            First_Param := False;
         else
            Report (", ");
            Report_No_EOL
              (Rule_Name_Padding &
               "Enum = " & Rule.Enum_Prefix.all,
              Indent_Level);
         end if;
      end if;

      if not Derived_Prefixes.Is_Empty (Rule.Derived_Prefix) then
         C := Derived_Prefixes.First (Rule.Derived_Prefix);

         while C /= Derived_Prefixes.No_Element loop

            if First_Param then
               Report_No_EOL (": Derived  = " & Derived_Pref (C));
               First_Param := False;
            else
               Report (", ");
               Report_No_EOL
                 (Rule_Name_Padding &
                  "Derived = " & Derived_Pref (C),
                 Indent_Level);
            end if;

            C := Next (C);
         end loop;
      end if;

      --  We have to print out Exclusive parameter, but this would make sense
      --  only if at least one prefix is specified

      if not First_Param and then Rule.Exclusive then
         Report (", ");
         Report_No_EOL
           (Rule_Name_Padding & "Exclusive");
      end if;

   end Print_Rule;

   ----------------------------------------------
   -- Print_Rule_To_File (Identifier_Prefixes) --
   ----------------------------------------------

   overriding procedure Print_Rule_To_File
     (Rule         : Identifier_Prefixes_Rule_Type;
      Rule_File    : File_Type;
      Indent_Level : Natural := 0)
   is
      First_Param       : Boolean         := True;
      Rule_Name_Padding : constant String :=
        (1 .. Rule.Name'Length + 4 => ' ');
      C : Derived_Prefixes.Cursor;

   begin
      Print_Rule_To_File (Rule_Template (Rule), Rule_File, Indent_Level);

      if Rule.Type_Prefix /= null then
         Put (Rule_File, ": Type = " & Rule.Type_Prefix.all);
         First_Param := False;
      end if;

      if Rule.Concurrent_Prefix /= null then
         if First_Param then
            Put (Rule_File, ": Concurrent = " & Rule.Concurrent_Prefix.all);
            First_Param := False;
         else
            Put_Line (Rule_File, ", ");

            for J in 1 .. Indent_Level loop
               Put (Rule_File, Get_Indent_String);
            end loop;

            Put (Rule_File,
                 Rule_Name_Padding &
                 "Concurrent = " & Rule.Concurrent_Prefix.all);
         end if;
      end if;

      if Rule.Access_Prefix /= null then
         if First_Param then
            Put (Rule_File, ": Access = " & Rule.Access_Prefix.all);
            First_Param := False;
         else
            Put_Line (Rule_File, ", ");

            for J in 1 .. Indent_Level loop
               Put (Rule_File, Get_Indent_String);
            end loop;

            Put (Rule_File,
                 Rule_Name_Padding & "Access = " & Rule.Access_Prefix.all);
         end if;
      end if;

      if Rule.Class_Access_Prefix /= null then
         if First_Param then
            Put
              (Rule_File, ": Class_Access = " & Rule.Class_Access_Prefix.all);
            First_Param := False;
         else
            Put_Line (Rule_File, ", ");

            for J in 1 .. Indent_Level loop
               Put (Rule_File, Get_Indent_String);
            end loop;

            Put (Rule_File,
                 Rule_Name_Padding &
                 "Class_Access = " & Rule.Class_Access_Prefix.all);
         end if;
      end if;

      if Rule.Subprogram_Access_Prefix /= null then
         if First_Param then
            Put (Rule_File,
                ": Subprogram_Access = " & Rule.Subprogram_Access_Prefix.all);
            First_Param := False;
         else
            Put_Line (Rule_File, ", ");

            for J in 1 .. Indent_Level loop
               Put (Rule_File, Get_Indent_String);
            end loop;

            Put (Rule_File,
                 Rule_Name_Padding &
                 "Subprogram_Access = " & Rule.Subprogram_Access_Prefix.all);
         end if;
      end if;

      if Rule.Constant_Prefix /= null then
         if First_Param then
            Put (Rule_File, ": Constant = " & Rule.Constant_Prefix.all);
            First_Param := False;
         else
            Put_Line (Rule_File, ", ");

            for J in 1 .. Indent_Level loop
               Put (Rule_File, Get_Indent_String);
            end loop;

            Put (Rule_File,
                 Rule_Name_Padding & "Constant = " & Rule.Constant_Prefix.all);
         end if;
      end if;

      if Rule.Exception_Prefix /= null then
         if First_Param then
            Put (Rule_File, ": Exception = " & Rule.Exception_Prefix.all);
            First_Param := False;
         else
            Put_Line (Rule_File, ", ");

            for J in 1 .. Indent_Level loop
               Put (Rule_File, Get_Indent_String);
            end loop;

            Put (Rule_File,
                 Rule_Name_Padding & "Exception = " &
                 Rule.Exception_Prefix.all);
         end if;
      end if;

      if Rule.Enum_Prefix /= null then
         if First_Param then
            Put (Rule_File, ": Enum = " & Rule.Enum_Prefix.all);
            First_Param := False;
         else
            Put_Line (Rule_File, ", ");

            for J in 1 .. Indent_Level loop
               Put (Rule_File, Get_Indent_String);
            end loop;

            Put (Rule_File,
                 Rule_Name_Padding & "Enum = " & Rule.Enum_Prefix.all);
         end if;
      end if;

      if not Derived_Prefixes.Is_Empty (Rule.Derived_Prefix) then
         C := Derived_Prefixes.First (Rule.Derived_Prefix);

         while C /= Derived_Prefixes.No_Element loop

            if First_Param then
               Put (Rule_File, ": Derived  = " & Derived_Pref (C));
               First_Param := False;
            else
               Put_Line (Rule_File, ", ");

               for J in 1 .. Indent_Level loop
                  Put (Rule_File, Get_Indent_String);
               end loop;

               Put (Rule_File,
                    Rule_Name_Padding & "Derived = " & Derived_Pref (C));
            end if;

            C := Next (C);
         end loop;
      end if;

      --  We have to print out Exclusive parameter, but this would make sense
      --  only if at least one prefix is specified

      if not First_Param and then Rule.Exclusive then
         Put_Line (Rule_File, ", ");
         Put (Rule_File, Rule_Name_Padding & "Exclusive");
      end if;
   end Print_Rule_To_File;

   --------------------------------------------------
   -- Process_Rule_Parameter (Identifier_Prefixes) --
   --------------------------------------------------

   overriding procedure Process_Rule_Parameter
     (Rule       : in out Identifier_Prefixes_Rule_Type;
      Param      :        String;
      Enable     :        Boolean;
      Defined_At : String)
   is
      pragma Unreferenced (Defined_At);
      First_Str_Idx, Last_Str_Idx : Natural;
      --  Beginning and end of the 'string' part of the parameter, see the
      --  rule parameter description in the spec. First_Str_Idx is set to 0 if
      --  the parameter does not contain a '=' character.

      First_Par_Idx, Last_Par_Idx : Natural;
      --  If the parameter contains a '=' character, set to point to the
      --  beginning and the end of the part of the parameter that precedes '='.
      --  Otherwise First_Par_Idx points to the first, and Last_Par_Idx - to
      --  the last non-blank character in Param (First_Idx .. Last_Idx)

      Is_Legal_Prefix       : Boolean := False;
      Is_Legal_Der_Type_Par : Boolean := False;

      Parameter_Kind : Identifier_Prefixes_Parameter_Kinds;

   begin
      if Param = "" then
         if Enable then
            Rule.Rule_State := Enabled;
         else
            Rule.Rule_State := Disabled;
         end if;

         return;
      end if;

      Parse_Par
        (First_Par_Idx, Last_Par_Idx, First_Str_Idx, Last_Str_Idx, Param);

      Parameter_Kind :=
        Get_Identifier_Prefixes_Parameter_Kind
          (Param (First_Par_Idx .. Last_Par_Idx));

      if Parameter_Kind = Not_A_Parameter then
         Error ("(" & Rule.Name.all & ") wrong parameter : " &
                Param & ", ignored");
         return;
      end if;

      if First_Str_Idx = 0 then

         if Enable then  --  +R
            if Parameter_Kind =  Exclusive_Par then
               Rule.Exclusive := True;

               if Has_Synonym (Rule) then
                  Free (Rule.Exclusive_Prefix_Synonym);
                  Rule.Exclusive_Prefix_Synonym :=
                    new String'(Rule_Synonym (Rule));
               end if;

               if At_Least_One_Prefix_Set (Rule) then
                  Rule.Rule_State := Enabled;
               end if;
            else
               Error
                ("(" & Rule.Name.all & ") wrong parameter : " &
                 Param & ", ignored");
            end if;
         else    --  -R
            case Parameter_Kind is
               when All_Prefixes_Par =>
                  Free_All_Prefixes (Rule);
                  Rule.Rule_State := Disabled;
               when Type_Par =>
                  Free (Rule.Type_Prefix);
                  Free (Rule.Type_Prefix_Synonym);
               when Concurrent_Par =>
                  Free (Rule.Concurrent_Prefix);
                  Free (Rule.Concurrent_Prefix_Synonym);
               when Access_Par =>
                  Free (Rule.Access_Prefix);
                  Free (Rule.Access_Prefix_Synonym);
               when Class_Access_Par =>
                  Free (Rule.Class_Access_Prefix);
                  Free (Rule.Class_Access_Prefix_Synonym);
               when Subprogram_Access_Par =>
                  Free (Rule.Subprogram_Access_Prefix);
                  Free (Rule.Subprogram_Access_Prefix_Synonym);
               when Derived_Par =>
                  Free_All_Prefixes (Rule);
                  Free (Rule.Derived_Prefix_Synonym);
               when Constant_Par =>
                  Free (Rule.Constant_Prefix);
                  Free (Rule.Constant_Prefix_Synonym);
               when Exception_Par =>
                  Free (Rule.Exception_Prefix);
                  Free (Rule.Exception_Prefix_Synonym);
               when Enum_Par =>
                  Free (Rule.Enum_Prefix);
                  Free (Rule.Enum_Prefix_Synonym);
               when Exclusive_Par =>
                  Rule.Exclusive := False;
                  Free (Rule.Exclusive_Prefix_Synonym);
               when others =>
                  pragma Assert (False);
                  return;
            end case;

            if not At_Least_One_Prefix_Set (Rule) then
               Rule.Rule_State := Disabled;
            end if;

         end if;

      else

         if not Enable
           or else Parameter_Kind = Exclusive_Par
         then
            Error
             ("(" & Rule.Name.all & ") wrong parameter : " &
              Param & ", ignored");
            return;
         end if;

         if Parameter_Kind = Derived_Par then
            Is_Legal_Der_Type_Par :=
              Is_Derived_Type_Par (Param (First_Str_Idx .. Last_Str_Idx));
         else
            Is_Legal_Prefix :=
              Is_Identifier_Prefix
                (To_Wide_String (Param (First_Str_Idx .. Last_Str_Idx)));
         end if;

         if not (Is_Legal_Prefix or else Is_Legal_Der_Type_Par) then
            Error
             ("(" & Rule.Name.all & ") wrong parameter : " &
              Param & ", ignored");
            return;
         end if;

         case Parameter_Kind is
            when Type_Par =>
               Rule.Type_Prefix :=
                 new String'(Param (First_Str_Idx .. Last_Str_Idx));

               if Has_Synonym (Rule) then
                  Free (Rule.Type_Prefix_Synonym);
                  Rule.Type_Prefix_Synonym :=
                    new String'(Rule_Synonym (Rule));
               end if;

            when Concurrent_Par =>
               Rule.Concurrent_Prefix :=
                 new String'(Param (First_Str_Idx .. Last_Str_Idx));

               if Has_Synonym (Rule) then
                  Free (Rule.Concurrent_Prefix_Synonym);
                  Rule.Concurrent_Prefix_Synonym :=
                    new String'(Rule_Synonym (Rule));
               end if;

            when Access_Par =>
               Rule.Access_Prefix :=
                 new String'(Param (First_Str_Idx .. Last_Str_Idx));

               if Has_Synonym (Rule) then
                  Free (Rule.Access_Prefix_Synonym);
                  Rule.Access_Prefix_Synonym :=
                    new String'(Rule_Synonym (Rule));
               end if;

            when Class_Access_Par =>
               Rule.Class_Access_Prefix :=
                 new String'(Param (First_Str_Idx .. Last_Str_Idx));

               if Has_Synonym (Rule) then
                  Free (Rule.Class_Access_Prefix_Synonym);
                  Rule.Class_Access_Prefix_Synonym :=
                    new String'(Rule_Synonym (Rule));
               end if;

            when Subprogram_Access_Par =>
               Rule.Subprogram_Access_Prefix :=
                 new String'(Param (First_Str_Idx .. Last_Str_Idx));

               if Has_Synonym (Rule) then
                  Rule.Subprogram_Access_Prefix_Synonym :=
                    new String'(Rule_Synonym (Rule));
               end if;

            when Derived_Par =>
               declare
                  Colon_Idx : constant Natural :=
                    Index (Param (First_Str_Idx .. Last_Str_Idx), ":");
                  New_Pref : Derived_Pref_Record;

                  C        : Derived_Prefixes.Cursor;
                  Inserted : Boolean;
               begin
                  New_Pref.Parent_Name := new String'
                    (Trim (Param (First_Str_Idx .. Colon_Idx - 1), Right));

                  New_Pref.Prefix := new String'
                    (Trim (Param (Colon_Idx + 1 .. Last_Str_Idx), Left));

                  Derived_Prefixes.Insert
                    (Container => Rule.Derived_Prefix,
                     New_Item  => New_Pref,
                     Position  => C,
                     Inserted  => Inserted);

                  if not Inserted then
                     Derived_Prefixes.Replace_Element
                       (Container => Rule.Derived_Prefix,
                        Position  => C,
                        New_Item  => New_Pref);
                  end if;
               end;

               if Has_Synonym (Rule) then
                  Free (Rule.Derived_Prefix_Synonym);
                  Rule.Derived_Prefix_Synonym :=
                    new String'(Rule_Synonym (Rule));
               end if;

            when Constant_Par =>
               Rule.Constant_Prefix :=
                 new String'(Param (First_Str_Idx .. Last_Str_Idx));

               if Has_Synonym (Rule) then
                  Free (Rule.Constant_Prefix_Synonym);
                  Rule.Constant_Prefix_Synonym :=
                    new String'(Rule_Synonym (Rule));
               end if;

            when Exception_Par =>
               Rule.Exception_Prefix :=
                 new String'(Param (First_Str_Idx .. Last_Str_Idx));

               if Has_Synonym (Rule) then
                  Free (Rule.Exception_Prefix_Synonym);
                  Rule.Exception_Prefix_Synonym :=
                    new String'(Rule_Synonym (Rule));
               end if;

            when Enum_Par =>
               Rule.Enum_Prefix :=
                 new String'(Param (First_Str_Idx .. Last_Str_Idx));

               if Has_Synonym (Rule) then
                  Free (Rule.Enum_Prefix_Synonym);
                  Rule.Enum_Prefix_Synonym :=
                    new String'(Rule_Synonym (Rule));
               end if;

            when others =>
               pragma Assert (False);
               return;
         end case;

         Rule.Rule_State := Enabled;
      end if;

   end Process_Rule_Parameter;

   ---------------------------------------------
   -- Rule_Check_Pre_Op (Identifier_Prefixes) --
   ---------------------------------------------

   overriding procedure Rule_Check_Pre_Op
     (Rule    : in out Identifier_Prefixes_Rule_Type;
      Element :        Asis.Element;
      Control : in out Traverse_Control;
      State   : in out Rule_Traversal_State)
   is
      pragma Unreferenced (Control);
      Tmp             : Asis.Element;
      Check_Exclusive : Boolean := False;
      Derived_Check   : Derived_Pref_Record;
      C_Pref_To_Check : Derived_Prefixes.Cursor;
   begin
      case Defining_Name_Kind (Element) is
         when A_Defining_Identifier =>
            Tmp := Get_Enclosing_Element;

            if Declaration_Kind (Tmp) = A_Subtype_Declaration then
               Tmp := Corresponding_First_Subtype (Tmp);
            end if;
         when A_Defining_Enumeration_Literal =>
            Tmp := Element;
         when others =>
            return;
      end case;

      case Flat_Element_Kind (Tmp) is

         when A_Defining_Enumeration_Literal =>
            if Rule.Enum_Prefix /= null then
               if not Has_Prefix
                 (Element, To_Wide_String (Rule.Enum_Prefix.all))
               then
                  State.Detected    := True;
                  State.Diagnosis   := 9;
                  State.Diag_Params :=
                    Enter_String
                      ("%1%" & To_String (Defining_Name_Image (Element))
                       &
                       "%2%" & Rule.Enum_Prefix.all);
               end if;

               return;
            else
               Check_Exclusive := True;
            end if;

         when A_Function_Renaming_Declaration =>
            if not Is_Equal (Corresponding_Declaration (Tmp), Tmp) then
               --  Renaming-as-body, completion of another declaration, so
               return;
            end if;

            Tmp := Corresponding_Base_Entity (Tmp);

            if Expression_Kind (Tmp) = A_Selected_Component then
               Tmp := Selector (Tmp);
            end if;

            if Expression_Kind (Tmp) = An_Enumeration_Literal then
               if Rule.Enum_Prefix /= null then
                  if not Has_Prefix
                    (Element, To_Wide_String (Rule.Enum_Prefix.all))
                  then
                     State.Detected    := True;
                     State.Diagnosis   := 9;
                     State.Diag_Params :=
                       Enter_String
                         ("%1%" & To_String (Defining_Name_Image (Element))
                          &
                          "%2%" & Rule.Enum_Prefix.all);
                  end if;

                  return;
               else
                  Check_Exclusive := True;
               end if;
            else
               Check_Exclusive := True;
            end if;

         when A_Procedure_Renaming_Declaration =>
            if not Is_Equal (Corresponding_Declaration (Tmp), Tmp) then
               --  Renaming-as-body, completion of another declaration, so
               return;
            end if;

         when A_Constant_Declaration          |
              A_Deferred_Constant_Declaration |
              An_Integer_Number_Declaration   |
              A_Real_Number_Declaration       |
              An_Object_Renaming_Declaration  =>

            if Flat_Element_Kind (Tmp) = A_Constant_Declaration
             and then
               not Is_Nil (Corresponding_Constant_Declaration (Element))
            then
               --  No check for names from full declarations that correspond to
               --  deferred constants
               return;
            end if;

            if Flat_Element_Kind (Tmp) /= An_Object_Renaming_Declaration
              or else
               Is_Constant (First_Name (Tmp))
            then
               if Rule.Constant_Prefix /= null then
                  if not Has_Prefix
                    (Element, To_Wide_String (Rule.Constant_Prefix.all))
                  then
                     State.Detected    := True;
                     State.Diagnosis   := 8;
                     State.Diag_Params :=
                       Enter_String
                         ("%1%" & To_String (Defining_Name_Image (Element))
                          &
                          "%2%" & Rule.Constant_Prefix.all);
                  end if;

                  return;
               end if;
            end if;

            Check_Exclusive := True;

         when An_Exception_Declaration          |
              An_Exception_Renaming_Declaration =>
            if Rule.Exception_Prefix /= null then
               if not Has_Prefix
                 (Element, To_Wide_String (Rule.Exception_Prefix.all))
               then
                  State.Detected    := True;
                  State.Diagnosis   := 11;
                  State.Diag_Params :=
                    Enter_String
                      ("%1%" & To_String (Defining_Name_Image (Element))
                       &
                       "%2%" & Rule.Exception_Prefix.all);
               end if;

               return;
            else
               Check_Exclusive := True;
            end if;

         when An_Ordinary_Type_Declaration    |
              A_Task_Type_Declaration         |
              A_Protected_Type_Declaration    |
              A_Private_Type_Declaration      |
              A_Private_Extension_Declaration |
              A_Formal_Type_Declaration       =>

            if Flat_Element_Kind (Tmp) in
               An_Ordinary_Type_Declaration .. A_Protected_Type_Declaration
              and then
               Declaration_Kind (Corresponding_Type_Declaration (Tmp)) in
               A_Private_Type_Declaration .. A_Private_Extension_Declaration
            then
               --  No check for full type declarations corresponding to
               --  private types
               return;
            end if;

            if Flat_Element_Kind (Tmp) in
               A_Task_Type_Declaration .. A_Protected_Type_Declaration
              and then
               Rule.Concurrent_Prefix /= null
            then
               if not Has_Prefix
                 (Element, To_Wide_String (Rule.Concurrent_Prefix.all))
               then
                  State.Detected := True;

                  if Flat_Element_Kind (Tmp) = A_Task_Type_Declaration then
                     State.Diagnosis := 2;
                  else
                     State.Diagnosis := 3;
                  end if;

                  State.Diag_Params :=
                    Enter_String
                      ("%1%" & To_String (Defining_Name_Image (Element))
                       &
                       "%2%" & Rule.Concurrent_Prefix.all);
               end if;

               return;
            end if;

            Tmp := Type_Declaration_View (Tmp);

            if Type_Kind (Tmp) = An_Access_Type_Definition
              or else
               Formal_Type_Kind (Tmp) = A_Formal_Access_Type_Definition
            then

               if Access_Type_Kind (Tmp) in
                  An_Access_To_Procedure .. An_Access_To_Protected_Function
                 and then
                   Rule.Subprogram_Access_Prefix /= null
               then

                  if not Has_Prefix
                    (Element,
                     To_Wide_String (Rule.Subprogram_Access_Prefix.all))
                  then
                     State.Detected    := True;
                     State.Diagnosis   := 6;
                     State.Diag_Params :=
                       Enter_String
                         ("%1%" & To_String (Defining_Name_Image (Element))
                          &
                          "%2%" & Rule.Subprogram_Access_Prefix.all);
                  end if;

                  return;

               end if;

               if Is_Access_To_Class (Tmp) and then
                  Rule.Class_Access_Prefix /= null
               then

                  if not Has_Prefix
                    (Element,
                     To_Wide_String (Rule.Class_Access_Prefix.all))
                  then
                     State.Detected    := True;
                     State.Diagnosis   := 5;
                     State.Diag_Params :=
                       Enter_String
                         ("%1%" & To_String (Defining_Name_Image (Element))
                          &
                          "%2%" & Rule.Class_Access_Prefix.all);
                  end if;

                  return;

               end if;

               if Rule.Access_Prefix /= null then

                  if not Has_Prefix
                    (Element, To_Wide_String (Rule.Access_Prefix.all))
                  then
                     State.Detected    := True;
                     State.Diagnosis   := 4;
                     State.Diag_Params :=
                       Enter_String
                         ("%1%" & To_String (Defining_Name_Image (Element))
                          &
                          "%2%" & Rule.Access_Prefix.all);
                  end if;

                  return;

               end if;

            end if;

            if Type_Kind (Tmp) in
               A_Derived_Type_Definition ..
                 A_Derived_Record_Extension_Definition
              or else
               Formal_Type_Kind (Tmp) = A_Formal_Derived_Type_Definition
              or else
               Declaration_Kind (Get_Enclosing_Element) =
                 A_Private_Extension_Declaration
            then
               Derived_Check.Parent_Name :=
                 new String'(To_String (Get_Full_Parent_Name (Tmp)));

               if Derived_Check.Parent_Name.all /= "" then
                  C_Pref_To_Check :=
                    Find (Rule.Derived_Prefix, Derived_Check);

                  if C_Pref_To_Check /= Derived_Prefixes.No_Element then
                     if not Has_Prefix
                       (Element,
                        To_Wide_String
                          (Derived_Prefixes.Element
                             (C_Pref_To_Check).Prefix.all))
                     then
                        State.Detected    := True;
                        State.Diagnosis   := 7;
                        State.Diag_Params :=
                          Enter_String
                            ("%1%" & To_String (Defining_Name_Image (Element))
                             &
                             "%2%" & Derived_Prefixes.Element
                                       (C_Pref_To_Check).Prefix.all
                             &
                             "%3%" & Derived_Check.Parent_Name.all);
                     end if;

                     return;
                  end if;

               end if;

            end if;

            if Rule.Type_Prefix /= null then
               if not Has_Prefix
                 (Element, To_Wide_String (Rule.Type_Prefix.all))
               then
                  State.Detected  := True;
                  State.Diagnosis := 1;
                  State.Diag_Params :=
                    Enter_String
                      ("%1%" & To_String (Defining_Name_Image (Element))
                       &
                       "%2%" & Rule.Type_Prefix.all);
               end if;

               return;
            else
               Check_Exclusive := True;
            end if;

         when An_Incomplete_Type_Declaration       |
              A_Tagged_Incomplete_Type_Declaration |
              A_Formal_Incomplete_Type_Declaration =>
            --  These names are never checked
            return;

         when A_Procedure_Body_Stub .. A_Function_Body_Stub =>
            if Is_Nil (Corresponding_Declaration (Tmp)) then
               Check_Exclusive := True;
            else
               --  Completion of another declaration
               return;
            end if;

         when A_Procedure_Body_Declaration |
              A_Function_Body_Declaration  =>

            if Is_Subunit (Tmp) or else
               not Is_Nil (Corresponding_Declaration (Tmp))
            then
               --  Completion of another declaration
               return;
            else
               Check_Exclusive := True;
            end if;

         when A_Package_Body_Declaration   |
              A_Task_Body_Declaration      |
              A_Protected_Body_Declaration |
              An_Entry_Body_Declaration    |
              A_Package_Body_Stub          |
              A_Task_Body_Stub             |
              A_Protected_Body_Stub        =>
            --  Completion of another declaration
            return;
         when others =>
            Check_Exclusive := True;
      end case;

      if Check_Exclusive
       and then
         Rule.Exclusive
       and then
         Has_Specific_Prefix (Element)
      then
         State.Detected    := True;
         State.Diagnosis   := 10;
         State.Diag_Params :=
           Enter_String ("%1%" & To_String (Defining_Name_Image (Element)));
      end if;

   end Rule_Check_Pre_Op;

   ------------------------------------------
   -- Rule_Parameter (Identifier_Prefixes) --
   ------------------------------------------

   overriding function Rule_Parameter
     (Rule : Identifier_Prefixes_Rule_Type;
      Diag : String)
      return String
   is
      pragma Unreferenced (Rule);
   begin
      if Index (Diag, "task") /= 0
        or else
         Index (Diag, "protected") /= 0
      then
         return "concurrent";
      elsif Index (Diag, "access-to-class") /= 0 then
         return "class_acces";
      elsif Index (Diag, "access-to-subprogram") /= 0 then
         return "subprogram_access";
      elsif Index (Diag, "derived") /= 0 then
         return "derived";
      elsif Index (Diag, "constants") /= 0 then
         return "constant";
      elsif Index (Diag, "enumeration") /= 0 then
         return "enum";
      elsif Index (Diag, "exceptions") /= 0 then
         return "exception";
      elsif Index (Diag, "access") /= 0 then
         return "access";
      elsif Index (Diag, "subtypes") /= 0 then
         return "type";
      elsif Index (Diag, "reserved") /= 0 then
         return "exclusive";
      else
         return "";
      end if;
   end Rule_Parameter;

   -----------------------------------------
   -- XML_Rule_Help (Identifier_Prefixes) --
   -----------------------------------------

   overriding procedure XML_Print_Rule
     (Rule         : Identifier_Prefixes_Rule_Type;
      Indent_Level : Natural := 0)
   is
      C                : Derived_Prefixes.Cursor;
      Prefix_Specified : Boolean := False;
   begin
      XML_Report
        ("<rule id=""" & Rule_Name (Rule) & """>",
         Indent_Level);

      if Rule.Type_Prefix /= null then
         XML_Report
           ("<parameter>Type=" & Rule.Type_Prefix.all & "</parameter>",
            Indent_Level + 1);
         Prefix_Specified := True;
      end if;

      if Rule.Concurrent_Prefix /= null then
         XML_Report
           ("<parameter>Concurrent=" & Rule.Concurrent_Prefix.all &
              "</parameter>",
            Indent_Level + 1);
         Prefix_Specified := True;
      end if;

      if Rule.Access_Prefix /= null then
         XML_Report
           ("<parameter>Access=" & Rule.Access_Prefix.all & "</parameter>",
            Indent_Level + 1);
         Prefix_Specified := True;
      end if;

      if Rule.Class_Access_Prefix /= null then
         XML_Report
           ("<parameter>Class_Access=" & Rule.Class_Access_Prefix.all &
              "</parameter>",
            Indent_Level + 1);
         Prefix_Specified := True;
      end if;

      if Rule.Subprogram_Access_Prefix /= null then
         XML_Report
           ("<parameter>Subprogram_Access="     &
              Rule.Subprogram_Access_Prefix.all & "</parameter>",
            Indent_Level + 1);
         Prefix_Specified := True;
      end if;

      if Rule.Constant_Prefix /= null then
         XML_Report
           ("<parameter>Constant=" & Rule.Constant_Prefix.all & "</parameter>",
            Indent_Level + 1);
         Prefix_Specified := True;
      end if;

      if Rule.Exception_Prefix /= null then
         XML_Report
           ("<parameter>Exception=" & Rule.Exception_Prefix.all &
              "</parameter>",
            Indent_Level + 1);
         Prefix_Specified := True;
      end if;

      if Rule.Enum_Prefix /= null then
         XML_Report
           ("<parameter>Enum=" & Rule.Enum_Prefix.all & "</parameter>",
            Indent_Level + 1);
         Prefix_Specified := True;
      end if;

      if not Derived_Prefixes.Is_Empty (Rule.Derived_Prefix) then
         C := Derived_Prefixes.First (Rule.Derived_Prefix);

         while C /= Derived_Prefixes.No_Element loop
            XML_Report
              ("<parameter>Derived=" & Derived_Pref (C) & "</parameter>",
               Indent_Level + 1);
            Prefix_Specified := True;

            C := Next (C);
         end loop;
      end if;

      --  We have to print out Exclusive parameter, but this would make sense
      --  only if at least one prefix is specified

      if Prefix_Specified and then Rule.Exclusive then
         XML_Report ("<parameter>Exclusive</parameter>", Indent_Level + 1);
      end if;

      XML_Report ("</rule>", Indent_Level);
   end XML_Print_Rule;

   -----------------------------------------
   -- XML_Rule_Help (Identifier_Prefixes) --
   -----------------------------------------

   overriding procedure XML_Rule_Help
     (Rule  : Identifier_Prefixes_Rule_Type;
      Level : Natural)
   is
   begin
      Info (Level * Ident_String               &
            "<field switch=""+R"               &
            Rule.Name.all                      &
            ":Type"""                          &
            " label="""                        &
            "prefix for type names"            &
            " (empty string disables check)""" &
            " separator=""="""                 &
            " switch-off=""-R"                 &
            Rule.Name.all                      &
            ":Type"""                          &
            "/>");

      Info (Level * Ident_String                       &
            "<field switch=""+R"                       &
            Rule.Name.all                              &
            ":Concurrent"""                            &
            " label="""                                &
            "prefix for task and protected type names" &
            " (empty string disables check)"""         &
            " separator=""="""                         &
            " switch-off=""-R"                         &
            Rule.Name.all                              &
            ":Concurrent"""                            &
            "/>");

      Info (Level * Ident_String               &
            "<field switch=""+R"               &
            Rule.Name.all                      &
            ":Access"""                        &
            " label="""                        &
            "prefix for access type names"     &
            " (empty string disables check)""" &
            " separator=""="""                 &
            " switch-off=""-R"                 &
            Rule.Name.all                      &
            ":Access"""                        &
            "/>");

      Info (Level * Ident_String                 &
            "<field switch=""+R"                 &
            Rule.Name.all                        &
            ":Class_Access"""                    &
            " label="""                          &
            "prefix for class access type names" &
            " (empty string disables check)"""   &
            " separator=""="""                   &
            " switch-off=""-R"                   &
            Rule.Name.all                        &
            ":Class_Access"""                    &
            "/>");

      Info (Level * Ident_String                         &
            "<field switch=""+R"                         &
            Rule.Name.all                                &
            ":Subprogram_Access"""                       &
            " label="""                                  &
            "prefix for access-to-subprogram type names" &
            " (empty string disables check)"""           &
            " separator=""="""                           &
            " switch-off=""-R"                           &
            Rule.Name.all                                &
            ":Subprogram_Access"""                       &
            "/>");

      Info (Level * Ident_String               &
            "<field switch=""+R"               &
            Rule.Name.all                      &
            ":Derived"""                       &
            " label="""                        &
            "prefix for derived type names"    &
            " (empty string disables check)""" &
            " separator=""="""                 &
            " switch-off=""-R"                 &
            Rule.Name.all                      &
            ":Derived"""                       &
            "/>");

      Info (Level * Ident_String               &
            "<field switch=""+R"               &
            Rule.Name.all                      &
            ":Constant"""                      &
            " label="""                        &
            "prefix for constant names"        &
            " (empty string disables check)""" &
            " separator=""="""                 &
            " switch-off=""-R"                 &
            Rule.Name.all                      &
            ":Constant"""                      &
            "/>");

      Info (Level * Ident_String               &
            "<field switch=""+R"               &
            Rule.Name.all                      &
            ":Exception"""                     &
            " label="""                        &
            "prefix for exception names"       &
            " (empty string disables check)""" &
            " separator=""="""                 &
            " switch-off=""-R"                 &
            Rule.Name.all                      &
            ":Exception"""                     &
            "/>");

      Info (Level * Ident_String               &
            "<field switch=""+R"               &
            Rule.Name.all                      &
            ":Enum"""                          &
            " label="""                        &
            "prefix for enumeration literals"  &
            " (empty string disables check)""" &
            " separator=""="""                 &
            " switch-off=""-R"                 &
            Rule.Name.all                      &
            ":Enum"""                          &
            "/>");

      Info (Level * Ident_String               &
            "<check  switch=""+R"              &
            Rule.Name.all                      &
            ":Exclusive"""                     &
            " label="""                        &
            "strong check mode""/>");

   end XML_Rule_Help;

   ---------------------------------
   -- Implicit_IN_Mode_Parameters --
   ---------------------------------

   ---------------------------------------------
   -- Init_Rule (Implicit_IN_Mode_Parameters) --
   ---------------------------------------------

   procedure Init_Rule (Rule : in out Implicit_IN_Mode_Parameters_Rule_Type) is
   begin
      Init_Rule (Rule_Template (Rule));

      Rule.Name        := new String'("Implicit_IN_Mode_Parameters");
      Rule.Synonym     := new String'("Implicit_IN_Parameter_Mode");
      Rule.Rule_Status := Fully_Implemented;
      Rule.Help_Info   := new String'("implicit IN mode in parameter " &
                                      "specifications");
      Rule.Diagnosis   := new String'("implicit IN mode in parameter " &
                                      "specification");
   end Init_Rule;

   -----------------------------------------------------
   -- Rule_Check_Pre_Op (Implicit_IN_Mode_Parameters) --
   -----------------------------------------------------

   procedure Rule_Check_Pre_Op
     (Rule    : in out Implicit_IN_Mode_Parameters_Rule_Type;
      Element :        Asis.Element;
      Control : in out Traverse_Control;
      State   : in out Rule_Traversal_State)
   is
      pragma Unreferenced (Rule, Control);
   begin

      if Declaration_Kind (Element) = A_Parameter_Specification
       and then
         Mode_Kind (Element) = A_Default_In_Mode
       and then
         Definition_Kind (Object_Declaration_View (Element)) /=
           An_Access_Definition
      then
         State.Detected := True;
      end if;

   end Rule_Check_Pre_Op;

   ------------------------------------------
   -- Implicit_SMALL_For_Fixed_Point_Types --
   ------------------------------------------

   ------------------------------------------------------
   -- Init_Rule (Implicit_SMALL_For_Fixed_Point_Types) --
   ------------------------------------------------------

   procedure Init_Rule
     (Rule : in out Implicit_SMALL_For_Fixed_Point_Types_Rule_Type)
   is
   begin
      Init_Rule (Rule_Template (Rule));

      Rule.Name        := new String'("Implicit_SMALL_For_Fixed_Point_Types");
      Rule.Synonym     := new String'("Missing_Small_For_Fixed_Point_Type");
      Rule.Rule_Status := Fully_Implemented;
      Rule.Help_Info   := new String'("fixed point type declarations with no "
                                    & "'Small clause");
      Rule.Diagnosis   := new String'("fixed point type declaration with no "
                                    & "'Small clause");
   end Init_Rule;

   --------------------------------------------------------------
   -- Rule_Check_Pre_Op (Implicit_SMALL_For_Fixed_Point_Types) --
   --------------------------------------------------------------

   procedure Rule_Check_Pre_Op
     (Rule    : in out Implicit_SMALL_For_Fixed_Point_Types_Rule_Type;
      Element :        Asis.Element;
      Control : in out Traverse_Control;
      State   : in out Rule_Traversal_State)
   is
      pragma Unreferenced (Rule, Control);
   begin

      if Type_Kind (Element) = An_Ordinary_Fixed_Point_Definition then
         State.Detected := True;

         declare
            Rep_Clauses : constant Asis.Element_List :=
              Corresponding_Representation_Clauses (Get_Enclosing_Element);
         begin

            for J in Rep_Clauses'Range loop

               if Representation_Clause_Kind (Rep_Clauses (J)) =
                    An_Attribute_Definition_Clause
                 and then
                  Attribute_Kind
                    (Representation_Clause_Name (Rep_Clauses (J))) =
                      A_Small_Attribute
               then
                  State.Detected := False;
                  exit;
               end if;

            end loop;

         end;

      end if;

   end Rule_Check_Pre_Op;

   ---------------------------------------
   -- Improperly_Located_Instantiations --
   ---------------------------------------

   ---------------------------------------------------
   -- Init_Rule (Improperly_Located_Instantiations) --
   ---------------------------------------------------

   procedure Init_Rule
     (Rule : in out Improperly_Located_Instantiations_Rule_Type)
   is
   begin
      Init_Rule (Rule_Template (Rule));

      Rule.Name        := new String'("Improperly_Located_Instantiations");
      Rule.Synonym    := new String'("Unreasonable_Places_For_Instantiations");
      Rule.Rule_Status := Fully_Implemented;
      Rule.Help_Info  := new String'("instantiations that can cause problems");
      Rule.Diagnosis   :=
        new String'("#1#instantiation in a subprogram body" &
                    "#2#instantiation in a library package spec" &
                    "#3#instantiation in a generic library package spec");
   end Init_Rule;

   -----------------------------------------------------------
   -- Rule_Check_Pre_Op (Improperly_Located_Instantiations) --
   -----------------------------------------------------------

   procedure Rule_Check_Pre_Op
     (Rule    : in out Improperly_Located_Instantiations_Rule_Type;
      Element :        Asis.Element;
      Control : in out Traverse_Control;
      State   : in out Rule_Traversal_State)
   is
      pragma Unreferenced (Rule, Control);
      Encl_CU   : Asis.Compilation_Unit;
      Encl_Body : Asis.Element;
      Step_Up   : Elmt_Idx := 0;

   begin

      if Declaration_Kind (Element) in A_Generic_Instantiation then
         Encl_CU := Enclosing_Compilation_Unit (Element);

         case Unit_Kind (Encl_CU) is
            when A_Package =>
               State.Detected := True;
               State.Diagnosis := 2;

            when A_Generic_Package =>
               State.Detected := True;
               State.Diagnosis := 3;

            when A_Subprogram_Body        |
                 A_Procedure_Body_Subunit |
                 A_Function_Body_Subunit  =>
               State.Detected := True;
               State.Diagnosis := 1;

            when A_Package_Body           |
                 A_Protected_Body_Subunit =>
               Encl_Body := Get_Enclosing_Element;

               while not Is_Nil (Encl_Body) loop

                  if Declaration_Kind (Encl_Body) in
                    A_Procedure_Body_Declaration ..
                    A_Function_Body_Declaration
                  then
                     State.Detected := True;
                     State.Diagnosis := 1;
                     exit;

                  elsif Declaration_Kind (Encl_Body) = A_Task_Body_Declaration
                    or else
                        Declaration_Kind (Encl_Body) =
                          An_Entry_Body_Declaration
                  then
                     exit;
                  else
                     Step_Up := Step_Up + 1;
                     Encl_Body := Get_Enclosing_Element (Step_Up);
                  end if;

               end loop;

            when A_Subprogram_Declaration    |
                 A_Generic_Procedure         |
                 A_Generic_Function          |
                 A_Renaming                  |
                 A_Generic_Unit_Instance     |
                 A_Package_Body_Subunit      |
                 A_Task_Body_Subunit         |
                 A_Nonexistent_Declaration   |
                 A_Nonexistent_Body          |
                 A_Configuration_Compilation |
                 An_Unknown_Unit             |
                 Not_A_Unit                  =>

               null;
         end case;
      end if;

   end Rule_Check_Pre_Op;

   ----------------------
   -- Numeric_Indexing --
   ----------------------

   ----------------------------------
   -- Init_Rule (Numeric_Indexing) --
   ----------------------------------

   procedure Init_Rule (Rule : in out Numeric_Indexing_Rule_Type) is
   begin
      Init_Rule (Rule_Template (Rule));

      Rule.Name        := new String'("Numeric_Indexing");
      Rule.Rule_Status := Fully_Implemented;
      Rule.Help_Info   := new String'("use integer literals as indexes");
      Rule.Diagnosis   := new String'("integer literal as index value");
   end Init_Rule;

   ------------------------------------------
   -- Rule_Check_Pre_Op (Numeric_Indexing) --
   ------------------------------------------

   procedure Rule_Check_Pre_Op
     (Rule    : in out Numeric_Indexing_Rule_Type;
      Element :        Asis.Element;
      Control : in out Traverse_Control;
      State   : in out Rule_Traversal_State)
   is
      pragma Unreferenced (Rule, Control);
      EE   : Asis.Element;
      Call : Asis.Element;
      Pref : Asis.Element;
   begin

      if Expression_Kind (Element) = An_Integer_Literal then
         EE := Get_Enclosing_Element;

         if Expression_Kind (EE) = An_Indexed_Component then
            State.Detected := True;
         elsif Association_Kind (EE) = A_Parameter_Association then
            Call := Get_Enclosing_Element (1);
            EE   := Get_Enclosing_Element (2);

            if Expression_Kind (EE) = An_Indexed_Component then
               --  Check if we have a call to a predefined unary "-"

               Pref := Prefix (Call);
               Pref := Normalize_Reference (Pref);

               if Operator_Kind (Pref) = A_Unary_Minus_Operator
                 and then
                  Is_Predefined_Operator (Pref)
               then
                  State.Detected := True;
               end if;
            end if;
         end if;

      end if;

   end Rule_Check_Pre_Op;

   ---------------------------------
   -- Non_Short_Circuit_Operators --
   ---------------------------------

   ---------------------------------------------
   -- Init_Rule (Non_Short_Circuit_Operators) --
   ---------------------------------------------

   procedure Init_Rule (Rule : in out Non_Short_Circuit_Operators_Rule_Type) is
   begin
      Init_Rule (Rule_Template (Rule));

      Rule.Name        := new String'("Non_Short_Circuit_Operators");
      Rule.Synonym     := new String'("Use_Of_Non_Short_Circuit");
      Rule.Rule_Status := Fully_Implemented;
      Rule.Help_Info   := new String'("use of predefined AND and OR for " &
                                      "boolean types");
      Rule.Diagnosis   :=
        new String'("#1#use of predefined AND for boolean type" &
                    "#2#use of predefined OR for boolean type");
   end Init_Rule;

   -----------------------------------------------------
   -- Rule_Check_Pre_Op (Non_Short_Circuit_Operators) --
   -----------------------------------------------------

   procedure Rule_Check_Pre_Op
     (Rule    : in out Non_Short_Circuit_Operators_Rule_Type;
      Element :        Asis.Element;
      Control : in out Traverse_Control;
      State   : in out Rule_Traversal_State)
   is
      pragma Unreferenced (Rule, Control);
      Call : Asis.Element;
   begin

      if Operator_Kind (Element) in An_And_Operator .. An_Or_Operator then

         Call := Get_Enclosing_Element;

         if Expression_Kind (Call) = A_Selected_Component then
            Call := Get_Enclosing_Element (Steps_Up => 1);
         end if;

         if Expression_Kind (Call) = A_Function_Call
           and then
            Is_Predefined_Operator (Element)
           and then
            Is_Boolean_Logical_Op (Element)
         then
            State.Detected := True;

            if Operator_Kind (Element) = An_And_Operator then
               State.Diagnosis := 1;
            else
               State.Diagnosis := 2;
            end if;

         end if;

      end if;

   end Rule_Check_Pre_Op;

   ----------------------------
   -- Non_Visible_Exceptions --
   ----------------------------

   --------------------------------------
   -- Has_Tip (Non_Visible_Exceptions) --
   --------------------------------------

   function Has_Tip (Rule : Non_Visible_Exceptions_Rule_Type) return Boolean is
      pragma Unreferenced (Rule);
   begin
      return True;
   end Has_Tip;

   ----------------------------------------
   -- Init_Rule (Non_Visible_Exceptions) --
   ----------------------------------------

   procedure Init_Rule (Rule : in out Non_Visible_Exceptions_Rule_Type) is
   begin
      Init_Rule (Rule_Template (Rule));

      Rule.Name        := new String'("Non_Visible_Exceptions");
      Rule.Rule_Status := Fully_Implemented;
      Rule.Help_Info   := new String'("potential propagations of " &
                                      "non-visible exceptions");
      Rule.Diagnosis   :=
        new String'("#1#no handler for this exception in enclosing body" &
                    "#2#no handler for this exception in enclosing block" &
                    "#3#propagates the local exception " &
                    "declared at line %1% outside its visibility");
   end Init_Rule;

   ------------------------------------------------
   -- Rule_Check_Pre_Op (Non_Visible_Exceptions) --
   ------------------------------------------------

   procedure Rule_Check_Pre_Op
     (Rule    : in out Non_Visible_Exceptions_Rule_Type;
      Element :        Asis.Element;
      Control : in out Traverse_Control;
      State   : in out Rule_Traversal_State)
   is
      pragma Unreferenced (Rule, Control);
      Handler     : Asis.Element;
      Handled_Exc : Asis.Element;
      Raised_Exc  : Asis.Element;
      Frame       : Asis.Element;
      Step_Up     : Elmt_Idx := 0;
   begin
      --  First part of the rule - declarations of local non-handled
      --  exceptions:

      if Defining_Name_Kind (Element) = A_Defining_Identifier
        and then
         Declaration_Kind (Get_Enclosing_Element) = An_Exception_Declaration
        and then
         (Declaration_Kind (Get_Enclosing_Element (Steps_Up => 1)) in
            A_Procedure_Body_Declaration .. A_Function_Body_Declaration
         or else
          Declaration_Kind (Get_Enclosing_Element (Steps_Up => 1)) =
             A_Task_Body_Declaration
         or else
          Statement_Kind (Get_Enclosing_Element (Steps_Up => 1)) =
             A_Block_Statement)
      then
         State.Detected := not
           Is_Handled
             (Exc => Element,
              By  => Get_Handlers (Get_Enclosing_Element (Steps_Up => 1)));

         if Statement_Kind (Get_Enclosing_Element (Steps_Up => 1)) =
              A_Block_Statement
         then
            State.Diagnosis := 2;
         else
            State.Diagnosis := 1;
         end if;

      end if;

      --  Second part of the rule - potential propagation of a local exception
      --  outside its visibility

      if Statement_Kind (Element) = A_Raise_Statement then

         Handler := Get_Enclosing_Element (Step_Up);

         while Element_Kind (Handler) in A_Statement .. A_Path loop
            Step_Up := Step_Up + 1;
            Handler := Get_Enclosing_Element (Step_Up);
         end loop;

         Frame := Get_Enclosing_Element (Steps_Up => Step_Up + 1);

         if Element_Kind (Handler) = An_Exception_Handler
           and then
            (Declaration_Kind (Frame) in
             A_Procedure_Body_Declaration .. A_Function_Body_Declaration
            or else
             Declaration_Kind (Frame) = A_Task_Body_Declaration
            or else
             Statement_Kind (Frame) = A_Block_Statement)
         then

            --  Two different cases, depending if the raise statement contains
            --  an exception name

            Raised_Exc := Raised_Exception (Element);

            if Is_Nil (Raised_Exc) then

               declare
                  Handled_Excs : constant Asis.Element_List :=
                    Exception_Choices (Handler);
               begin
                  for J in Handled_Excs'Range loop

                     if Definition_Kind (Handled_Excs (J)) =
                        An_Others_Choice
                     then
                        exit;
                     end if;

                     Handled_Exc :=
                       Enclosing_Element
                         (Get_Name_Definition (Handled_Excs (J)));

                     if Is_Equal
                          (Enclosing_Element (Handled_Exc), Frame)
                     then
                        State.Detected  := True;
                        State.Diagnosis := 3;
                        State.Diag_Params := Enter_String
                          ("%1%" & Element_Span (Handled_Exc).First_Line'Img);
                        exit;
                     end if;

                  end loop;

               end;

            else
               Raised_Exc :=
                 Enclosing_Element (Get_Name_Definition (Raised_Exc));

               if Is_Equal
                    (Enclosing_Element (Raised_Exc), Frame)
               then
                  State.Detected  := True;
                  State.Diagnosis := 3;
                  State.Diag_Params := Enter_String
                    ("%1%" & Element_Span (Raised_Exc).First_Line'Img);
               end if;
            end if;

         end if;

      end if;

   end Rule_Check_Pre_Op;

   ------------------------------------------------
   -- XML_Rule_Help_Tip (Non_Visible_Exceptions) --
   ------------------------------------------------

   procedure XML_Rule_Help_Tip
     (Rule  : Non_Visible_Exceptions_Rule_Type;
      Level : Natural)
   is
      pragma Unreferenced (Rule);
   begin
      Info_No_EOL (Level * Ident_String & "<tip>");
      Info ("Flag constructs leading to the possibility of propagating an");
      Info ("exception out of the scope in which the exception is declared.");
      Info ("Two cases are detected:");
      Info ("* An exception declaration in a subprogram body, task body");
      Info ("or block statement is flagged if the body or statement does not");
      Info ("contain a handler for that exception or a handler with an ");
      Info ("others choice.");
      Info ("* A raise statement in an exception handler of a subprogram");
      Info ("body, task body or block statement is flagged if it (re)raises");
      Info ("a locally declared exception. This may occur under the");
      Info ("following circumstances:");
      Info (" - it explicitly raises a locally declared exception, or");
      Info (" - it does not specify an exception name (i.e., it is simply");
      Info ("raise;) and the enclosing handler contains a locally declared");
      Info ("exception in its exception choices.");
      Info ("Renamings of local exceptions are not flagged.</tip>");
   end XML_Rule_Help_Tip;

   ----------------------
   -- Numeric_Literals --
   ----------------------

   --------------------------------------
   -- Annotate_Rule (Numeric_Literals) --
   --------------------------------------

   overriding function Annotate_Rule
     (Rule : Numeric_Literals_Rule_Type;
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
      end if;

      if Has_Synonym (Rule) then
         return " [" & Rule_Synonym (Rule) & "]";
      end if;

      Result := new String'(" [" & Rule_Name (Rule));

      case Rule.Up_To is
         when -1 =>
            Tmp := new String'(Result.all);
            Free (Result);
            Result := new String'(Tmp.all & ":All");
            Is_First_Par := False;
         when 1 =>
            null;
         when others =>
            Tmp := new String'(Result.all);
            Free (Result);
            Result := new String'(Tmp.all & ":" & Image (Rule.Up_To));
            Is_First_Par := False;
      end case;

      Free (Tmp);

      if Rule.Statements_Only then
         Tmp := new String'(Result.all);
         Free (Result);
         Result := new String'(
           Tmp.all &
           (if Is_First_Par then ':' else ',') &
           "Statements_Only");
         Free (Tmp);
      end if;

      declare
         Final_Res : constant String := Result.all & "]";
      begin
         Free (Result);
         return Final_Res;
      end;

   end Annotate_Rule;

   ----------------------------------
   -- Init_Rule (Numeric_Literals) --
   ----------------------------------

   procedure Init_Rule (Rule : in out Numeric_Literals_Rule_Type) is
   begin
      Init_Rule (Rule_Template (Rule));

      Rule.Name        := new String'("Numeric_Literals");
      Rule.Rule_Status := Fully_Implemented;
      Rule.Help_Info   := new String'("numeric literals");
      Rule.Diagnosis   := new String'("numeric literal (%1%) outside a "
                                    & "constant declaration");
   end Init_Rule;

   -----------------------------------------------
   -- Process_Rule_Parameter (Numeric_Literals) --
   -----------------------------------------------

   procedure Process_Rule_Parameter
     (Rule       : in out Numeric_Literals_Rule_Type;
      Param      :        String;
      Enable     :        Boolean;
      Defined_At : String)
   is
      Needs_Redefinition_Warning : Boolean := False;
      Rule_Settings_Redefined    : Boolean := False;
   begin

      if Param = "" then
         if Enable then
            Rule.Rule_State := Enabled;
         else
            Rule.Rule_State := Disabled;

            --  Restore defaults:
            Rule.Up_To           := 1;
            Rule.Statements_Only := False;
            Rule.Defined_At := Nil_String_Loc;
         end if;

         return;
      end if;

      if Enable then
         if Gnatcheck.Options.Check_Param_Redefinition
           and then
            Rule.Rule_State = Enabled
         then
            Needs_Redefinition_Warning := True;
         end if;

         Rule.Rule_State := Enabled;

         if To_Lower (Param) = "all" then
            if Needs_Redefinition_Warning
              and then
               Rule.Up_To /= -1
            then
               Rule_Settings_Redefined := True;
            end if;

            Rule.Up_To := -1;
         elsif To_Lower (Param) = "statements_only" then
            if Needs_Redefinition_Warning
              and then
               not Rule.Statements_Only
            then
               Rule_Settings_Redefined := True;
            end if;

            Rule.Statements_Only := True;
         else

            begin
               if Needs_Redefinition_Warning
                 and then
                  Rule.Up_To /= Natural'Value (Param)
               then
                  Rule_Settings_Redefined := True;
               end if;

               Rule.Up_To := Natural'Value (Param);
            exception
               when Constraint_Error =>
                  Error ("(" & Rule.Name.all & ") wrong parameter: " & Param);
            end;

         end if;

         if Rule_Settings_Redefined then
            Error
             ("redefining at " &
              (if Defined_At = "" then
                  "command line"
               else
                  Defined_At) &
              " settings for rule " & Rule.Name.all &
              " defined at "  &
              (if Rule.Defined_At = Nil_String_Loc then
                  "command line"
               else
                  Get_String (Rule.Defined_At)));
         end if;

         Rule.Defined_At := Enter_String (Defined_At);

      else
         Error ("(" & Rule.Name.all & ") no parameter allowed for -R");
      end if;

   end Process_Rule_Parameter;

   ------------------------------------------
   -- Rule_Check_Pre_Op (Numeric_Literals) --
   ------------------------------------------

   procedure Rule_Check_Pre_Op
     (Rule    : in out Numeric_Literals_Rule_Type;
      Element :        Asis.Element;
      Control : in out Traverse_Control;
      State   : in out Rule_Traversal_State)
   is
      pragma Unreferenced (Control);
      pragma Unmodified  (Rule);

      Arg_Kind : constant Expression_Kinds := Expression_Kind (Element);
      Integer_Literal_Value : Natural;
   begin

      if Arg_Kind in An_Integer_Literal .. A_Real_Literal then

         if Arg_Kind = An_Integer_Literal
           and then
            Rule.Up_To > 0
         then
            begin
               Integer_Literal_Value :=
                 Natural'Value (To_String (Value_Image (Element)));
            exception
               when Constraint_Error =>
                  --  The value is definitely too big to be an exception for
                  --  this rule!
                  Integer_Literal_Value := Natural'Last;
            end;

            if Integer_Literal_Value <= Rule.Up_To then
               --  Literal is too small to be flagged
               return;
            end if;

         end if;

         declare
            Encl_El     : Asis.Element := Get_Enclosing_Element;
            Old_Encl_El : Asis.Element := Element;
            Step_Up     : Elmt_Idx     := 0;
         begin
            while Element_Kind (Encl_El) = An_Expression
                or else
                  Path_Kind (Encl_El) in
                    A_Case_Expression_Path .. An_Else_Expression_Path
                or else
                  (Element_Kind (Encl_El) = An_Association
                  and then
                   Association_Kind (Encl_El) /=
                     An_Array_Component_Association)
                or else
                   (Association_Kind (Encl_El) =
                     An_Array_Component_Association
                   and then
                     Is_Equal (Old_Encl_El, Component_Expression (Encl_El)))
                or else
                  (Definition_Kind (Encl_El) = A_Discrete_Subtype_Definition
                 and then
                   Declaration_Kind (Get_Enclosing_Element (Step_Up + 1)) =
                     A_Loop_Parameter_Specification)
            loop
               Step_Up     := Step_Up + 1;
               Old_Encl_El := Encl_El;
               Encl_El     := Get_Enclosing_Element (Step_Up);
            end loop;

            if not (Declaration_Kind (Encl_El) = A_Constant_Declaration
                  or else
                   Declaration_Kind (Encl_El) in
                     An_Integer_Number_Declaration ..
                     A_Real_Number_Declaration
                  or else
                   Clause_Kind (Encl_El)  in
                     A_Representation_Clause | A_Component_Clause
                  or else
                   Definition_Kind (Encl_El) = An_Aspect_Specification
                  or else
                   (Discrete_Range_Kind (Encl_El) =
                      A_Discrete_Simple_Expression_Range
                   and then
                    Clause_Kind (Get_Enclosing_Element (Step_Up + 1)) =
                      A_Component_Clause))
            then
               if Rule.Statements_Only then
                  State.Detected :=
                    Element_Kind (Encl_El) in A_Statement .. A_Path
                      or else
                    Declaration_Kind (Encl_El) =
                      A_Loop_Parameter_Specification;
               else
                  State.Detected := True;
               end if;
            end if;

         end;

         if State.Detected then
            State.Diag_Params := Enter_String ("%1%" &
              To_String (Value_Image (Element)));
         end if;

      end if;

   end Rule_Check_Pre_Op;

   -------------------------------------------------
   -- XML_Rule_Parameters_Help (Numeric_Literals) --
   -------------------------------------------------

   procedure XML_Rule_Help
     (Rule  : Numeric_Literals_Rule_Type;
      Level : Natural)
   is
   begin

--      Info (Level * Ident_String              &
--            "<check switch=""+R"              &
--            Rule.Name.all                     &
--            ":ALL"                            &
--            """ label="""                     &
--            "all integer literals"""          &
--            "/>");

      Info (Level * Ident_String               &
            "<field switch=""+R"               &
            Rule.Name.all                      &
            """ separator="":"""               &
            " label="""                        &
            "Checks that no numeric literal "  &
            "is greater than the entered "     &
            "number, or set ALL to check all " &
            "numeric literals"""               &
            "/>");

      Info (Level * Ident_String               &
            "<check switch=""+R"               &
            Rule.Name.all                      &
            ":Statements_Only"                 &
            """ label="""                      &
            "check numeric literals "          &
            "on statements only"""             &
            "/>");

--      Info (Level * Ident_String              &
--            "<spin switch=""+R"               &
--            Rule.Name.all                     &
--            """ label="""                     &
--            "integer literals greater than"   &
--            """ min=""1"                      &
--            """ max=""99999"""                &
--            " default=""0"                    &
--            """ separator="":"""              &
--            "/>");
   end XML_Rule_Help;

   --------------------------
   -- OTHERS_In_Aggregates --
   --------------------------

   --------------------------------------
   -- Init_Rule (OTHERS_In_Aggregates) --
   --------------------------------------

   procedure Init_Rule (Rule : in out OTHERS_In_Aggregates_Rule_Type) is
   begin
      Init_Rule (Rule_Template (Rule));

      Rule.Name        := new String'("OTHERS_In_Aggregates");
      Rule.Rule_Status := Fully_Implemented;
      Rule.Help_Info   := new String'("OTHERS choices in aggregates");
      Rule.Diagnosis   := new String'("OTHERS choice in aggregate");
   end Init_Rule;

   ----------------------------------------------
   -- Rule_Check_Pre_Op (OTHERS_In_Aggregates) --
   ----------------------------------------------

   procedure Rule_Check_Pre_Op
     (Rule    : in out OTHERS_In_Aggregates_Rule_Type;
      Element :        Asis.Element;
      Control : in out Traverse_Control;
      State   : in out Rule_Traversal_State)
   is
      pragma Unreferenced (Rule, Control);
      Aggregate : Asis.Element;
   begin

      if Definition_Kind (Element) = An_Others_Choice then

         Aggregate := Get_Enclosing_Element (Steps_Up => 1);

         case Expression_Kind (Aggregate) is

            when An_Extension_Aggregate =>
               State.Detected := True;
            when A_Record_Aggregate           |
                 A_Positional_Array_Aggregate |
                 A_Named_Array_Aggregate      =>

               declare
                  Associations : constant Asis.Element_List :=
                    Get_Associations (Aggregate);
               begin

                  if Associations'Length >= 3 then
                     State.Detected := True;
                  elsif Associations'Length = 2 then

                     declare
                        Choices : constant Asis.Element_List :=
                          Get_Choices (Associations (Associations'First));
                     begin

                        if Choices'Length >= 2 then
                           State.Detected := True;
                        elsif Choices'Length = 1 then

                           if Definition_Kind (Choices (Choices'First)) =
                             A_Discrete_Range
                           then
                              State.Detected := True;
                           end if;

                        end if;

                     end;

                  end if;

               end;

            when others =>
               null;
         end case;

      end if;

   end Rule_Check_Pre_Op;

   -------------------------------
   -- OTHERS_In_CASE_Statements --
   -------------------------------

   -------------------------------------------
   -- Init_Rule (OTHERS_In_CASE_Statements) --
   -------------------------------------------

   procedure Init_Rule (Rule : in out OTHERS_In_CASE_Statements_Rule_Type) is
   begin
      Init_Rule (Rule_Template (Rule));

      Rule.Name        := new String'("OTHERS_In_CASE_Statements");
      Rule.Rule_Status := Fully_Implemented;
      Rule.Help_Info   := new String'("OTHERS choices in case statements");
      Rule.Diagnosis   := new String'("OTHERS choice in case statement");
   end Init_Rule;

   ---------------------------------------------------
   -- Rule_Check_Pre_Op (OTHERS_In_CASE_Statements) --
   ---------------------------------------------------

   procedure Rule_Check_Pre_Op
     (Rule    : in out OTHERS_In_CASE_Statements_Rule_Type;
      Element :        Asis.Element;
      Control : in out Traverse_Control;
      State   : in out Rule_Traversal_State)
   is
      pragma Unreferenced (Rule, Control);
   begin

      if Definition_Kind (Element) = An_Others_Choice
       and then
         Path_Kind (Get_Enclosing_Element) = A_Case_Path
      then
         State.Detected := True;
      end if;

   end Rule_Check_Pre_Op;

   ----------------------------------
   -- OTHERS_In_Exception_Handlers --
   ----------------------------------

   ----------------------------------------------
   -- Init_Rule (OTHERS_In_Exception_Handlers) --
   ----------------------------------------------

   procedure Init_Rule
     (Rule : in out OTHERS_In_Exception_Handlers_Rule_Type)
   is
   begin
      Init_Rule (Rule_Template (Rule));

      Rule.Name        := new String'("OTHERS_In_Exception_Handlers");
      Rule.Rule_Status := Fully_Implemented;
      Rule.Help_Info   := new String'("OTHERS choices in exception handlers");
      Rule.Diagnosis   := new String'("OTHERS choice in exception handler");
   end Init_Rule;

   ------------------------------------------------------
   -- Rule_Check_Pre_Op (OTHERS_In_Exception_Handlers) --
   ------------------------------------------------------

   procedure Rule_Check_Pre_Op
     (Rule    : in out OTHERS_In_Exception_Handlers_Rule_Type;
      Element :        Asis.Element;
      Control : in out Traverse_Control;
      State   : in out Rule_Traversal_State)
   is
      pragma Unreferenced (Rule, Control);
   begin

      if Definition_Kind (Element) = An_Others_Choice
       and then
         Element_Kind (Get_Enclosing_Element) = An_Exception_Handler
      then
         State.Detected := True;
      end if;

   end Rule_Check_Pre_Op;

   --------------------------------------
   -- Overly_Nested_Control_Structures --
   --------------------------------------

   --------------------------------------------------
   -- Init_Rule (Overly_Nested_Control_Structures) --
   --------------------------------------------------

   procedure Init_Rule
     (Rule : in out Overly_Nested_Control_Structures_Rule_Type)
   is
   begin
      Init_Rule (One_Integer_Parameter_Rule_Template (Rule));

      Rule.Name        := new String'("Overly_Nested_Control_Structures");
      Rule.Synonym     := new String'("Control_Structure_Nesting");
      Rule.Rule_Status := Fully_Implemented;
      Rule.Help_Info   := new String'("deep nesting level of " &
                                      "control structures");
      Rule.Diagnosis   := new String'("nesting level of control structures " &
                                      "too deep");
   end Init_Rule;

   ----------------------------------------------------------
   -- Rule_Check_Pre_Op (Overly_Nested_Control_Structures) --
   ----------------------------------------------------------

   procedure Rule_Check_Pre_Op
     (Rule    : in out Overly_Nested_Control_Structures_Rule_Type;
      Element :        Asis.Element;
      Control : in out Traverse_Control;
      State   : in out Rule_Traversal_State)
   is
      pragma Unreferenced (Control);
      pragma Unmodified  (Rule);

      Nesting_Level : Natural  := 0;
      Step_Up       : Elmt_Idx := 0;
      Encl_El       : Asis.Element;
   begin

      if Is_Control_Structure (Element) then
         Encl_El := Get_Enclosing_Element (Step_Up);

         while Element_Kind (Encl_El) in A_Statement .. A_Path loop

            if Is_Control_Structure (Encl_El) then
               Nesting_Level := Nesting_Level + 1;

               if Nesting_Level > Rule.Rule_Limit then
                  State.Detected := True;
                  exit;
               end if;

            end if;

            Step_Up := Step_Up + 1;
            Encl_El := Get_Enclosing_Element (Step_Up);

         end loop;

      end if;

   end Rule_Check_Pre_Op;

   -----------------------------
   -- Parameters_Out_Of_Order --
   -----------------------------

   -----------------------------------------
   -- Init_Rule (Parameters_Out_Of_Order) --
   -----------------------------------------

   procedure Init_Rule (Rule : in out Parameters_Out_Of_Order_Rule_Type) is
   begin
      Init_Rule (Rule_Template (Rule));

      Rule.Name        := new String'("Parameters_Out_Of_Order");
      Rule.Synonym     := new String'("Parameter_Mode_Ordering");
      Rule.Rule_Status := Fully_Implemented;
      Rule.Help_Info   := new String'("formal parameters ordering");
      Rule.Diagnosis   := new String'(
        "#1#parameter %1% of mode %2% precedes parameter %3% of mode %4%"  &
        "#2#parameter %1% with default initialization precedes " &
        "parameter %2% without it");
   end Init_Rule;

   -------------------------------------------------
   -- Rule_Check_Pre_Op (Parameters_Out_Of_Order) --
   -------------------------------------------------

   procedure Rule_Check_Pre_Op
     (Rule    : in out Parameters_Out_Of_Order_Rule_Type;
      Element :        Asis.Element;
      Control : in out Traverse_Control;
      State   : in out Rule_Traversal_State)
   is
      pragma Unreferenced (Rule, Control);
      Arg_Kind      : constant Declaration_Kinds := Declaration_Kind (Element);
      Check_Profile : Boolean := False;
   begin

      case Arg_Kind is

         when A_Procedure_Declaration         |
              A_Function_Declaration          |
              A_Null_Procedure_Declaration    |
              An_Entry_Declaration            |
              A_Generic_Procedure_Declaration |
              A_Generic_Function_Declaration  |
              A_Formal_Procedure_Declaration  |
              A_Formal_Function_Declaration   =>

            Check_Profile := True;

         when A_Procedure_Body_Declaration |
              A_Function_Body_Declaration  |
              A_Procedure_Body_Stub        |
              A_Function_Body_Stub         =>

            Check_Profile := Acts_As_Spec (Element);

         when others =>
            null;
      end case;

      if Check_Profile then

         declare
            Params : constant Asis.Element_List := Parameter_Profile (Element);

            Prev_Mode : Mode_Kinds;
            Succ_Mode : Mode_Kinds;
            Prev_Name : Asis.Element;
            Succ_Name : Asis.Element;

            Prev_Par_Has_Default_Expr : Boolean;
         begin

            if Params'Length > 1 then
               Prev_Mode := Mode_Kind (Params (Params'First));
               Prev_Par_Has_Default_Expr :=
                 not Is_Nil
                   (Initialization_Expression (Params (Params'First)));

               for J in Params'First + 1 .. Params'Last loop

                  --  First, check if the mode ordering is right, that is
                  --  IN -> IN OUT -> OUT
                  --  This check does not make sense for functions:

                  if not (Arg_Kind = A_Function_Declaration
                       or else
                          Arg_Kind = A_Generic_Function_Declaration
                       or else
                          Arg_Kind = A_Formal_Function_Declaration
                       or else
                          Arg_Kind = A_Function_Body_Stub)
                  then

                     Succ_Mode := Mode_Kind (Params (J));

                     case Prev_Mode is
                        when An_In_Out_Mode =>
                           --  IN OUT -> IN is a violation:

                           if Succ_Mode in A_Default_In_Mode .. An_In_Mode then
                              Prev_Name := First_Name (Params (J - 1));
                              Succ_Name := First_Name (Params (J));

                              State.Detected    := True;
                              State.Diagnosis   := 1;
                              State.Diag_Params := Enter_String (
                                "%1%"                                       &
                                To_String (Defining_Name_Image (Prev_Name)) &
                                "%2%" & "IN OUT"                            &
                                "%3%"                                       &
                                To_String (Defining_Name_Image (Succ_Name)) &
                                "%4%" & "IN");

                              exit;
                           end if;

                        when An_Out_Mode =>

                           if Succ_Mode in A_Default_In_Mode .. An_In_Mode then
                              --  OUT -> IN is a violation:

                              Prev_Name := First_Name (Params (J - 1));
                              Succ_Name := First_Name (Params (J));

                              State.Detected    := True;
                              State.Diagnosis   := 1;
                              State.Diag_Params := Enter_String (
                                "%1%"                                       &
                                To_String (Defining_Name_Image (Prev_Name)) &
                                "%2%" & "OUT"                               &
                                "%3%"                                       &
                                To_String (Defining_Name_Image (Succ_Name)) &
                                "%4%" & "IN");

                              exit;
                           elsif Succ_Mode = An_In_Out_Mode then
                              --  OUT -> IN OUT is a violation:

                              Prev_Name := First_Name (Params (J - 1));
                              Succ_Name := First_Name (Params (J));

                              State.Detected    := True;
                              State.Diagnosis   := 1;
                              State.Diag_Params := Enter_String (
                                "%1%"                                       &
                                To_String (Defining_Name_Image (Prev_Name)) &
                                "%2%" & "OUT"                               &
                                "%3%"                                       &
                                To_String (Defining_Name_Image (Succ_Name)) &
                                "%4%" & "IN OUT");

                              exit;
                           end if;

                        when A_Default_In_Mode .. An_In_Mode =>
                           --  Any mode can follow IN mode
                           null;
                        when others =>
                           pragma Assert (False);
                           null;
                     end case;

                  end if;

                  --  Now check that IN parameters with default initialization
                  --  go last in the group of IN parameters:

                  if Succ_Mode in A_Default_In_Mode .. An_In_Mode
                    and then
                     Prev_Mode in A_Default_In_Mode .. An_In_Mode
                  then

                     if Prev_Par_Has_Default_Expr then
                        if Is_Nil (Initialization_Expression (Params (J))) then
                           Prev_Name := First_Name (Params (J - 1));
                           Succ_Name := First_Name (Params (J));

                           State.Detected    := True;
                           State.Diagnosis   := 2;
                           State.Diag_Params := Enter_String (
                             "%1%"                                       &
                             To_String (Defining_Name_Image (Prev_Name)) &
                             "%2%"                                       &
                             To_String (Defining_Name_Image (Succ_Name)));

                           exit;
                        end if;
                     else
                        Prev_Par_Has_Default_Expr :=
                          not Is_Nil (Initialization_Expression (Params (J)));

                     end if;

                  end if;

                  Prev_Mode := Succ_Mode;

               end loop;

            end if;

         end;

      end if;

   end Rule_Check_Pre_Op;

   ---------------------------------------------------------
   -- Positional_Actuals_For_Defaulted_Generic_Parameters --
   ---------------------------------------------------------

   ---------------------------------------------------------------------
   -- Init_Rule (Positional_Actuals_For_Defaulted_Generic_Parameters) --
   ---------------------------------------------------------------------

   procedure Init_Rule (Rule : in out
     Positional_Actuals_For_Defaulted_Generic_Parameters_Rule_Type)
   is
   begin
      Init_Rule (Rule_Template (Rule));

      Rule.Name        :=
        new String'("Positional_Actuals_For_Defaulted_Generic_Parameters");
      Rule.Rule_Status := Fully_Implemented;
      Rule.Help_Info   := new String'("positional generic actuals for " &
                                      "defaulted generic parameters");
      Rule.Diagnosis   := new String'("use named notation when passing " &
        "actual to defaulted generic parameter");
   end Init_Rule;

   ----------------------------------------------------------------------------
   -- Rule_Check_Pre_Op(Positional_Actuals_For_Defaulted_Generic_Parameters) --
   ----------------------------------------------------------------------------

   procedure Rule_Check_Pre_Op
     (Rule    : in out
        Positional_Actuals_For_Defaulted_Generic_Parameters_Rule_Type;
      Element :        Asis.Element;
      Control : in out Traverse_Control;
      State   : in out Rule_Traversal_State)
   is
      pragma Unreferenced (Rule, Control);
      Gen_Decl : Asis.Element;
   begin

      if Association_Kind (Element) = A_Generic_Association
        and then
         Is_Nil (Formal_Parameter (Element))
      then

         --  Compute the corresponding generic declaration.
         Gen_Decl := Generic_Unit_Name (Get_Enclosing_Element);
         Gen_Decl := Normalize_Reference (Gen_Decl);
         Gen_Decl := Corresponding_Name_Declaration (Gen_Decl);

         if Declaration_Kind (Gen_Decl) in
           A_Generic_Package_Renaming_Declaration ..
             A_Generic_Function_Renaming_Declaration
         then
            Gen_Decl := Corresponding_Base_Entity (Gen_Decl);
            Gen_Decl := Normalize_Reference (Gen_Decl);
            Gen_Decl := Corresponding_Name_Declaration (Gen_Decl);
         end if;

         declare
            Formal_Params : constant Asis.Element_List :=
              Generic_Formal_Part (Gen_Decl);
            Actuals : constant Asis.Element_List :=
              Generic_Actual_Part (Get_Enclosing_Element);

            Move_Act  : Natural  := 0;
            Move_Form : Natural  := 0;
            Form_Idx  : Natural  := 0;
         begin

            for J in Actuals'Range loop
               if Is_Equal (Actuals (J), Element) then
                  exit;
               end if;

               Move_Act := Move_Act + 1;
            end loop;

            --  Now Move_Act gives us a number of the actual parameter in
            --  question in the call minus 1. This parameter is in positional
            --  association, so we have to count to the corresponding generic
            --  formal. The problem here is that we can have more than one
            --  formal parameter declared in one parameter specification.

            for J in Formal_Params'Range loop

               if Element_Kind (Formal_Params (J)) /= A_Clause then

                  Move_Form := Move_Form + Names (Formal_Params (J))'Length;

                  if Move_Form > Move_Act then
                     Form_Idx := J;
                     exit;
                  end if;

               end if;

            end loop;

            case Declaration_Kind (Formal_Params (Form_Idx)) is
               when A_Formal_Object_Declaration =>
                  State.Detected :=
                    not Is_Nil (Initialization_Expression
                      (Formal_Params (Form_Idx)));

               when A_Formal_Procedure_Declaration |
                    A_Formal_Function_Declaration =>
                  State.Detected :=
                    Default_Kind (Formal_Params (Form_Idx)) /= A_Nil_Default;

               when others =>
                  null;
            end case;

         end;

      end if;

   end Rule_Check_Pre_Op;

   -------------------------------------------------
   -- Positional_Actuals_For_Defaulted_Parameters --
   -------------------------------------------------

   -------------------------------------------------------------
   -- Init_Rule (Positional_Actuals_For_Defaulted_Parameters) --
   -------------------------------------------------------------

   procedure Init_Rule
     (Rule : in out Positional_Actuals_For_Defaulted_Parameters_Rule_Type)
   is
   begin
      Init_Rule (Rule_Template (Rule));

      Rule.Name        :=
        new String'("Positional_Actuals_For_Defaulted_Parameters");
      Rule.Rule_Status := Fully_Implemented;
      Rule.Help_Info   := new String'("positional actuals for " &
                                      "defaulted parameters");
      Rule.Diagnosis   := new String'("use named notation when passing " &
        "actual to defaulted parameter");
   end Init_Rule;

   ---------------------------------------------------------------------
   -- Rule_Check_Pre_Op (Positional_Actuals_For_Defaulted_Parameters) --
   ---------------------------------------------------------------------

   procedure Rule_Check_Pre_Op
     (Rule    : in out Positional_Actuals_For_Defaulted_Parameters_Rule_Type;
      Element :        Asis.Element;
      Control : in out Traverse_Control;
      State   : in out Rule_Traversal_State)
   is
      pragma Unreferenced (Rule, Control);
   begin

      if Association_Kind (Element) = A_Parameter_Association
        and then
         Is_Nil (Formal_Parameter (Element))
        and then
         not Is_Call_To_Operator_Function (Get_Enclosing_Element)
        and then
         not Is_Call_To_Attribute_Subprogram (Get_Enclosing_Element)
      then

         if not Is_Nil (Initialization_Expression
                          (Get_Parameter_Declaration (Element)))
         then
            State.Detected := True;
         end if;

      end if;

   end Rule_Check_Pre_Op;

   ---------------------------
   -- Positional_Components --
   ---------------------------

   ---------------------------------------
   -- Init_Rule (Positional_Components) --
   ---------------------------------------

   procedure Init_Rule
     (Rule : in out Positional_Components_Rule_Type)
   is
   begin
      Init_Rule (Rule_Template (Rule));

      Rule.Name        := new String'("Positional_Components");
      Rule.Synonym     := new String'("Positional_Component_Associations");
      Rule.Rule_Status := Fully_Implemented;
      Rule.Help_Info   := new String'("positional components associations " &
                                     "in aggregates");
      Rule.Diagnosis   := new String'("aggregate with a positional " &
                                     "component association");
   end Init_Rule;

   -----------------------------------------------
   -- Rule_Check_Pre_Op (Positional_Components) --
   -----------------------------------------------

   procedure Rule_Check_Pre_Op
     (Rule    : in out Positional_Components_Rule_Type;
      Element :        Asis.Element;
      Control : in out Traverse_Control;
      State   : in out Rule_Traversal_State)
   is
      pragma Unreferenced (Rule, Control);
   begin

      case Expression_Kind (Element) is
         when A_Record_Aggregate      |
              An_Extension_Aggregate  =>
            State.Detected := Has_Positional_Association (Element);
         when  A_Positional_Array_Aggregate =>
            State.Detected := True;
         when others =>
            null;
      end case;

   end Rule_Check_Pre_Op;

   -----------------------------------
   -- Positional_Generic_Parameters --
   -----------------------------------

   -----------------------------------------------
   -- Init_Rule (Positional_Generic_Parameters) --
   -----------------------------------------------

   procedure Init_Rule
     (Rule : in out Positional_Generic_Parameters_Rule_Type)
   is
   begin
      Init_Rule (Rule_Template (Rule));

      Rule.Name        := new String'("Positional_Generic_Parameters");
      Rule.Synonym     := new String'("Positional_Generic_Associations");
      Rule.Rule_Status := Fully_Implemented;
      Rule.Help_Info   := new String'("positional generic associations");
      Rule.Diagnosis   := new String'("positional generic association");
   end Init_Rule;

   -------------------------------------------------------
   -- Rule_Check_Pre_Op (Positional_Generic_Parameters) --
   -------------------------------------------------------

   procedure Rule_Check_Pre_Op
     (Rule    : in out Positional_Generic_Parameters_Rule_Type;
      Element :        Asis.Element;
      Control : in out Traverse_Control;
      State   : in out Rule_Traversal_State)
   is
      pragma Unreferenced (Rule, Control);
   begin

      if Association_Kind (Element) = A_Generic_Association
        and then
         Is_Nil (Formal_Parameter (Element))
      then
         if not Has_One_Parameter (Get_Enclosing_Element) then
            State.Detected := True;
         end if;
      end if;

   end Rule_Check_Pre_Op;

   ---------------------------
   -- Positional_Parameters --
   ---------------------------

   ---------------------------------------------------------
   -- Activate_In_Test_Mode (Unconstrained_Array_Returns) --
   ---------------------------------------------------------

   overriding procedure Activate_In_Test_Mode
     (Rule : in out Positional_Parameters_Rule_Type)
   is
   begin
      Process_Rule_Parameter
        (Rule       => Rule,
         Param      => "All",
         Enable     => True,
         Defined_At => "");

   end Activate_In_Test_Mode;

   --------------------------------------------------
   -- Exception_Name (Unconstrained_Array_Returns) --
   --------------------------------------------------

   function Exception_Name
     (Rule      : Positional_Parameters_Rule_Type;
      Exc_Index : Exception_Index)
      return      String
   is
      pragma Unreferenced (Rule);
   begin
      case Exc_Index is
         when 1 =>
            return "All";
         when others =>
            return "";
      end case;
   end Exception_Name;

   ----------------------------------------------------
   -- Exception_Number (Unconstrained_Array_Returns) --
   ----------------------------------------------------

   function Exception_Number
     (Rule     : Positional_Parameters_Rule_Type;
      Exc_Name : String)
      return     Exception_Numbers
   is
      pragma Unreferenced (Rule);
      Result : Exception_Numbers := Not_An_Exception;
      Normalized_Exc_Name : constant String := To_Lower (Exc_Name);
   begin
      if Normalized_Exc_Name = "all" then
         Result := 1;
      end if;

      return Result;
   end Exception_Number;

   ---------------------------------------
   -- Init_Rule (Positional_Parameters) --
   ---------------------------------------

   overriding procedure Init_Rule
     (Rule : in out Positional_Parameters_Rule_Type)
   is
   begin
      Init_Rule (Rule_Template (Rule));

      Rule.Name        := new String'("Positional_Parameters");
      Rule.Synonym     := new String'("Positional_Parameter_Associations");
      Rule.Rule_Status := Fully_Implemented;
      Rule.Help_Info   := new String'("positional associations in " &
                                      "subprogram and entry calls");
      Rule.Diagnosis   := new String'("positional parameter association");
   end Init_Rule;

   -----------------------------------------------
   -- Rule_Check_Pre_Op (Positional_Parameters) --
   -----------------------------------------------

   overriding procedure Rule_Check_Pre_Op
     (Rule    : in out Positional_Parameters_Rule_Type;
      Element :        Asis.Element;
      Control : in out Traverse_Control;
      State   : in out Rule_Traversal_State)
   is
      pragma Unreferenced (Control);
   begin

      if Association_Kind (Element) = A_Parameter_Association
        and then
         Is_Nil (Formal_Parameter (Element))
      then
         --  Now - check for exceptions:

         if not (
           --  unconditional exceptions
           Is_Call_To_Operator_Function (Get_Enclosing_Element)
           or else
             Is_Call_To_Attribute_Subprogram (Get_Enclosing_Element)
           --  exceptions that depends on parameter value
           or else
             (not Rule.Exceptions (1)
             and then
              Has_One_Parameter (Get_Enclosing_Element))
           or else
            (Is_Prefix_Notation (Get_Enclosing_Element)
            and then
             Is_Prefix_Notation_Exception (Element, not Rule.Exceptions (1))))
         then
            State.Detected := True;
         end if;

      end if;

   end Rule_Check_Pre_Op;

   -----------------------------------
   -- Predefined_Numeric_Types_Rule --
   -----------------------------------

   -----------------------------------------
   -- Init_Rule (Predefined_Numeric_Types --
   -----------------------------------------

   procedure Init_Rule (Rule : in out Predefined_Numeric_Types_Rule_Type) is
   begin
      Init_Rule (Rule_Template (Rule));

      Rule.Name        := new String'("Predefined_Numeric_Types");
      Rule.Rule_Status := Fully_Implemented;
      Rule.Help_Info   := new String'("explicit references to predefined " &
                                      "numeric subtypes");
      Rule.Diagnosis   := new String'("explicit reference to predefined " &
                                      "numeric subtype");
   end Init_Rule;

   -------------------------------------------------
   -- Rule_Check_Pre_Op (Predefined_Numeric_Types --
   -------------------------------------------------

   procedure Rule_Check_Pre_Op
     (Rule    : in out Predefined_Numeric_Types_Rule_Type;
      Element :        Asis.Element;
      Control : in out Traverse_Control;
      State   : in out Rule_Traversal_State)
   is
      pragma Unreferenced (Rule, Control);
   begin

      if Expression_Kind (Element) = An_Identifier
        and then
         Is_Ref_To_Standard_Num_Subtype (Element)
      then
         State.Detected := True;
      end if;

   end Rule_Check_Pre_Op;

   ---------------------------------
   -- Raising_External_Exceptions --
   ---------------------------------

   ---------------------------------------------
   -- Init_Rule (Raising_External_Exceptions) --
   ---------------------------------------------

   procedure Init_Rule (Rule : in out Raising_External_Exceptions_Rule_Type) is
   begin
      Init_Rule (Rule_Template (Rule));

      Rule.Name        := new String'("Raising_External_Exceptions");
      Rule.Synonym     := new String'("Visible_Exceptions");
      Rule.Rule_Status := Fully_Implemented;
      Rule.Help_Info   := new String'("visibility of exceptions raised by " &
                                      "routines declared in library package");
      Rule.Diagnosis   := new String'("raised exception is not declared in " &
                                      "visible part of enclosing library " &
                                      "package");
   end Init_Rule;

   -----------------------------------------------------
   -- Rule_Check_Pre_Op (Raising_External_Exceptions) --
   -----------------------------------------------------

   procedure Rule_Check_Pre_Op
     (Rule    : in out Raising_External_Exceptions_Rule_Type;
      Element :        Asis.Element;
      Control : in out Traverse_Control;
      State   : in out Rule_Traversal_State)
   is
      pragma Unreferenced (Rule, Control);

      Raised_Exc : Asis.Element;
      Encl_CU    : Asis.Compilation_Unit :=
        Enclosing_Compilation_Unit (Element);
   begin

      if Statement_Kind (Element) = A_Raise_Statement
        and then
         (Unit_Kind (Encl_CU) = A_Package
         or else
          Unit_Kind (Encl_CU) = A_Generic_Package
         or else
          Unit_Kind (Encl_CU) = A_Package_Body)
      then
         Raised_Exc := Raised_Exception (Element);

         if not Is_Nil (Raised_Exc) then
            Raised_Exc := Normalize_Reference (Raised_Exc);
            Raised_Exc := Corresponding_Name_Definition (Raised_Exc);

            --  Note, that we do not unwind renamings, that is, if Raised_Exc
            --  is a renaming of a Standard exception that takes place in
            --  another package, we consider this as a rule violation.

            if not Is_From_Standard (Raised_Exc) then

               if Unit_Kind (Encl_CU) = A_Package_Body then
                  Encl_CU := Corresponding_Declaration (Encl_CU);

                  if not Is_Equal (Enclosing_Compilation_Unit (Raised_Exc),
                                   Encl_CU)
                  then
                     State.Detected := True;
                  else
                     State.Detected := not Is_Public (Raised_Exc);
                  end if;

               end if;

            end if;

         end if;

      end if;

   end Rule_Check_Pre_Op;

   -----------------------------------
   -- Raising_Predefined_Exceptions --
   -----------------------------------

   ---------------------------------------
   -- Init_Rule (Raising_Predefined_Exceptions) --
   ---------------------------------------

   procedure Init_Rule (Rule : in out Raising_Predefined_Exceptions_Rule_Type)
   is
   begin
      Init_Rule (Rule_Template (Rule));

      Rule.Name        := new String'("Raising_Predefined_Exceptions");
      Rule.Synonym     := new String'("Predefined_Exceptions");
      Rule.Rule_Status := Fully_Implemented;
      Rule.Help_Info   := new String'("explicit raise of predefined " &
                                      "exceptions");
      Rule.Diagnosis   := new String'("explicit raise of a predefined " &
                                      "exception");
   end Init_Rule;

   -------------------------------------------------------
   -- Rule_Check_Pre_Op (Raising_Predefined_Exceptions) --
   -------------------------------------------------------

   procedure Rule_Check_Pre_Op
     (Rule    : in out Raising_Predefined_Exceptions_Rule_Type;
      Element :        Asis.Element;
      Control : in out Traverse_Control;
      State   : in out Rule_Traversal_State)
   is
      pragma Unreferenced (Rule, Control);
      Raised_Exc : Asis.Element;
   begin

      if Statement_Kind (Element) = A_Raise_Statement then
         Raised_Exc := Raised_Exception (Element);

         if not Is_Nil (Raised_Exc) then

            Raised_Exc := Normalize_Reference (Raised_Exc);
            Raised_Exc := Corresponding_Name_Declaration (Raised_Exc);

            if Declaration_Kind (Raised_Exc) =
               An_Exception_Renaming_Declaration
            then
               Raised_Exc := Corresponding_Base_Entity (Raised_Exc);
               Raised_Exc := Normalize_Reference (Raised_Exc);
               Raised_Exc := Corresponding_Name_Declaration (Raised_Exc);
            end if;

            State.Detected := Is_From_Standard (Raised_Exc);

         end if;

      end if;

   end Rule_Check_Pre_Op;

   -------------------------------
   -- Unassigned_OUT_Parameters --
   -------------------------------

   --------------------------------------------------------
   -- Data structures and local subprograms for the rule --
   --------------------------------------------------------

   type Formal_Parameter_Record is record
      Par_Def_Name : Asis.Element;
      --  Defining name of the parameter
      Assigned : Boolean := False;
      --  Flag indicating if this parameter has got a value.
   end record;

   package OUT_Parameters_Table is new GNAT.Table
     (Table_Component_Type => Formal_Parameter_Record,
      Table_Index_Type     => Natural,
      Table_Low_Bound      =>  1,
      Table_Initial        => 20,
      Table_Increment      => 50,
      Table_Name           => "OUT parameters");

   procedure Set_OUT_Parameters (El : Asis.Element);
   --  Supposing that El is a procedure body declaration, sets in
   --  OUT_Parameters_Table the list of OUT parameters of this procedure

   function Get_Bad_Parameter_List return String_Loc;
   --  Forms from the content of OUT_Parameters_Table the list of the bad
   --  parameter names to be placed in the diagnosis and returns the
   --  corresponding pointer in the string table.

   Check_Handler : Boolean;
   --  We need this global flag to decide if we have to traverse exceptions
   --  handlers.

   First_Body : Boolean;
   --  We need this flag to make the difference between the procedure body
   --  declaration from which the traversal starts (we have to analyze it), and
   --  all the other declarations, that should be skipped during the traversal

   procedure Check_Reference
     (Element :        Asis.Element;
      Control : in out Traverse_Control;
      State   : in out Natural);
   --  Checks if the argument is a reference to OUT parameter that sets its
   --  value. If such a reference is detected, updates parameter records in
   --  OUT_Parameters_Table. Decreases State each time when detects that one
   --  more OUT parameter gets a value. terminate the traversal when all the
   --  parameters have got values (State gets the value 0)

   procedure No_Opeation
     (Element :        Asis.Element;
      Control : in out Traverse_Control;
      State   : in out Natural);
   --  Does nothing.

   procedure Check_References is new Asis.Iterator.Traverse_Element
     (Pre_Operation     => Check_Reference,
      Post_Operation    => No_Opeation,
      State_Information => Natural);

   -------------------------------------------------
   -- Check_Reference (Unassigned_OUT_Parameters) --
   -------------------------------------------------

   procedure Check_Reference
     (Element :        Asis.Element;
      Control : in out Traverse_Control;
      State   : in out Natural)
   is
      Tmp_El        : Asis.Element;
      Old_Enclosing : Asis.Element;
      Par_Idx       : Natural;
   begin

      --  If we are here, State cannot be 0, so we have to do the job,,,

      case Flat_Element_Kind (Element) is

         when Flat_Declaration_Kinds =>

            if First_Body then
               First_Body := False;
            else
               Control := Abandon_Children;
            end if;

         when An_Exception_Handler =>

            if Declaration_Kind (Enclosing_Element (Element)) =
                 A_Procedure_Body_Declaration
              and then
                 Check_Handler
            then
               --  If we are here, the only possibility is that we are checking
               --  an exception handler from some procedure body.
               null;
            else
               --  If we are here, we are in some "inner" exception handler
               --  (note, that we skip all the declaration except the procedure
               --  body declaration for which the traversing is started). We
               --  just skip it)

               Control := Abandon_Children;
            end if;

         when An_Identifier =>
            Tmp_El := Get_Corresponding_Definition (Element);

            if Defining_Name_Kind (Tmp_El) = A_Defining_Identifier then
               Tmp_El := Corresponding_Body_Parameter_Definition (Tmp_El);
            end if;

            if Defining_Name_Kind (Tmp_El) = A_Defining_Identifier then
               Par_Idx := 0;

               for J in 1 .. OUT_Parameters_Table.Last loop

                  if Is_Equal
                       (Tmp_El, OUT_Parameters_Table.Table (J).Par_Def_Name)
                  then
                     Par_Idx := J;
                     exit;
                  end if;

               end loop;

               if Par_Idx > 0
                 and then
                  not OUT_Parameters_Table.Table (Par_Idx).Assigned
               then
                  --  And now we have to check if Element is in a position that
                  --  can result in assigning a value to the corresponding OUT
                  --  parameter

                  Old_Enclosing := Element;
                  Tmp_El        := Enclosing_Element (Old_Enclosing);

                  while Element_Kind (Tmp_El) = An_Expression loop

                     if (Expression_Kind (Tmp_El) = An_Indexed_Component
                       and then
                        not Is_Equal (Old_Enclosing, Prefix (Tmp_El)))

                      or else

                        (Expression_Kind (Tmp_El) = An_Explicit_Dereference
                       and then
                         Is_Equal (Old_Enclosing, Prefix (Tmp_El)))

                     then
                        --  The first condition means that we have an index in
                        --  an indexed component. The second condition means
                        --  that we have a prefix of explicit dereference. In
                        --  both cases the object in question cannot get
                        --  a value
                        exit;
                     end if;

                     Old_Enclosing := Tmp_El;
                     Tmp_El        := Enclosing_Element (Old_Enclosing);
                  end loop;

                  if Statement_Kind (Tmp_El) = An_Assignment_Statement
                    and then
                      Is_Equal
                        (Old_Enclosing, Assignment_Variable_Name (Tmp_El))
                  then
                     OUT_Parameters_Table.Table (Par_Idx).Assigned := True;
                     State := State - 1;

                  elsif Association_Kind (Tmp_El) =
                        A_Parameter_Association
                  then
                     --  Here we have to check if it is an actual for OUT
                     --  or IN OUT parameter

                     --  ??? See pre-operation for
                     --  Positional_Actuals_For_Defaulted_Parameters rule -
                     --  there is definitely some duplication here!

                     Old_Enclosing := Enclosing_Element (Tmp_El);

                     if not (Expression_Kind (Old_Enclosing) =
                             A_Function_Call
                           or else
                             Is_Call_To_Attribute_Subprogram (Old_Enclosing))
                     then
                        Old_Enclosing := Get_Parameter_Declaration (Tmp_El);

                        if Mode_Kind (Old_Enclosing) in
                           An_Out_Mode .. An_In_Out_Mode
                        then
                           OUT_Parameters_Table.Table (Par_Idx).Assigned :=
                             True;
                           State := State - 1;
                        end if;

                     end if;

                  end if;

               end if;

            end if;

         when others =>
            null;
      end case;

      if State = 0 then
         Control := Terminate_Immediately;
      end if;

   end Check_Reference;

   --------------------------------------------------------
   -- Get_Bad_Parameter_List (Unassigned_OUT_Parameters) --
   --------------------------------------------------------

   function Get_Bad_Parameter_List return String_Loc is
      Str, Tmp_Str : String_Access;
      Result       : String_Loc;
   begin

      for J in 1 .. OUT_Parameters_Table.Last loop

         if not OUT_Parameters_Table.Table (J).Assigned then

            if Tmp_Str = null then
               --  first parameter to report
               Str :=
                 new String'("%1%" & To_String
                   (Defining_Name_Image
                      (OUT_Parameters_Table.Table (J).Par_Def_Name)));
            else
               Free (Str);

               Str :=
                 new String'(Tmp_Str.all & ", " & To_String
                   (Defining_Name_Image
                      (OUT_Parameters_Table.Table (J).Par_Def_Name)));
            end if;

            Free (Tmp_Str);
            Tmp_Str := new String'(Str.all);
         end if;

      end loop;

      Result := Enter_String (Str.all & "%1%");

      Free (Str);
      Free (Tmp_Str);

      return (Result);

   end Get_Bad_Parameter_List;

   -------------------------------------------
   -- Init_Rule (Unassigned_OUT_Parameters) --
   -------------------------------------------

   procedure Init_Rule (Rule : in out Unassigned_OUT_Parameters_Rule_Type) is
   begin
      Init_Rule (Rule_Template (Rule));

      Rule.Name        := new String'("Unassigned_OUT_Parameters");
      Rule.Rule_Status := Fully_Implemented;
      Rule.Help_Info   := new String'("OUT parameters do not get values "     &
                                      "in procedure bodies");
      Rule.Diagnosis   := new String'("#1#procedure body does not define "    &
                                         "values for OUT parameters: %1%"     &
                                      "#2#exception handler does not define " &
                                         "values for OUT parameters: %1%");
   end Init_Rule;

   ---------------------------------------------
   -- No_Opeation (Unassigned_OUT_Parameters) --
   ---------------------------------------------

   procedure No_Opeation
     (Element :        Asis.Element;
      Control : in out Traverse_Control;
      State   : in out Natural)
   is
      pragma Unreferenced (Element, Control, State);
   begin
      null;
   end No_Opeation;

   ---------------------------------------------------
   -- Rule_Check_Pre_Op (Unassigned_OUT_Parameters) --
   ---------------------------------------------------

   procedure Rule_Check_Pre_Op
     (Rule    : in out Unassigned_OUT_Parameters_Rule_Type;
      Element :        Asis.Element;
      Control : in out Traverse_Control;
      State   : in out Rule_Traversal_State)
   is
      pragma Unreferenced (Rule, Control);
      Get_Params_From   : Asis.Element;
      Unassigned_Params : Natural;
      --  Unassigned_Params indicates the number of OUT parameters for that we
      --  do not know that they have got values
      Check_Ref_Control : Traverse_Control := Continue;
   begin

      if Declaration_Kind (Element) = A_Procedure_Body_Declaration
        or else
         (Element_Kind (Element) = An_Exception_Handler
          and then
            Declaration_Kind (Get_Enclosing_Element) =
            A_Procedure_Body_Declaration)
      then

         if Element_Kind (Element) = An_Exception_Handler then

            if Raises_Exception (Element) then
               return;
            end if;

            Get_Params_From := Get_Enclosing_Element;
            First_Body      := False;
            Check_Handler   := True;
         else
            Get_Params_From := Element;
            First_Body      := True;
            Check_Handler   := False;
         end if;

         OUT_Parameters_Table.Init;
         Set_OUT_Parameters (Get_Params_From);
         Unassigned_Params := OUT_Parameters_Table.Last;

         if Unassigned_Params > 0 then
            Check_References (Element, Check_Ref_Control, Unassigned_Params);

            if Unassigned_Params > 0 then
               State.Detected := True;

               if Declaration_Kind (Element) =
                    A_Procedure_Body_Declaration
               then
                  State.Diagnosis := 1;
               else
                  State.Diagnosis := 2;
               end if;

               State.Diag_Params := Get_Bad_Parameter_List;
            end if;

         end if;

      end if;

   end Rule_Check_Pre_Op;

   ----------------------------------------------------
   -- Set_OUT_Parameters (Unassigned_OUT_Parameters) --
   ----------------------------------------------------

   procedure Set_OUT_Parameters (El : Asis.Element) is
      Par_Specs : constant Asis.Element_List := Parameter_Profile (El);
   begin

      for J in Par_Specs'Range loop

         if Mode_Kind (Par_Specs (J)) = An_Out_Mode then

            declare
               Nms : constant Asis.Element_List := Names (Par_Specs (J));
            begin

               for K in Nms'Range loop
                  OUT_Parameters_Table.Append
                    ((Par_Def_Name => Nms (K),
                      Assigned     => False));
               end loop;

            end;

         end if;

      end loop;

   end Set_OUT_Parameters;

   -----------------------------------------
   -- Uncommented_BEGIN_In_Package_Bodies --
   -----------------------------------------

   -----------------------------------------------------
   -- Init_Rule (Uncommented_BEGIN_In_Package_Bodies) --
   -----------------------------------------------------

   procedure Init_Rule
     (Rule : in out Uncommented_BEGIN_In_Package_Bodies_Rule_Type) is
   begin
      Init_Rule (Rule_Template (Rule));

      Rule.Name        := new String'("Uncommented_BEGIN_In_Package_Bodies");
      Rule.Synonym     := new String'("Non_Marked_BEGIN_In_Package_Body");
      Rule.Rule_Status := Fully_Implemented;
      Rule.Help_Info   := new String'("BEGIN keywords in package bodies " &
                                     "non-marked with " &
                                      "comment with package name");
      Rule.Diagnosis   := new String'("#1#mark BEGIN with package name (%1%)" &
                                      "#2#place BEGIN in package body " &
                                       "on separate line");
   end Init_Rule;

   -------------------------------------------------------------
   -- Rule_Check_Pre_Op (Uncommented_BEGIN_In_Package_Bodies) --
   -------------------------------------------------------------

   procedure Rule_Check_Pre_Op
     (Rule    : in out Uncommented_BEGIN_In_Package_Bodies_Rule_Type;
      Element :        Asis.Element;
      Control : in out Traverse_Control;
      State   : in out Rule_Traversal_State)
   is
      pragma Unreferenced (Rule, Control);
   begin

      if Declaration_Kind (Element) = A_Package_Body_Declaration
       and then
         Has_Statements_And_Decls (Element)
      then

         declare
            Dcls : constant Asis.Element_List :=
              Body_Declarative_Items (Element, Include_Pragmas => True);
            Last_Dcl : constant Positive := Dcls'Last;

            Stmts : constant Asis.Element_List :=
              Body_Statements (Element, Include_Pragmas => True);
            First_Stmt : constant Positive := Stmts'First;

            LList : constant Line_List := Lines
              (Element    => Element,
               First_Line => Element_Span (Dcls (Last_Dcl)).Last_Line,
               Last_Line  => Element_Span (Stmts (First_Stmt)).First_Line);

            Begin_Line  : Line_Number_Positive;
            Begin_Start : Character_Position;
            Begin_Found : Boolean := False;
         begin

            --  First, check a most reasonable case - if we have BEGIN on a
            --  separate line between the last declaration and the first
            --  statement

            for J in LList'First + 1 .. LList'Last - 1 loop
               --  In this range, the only word the non-comment image of a
               --  line can contain is 'BEGIN'

               if To_Lower
                    (ASIS_Trim (To_String (Non_Comment_Image (LList (J))))) =
                  "begin"
               then
                  Begin_Found := True;

                  declare
                     Img : constant Program_Text :=
                       Non_Comment_Image (LList (J));
                  begin
                     Begin_Start := 1;

                     for J in Img'Range loop
                        exit when  Img (J) = 'b' or else Img (J) = 'B';

                        Begin_Start := Begin_Start + 1;
                     end loop;

                  end;

                  Begin_Line := J;

                  exit;

               end if;

            end loop;

            if Begin_Found then

               declare
                  Img : constant String :=
                    ASIS_Trim (To_String (Comment_Image (LList (Begin_Line))));

                  Firts_Idx : Natural := Img'First;
                  Last_Idx  : Natural := Img'Last;
               begin

                  if Img'Length = 0 then
                     State.Detected := True;
                  else
                     Firts_Idx := Img'First + 2;

                     while Is_White_Space (Img (Firts_Idx))
                      and then
                           Firts_Idx <= Last_Idx
                     loop
                        Firts_Idx := Firts_Idx + 1;
                     end loop;

                     for J in Firts_Idx + 1 .. Last_Idx loop

                        if Is_White_Space (Img (J)) then
                           Last_Idx := J - 1;
                           exit;
                        end if;

                     end loop;

                     State.Detected :=
                       To_Lower (Img (Firts_Idx .. Last_Idx)) /=
                       To_Lower
                         (To_String
                           (Defining_Name_Image (First_Name (Element))));
                     State.Line   := Begin_Line;
                     State.Column := Begin_Start;
                  end if;

               end;

               if State.Detected then
                  State.Diagnosis := 1;
               end if;

            else
               --  Pathological case - BEGIN in the same line as either the
               --  last declaration  or the first statement
               State.Detected  := True;
               State.Diagnosis := 2;
               State.Line      := Element_Span (Stmts (First_Stmt)).First_Line;
               State.Column    := 1;

            end if;

            if State.Detected and then State.Diagnosis = 1 then
               State.Diag_Params :=
                 Enter_String
                   ("%1%" &
                    To_String (Defining_Name_Image (First_Name (Element))));
            end if;

         end;

      end if;

   end Rule_Check_Pre_Op;

   ------------------------------
   -- Unnamed_Blocks_And_Loops --
   ------------------------------

   ------------------------------------------
   -- Init_Rule (Unnamed_Blocks_And_Loops) --
   ------------------------------------------

   procedure Init_Rule (Rule : in out Unnamed_Blocks_And_Loops_Rule_Type) is
   begin
      Init_Rule (Rule_Template (Rule));

      Rule.Name        := new String'("Unnamed_Blocks_And_Loops");
      Rule.Synonym     := new String'("Non_Named_Blocks_And_Loops");
      Rule.Rule_Status := Fully_Implemented;
      Rule.Help_Info   := new String'("compound statements naming");
      Rule.Diagnosis   :=
        new String'("#1#non-named block statement"      &
                    "#2#non-named nested loop statement" &
                    "#3#non-named nesting loop statement");
   end Init_Rule;

   --------------------------------------------------
   -- Rule_Check_Pre_Op (Unnamed_Blocks_And_Loops) --
   --------------------------------------------------

   procedure Rule_Check_Pre_Op
     (Rule    : in out Unnamed_Blocks_And_Loops_Rule_Type;
      Element :        Asis.Element;
      Control : in out Traverse_Control;
      State   : in out Rule_Traversal_State)
   is
      pragma Unreferenced (Rule, Control);
      Enclosing_El : Asis.Element;
      Step_Up      : Elmt_Idx := 0;
   begin

      case Statement_Kind (Element) is
         when A_Block_Statement =>

            if Is_Nil (Statement_Identifier (Element)) then
               State.Detected  := True;
               State.Diagnosis := 1;
            end if;

         when A_Loop_Statement       |
              A_While_Loop_Statement |
              A_For_Loop_Statement   =>

            if Is_Nil (Statement_Identifier (Element)) then
               --  First, check if the loop is nested. In case if a loop
               --  statement is enclosed in another loop and itself contains a
               --  loop statement, we generate the second diagnostic variant

               Enclosing_El := Get_Enclosing_Element (Step_Up);

               while Element_Kind (Enclosing_El) in A_Statement .. A_Path loop

                  if Statement_Kind (Enclosing_El) in
                       A_Loop_Statement .. A_For_Loop_Statement
                  then
                     State.Detected  := True;
                     State.Diagnosis := 2;
                     exit;
                  end if;

                  Step_Up := Step_Up + 1;
                  Enclosing_El := Get_Enclosing_Element (Step_Up);

               end loop;

               if not State.Detected then
                  --  Non nested loop, but it may contain other loops
                  State.Detected := Contains_Loop (Element);

                  if State.Detected then
                     State.Diagnosis := 3;
                  end if;

               end if;

            end if;

         when others =>
            null;
      end case;

   end Rule_Check_Pre_Op;

end Gnatcheck.Rules.Custom_1;
