------------------------------------------------------------------------------
--                                                                          --
--                          GNATCHECK COMPONENTS                            --
--                                                                          --
--                    A S I S _ U L . U T I L I T I E S                     --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                     Copyright (C) 2007-2019, AdaCore                     --
--                                                                          --
-- Asis Utility Library (ASIS UL) is free software; you can redistribute it --
-- and/or  modify  it  under  terms  of  the  GNU General Public License as --
-- published by the Free Software Foundation; either version 3, or (at your --
-- option)  any later version.  ASIS UL  is distributed in the hope that it --
-- will  be  useful,  but  WITHOUT  ANY  WARRANTY; without even the implied --
-- warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the --
-- GNU  General Public License for more details. You should have received a --
-- copy of the  GNU General Public License  distributed with GNAT; see file --
-- COPYING3. If not,  go to http://www.gnu.org/licenses for a complete copy --
-- of the license.                                                          --
--                                                                          --
-- ASIS UL is maintained by AdaCore (http://www.adacore.com).               --
--                                                                          --
------------------------------------------------------------------------------
pragma Ada_2012;

with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Strings;             use Ada.Strings;
with Ada.Strings.Fixed;       use Ada.Strings.Fixed;

with Asis.Compilation_Units;  use Asis.Compilation_Units;
with Asis.Declarations;       use Asis.Declarations;
with Asis.Definitions;        use Asis.Definitions;
with Asis.Elements;           use Asis.Elements;
with Asis.Expressions;        use Asis.Expressions;
with Asis.Extensions;         use Asis.Extensions;
with Asis.Iterator;           use Asis.Iterator;
with Asis.Statements;         use Asis.Statements;

with Asis.Set_Get;            use Asis.Set_Get;

with A4G.A_Sem;               use A4G.A_Sem;
with A4G.A_Types;             use A4G.A_Types;
with A4G.Asis_Tables;         use A4G.Asis_Tables;
with A4G.Contt.UT;            use A4G.Contt.UT;
with A4G.Int_Knds;            use A4G.Int_Knds;
with A4G.Mapping;             use A4G.Mapping;
with A4G.Vcheck;              use A4G.Vcheck;

with Atree;                   use Atree;
with Einfo;                   use Einfo;
with Elists;                  use Elists;
with Fname;                   use Fname;
with Lib;                     use Lib;
with Namet;
with Nlists;                  use Nlists;
with Sem_Aux;                 use Sem_Aux;
with Sinfo;                   use Sinfo;
with Sinput;                  use Sinput;
with Types;                   use Types;

with ASIS_UL.Common;          use ASIS_UL.Common;
with ASIS_UL.Misc;
with ASIS_UL.Output;          use ASIS_UL.Output;

package body ASIS_UL.Utilities is

   Package_Name : constant String := "ASIS_UL.Utilities.";

   -----------------------
   -- Local subprograms --
   -----------------------

   function Contains
     (Outer : Element;
      Inner : Element)
      return  Boolean;
   --  Checks if Outer contains Inner. At the moment this function is
   --  implemented for explicit elements only, or, more precisely, for the
   --  situation when for both arguments Is_Text_Available. If at least one of
   --  the parameters does not have a text properties available, False is
   --  returned.
   --
   --  Note, that the current implementation assumes that both arguments are
   --  from the same Compilation_Unit!

   function Internal_Full_Image (E : Asis.Element) return Program_Text;
   --  Part of the Full_Expanded_Name_Image implementation, makes the main
   --  recursive step. Goes up the ASIS tree until the first namable scope is
   --  found and returns either the image of the name of the scope (and empty
   --  string in case of nameless block statement) prepended by a dot and the
   --  result of the call to Internal_Full_Image applied to the enclosing
   --  element of the scope. In case if this is the last namable scope on the
   --  way to program unit top, returns its name and does not do any recursive
   --  step. In case of a subunit, goes to the parent unit.

   function Is_Namable_Scope (E : Element) return Boolean;
   --  Checks if the argument is a program unit definition is a scope that may
   --  contain a type or variable declaration

   function Is_Static_Subtype (E : Element) return Boolean;
   --  Checks if the argument is a static subtype indication or a static
   --  A_Discrete_Subtype_Definition. This function is supposed to be applied
   --  to discrete subtype indications (and definitions) of the form
   --  subtype_mark [constraint].

   procedure Capitalize (S : in out String);
   --  Assuming that S has a syntax of an Ada identifier, changes the casing
   --  of S to proper case.

   ------------------------------
   -- Ada_Attribute_Designator --
   ------------------------------

   function Ada_Attribute_Designator
     (Attr : Asis.Attribute_Kinds)
      return String
   is
      Result    : String := Attr'Img;
      First_Idx : Positive;
      Last_Idx  : Positive;
   begin
      case Attr is
         when Not_An_Attribute                    |
              An_Implementation_Defined_Attribute |
              An_Unknown_Attribute                =>
            raise Constraint_Error;

         when others =>
            First_Idx := Index (Result, "_", Forward) + 1;
            Last_Idx  := Index (Result, "_", Backward) - 1;
      end case;

      Capitalize (Result (First_Idx .. Last_Idx));

      return Result (First_Idx .. Last_Idx);
   end Ada_Attribute_Designator;

   ---------------------------
   -- Ada_Pragma_Identifier --
   ---------------------------

   function Ada_Pragma_Identifier
     (Attr : Asis.Pragma_Kinds)
      return String
   is
      Result    : String := Attr'Img;
      First_Idx : Positive;
      Last_Idx  : Positive;
   begin
      case Attr is
         when Not_A_Pragma                     |
              An_Implementation_Defined_Pragma |
              An_Unknown_Pragma                =>
            raise Constraint_Error;

         when others =>
            First_Idx := Index (Result, "_", Forward) + 1;
            Last_Idx  := Index (Result, "_", Backward) - 1;
      end case;

      Capitalize (Result (First_Idx .. Last_Idx));

      return Result (First_Idx .. Last_Idx);
   end Ada_Pragma_Identifier;

   ------------------------------
   -- Full_Expanded_Name_Image --
   ------------------------------

   function Full_Expanded_Name_Image
     (Name : Asis.Element)
      return Program_Text
   is
      EE : constant Asis.Element := Enclosing_Element (Name);
   begin
      if Defining_Name_Kind (Name) = A_Defining_Expanded_Name or else
         Is_Nil (Enclosing_Element (EE))
      then
         return Defining_Name_Image (Name);
      else
         return Internal_Full_Image (Enclosing_Element (EE)) & '.' &
                Defining_Name_Image (Name);
      end if;

   end Full_Expanded_Name_Image;

   -------------------------
   -- Internal_Full_Image --
   -------------------------

   function Internal_Full_Image (E : Asis.Element) return Program_Text is
      Next_Scope : Asis.Element := E;
      Tmp        : Asis.Element;
   begin

      while not Is_Namable_Scope (Next_Scope) loop
         Next_Scope := Enclosing_Element (Next_Scope);
         pragma Assert (not (Is_Nil (Next_Scope)));
      end loop;

      if Is_Part_Of_Instance (Next_Scope) then
         Tmp := Enclosing_Element (Next_Scope);

         if Declaration_Kind (Tmp) in A_Generic_Instantiation
           and then
            Is_Nil (Enclosing_Element (Tmp))
         then
            return Defining_Name_Image (First_Name (Tmp));
         end if;
      end if;

      if Is_Nil (Enclosing_Element (Next_Scope)) then
         return Defining_Name_Image (First_Name (Next_Scope));
      end if;

      case Flat_Element_Kind (Next_Scope) is
         when A_Procedure_Body_Declaration |
              A_Function_Body_Declaration  |
              A_Package_Body_Declaration   |
              A_Task_Body_Declaration      |
              A_Protected_Body_Declaration =>
            --  It can be a subunit here
            if Is_Subunit (Next_Scope) then
               Next_Scope := Corresponding_Body_Stub (Next_Scope);
               Next_Scope := First_Name (Next_Scope);

               return Full_Expanded_Name_Image (Next_Scope);
            end if;

         when A_Block_Statement =>
            if Is_Nil (Statement_Identifier (Next_Scope)) then
               return Internal_Full_Image (Enclosing_Element (Next_Scope)) &
                       ".";
            else
               return Internal_Full_Image (Enclosing_Element (Next_Scope)) &
                       "." & Defining_Name_Image
                               (Statement_Identifier (Next_Scope));
            end if;

         when others =>
            null;
      end case;

      return Internal_Full_Image (Enclosing_Element (Next_Scope)) & '.' &
             Defining_Name_Image (First_Name (Next_Scope));
   end Internal_Full_Image;

   ----------------------
   -- Is_Namable_Scope --
   ----------------------

   function Is_Namable_Scope (E : Element) return Boolean is
   begin
      case Flat_Element_Kind (E) is
         when A_Block_Statement               |
              A_Task_Type_Declaration         |
              A_Protected_Type_Declaration    |
              A_Single_Task_Declaration       |
              A_Single_Protected_Declaration  |
              A_Procedure_Body_Declaration    |
              A_Function_Body_Declaration     |
              A_Package_Declaration           |
              A_Package_Body_Declaration      |
              A_Task_Body_Declaration         |
              A_Protected_Body_Declaration    |
              A_Generic_Package_Declaration   |
              A_Generic_Procedure_Declaration |       --  ???
              A_Generic_Function_Declaration  |       --  ???
              A_Formal_Package_Declaration    |       --  ???
              A_Formal_Package_Declaration_With_Box   --  ???
              =>
            return True;
         when others =>
            return False;
      end case;
   end Is_Namable_Scope;

   ---------------------------------------
   -- Is_Recursive_Component_Definition --
   ---------------------------------------

   function Is_Recursive_Component_Definition
     (Def  : Asis.Element)
      return Boolean
   is
      Result    : Boolean := False;
      Tmp       : Asis.Element;
      Type_Decl : Asis.Element;
   begin
      if Access_Definition_Kind (Def) in
         An_Anonymous_Access_To_Variable .. An_Anonymous_Access_To_Constant
      then
         --  First, check if we are in component definition:
         Tmp := Enclosing_Element (Enclosing_Element (Def));

         if Declaration_Kind (Tmp) = A_Component_Declaration then
            --  Compute the defining name of enclosing type

            Type_Decl := Enclosing_Element (Tmp);

            while Element_Kind (Type_Decl) /= A_Declaration loop
               Type_Decl := Enclosing_Element (Type_Decl);
            end loop;

            Tmp := Anonymous_Access_To_Object_Subtype_Mark (Def);
            Tmp := Normalize_Reference (Tmp);
            Tmp := Corresponding_Name_Declaration (Tmp);

            Result := Is_Equal (Tmp, Type_Decl);

         end if;
      end if;

      return Result;
   end Is_Recursive_Component_Definition;

   ---------------
   -- Is_Tagged --
   ---------------

   function Is_Tagged (Dcl : Asis.Element) return Boolean is
      Result : Boolean := False;
   begin
      case Declaration_Kind (Dcl) is
         when A_Tagged_Incomplete_Type_Declaration |
              A_Private_Extension_Declaration      =>
            Result := True;
         when A_Private_Type_Declaration =>
            Result :=
              Definition_Kind (Type_Declaration_View (Dcl)) =
                A_Tagged_Private_Type_Definition;
         when An_Ordinary_Type_Declaration =>
            Result :=
              Asis.Elements.Type_Kind (Type_Declaration_View (Dcl)) in
                A_Derived_Record_Extension_Definition |
                A_Tagged_Record_Type_Definition       |
                An_Interface_Type_Definition;
         when others =>
            null;
      end case;

      return Result;
   end Is_Tagged;

   -----------------
   -- Is_Volatile --
   -----------------

   function Is_Volatile (Def_Name : Asis.Element) return Boolean is
      E      : constant Entity_Id := R_Node (Def_Name);
      Result :           Boolean   := False;
   begin

      if Defining_Name_Kind (Def_Name) = A_Defining_Identifier then
         Result := Treat_As_Volatile (E);
      end if;

      return Result;
   end Is_Volatile;

   ----------------------
   -- Is_Volatile_Type --
   ----------------------

   function Is_Volatile_Type (Subtype_Ref : Asis.Element) return Boolean is
      Tmp        : Asis.Element;
      Pragma_Arg : Asis.Element;
      Result     : Boolean := False;
   begin

      if Attribute_Kind (Subtype_Ref) /= A_Class_Attribute then
         Tmp := Normalize_Reference (Subtype_Ref);
         Tmp := Corresponding_Name_Declaration (Tmp);
         Tmp := Corresponding_First_Subtype (Tmp);

         if Declaration_Kind (Tmp) = An_Ordinary_Type_Declaration then

            declare
               Corr_Pragmas : constant Asis.Element_List :=
                 Corresponding_Pragmas (Tmp);
            begin

               for J in Corr_Pragmas'Range loop

                  if Pragma_Kind (Corr_Pragmas (J)) = A_Volatile_Pragma then
                     Pragma_Arg :=
                       Pragma_Argument_Associations (Corr_Pragmas (J)) (1);

                     Pragma_Arg := Actual_Parameter (Pragma_Arg);

                     if Expression_Kind (Pragma_Arg) = An_Identifier then
                        Pragma_Arg :=
                          Corresponding_Name_Definition (Pragma_Arg);

                        if Is_Equal (Pragma_Arg, Names (Tmp) (1)) then
                           Result := True;
                           exit;
                        end if;

                     end if;
                  end if;

               end loop;

               if not Result then
                  Tmp := Type_Declaration_View (Tmp);

                  if Asis.Elements.Type_Kind (Tmp) =
                    A_Derived_Type_Definition
                  then
                     --  Here we have to traverse the derivation chain looking
                     --  for the Volatile pragma applied to some of the parent
                     --  types

                     Tmp    := Parent_Subtype_Indication (Tmp);
                     Tmp    := Asis.Definitions.Subtype_Mark (Tmp);
                     Result := Is_Volatile_Type (Tmp);
                  end if;

               end if;

            end;

         end if;

      end if;

      return Result;
   end Is_Volatile_Type;

   ------------------
   -- Is_Component --
   ------------------

   function Is_Component (El : Asis.Element) return Boolean is
      Result : Boolean := False;
   begin

      if Expression_Kind (El) = A_Selected_Component then
         Result := Nkind (R_Node (El)) /= N_Expanded_Name;
      end if;

      return Result;
   end Is_Component;

   ----------------------------
   -- Adds_New_Nesting_Level --
   ----------------------------

   function Adds_New_Nesting_Level
     (El_Kind : Flat_Element_Kinds)
      return    Boolean
   is
      Result : Boolean := False;
   begin

      case El_Kind is
         when A_Procedure_Body_Declaration       |
              A_Function_Body_Declaration        |
              A_Package_Declaration              |
              A_Package_Body_Declaration         |
              A_Task_Body_Declaration            |
              A_Protected_Body_Declaration       |
              An_Entry_Body_Declaration          |
              A_Generic_Package_Declaration      |
              An_If_Statement                    |
              A_Case_Statement                   |
              A_Loop_Statement                   |
              A_While_Loop_Statement             |
              A_For_Loop_Statement               |
              A_Block_Statement                  |
              An_Accept_Statement                |
              A_Selective_Accept_Statement       |
              A_Timed_Entry_Call_Statement       |
              A_Conditional_Entry_Call_Statement |
              An_Asynchronous_Select_Statement   =>
            Result := True;
         when others =>
            null;
      end case;

      return Result;
   end Adds_New_Nesting_Level;

   ------------
   -- Before --
   ------------

   function Before (First : Span; Second : Span) return Boolean is
      Result : Boolean := True;
   begin
      if Is_Nil (First) or else Is_Nil (Second) then
         Result := False;
      end if;

      if Result
        and then
         (First.First_Line > Second.First_Line
         or else
          (First.First_Line = Second.First_Line
          and then
           First.First_Column >= Second.First_Column))
      then
         Result := False;
      end if;

      if Result
        and then
         (First.Last_Line > Second.Last_Line
        or else
         (First.Last_Line = Second.Last_Line
         and then
          First.Last_Column >= Second.Last_Column))
      then
         Result := False;
      end if;

      return Result;
   end Before;

   function Before
     (First : Asis.Element;
      Second  : Asis.Element)
      return  Boolean
   is
      Result : Boolean := False;
   begin
      if Is_Text_Available (First)
       and then
         Is_Text_Available (Second)
       and then
         Is_Equal (Enclosing_Compilation_Unit (First),
                   Enclosing_Compilation_Unit (Second))
      then
         Result :=
           Before (First  => Element_Span (First),
                   Second => Element_Span (Second));
      end if;

      return Result;
   end Before;

   -------------------------------------
   -- Belongs_To_Multiple_Inheritance --
   -------------------------------------

   function Belongs_To_Multiple_Inheritance (Decl : Element) return Boolean is
      Parent_Type : Asis.Element;
      Result      : Boolean := False;
   begin
      Parent_Type := Primitive_Owner (Decl);

      while Asis.Elements.Type_Kind (Parent_Type) =
               A_Derived_Record_Extension_Definition
     loop
         if not Is_Nil (Definition_Interface_List (Parent_Type)) then
            Result := True;
            exit;
         end if;

         Parent_Type := Corresponding_Parent_Subtype (Parent_Type);

         if Declaration_Kind (Parent_Type) = A_Subtype_Declaration then
            Parent_Type := Corresponding_First_Subtype (Parent_Type);
         end if;

         if Declaration_Kind (Parent_Type) = A_Private_Type_Declaration then
            exit;
         end if;

         if Declaration_Kind (Parent_Type) =
               A_Private_Extension_Declaration
         then
            Parent_Type := Corresponding_Type_Declaration (Parent_Type);
         end if;

         Parent_Type := Type_Declaration_View (Parent_Type);

      end loop;

      return Result;
   end Belongs_To_Multiple_Inheritance;

   ------------------------------
   -- Can_Create_Return_Object --
   ------------------------------

   function Can_Create_Return_Object (SM : Asis.Element) return Boolean is
      SM_Entity : constant Entity_Id := Entity (R_Node (SM));
      SM_Type   :          Entity_Id;
      Result    :          Boolean   := False;
   begin
      if Present (SM_Entity) then

         SM_Type := Etype (SM_Entity);

         if not (Is_Abstract_Type (SM_Type)
               or else
                Is_Limited_Record (SM_Type)
               or else
                Is_Limited_Composite (SM_Type))
         then
            Result :=
              not ((Has_Discriminants (SM_Type) or else
                    Is_Array_Type (SM_Type))
                  and then
                    not Is_Constrained (SM_Entity));
         end if;

      end if;

      return Result;
   end Can_Create_Return_Object;

   -------------------------------------
   -- Can_Have_Dispatching_Operations --
   -------------------------------------

   function Can_Have_Dispatching_Operations
     (El   : Asis.Element)
      return Boolean
   is
      Result : Boolean := False;
   begin

      case Definition_Kind (El) is

         when A_Type_Definition =>

            case Asis.Elements.Type_Kind (El) is
               when A_Derived_Record_Extension_Definition |
                    A_Tagged_Record_Type_Definition       =>
                  Result :=
                    Is_Nil (Corresponding_Type_Declaration
                              (Enclosing_Element (El)));

               when An_Interface_Type_Definition =>

                  Result := True;
               when others =>
                  null;
            end case;

         when A_Tagged_Private_Type_Definition |
              A_Private_Extension_Definition   =>
            Result := True;
         when others =>
            null;
      end case;

      return Result;
   end Can_Have_Dispatching_Operations;

   --------------------------------
   -- Can_Have_Elaboration_Calls --
   --------------------------------

   function Can_Have_Elaboration_Calls (El : Asis.Element) return Boolean is
      Arg_Kind : constant Flat_Element_Kinds := Flat_Element_Kind (El);
      Tmp_El   :          Asis.Element       := Nil_Element;
      Result   :          Boolean            := False;
   begin

      --  Note that we do not have to care about evaluating default parameters
      --  for IN formal objects for package instantiations- the corresponding
      --  default expression will be processed as a part of processing the
      --  expanded generic. But we have to care about subprogram
      --  instantiations

      case Arg_Kind is
         when A_Variable_Declaration =>
            Result := Is_Nil (Initialization_Expression (El));

         when A_Function_Call            |
              A_Procedure_Call_Statement =>

            --  Calls to operator functions and to attribute subprograms cannot
            --  evaluate any default expressions:

            if Arg_Kind = A_Procedure_Call_Statement then
               Tmp_El := Called_Name (El);
            elsif Arg_Kind = A_Function_Call then
               Tmp_El := Prefix (El);
            else
               Tmp_El := Nil_Element;
            end if;

            if Expression_Kind (Tmp_El) = A_Selected_Component then
               Tmp_El := Selector (Tmp_El);
            end if;

            Result := not (Expression_Kind (Tmp_El) = An_Attribute_Reference
                  or else
                           Expression_Kind (Tmp_El) = An_Operator_Symbol);

         when A_Procedure_Instantiation  |
              A_Function_Instantiation   |
              An_Allocation_From_Subtype |
              An_Entry_Call_Statement    =>

            Result := True;
         when others =>
            null;
      end case;

      return Result;
   end Can_Have_Elaboration_Calls;

   ----------------
   -- Capitalize --
   ----------------

   procedure Capitalize (S : in out String) is
      Next_Char_Uppercase : Boolean := True;
   begin
      for J in S'Range loop

         if Next_Char_Uppercase then
            S (J) := To_Upper (S (J));
         else
            S (J) := To_Lower (S (J));
         end if;

         if S (J) = '_' then
            Next_Char_Uppercase := True;
         else
            Next_Char_Uppercase := False;
         end if;
      end loop;
   end Capitalize;

   --------------
   -- Contains --
   --------------

   function Contains
     (Outer : Element;
      Inner : Element)
      return  Boolean
   is
      Outer_Span : Span;
      Inner_Span : Span;
      Result     : Boolean := False;
   begin

      if Is_Text_Available (Outer) and then
         Is_Text_Available (Inner)
      then
         Outer_Span := Element_Span (Outer);
         Inner_Span := Element_Span (Inner);

         if (Outer_Span.First_Line < Inner_Span.First_Line
            or else
             (Outer_Span.First_Line = Inner_Span.First_Line and then
              Outer_Span.First_Column <= Inner_Span.First_Column))
         and then
            (Outer_Span.Last_Line  > Inner_Span.Last_Line
            or else
             (Outer_Span.Last_Line = Inner_Span.Last_Line and then
              Outer_Span.Last_Column >= Inner_Span.Last_Column))
         then
            Result := True;
         end if;

      end if;

      return Result;
   end Contains;

   -------------------------
   -- Contains_Raise_Stmt --
   -------------------------

   function Contains_Raise_Stmt (C : Asis.Element) return Boolean is
      Result  : Boolean          := False;
      Control : Traverse_Control := Continue;

      procedure Check_For_Raise
        (Element :        Asis.Element;
         Control : in out Traverse_Control;
         State   : in out Boolean);
      --  If Element is a raise statement, sets State to True and Control to
      --  terminate_Immediatelly. Otherwise does nothing

      procedure No_Op
        (Element :        Asis.Element;
         Control : in out Traverse_Control;
         State   : in out Boolean);
      --  Does nothing

      procedure Look_For_Raise is new
        Traverse_Element (Boolean, Check_For_Raise, No_Op);

      procedure Check_For_Raise
        (Element :        Asis.Element;
         Control : in out Traverse_Control;
         State   : in out Boolean)
      is
      begin
         if Statement_Kind (Element) = A_Raise_Statement then
            State   := True;
            Control := Terminate_Immediately;
         end if;
      end Check_For_Raise;

      procedure No_Op
        (Element :        Asis.Element;
         Control : in out Traverse_Control;
         State   : in out Boolean)
      is
      begin
         null;
      end No_Op;

   begin
      Look_For_Raise (C, Control, Result);

      return Result;
   end Contains_Raise_Stmt;

   -----------------------
   -- Defines_Predicate --
   -----------------------

   function Defines_Predicate (A : Asis.Element) return Boolean is
      Result      : Boolean := False;
      Aspect_M    : Asis.Element;
      Aspect_Name : Program_Text_Access;
   begin
      if Definition_Kind (A) = An_Aspect_Specification then
         Aspect_M := Aspect_Mark (A);

         if Expression_Kind (Aspect_M) = An_Attribute_Reference then
            Aspect_M := Prefix (Aspect_M);
         end if;

         Aspect_Name :=
           new Program_Text'
                 (ASIS_UL.Misc.To_Lower_Case (Name_Image (Aspect_M)));

         Result := Aspect_Name.all = "static_predicate"
                or else
                   Aspect_Name.all = "dynamic_predicate"
                or else
                   Aspect_Name.all = "type_invariant"
                or else
                   Aspect_Name.all = "pre"
                or else
                   Aspect_Name.all = "post";

         Free (Aspect_Name);
      end if;

      return Result;
   end Defines_Predicate;

   ----------------------------
   -- Dispatching_Operations --
   ----------------------------

   function Dispatching_Operations
     (Type_Def : Asis.Element)
      return     Asis.Element_List
   is
      Type_Node      : Node_Id;
      Prim_Ops       : Elist_Id;
      Next_Pr_El     : Elmt_Id;
      Next_Primitive : Node_Id;

      Res_Node    : Node_Id;
      Res_NF_1    : Node_Id;
      Res_Kind    : Internal_Element_Kinds;
      Inherited   : Boolean;
      Res_Element : Asis.Element;
   begin
      --  See ASIS_UL.Global_State.Utilities.Implemented_Operations. Is any
      --  factorization possible here?

      if not Can_Have_Dispatching_Operations (Type_Def) then
         Raise_ASIS_Inappropriate_Element
           (Diagnosis  => Package_Name & "Dispatching_Operations",
            Wrong_Kind => Int_Kind (Type_Def));
      end if;

      Type_Node := Node (Enclosing_Element (Type_Def));
      Type_Node := Defining_Identifier (Type_Node);

      if not Has_Primitive_Operations (Type_Node) then
         return Nil_Element_List;
      end if;

      Asis_Element_Table.Init;

      Prim_Ops  := Primitive_Operations (Type_Node);

      Next_Pr_El := First_Elmt (Prim_Ops);

      while Present (Next_Pr_El) loop
         Next_Primitive := Node (Next_Pr_El);

         if No (Interface_Alias (Next_Primitive)) then

            if Comes_From_Source (Next_Primitive) then
               Inherited := False;
               Res_Node  := Parent (Parent (Next_Primitive));
               Res_NF_1  := Empty;
               Res_Kind  := Not_An_Element;
            else
               --  See implementation of
               --  Asis.Definitions.Implicit_Inherited_Subprograms:

               Inherited := True;
               Res_NF_1  := Next_Primitive;
               Res_Node  := Explicit_Parent_Subprogram (Next_Primitive);
               Res_Node  := Parent (Res_Node);

               if Ekind (Next_Primitive) = E_Function then
                  Res_Kind := A_Function_Declaration;
               elsif Null_Present (Res_Node) then
                  Res_Kind := A_Null_Procedure_Declaration;
               else
                  Res_Kind := A_Procedure_Declaration;
               end if;

               Res_Node := Parent (Res_Node);
            end if;

            Res_Element :=
              Node_To_Element_New
                (Node          => Res_Node,
                 Node_Field_1  => Res_NF_1,
                 Internal_Kind => Res_Kind,
                 Inherited     => Inherited,
                 In_Unit       => Enclosing_Unit
                                    (Encl_Cont_Id (Type_Def),
                                     Next_Primitive));

            if Is_From_Instance (Type_Def) then
               Set_From_Instance (Res_Element, True);
            else
               Set_From_Instance (Res_Element, False);
            end if;

            Asis_Element_Table.Append (Res_Element);
         end if;

         Next_Pr_El := Next_Elmt (Next_Pr_El);
      end loop;

      return Asis.Element_List
               (Asis_Element_Table.Table (1 .. Asis_Element_Table.Last));

   end Dispatching_Operations;

   ---------------------------------
   -- Does_Not_Add_New_Components --
   ---------------------------------

   function Does_Not_Add_New_Components (El : Asis.Element) return Boolean is
      Result : Boolean := False;
      Tmp    : Asis.Element;
   begin

      if Asis.Elements.Type_Kind (El) =
           A_Derived_Record_Extension_Definition
      then

         Tmp := Asis.Definitions.Record_Definition (El);

         if Definition_Kind (Tmp) = A_Null_Record_Definition then
            Result := True;
         elsif Record_Components (Tmp)'Length = 1
             and then
               Definition_Kind (Record_Components (Tmp) (1)) = A_Null_Component
         then
            Result := True;
         end if;

      end if;

      return Result;
   end Does_Not_Add_New_Components;

   ---------------------
   -- Call_Parameters --
   ---------------------

   function Call_Parameters (Call : Asis.Element) return Asis.Element_List is
   begin

      case Flat_Element_Kind (Call) is
         when A_Procedure_Call_Statement |
              An_Entry_Call_Statement    =>
            return Call_Statement_Parameters (Call);
         when A_Function_Call =>
            return Function_Call_Parameters (Call);
         when others =>
            pragma Assert (False);
            return Nil_Element_List;
      end case;

   end Call_Parameters;

   --------------------------------------
   -- Unwind_Derivations_And_Subtyping --
   --------------------------------------

   function Unwind_Derivations_And_Subtyping
     (Decl : Asis.Element)
      return Asis.Element
   is
      Result : Asis.Element := Decl;
      Def    : Asis.Element := Type_Declaration_View (Result);
   begin

      if Definition_Kind (Def) = A_Subtype_Indication then
         Result := Corresponding_First_Subtype (Def);
         Def    := Type_Declaration_View (Result);
      end if;

      if Asis.Elements.Type_Kind (Def) in A_Derived_Type_Definition ..
            A_Derived_Record_Extension_Definition
      then
         Result := Corresponding_Root_Type (Def);
      end if;

      return Result;

   end Unwind_Derivations_And_Subtyping;

   --------------------
   -- Get_Called_Ref --
   --------------------

   function Get_Called_Ref (Call : Asis.Element) return Asis.Element is
   begin

      if Expression_Kind (Call) = A_Function_Call then
         return Prefix (Call);
      else
         return Called_Name (Call);
      end if;

   end Get_Called_Ref;

   --------------------
   -- Called_Profile --
   --------------------

   function Called_Profile (Call : Asis.Element) return Asis.Element_List is
      Get_Profile_From : Asis.Element;
      Tmp              : Asis.Element;
   begin

      --  Filter out the case of a call to a predefined operator

      if Expression_Kind (Call) = A_Function_Call then
         Tmp := Prefix (Call);
         Tmp := Normalize_Reference (Tmp);

         if Is_Predefined_Operator (Tmp) then
            return Nil_Element_List;
         end if;

      end if;

      --  Filter out the case of a call to attribute subprogram:

      if Is_Call_To_Attribute_Subprogram (Call) then
         return Nil_Element_List;
      end if;

      case Flat_Element_Kind (Call) is
         when A_Procedure_Call_Statement |
              An_Entry_Call_Statement    =>
            Get_Profile_From := Corresponding_Called_Entity (Call);
         when A_Function_Call =>
            Get_Profile_From := Corresponding_Called_Function (Call);
         when others =>
            pragma Assert (False);
            return Nil_Element_List;
      end case;

      if Is_Nil (Get_Profile_From) then
         --  two possibilities: either a dispatching call or a dynamic call
         --  through access-to-subprogram value

         if Is_Dispatching_Call (Call) then
            Tmp              := Get_Called_Ref (Call);
            Tmp              := Normalize_Reference (Tmp);
            Get_Profile_From := Corresponding_Name_Declaration (Tmp);
         else
            --  Call through access-to-subprogram value
            Get_Profile_From := Get_Called_Ref (Call);

            if Expression_Kind (Get_Profile_From) =
                 An_Explicit_Dereference
            then
               Get_Profile_From := Prefix (Get_Profile_From);
            end if;

            Tmp              := Get_Profile_From;
            Get_Profile_From :=
              Corresponding_Expression_Type (Get_Profile_From);

            if not Is_Nil (Get_Profile_From) then
               Get_Profile_From :=
                 Unwind_Derivations_And_Subtyping (Get_Profile_From);
            else
               --  Here we have the dynamic call to a subprogram through
               --  anonymous access-to-subprogram type.

               Get_Profile_From := Normalize_Reference (Tmp);

               case Expression_Kind (Get_Profile_From) is
                  when An_Identifier =>
                     Get_Profile_From :=
                       Corresponding_Name_Declaration (Get_Profile_From);

                  when An_Indexed_Component =>
                     --  No need to check for Corresponding_Expression_Type -
                     --  we have already tried this out, and the result is
                     --  Nil_Element. Here we have an indexed component that
                     --  has an anonymous array type.
                     Get_Profile_From := Prefix (Get_Profile_From);
                     Get_Profile_From :=
                        Corresponding_Expression_Type (Get_Profile_From);

                     if Is_Nil (Get_Profile_From) then
                        --  Anonymous access types - endless nightmare :(
                        raise Non_Implemented_Error;
                     end if;

                     Get_Profile_From := Unwind_Derivations_And_Subtyping
                       (Get_Profile_From);

                  when others =>
                     raise Non_Implemented_Error;
               end case;

            end if;

         end if;

      end if;

      if Declaration_Kind (Get_Profile_From) in
        A_Procedure_Instantiation  .. A_Function_Instantiation
      then
         Get_Profile_From := Corresponding_Declaration (Get_Profile_From);
      end if;

      case Declaration_Kind (Get_Profile_From) is

         when A_Procedure_Declaration          |
              A_Function_Declaration           |
              A_Procedure_Body_Declaration     |
              A_Function_Body_Declaration      |
              A_Procedure_Renaming_Declaration |
              A_Function_Renaming_Declaration  |
              An_Entry_Declaration             |
              An_Entry_Body_Declaration        |
              A_Procedure_Body_Stub            |
              A_Function_Body_Stub             |
              A_Generic_Procedure_Declaration  |
              A_Generic_Function_Declaration   |
              A_Formal_Procedure_Declaration   |
              A_Formal_Function_Declaration    =>

            return Parameter_Profile (Get_Profile_From);

         when An_Ordinary_Type_Declaration =>
            Get_Profile_From := Type_Declaration_View (Get_Profile_From);

            case Asis.Elements.Type_Kind (Get_Profile_From) is
               when An_Access_Type_Definition =>
                  pragma Assert (Access_Type_Kind (Get_Profile_From) in
                                   Asis.Access_To_Subprogram_Definition);

                  return
                    Access_To_Subprogram_Parameter_Profile (Get_Profile_From);

               when An_Unconstrained_Array_Definition |
                    A_Constrained_Array_Definition    =>
                  Get_Profile_From :=
                    Array_Component_Definition (Get_Profile_From);

                  Get_Profile_From :=
                    Component_Definition_View (Get_Profile_From);

                  return
                    Access_To_Subprogram_Parameter_Profile (Get_Profile_From);
               when others =>
                  raise Non_Implemented_Error;
            end case;

         when A_Parameter_Specification      |
              A_Variable_Declaration         |
              A_Constant_Declaration         |
              A_Discriminant_Specification   |
              An_Object_Renaming_Declaration =>
            --  Use of anonymous Access-to-subprogram type
            Get_Profile_From := Object_Declaration_View (Get_Profile_From);
            return Access_To_Subprogram_Parameter_Profile (Get_Profile_From);

         when A_Component_Declaration =>
            --  Use of anonymous Access-to-subprogram type
            Get_Profile_From := Object_Declaration_View (Get_Profile_From);
            Get_Profile_From := Component_Definition_View (Get_Profile_From);
            return Access_To_Subprogram_Parameter_Profile (Get_Profile_From);

         when others =>
            raise Non_Implemented_Error;
      end case;

   end Called_Profile;

   -------------------------------
   -- Get_Parameter_Declaration --
   -------------------------------

   function Get_Parameter_Declaration (El : Asis.Element) return Asis.Element
   is
      Formal_Par : constant Asis.Element := Formal_Parameter (El);
      Actual_Par : constant Asis.Element := Actual_Parameter (El);
      Result     :          Asis.Element;
   begin

      if Is_Nil (Formal_Par) then

         declare
            Call      : constant Asis.Element      := Enclosing_Element (El);
            Norm_Pars : constant Asis.Element_List :=
              (if Expression_Kind (Call) = A_Function_Call then
                    Function_Call_Parameters (Call, Normalized => True)

               else
                  Call_Statement_Parameters (Call, Normalized => True));
         begin

            for J in Norm_Pars'Range loop
               if Is_Equal (Actual_Parameter (Norm_Pars (J)), Actual_Par) then
                  Result :=
                    Enclosing_Element (Formal_Parameter (Norm_Pars (J)));
                  exit;
               end if;
            end loop;
         end;
      else
         Result := Corresponding_Name_Declaration (Formal_Par);
      end if;

      pragma Assert (not Is_Nil (Result));

      return Result;
   end Get_Parameter_Declaration;

   ------------------------
   -- Get_Called_Element --
   ------------------------

   function Get_Called_Element (Call : Asis.Element) return Asis.Element is
      Result : Asis.Element := Nil_Element;
      Tmp    : Asis.Element;
   begin

      if Expression_Kind (Call) in A_Function_Call | An_Indexed_Component then
         --  An_Indexed_Component can be Is_Generalized_Indexing only here!
         Result := Corresponding_Called_Function (Call);
      else
         Result := Corresponding_Called_Entity (Call);
      end if;

      if Is_Nil (Result)
        and then
         Is_Dispatching_Call (Call)
      then
         if Expression_Kind (Call) = A_Function_Call then
            Tmp := Prefix (Call);
         else
            Tmp := Called_Name (Call);
         end if;

         Result := Corresponding_Name_Declaration (Normalize_Reference (Tmp));
      end if;

      return Result;
   end Get_Called_Element;

   --------------------
   -- Get_Statements --
   --------------------

   function Get_Statements (El : Asis.Element) return Asis.Element_List is
   begin
      case Int_Kind (El) is
         when A_Procedure_Body_Declaration |
              A_Function_Body_Declaration  |
              A_Package_Body_Declaration   |
              A_Task_Body_Declaration      |
              An_Entry_Body_Declaration    =>
            return Body_Statements (El);

         when A_Loop_Statement       |
              A_While_Loop_Statement |
              A_For_Loop_Statement   =>
            return Loop_Statements (El);

         when An_If_Path .. A_Then_Abort_Path =>
            return Sequence_Of_Statements (El);

         when A_Block_Statement =>
            return Block_Statements (El);

         when An_Extended_Return_Statement =>
            return Extended_Return_Statements (El);

         when An_Accept_Statement =>
            return Accept_Body_Statements (El);
         when others =>
            Raise_ASIS_Inappropriate_Element
              (Diagnosis  => Package_Name & "Get_Statements",
               Wrong_Kind => Int_Kind (El));
            return Nil_Element_List;
      end case;
   end Get_Statements;

   ---------------------------
   -- Get_Subtype_Structure --
   ---------------------------

   function Get_Subtype_Structure (Def : Asis.Element) return Asis.Element is
      Result   : Asis.Element := Def;
   begin
      if Definition_Kind (Def) = A_Subtype_Indication then
         Result := Asis.Definitions.Subtype_Mark  (Result);
      else
         Result := Anonymous_Access_To_Object_Subtype_Mark  (Result);
      end if;

      Result := Normalize_Reference            (Result);
      Result := Corresponding_Name_Declaration (Result);
      Result := Get_Type_Structure             (Result);

      return Result;
   end Get_Subtype_Structure;

   ------------------------
   -- Get_Type_Structure --
   ------------------------

   function Get_Type_Structure (Decl : Asis.Element) return Asis.Element is
      Arg_Kind : constant Declaration_Kinds := Declaration_Kind (Decl);
      Result   :          Asis.Element      := Decl;
      Tmp      :          Asis.Element;
   begin
      --  Should be replaced with regular kind check!
      pragma Assert
        (Arg_Kind in
           An_Ordinary_Type_Declaration .. A_Subtype_Declaration
              or else
         Arg_Kind = A_Formal_Type_Declaration);

      --  We cannot use Asis,Definitions queries Corresponding_Root_Type or
      --  Corresponding_Type_Structure - they unwind derivations, so we can
      --  miss extension components

      case Arg_Kind is
         when A_Task_Type_Declaration              |
              A_Protected_Type_Declaration         |
              An_Incomplete_Type_Declaration       |
              A_Tagged_Incomplete_Type_Declaration |
              A_Formal_Type_Declaration            =>
            null;
         when An_Ordinary_Type_Declaration =>
            Tmp := Type_Declaration_View (Result);

            if Asis.Elements.Type_Kind (Tmp) = A_Derived_Type_Definition
              or else
                (Asis.Elements.Type_Kind (Tmp) =
                   A_Derived_Record_Extension_Definition
               and then
                Does_Not_Add_New_Components (Tmp))
            then
               Result := Parent_Subtype_Indication (Tmp);
               Result := Get_Subtype_Structure     (Result);
            end if;

         when A_Private_Type_Declaration |
              A_Private_Extension_Declaration =>
            Result :=
              Get_Type_Structure (Corresponding_Type_Completion (Result));

         when A_Subtype_Declaration =>
            Result := Type_Declaration_View (Result);
            Result := Get_Subtype_Structure (Result);
         when others =>
            pragma Assert (False);
            null;

      end case;

      return Result;
   end Get_Type_Structure;

   -------------------------------
   -- GNAT_Attribute_Designator --
   -------------------------------

   function GNAT_Attribute_Designator
     (Attr : Snames.Attribute_Id)
      return String
   is
      Result    : String := Attr'Img;
      First_Idx : constant Positive := Index (Result, "_", Forward) + 1;
      Last_Idx  : constant Positive := Result'Last;
   begin
      Capitalize (Result (First_Idx .. Last_Idx));

      return Result (First_Idx .. Last_Idx);
   end GNAT_Attribute_Designator;

   ----------------------------
   -- GNAT_Pragma_Identifier --
   ----------------------------

   function GNAT_Pragma_Identifier
     (Attr : Snames.Pragma_Id)
      return String
   is
      Result    : String := Attr'Img;
      First_Idx : constant Positive := Index (Result, "_", Forward) + 1;
      Last_Idx  : constant Positive := Result'Last;
   begin
      Capitalize (Result (First_Idx .. Last_Idx));

      return Result (First_Idx .. Last_Idx);
   end GNAT_Pragma_Identifier;

   -----------------------
   -- Has_Pragma_Inline --
   -----------------------

   function Has_Pragma_Inline (Subpr : Element) return Boolean is
      Arg          : Asis.Element := Subpr;
      Subpr_Entity : Entity_Id;
      Result       : Boolean := False;
   begin

      --  In case of a generic instantiation (or expanded subprogram) there can
      --  be two sources of pragma Inline:
      --
      --  - Inline is applied to the instantiation itself,in this case
      --    Has_Pragma_Inline flag is set for the entity of expanded spec;
      --
      --  - Inline is applied to the generic subprogram, in this case
      --    Has_Pragma_Inline is set for generic subprogram entity.
      --
      --  This makes the logic of this check a bit twisty :(

      --  First, get rid of unexpected elements:

      case Declaration_Kind (Arg) is
         when A_Procedure_Declaration            |
              A_Function_Declaration             |
              A_Procedure_Body_Declaration       |
              A_Function_Body_Declaration        |
              An_Expression_Function_Declaration |
              A_Generic_Procedure_Declaration    |
              A_Generic_Function_Declaration     |
              A_Procedure_Instantiation          |
              A_Function_Instantiation           =>
            --  ???WHAT ABOUT SUBPROGRAM RENAMINGS???

            null; --  Just to continue...
         when others =>
            return False;
      end case;

      --  If we have an instantiation or an expanded subprogram, check if we
      --  have Inline for generic:

      if Special_Case (Arg) = Expanded_Subprogram_Instantiation then
         Arg := Enclosing_Element (Arg);
      end if;

      if Declaration_Kind (Arg) in
         A_Procedure_Instantiation .. A_Function_Instantiation
      then
         Arg := Generic_Unit_Name (Arg);
         Arg := Normalize_Reference (Arg);
         Arg := Corresponding_Name_Declaration (Arg);

         if Declaration_Kind (Arg) in
           A_Generic_Procedure_Renaming_Declaration ..
           A_Generic_Function_Renaming_Declaration
         then
            Arg := Corresponding_Base_Entity (Arg);
            Arg := Normalize_Reference (Arg);
            Arg := Corresponding_Name_Declaration (Arg);
         end if;

         Subpr_Entity := R_Node (Arg);
         Subpr_Entity := Defining_Unit_Name (Specification (Subpr_Entity));

         --  Looks like a corner case, but still:
         if Nkind (Subpr_Entity) = N_Defining_Program_Unit_Name then
            Subpr_Entity := Defining_Identifier (Subpr_Entity);
         end if;

         Result := Has_Pragma_Inline (Subpr_Entity);
      end if;

      if Result then
         return True;
      end if;

      --  If we have an instantiation (and we are here - that is, generic
      --  does not have Inline!), we have to check expanded spec:
      Arg := Subpr;

      if Declaration_Kind (Arg) in
         A_Procedure_Instantiation .. A_Function_Instantiation
      then
         Arg := Corresponding_Declaration (Arg);
      end if;

      --  Arg cannot be an instantiation any more

      Subpr_Entity := R_Node (Arg);
      Subpr_Entity := Defining_Unit_Name (Specification (Subpr_Entity));

      --  Looks like a corner case, but still:
      if Nkind (Subpr_Entity) = N_Defining_Program_Unit_Name then
         Subpr_Entity := Defining_Identifier (Subpr_Entity);
      end if;

      Result := Has_Pragma_Inline (Subpr_Entity);

      if not Result
        and then
         Declaration_Kind (Arg) in
           A_Procedure_Body_Declaration |
           A_Function_Body_Declaration  |
           An_Expression_Function_Declaration
      then
         Result := Has_Pragma_Inline (Corresponding_Declaration (Arg));
      end if;

      return Result;
   end Has_Pragma_Inline;

   ---------------------
   -- In_Private_Part --
   ---------------------

   function In_Private_Part
     (Pack    : Asis.Element;
      Element : Asis.Element)
      return    Boolean
   is
      Result : Boolean := False;
   begin
      if Is_Text_Available (Pack)
       and then
         Is_Text_Available (Element)
       and then
         Is_Equal (Enclosing_Compilation_Unit (Pack),
                   Enclosing_Compilation_Unit (Element))
       and then
         (Declaration_Kind (Pack) = A_Package_Declaration
         or else
          Declaration_Kind (Pack) = A_Generic_Package_Declaration)
      then
         declare
            Private_Dcls : constant Element_List :=
              Private_Part_Declarative_Items (Pack, Include_Pragmas => True);
            Private_Span : Span;
            Arg_Span     : Span;
         begin

            if not Is_Nil (Private_Dcls) then
               Arg_Span := Element_Span (Private_Dcls (Private_Dcls'First));

               Private_Span.First_Line   := Arg_Span.First_Line;
               Private_Span.First_Column := Arg_Span.First_Column;

               Arg_Span := Element_Span (Private_Dcls (Private_Dcls'Last));

               Private_Span.Last_Line   := Arg_Span.Last_Line;
               Private_Span.Last_Column := Arg_Span.Last_Column;

               Arg_Span := Element_Span (Element);

               Result := Inclides (Whole => Private_Span, Part => Arg_Span);
            end if;

         end;

      end if;

      return Result;
   end In_Private_Part;

   --------------
   -- Inclides --
   --------------

   function Inclides (Whole : Span; Part : Span) return Boolean is
      Result : Boolean := True;
   begin
      if Is_Nil (Whole) or else Is_Nil (Part) then
         Result := False;
      end if;

      if Result
        and then
         (Whole.First_Line > Part.First_Line
         or else
          (Whole.First_Line = Part.First_Line
          and then
           Whole.First_Column > Part.First_Column))
      then
         Result := False;
      end if;

      if Result
        and then
         (Whole.Last_Line < Part.Last_Line
        or else
         (Whole.Last_Line = Part.Last_Line
         and then
          Whole.Last_Column < Part.Last_Column))
      then
         Result := False;
      end if;

      return Result;
   end Inclides;

   function Inclides
     (Whole : Asis.Element;
      Part  : Asis.Element)
      return  Boolean
   is
      Result : Boolean := False;
   begin
      if Is_Text_Available (Whole)
       and then
         Is_Text_Available (Part)
       and then
         Is_Equal (Enclosing_Compilation_Unit (Whole),
                   Enclosing_Compilation_Unit (Part))
      then
         Result :=
           Inclides (Whole => Element_Span (Whole),
                     Part  => Element_Span (Part));
      end if;

      return Result;
   end Inclides;

   -----------------------
   -- Inheritance_Depth --
   -----------------------

   function Inheritance_Depth (Type_Def : Asis.Element) return Natural is
      Result          : Natural := 0;
      Parent_Type_Def : Asis.Element      := Nil_Element;
   begin

      if Asis.Elements.Type_Kind (Type_Def) =
           A_Derived_Record_Extension_Definition
      then
         Parent_Type_Def := Parent_Subtype_Indication (Type_Def);
         Parent_Type_Def := Asis.Definitions.Subtype_Mark (Parent_Type_Def);
      elsif Formal_Type_Kind (Type_Def) = A_Formal_Derived_Type_Definition then
         Parent_Type_Def := Asis.Definitions.Subtype_Mark (Type_Def);
      elsif Definition_Kind (Type_Def) = A_Private_Extension_Definition then
         Parent_Type_Def := Ancestor_Subtype_Indication (Type_Def);
         Parent_Type_Def := Asis.Definitions.Subtype_Mark (Parent_Type_Def);
      end if;

      if not Is_Nil (Parent_Type_Def) then

         Parent_Type_Def := Normalize_Reference (Parent_Type_Def);
         Parent_Type_Def := Corresponding_Name_Declaration (Parent_Type_Def);
         Parent_Type_Def := Corresponding_First_Subtype (Parent_Type_Def);

         Parent_Type_Def := Type_Declaration_View (Parent_Type_Def);

         Result := 1 + Inheritance_Depth (Parent_Type_Def);
      end if;

      if Asis.Elements.Type_Kind (Type_Def) =
            A_Derived_Record_Extension_Definition
        or else
         Asis.Elements.Type_Kind (Type_Def) = An_Interface_Type_Definition
        or else
          Definition_Kind (Type_Def) = A_Private_Extension_Definition
        or else
          Formal_Type_Kind (Type_Def) = A_Formal_Derived_Type_Definition
        or else
          Formal_Type_Kind (Type_Def) = A_Formal_Interface_Type_Definition
      then

         declare

            Interface_List : constant Asis.Element_List :=
              Definition_Interface_List (Type_Def);
         begin

            if Is_Nil (Interface_List)
              and then
               Formal_Type_Kind (Type_Def) = A_Formal_Derived_Type_Definition
            then
               return Result - 1;
            elsif Interface_List'Length = 1
              and then
                  Formal_Type_Kind (Type_Def) =
                  A_Formal_Interface_Type_Definition
            then
               return
                 Inheritance_Depth (Interface_List (Interface_List'First));
            end if;

            for J in Interface_List'Range loop
               Parent_Type_Def := Normalize_Reference (Interface_List (J));
               Parent_Type_Def :=
                 Corresponding_Name_Declaration (Parent_Type_Def);
               Parent_Type_Def :=
                 Corresponding_First_Subtype (Parent_Type_Def);
               Parent_Type_Def := Type_Declaration_View (Parent_Type_Def);

               Result :=
                 Natural'Max (Result, 1 + Inheritance_Depth (Parent_Type_Def));
            end loop;

         end;
      end if;

      return Result;
   end Inheritance_Depth;

   --------------------
   -- Interface_List --
   --------------------

   function Interface_List (Decl : Asis.Element) return Asis.Element_List is
   begin

      if not May_Have_Interface_List (Decl) then
         Raise_ASIS_Inappropriate_Element
           (Diagnosis  => Package_Name & "Interface_List",
            Wrong_Kind => Int_Kind (Decl));
      end if;

      case Declaration_Kind (Decl) is
         when A_Task_Type_Declaration        |
              A_Protected_Type_Declaration   |
              A_Single_Task_Declaration      |
              A_Single_Protected_Declaration =>
            return Declaration_Interface_List (Decl);

         when others =>
            return Definition_Interface_List (Type_Declaration_View (Decl));
      end case;

   end Interface_List;

   -------------------------
   -- Is_Access_Attribute --
   -------------------------

   function Is_Access_Attribute (Attr : Asis.Element) return Boolean is
      Result : Boolean := False;
   begin

      case Attribute_Kind (Attr) is
         when An_Access_Attribute           |
              An_Unchecked_Access_Attribute =>
            Result := True;

         when An_Implementation_Defined_Attribute =>
            if To_Lower (To_String (Name_Image
                 (Attribute_Designator_Identifier (Attr)))) =
               "unrestricted_access"
            then
               Result := True;
            end if;
         when others =>
            null;
      end case;

      return Result;
   end Is_Access_Attribute;

   -------------------------------------
   -- Is_Call_To_Attribute_Subprogram --
   -------------------------------------

   function Is_Call_To_Attribute_Subprogram
     (El   : Asis.Element)
      return Boolean
   is
      Result      : Boolean      := False;
      Call_Prefix : Asis.Element := Nil_Element;
   begin

      case Flat_Element_Kind (El) is
         when A_Procedure_Call_Statement =>
            Call_Prefix := Called_Name (El);

         when A_Function_Call =>

            if Is_Prefix_Call (El) then
               Call_Prefix := Prefix (El);
            end if;

         when others =>
            null;
      end case;

      if Expression_Kind (Call_Prefix) = An_Attribute_Reference then
         Result := True;
      end if;

      return Result;

   end Is_Call_To_Attribute_Subprogram;

   -----------------------------
   -- Is_Constructor_Function --
   -----------------------------

   function Is_Constructor_Function (El : Asis.Element) return Boolean is
      Tmp    : Asis.Element;
      F_Node : Node_Id;
      P_Node : Node_Id;
      Arg    : Asis.Element := El;
      Result : Boolean      := False;
   begin
      case Declaration_Kind (El) is
         when A_Function_Declaration             |
              A_Function_Body_Declaration        |
              An_Expression_Function_Declaration |
              A_Function_Renaming_Declaration    |
              A_Function_Instantiation           =>

            if Is_Dispatching_Operation (El) then
               Tmp := First_Name (El);

               if Defining_Name_Kind (Tmp) /= A_Defining_Expanded_Name then
                  F_Node := R_Node (Tmp);

                  if Has_Controlling_Result (F_Node) then
                     if Declaration_Kind (Arg) = A_Function_Instantiation then
                        Arg := Corresponding_Declaration (Arg);
                     end if;

                     P_Node := R_Node (Arg);
                     P_Node := Specification (P_Node);
                     Result := True;

                     if Is_Non_Empty_List
                          (Parameter_Specifications (P_Node))
                     then
                        P_Node :=
                          First_Non_Pragma (Parameter_Specifications (P_Node));

                        while Present (P_Node) loop
                           if Is_Controlling_Formal
                                (Defining_Identifier (P_Node))
                           then
                              Result := False;
                              exit;
                           end if;

                           P_Node := Next_Non_Pragma (P_Node);
                        end loop;

                     end if;
                  end if;
               end if;
            end if;
         when others => null;
      end case;

      return Result;
   end Is_Constructor_Function;

   -------------------
   -- Is_Controlled --
   -------------------

   function Is_Controlled (Type_Name : Asis.Element) return Boolean is
      Result : Boolean := False;
   begin
      if Defining_Name_Kind (Type_Name) = A_Defining_Identifier then
         if Is_Controlled (R_Node (Type_Name)) then
            Result := True;
         end if;
      end if;

      return Result;
   end Is_Controlled;

   -----------------------------------
   -- Is_Controlling_Type_Operation --
   -----------------------------------

   function Is_Controlling_Type_Operation (El : Asis.Element) return Boolean is
      Type_Decl : Asis.Element;
      Result    : Boolean := False;
   begin
      if not Is_Part_Of_Inherited (El)
        and then
         Is_Dispatching_Operation (El)
      then
         Type_Decl := Primitive_Owner (El);
         Type_Decl := First_Name (Enclosing_Element (Type_Decl));

         if Is_Controlled (Type_Decl)
           and then
            Parameter_Profile (El)'Length = 1
           and then
            Mode_Kind (Parameter_Profile (El) (1)) = An_In_Out_Mode
         then
            declare
               El_Name : constant String :=
                 To_Lower (To_String (Defining_Name_Image (First_Name (El))));
            begin
               if El_Name = "initialize"
                 or else
                  El_Name = "finalize"
               then
                  Result := True;
               elsif El_Name = "adjust"
                 and then
                     not Is_Limited_Type (R_Node (Type_Decl))
               then
                  Result := True;
               end if;
            end;
         end if;

      end if;

      return Result;
   end Is_Controlling_Type_Operation;

   ---------------------
   -- Is_Derived_From --
   ---------------------

   function Is_Derived_From
      (Descendant : Element;
       Ancestor   : Element)
       return       Boolean
   is
      Result         : Boolean := False;
      Ancestor_Arg   : Asis.Element;
      Descendant_Arg : Asis.Element;
      Def            : Asis.Element;
   begin

      if Declaration_Kind (Ancestor) in
          A_Private_Type_Declaration .. A_Private_Extension_Declaration
      then
         Ancestor_Arg := Corresponding_Type_Declaration (Ancestor);
      else
         Ancestor_Arg := Ancestor;
      end if;

      if Declaration_Kind (Descendant) in
          A_Private_Type_Declaration .. A_Private_Extension_Declaration
      then
         Descendant_Arg := Corresponding_Type_Declaration (Descendant);
      else
         Descendant_Arg := Descendant;
      end if;

      if Declaration_Kind (Ancestor_Arg) in
           An_Ordinary_Type_Declaration .. A_Protected_Type_Declaration
       and then
         Declaration_Kind (Descendant_Arg) = An_Ordinary_Type_Declaration
      then
         Def := Type_Declaration_View (Descendant_Arg);

         while Asis.Elements.Type_Kind (Def) in A_Derived_Type_Definition ..
                 A_Derived_Record_Extension_Definition
         loop
            Def := Corresponding_Parent_Subtype (Def);

            if Is_Nil (Def) then
               --  case of
               --     type NT is new T'Base;
               Def := Parent_Subtype_Indication (Def);
               Def := Asis.Definitions.Subtype_Mark (Def);
               Def := Normalize_Reference (Def);
               Def := Corresponding_Name_Declaration (Def);
            end if;

            Def := Corresponding_First_Subtype (Def);

            if Declaration_Kind (Def) in
              A_Private_Type_Declaration .. A_Private_Extension_Declaration
            then
               Def := Corresponding_Type_Declaration (Def);
            end if;

            if Is_Equal (Def, Ancestor_Arg) then
               Result := True;
               exit;
            end if;

            Def := Type_Declaration_View (Def);

         end loop;

      end if;

      return Result;
   end Is_Derived_From;

   ------------------------
   -- Is_Executable_Body --
   ------------------------

   function Is_Executable_Body (El : Element) return Boolean is
      El_Kind : constant Flat_Element_Kinds := Flat_Element_Kind (El);
      Result  : Boolean                     := False;
   begin

      case El_Kind is

         when An_Expression_Function_Declaration |
              A_Procedure_Body_Declaration       |
              A_Function_Body_Declaration        |
              A_Task_Body_Declaration            |
              An_Entry_Body_Declaration          =>
            Result := True;
         when A_Package_Body_Declaration =>
            Result := Body_Statements (El)'Length > 0;
         when others =>
            null;
      end case;

      return Result;
   end Is_Executable_Body;

   ----------------
   -- Is_Handled --
   ----------------

   function Is_Handled
     (Exc  : Asis.Element;
      H    : Asis.Element)
      return Boolean
   is
      Choices     : constant Asis.Element_List := Exception_Choices (H);
      Handled_Exc : Asis.Element;
      Result      : Boolean := False;
   begin

      if Definition_Kind (Choices (Choices'First)) = An_Others_Choice then
         Result := not Contains_Raise_Stmt (H);
      else

         for J in Choices'Range loop
            Handled_Exc := Normalize_Reference (Choices (J));
            Handled_Exc := Corresponding_Name_Definition (Handled_Exc);
            Handled_Exc := Unwind_Exception_Renamings (Handled_Exc);

            if Is_Equal (Handled_Exc, Exc) then
               Result := not Contains_Raise_Stmt (H);
               exit;
            end if;
         end loop;

      end if;

      return Result;
   end Is_Handled;

   ----------------------------
   -- Is_Imported_Subprogram --
   ----------------------------

   function Is_Imported_Subprogram (El : Asis.Element) return Boolean is
      Argument : Asis.Element := El;
      Result   : Boolean      := False;
   begin

      if Declaration_Kind (Argument) in
         A_Procedure_Instantiation .. A_Function_Instantiation
      then
         Argument := Normalize_Reference (Generic_Unit_Name (Argument));
         Argument := Corresponding_Name_Declaration (Argument);
      end if;

      if Declaration_Kind (Argument) in
           A_Procedure_Declaration .. A_Function_Declaration
       or else
         Declaration_Kind (Argument) in
           A_Generic_Procedure_Declaration .. A_Generic_Function_Declaration
      then
         Argument := Corresponding_Body (Argument);

         case Pragma_Kind (Argument) is
            when An_Import_Pragma =>
               Result := True;
            when An_Implementation_Defined_Pragma =>
               Result :=
                 ASIS_UL.Misc.To_Lower_Case (Pragma_Name_Image (Argument)) =
                 "interface";
            when others =>
               null;
         end case;

      end if;

      return Result;
   end Is_Imported_Subprogram;

   ------------------------
   -- Is_In_Private_Part --
   ------------------------

   function Is_In_Private_Part
     (Decl  : Element;
      Scope : Element)
      return  Boolean
   is
      Result : Boolean := False;
   begin

      if Element_Kind (Decl) = A_Declaration
        and then
         (Declaration_Kind (Scope) = A_Package_Declaration
         or else
          Declaration_Kind (Scope) = A_Generic_Package_Declaration)
      then

         declare
            Private_Dls : constant Asis.Element_List :=
              Private_Part_Declarative_Items (Scope);

            First_Private_Line : Asis.Text.Line_Number_Positive;
            Last_Private_Line  : Asis.Text.Line_Number_Positive;
         begin

            if not Is_Nil (Private_Dls) then

               First_Private_Line :=
                 Element_Span (Private_Dls (Private_Dls'First)).First_Line;
               Last_Private_Line :=
                 Element_Span (Private_Dls (Private_Dls'Last)).Last_Line;

               Result := Element_Span (Decl).First_Line in
                            First_Private_Line .. Last_Private_Line;
            end if;

         end;

      end if;

      return Result;
   end Is_In_Private_Part;

   ------------------------
   -- Is_In_Visible_Part --
   ------------------------

   function Is_In_Visible_Part
     (Decl  : Element;
      Scope : Element)
      return  Boolean
   is
      Result : Boolean := False;
   begin

      if Element_Kind (Decl) = A_Declaration
        and then
         (Declaration_Kind (Scope) = A_Package_Declaration
         or else
          Declaration_Kind (Scope) = A_Generic_Package_Declaration)
      then

         declare
            Visible_Dls : constant Asis.Element_List :=
              Visible_Part_Declarative_Items (Scope);

            First_Public_Line : Asis.Text.Line_Number_Positive;
            Last_Public_Line  : Asis.Text.Line_Number_Positive;
         begin

            if not Is_Nil (Visible_Dls) then

               First_Public_Line :=
                 Element_Span (Visible_Dls (Visible_Dls'First)).First_Line;
               Last_Public_Line :=
                 Element_Span (Visible_Dls (Visible_Dls'Last)).Last_Line;

               Result := Element_Span (Decl).First_Line in
                           First_Public_Line .. Last_Public_Line;
            end if;

         end;

      end if;

      return Result;
   end Is_In_Visible_Part;

   ---------------------------
   -- Is_Indefinite_Subtype --
   ---------------------------

   function Is_Indefinite_Subtype (SM : Asis.Element) return Boolean is
      Result    : Boolean      := False;
      SM_Entity : Entity_Id;
   begin

      if Expression_Kind (SM) = A_Selected_Component
       or else
         Expression_Kind (SM) = An_Identifier
      then
         SM_Entity := Entity (R_Node  (SM));

         pragma Assert (Ekind (SM_Entity) in Einfo.Type_Kind);

         Result :=
           Has_Discriminants (SM_Entity) and then
             not Is_Constrained (SM_Entity);

      end if;

      return Result;
   end Is_Indefinite_Subtype;

   ---------------------
   -- Is_Modular_Type --
   ---------------------

   function Is_Modular_Type (Subtype_Ref : Asis.Element) return Boolean is
      Ent : Entity_Id;
   begin
      Ent := Entity (R_Node (Subtype_Ref));
      return Is_Modular_Integer_Type (Ent);
   end Is_Modular_Type;

   ---------------------------------
   -- Is_Non_Structural_Statement --
   ---------------------------------

   function Is_Non_Structural_Statement
     (Stmt         : Element;
      Exit_Is_Goto : Boolean := True)
      return         Boolean
   is
      Arg_Kind : constant Flat_Element_Kinds := Flat_Element_Kind (Stmt);

      Result  : Boolean          := False;
      Control : Traverse_Control := Continue;

      Target_Stmt : Element;

      procedure Pre_Operation
        (Element :        Asis.Element;
         Control : in out Traverse_Control;
         State   : in out Boolean);
      --  This procedure does most of the job. It checks if the element being
      --  visited does transfer the control outside Stmt. If this is really so
      --  it sets Result to True and terminates the traversal

      procedure Post_Operation
        (Element :        Asis.Element;
         Control : in out Traverse_Control;
         State   : in out Boolean);

      procedure Check_Statement is new
        Traverse_Element (Boolean, Pre_Operation, Post_Operation);

      procedure Pre_Operation
        (Element :        Asis.Element;
         Control : in out Traverse_Control;
         State   : in out Boolean)
      is
         Arg_Kind : constant Flat_Element_Kinds := Flat_Element_Kind (Element);
      begin

         case Arg_Kind is
            when Flat_Path_Kinds                    |
                 An_If_Statement                    |
                 A_Case_Statement                   |
                 A_Loop_Statement                   |
                 A_While_Loop_Statement             |
                 A_For_Loop_Statement               |
                 A_Block_Statement                  |
                 A_Selective_Accept_Statement       |
                 A_Timed_Entry_Call_Statement       |
                 A_Conditional_Entry_Call_Statement |
                 An_Asynchronous_Select_Statement   =>

               --  We may control transfer inside such a construct. So just
               --  continue...
               null;

            when A_Return_Statement                |
                 A_Raise_Statement                 |
                 A_Terminate_Alternative_Statement =>

               State   := True;
               Control := Terminate_Immediately;

            when An_Exit_Statement =>

               if Exit_Is_Goto then
                  Target_Stmt := Corresponding_Loop_Exited (Element);

                  if not Contains (Outer => Stmt, Inner => Target_Stmt) or else
                     Is_Equal (Stmt, Target_Stmt)
                  then
                     State   := True;
                     Control := Terminate_Immediately;
                  end if;

               end if;

            when A_Goto_Statement =>

               Target_Stmt := Corresponding_Destination_Statement (Element);

               if not Contains (Outer => Stmt, Inner => Target_Stmt) or else
                  Is_Equal (Stmt, Target_Stmt)
               then
                  State   := True;
                  Control := Terminate_Immediately;
               end if;

            when others =>
               --  Nothing interesting inside...
               Control := Abandon_Children;
         end case;

      end Pre_Operation;

      procedure Post_Operation
        (Element :        Asis.Element;
         Control : in out Traverse_Control;
         State   : in out Boolean)
      is
      begin
         pragma Unreferenced (Element);
         pragma Unreferenced (Control);
         pragma Unreferenced (State);
         null;
      end Post_Operation;

   begin

      if Arg_Kind = An_If_Statement                    or else
         Arg_Kind = A_Case_Statement                   or else
         Arg_Kind = A_Loop_Statement                   or else
         Arg_Kind = A_While_Loop_Statement             or else
         Arg_Kind = A_For_Loop_Statement               or else
         Arg_Kind = A_Selective_Accept_Statement       or else
         Arg_Kind = A_Timed_Entry_Call_Statement       or else
         Arg_Kind = A_Conditional_Entry_Call_Statement or else
         Arg_Kind = An_Asynchronous_Select_Statement
      then
         Check_Statement (Stmt, Control, Result);
      end if;

      return Result;
   end Is_Non_Structural_Statement;

   ---------------------
   -- Is_Program_Unit --
   ---------------------

   function Is_Program_Unit (El : Element) return Boolean is
      El_Kind : constant Flat_Element_Kinds := Flat_Element_Kind (El);
      Result  : Boolean                     := False;
   begin

      Result := False
        or else El_Kind = A_Task_Type_Declaration
        or else El_Kind = A_Protected_Type_Declaration
        or else El_Kind = A_Single_Task_Declaration
        or else El_Kind = A_Single_Protected_Declaration
        or else El_Kind = A_Package_Declaration
        or else El_Kind = A_Generic_Package_Declaration
        or else El_Kind = A_Package_Body_Declaration
        or else El_Kind = A_Procedure_Body_Declaration
        or else El_Kind = A_Function_Body_Declaration
        or else El_Kind = A_Task_Body_Declaration
        or else El_Kind = A_Protected_Body_Declaration
        or else El_Kind = An_Entry_Body_Declaration
        or else El_Kind = An_Expression_Function_Declaration;

      if not Result then

         Result := False
           or else El_Kind = A_Procedure_Declaration
           or else El_Kind = A_Function_Declaration
           or else El_Kind = A_Generic_Procedure_Declaration
           or else El_Kind = A_Generic_Function_Declaration
           or else El_Kind = A_Package_Instantiation
           or else El_Kind = A_Procedure_Instantiation
           or else El_Kind = A_Function_Instantiation
           or else El_Kind = A_Package_Renaming_Declaration
           or else El_Kind = A_Procedure_Renaming_Declaration
           or else El_Kind = A_Function_Renaming_Declaration
           or else El_Kind = A_Generic_Package_Renaming_Declaration
           or else El_Kind = A_Generic_Procedure_Renaming_Declaration
           or else El_Kind = A_Generic_Function_Renaming_Declaration;

         Result :=
           Result and then
           Is_Equal (El, Unit_Declaration (Enclosing_Compilation_Unit (El)));
      end if;

      return Result;

   end Is_Program_Unit;

   ------------------------
   -- Is_RM_Program_Unit --
   ------------------------

   function Is_RM_Program_Unit (El : Element) return Boolean is
      Result  : Boolean                     := False;
      El_Kind : constant Flat_Element_Kinds := Flat_Element_Kind (El);
   begin

      Result := False
        or else El_Kind = A_Task_Type_Declaration
        or else El_Kind = A_Protected_Type_Declaration
        or else El_Kind = A_Single_Task_Declaration
        or else El_Kind = A_Single_Protected_Declaration
        or else El_Kind = A_Procedure_Declaration
        or else El_Kind = A_Function_Declaration
        or else El_Kind = A_Procedure_Body_Declaration
        or else El_Kind = A_Function_Body_Declaration
        or else El_Kind = A_Package_Declaration
        or else El_Kind = A_Package_Body_Declaration
        or else El_Kind = A_Task_Body_Declaration
        or else El_Kind = A_Protected_Body_Declaration
        or else El_Kind = An_Entry_Body_Declaration
        or else El_Kind = A_Procedure_Body_Stub
        or else El_Kind = A_Function_Body_Stub
        or else El_Kind = A_Package_Body_Stub
        or else El_Kind = A_Task_Body_Stub
        or else El_Kind = A_Protected_Body_Stub
        or else El_Kind = A_Generic_Procedure_Declaration
        or else El_Kind = A_Generic_Function_Declaration
        or else El_Kind = A_Generic_Package_Declaration;

      if El_Kind = An_Entry_Declaration then
         Result :=
           Definition_Kind (Enclosing_Element (El)) = A_Protected_Definition;
      end if;

      return Result;

   end Is_RM_Program_Unit;

   ------------------------------
   -- Is_Publically_Accessible --
   ------------------------------

   function Is_Publically_Accessible (Decl : Element) return Boolean is
      Enclosing_CU : constant Asis.Compilation_Unit :=
        Enclosing_Compilation_Unit (Decl);
      Enclosing_Unit : Asis.Element;
      Local_Pkg      : Asis.Element;

      Result : Boolean := False;

   begin

      if Element_Kind (Decl) = A_Declaration
        and then
         (Unit_Kind (Enclosing_CU) = A_Package
          or else
          Unit_Kind (Enclosing_CU) = A_Generic_Package)
        and then
         Unit_Class (Enclosing_CU) = A_Public_Declaration
      then
         Enclosing_Unit := Unit_Declaration (Enclosing_CU);

         Result := Is_In_Visible_Part (Decl, Enclosing_Unit);

         if Result then
            --  Decl could be in the private part of some local package
            Local_Pkg := Enclosing_Element (Decl);

            while not Is_Equal (Local_Pkg, Enclosing_Unit) loop

               if Is_In_Private_Part (Decl, Local_Pkg) then
                  Result := False;
                  exit;
               end if;

               Local_Pkg := Enclosing_Element (Local_Pkg);
            end loop;

         end if;

      end if;

      return Result;
   end Is_Publically_Accessible;

   --------------------
   -- Is_Static_Loop --
   --------------------

   function Is_Static_Loop (Loop_Stmt : Element) return Boolean
   is
      Param_Definition : Element;

      Result : Boolean := False;
   begin

      if Flat_Element_Kind (Loop_Stmt) = A_For_Loop_Statement then

         Param_Definition :=
           Specification_Subtype_Definition
             (For_Loop_Parameter_Specification (Loop_Stmt));

         case Flat_Element_Kind (Param_Definition) is

            when A_Discrete_Subtype_Indication_As_Subtype_Definition =>

               Result := Is_Static_Subtype (Param_Definition);
               --  Is_Static_Subtype (Subtype_Constraint (Param_Definition));

            when A_Discrete_Range_Attribute_Reference_As_Subtype_Definition =>
               Result := Is_Static (Param_Definition);

            when A_Discrete_Simple_Expression_Range_As_Subtype_Definition =>

               Result := Is_Static (Lower_Bound (Param_Definition)) and then
                         Is_Static (Upper_Bound (Param_Definition));

            when others =>
               null;
         end case;

      end if;

      return Result;
   end Is_Static_Loop;

   -----------------------
   -- Is_Static_Subtype --
   -----------------------

   function Is_Static_Subtype (E : Element) return Boolean is
      Result   : Boolean                     := False;
      Arg_Kind : constant Flat_Element_Kinds := Flat_Element_Kind (E);

      Def_Name    : Element;
      Type_Entity : Entity_Id;
      Tmp         : Element;
   begin
      --  Note, that this NOT an ASIS secondary query, some routines from
      --  Einfo are used.

      --  First, return False for any non-expected or definitely non-static
      --  result

      if not (Arg_Kind = A_Subtype_Indication          or else
              Arg_Kind = A_Discrete_Subtype_Indication or else
              Arg_Kind = A_Discrete_Subtype_Indication_As_Subtype_Definition)
      then
         return False;
      end if;

      Tmp         := Asis.Definitions.Subtype_Mark (E);
      Tmp         := Normalize_Reference (Tmp);
      Def_Name    := Corresponding_Name_Definition (Tmp);
      Type_Entity := Node (Def_Name);

      if Is_Non_Static_Subtype (Type_Entity) or else
         Ekind (Type_Entity) not in Discrete_Kind
      then
         return False;
      end if;

      --  If we are here, we are sure that we are processing some discrete
      --  subtype indication

      Tmp := Subtype_Constraint (E);

      if not Is_Nil (Tmp) then

         if Flat_Element_Kind (Tmp) = A_Range_Attribute_Reference then
            Result := Is_Static (Tmp);
         else
            Result := Is_Static (Lower_Bound (Tmp)) and then
                      Is_Static (Upper_Bound (Tmp));
         end if;

         if not Result then
            --  The constraint is not static. No chance to be a static
            --  subtype...
            return False;
         end if;

      end if;

      --  If we are here, the constraint is either absent or static. So,
      --  checking the subtype mark

      Tmp := Type_Declaration_View (Enclosing_Element (Def_Name));

      if Flat_Element_Kind (Tmp) = A_Subtype_Indication then
         Result := Is_Static_Subtype (Tmp);
      else
         --  that is, here we have a type definition

         case Flat_Element_Kind (Tmp) is

            when A_Derived_Type_Definition =>
               Result := Is_Static_Subtype (Parent_Subtype_Indication (Tmp));

            when An_Enumeration_Type_Definition   |
                 A_Signed_Integer_Type_Definition |
                 A_Modular_Type_Definition =>
               Result := True;

            when others =>
               Result := False;
         end case;

      end if;

      return Result;
   end Is_Static_Subtype;

   --------------------------------------
   -- Is_Unchecked_Convertion_Instance --
   --------------------------------------

   function Is_Unchecked_Convertion_Instance (Decl : Element) return Boolean is
      Result   : Boolean := False;
      Arg      : Element;
      Arg_Unit : Compilation_Unit;
   begin
      if Declaration_Kind (Decl) = A_Function_Instantiation then
         Arg := Generic_Unit_Name (Decl);
         Arg := Normalize_Reference (Arg);
         Arg := Corresponding_Name_Declaration (Arg);

         if Declaration_Kind (Arg) =
           A_Generic_Function_Renaming_Declaration
         then
            Arg := Corresponding_Base_Entity (Arg);
            Arg := Corresponding_Name_Declaration (Arg);
         end if;

         Arg_Unit := Enclosing_Compilation_Unit (Arg);

         if Unit_Origin (Arg_Unit) = A_Predefined_Unit
           and then
            Unit_Full_Name (Arg_Unit) = "Ada.Unchecked_Conversion"
         then
            Result := True;
         end if;
      end if;

      return Result;
   end Is_Unchecked_Convertion_Instance;

   -----------------------------
   -- May_Have_Interface_List --
   -----------------------------

   function May_Have_Interface_List (Decl : Element) return Boolean is
      Result : Boolean := False;
   begin

      case Declaration_Kind (Decl) is
         when An_Ordinary_Type_Declaration =>
            Result :=
              Asis.Elements.Type_Kind (Type_Declaration_View (Decl)) =
                A_Derived_Record_Extension_Definition
            or else
              Asis.Elements.Type_Kind (Type_Declaration_View (Decl)) =
                An_Interface_Type_Definition;
         when A_Formal_Type_Declaration    =>
            Result :=
              Formal_Type_Kind (Type_Declaration_View (Decl)) =
                A_Formal_Derived_Type_Definition
            or else
              Formal_Type_Kind (Type_Declaration_View (Decl)) =
                A_Formal_Interface_Type_Definition;

         when A_Private_Extension_Declaration |
              A_Task_Type_Declaration         |
              A_Protected_Type_Declaration    |
              A_Single_Task_Declaration       |
              A_Single_Protected_Declaration  =>
            Result := True;
         when others =>
            null;
      end case;

      return Result;
   end May_Have_Interface_List;

   ------------------------
   -- Print_Tree_Sources --
   ------------------------

   procedure Print_Tree_Sources is
   begin
      for U in Main_Unit .. Last_Unit loop
         begin
            if not Is_Internal_File_Name
                     (File_Name (Source_Index (U)))
            then
               Namet.Get_Name_String (Full_File_Name (Source_Index (U)));
               Info (Namet.Name_Buffer (1 .. Namet.Name_Len));
            end if;

         --  In case of any non-expected problem:
         exception
            when others =>
               Info ("list may be incomplete");
               exit;
         end;
      end loop;

   end Print_Tree_Sources;

   -----------------------
   -- Primitive_Of_Type --
   -----------------------

   function Primitive_Of_Type (Op : Asis.Element) return Asis.Element is
      Result : Asis.Element;
   begin

      if Is_Part_Of_Inherited (Op) then
         Result := Enclosing_Element (Enclosing_Element (Op));
      else
         Result := Enclosing_Element (Primitive_Owner (Op));
      end if;

      return Result;
   end Primitive_Of_Type;

   --------------------------------
   -- Unwind_Exception_Renamings --
   --------------------------------

   function Unwind_Exception_Renamings
     (Exc  : Asis.Element)
      return Asis.Element
   is
      Result      : Asis.Element := Exc;
      Renamed_Exc : Asis.Element;
   begin
      Renamed_Exc := Enclosing_Element (Exc);

      if Declaration_Kind (Renamed_Exc) =
          An_Exception_Renaming_Declaration
      then
         Renamed_Exc := Renamed_Entity (Renamed_Exc);
         Renamed_Exc := Normalize_Reference (Renamed_Exc);
         Renamed_Exc := Corresponding_Name_Definition (Renamed_Exc);
         Result      := Unwind_Exception_Renamings (Renamed_Exc);
      end if;

      return Result;
   end Unwind_Exception_Renamings;

end ASIS_UL.Utilities;
