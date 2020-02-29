------------------------------------------------------------------------------
--                                                                          --
--                          GNATCHECK COMPONENTS                            --
--                                                                          --
--             G N A T C H E C K . A S I S _ U T I L I T I E S              --
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

with Ada.Characters.Conversions; use Ada.Characters.Conversions;
with Ada.Wide_Text_IO;           use Ada.Wide_Text_IO;

with Asis.Clauses;               use Asis.Clauses;
with Asis.Compilation_Units;     use Asis.Compilation_Units;
with Asis.Declarations;          use Asis.Declarations;
with Asis.Definitions;           use Asis.Definitions;
with Asis.Elements;              use Asis.Elements;
with Asis.Exceptions;
with Asis.Expressions;           use Asis.Expressions;
with Asis.Extensions;            use Asis.Extensions;
with Asis.Extensions.Flat_Kinds; use Asis.Extensions.Flat_Kinds;
with Asis.Iterator;              use Asis.Iterator;
with Asis.Statements;            use Asis.Statements;
with Asis.Text;                  use Asis.Text;

with ASIS_UL.Debug;
with ASIS_UL.Misc;               use ASIS_UL.Misc;
with ASIS_UL.Utilities;          use ASIS_UL.Utilities;

with GNAT.Table;

with Atree;                      use Atree;
with Einfo;                      use Einfo;
with Elists;                     use Elists;
with Namet;                      use Namet;
with Nlists;                     use Nlists;
with Sem_Aux;                    use Sem_Aux;
with Sinfo;                      use Sinfo;
with Snames;                     use Snames;
with Stand;                      use Stand;
with Types;                      use Types;

with Asis.Set_Get;               use Asis.Set_Get;

with A4G.A_Sem;                  use A4G.A_Sem;
with A4G.Int_Knds;               use A4G.Int_Knds;
with A4G.Vcheck;                 use A4G.Vcheck;

with Gnatcheck.Traversal_Stack;  use Gnatcheck.Traversal_Stack;

package body Gnatcheck.ASIS_Utilities is
   Package_Name : constant String := "Gnatcheck.ASIS_Utilities";

   -------------------------
   -- ASIS Elements Table --
   -------------------------

   --  Here we define the same structure as A4G.Asis_Tables.Asis_Element_Table.
   --  We need it to create the results of the functions returning
   --  Element_List, but we can not reuse A4G.Asis_Tables.Asis_Element_Table
   --  because it may be used by the standard ASIS queries we may need for our
   --  gnatcheck ASIS utilities.

   package Gnatcheck_Element_Table is new GNAT.Table (
     Table_Component_Type => Asis.Element,
     Table_Index_Type     => Natural,
     Table_Low_Bound      => 1,
     Table_Initial        => 100,
     Table_Increment      => 100,
     Table_Name           => "GNATCHECK Element List");

   -----------------------
   -- Local subprograms --
   -----------------------

   function Has_Predicate (Type_E : Entity_Id) return Boolean with
      Pre => Is_Type (Type_E);
   --  Checks if the argument denotes a subtype with dynamic predicate. Assumes
   --  the Type_E denotes a (sub)type.

   function Is_Constr_Error_Declaration (Decl : Asis.Element) return Boolean;
   function Is_Num_Error_Declaration (Decl : Asis.Element) return Boolean;
   --  Checks if the argument represents the declaration of the predefined
   --  exception Constraint_Error/Numeric_Error

   function Is_Task_Object_Declaration (Expr : Asis.Element) return Boolean;
   --  Check if the element if a declaration of (one or more) task object(s)
   --  Returns False for any unexpected object
   --
   --  Expected Declaration_Kinds:
   --       A_Variable_Declaration
   --       A_Constant_Declaration

   function Get_Called_Task (Call : Asis.Element) return Asis.Element;
   pragma Unreferenced (Get_Called_Task);
   --  Provided that Is_Task_Entry_Call (Call) computes the called
   --  task.
   --  What is "the called task" for different ways of defining a task
   --  object ???

   procedure Look_For_Loop_Pre_Op
     (Element :        Asis.Element;
      Control : in out Traverse_Control;
      State   : in out Boolean);
   --  Actual for Traverse_Element instantiation.
   --  Terminates the traversal and sets State ON when visiting a loop
   --  statement. Skips traversal of declarations, expressions and simple
   --  statements

   procedure Look_For_Modular_Component_Pre_Op
     (Element :        Asis.Element;
      Control : in out Traverse_Control;
      State   : in out Boolean);
   --  Actual for Traverse_Element instantiation.
   --  Terminates the traversal and sets State ON when visiting a component
   --  declaration that defines a component of a modular type

   procedure Empty_Bool_Post_Op
     (Element :        Asis.Element;
      Control : in out Traverse_Control;
      State   : in out Boolean);
   --  Actual for Traverse_Element instantiation.
   --  Does nothing.

   procedure Look_For_Loop is new Traverse_Element
     (State_Information => Boolean,
      Pre_Operation     => Look_For_Loop_Pre_Op,
      Post_Operation    => Empty_Bool_Post_Op);
   --  Looks for a loop statement enclosed by its Element argument and sets
   --  the result of the search to its State parameter. Declarations are not
   --  searched.

   procedure Look_For_Modular_Component is new Traverse_Element
     (State_Information => Boolean,
      Pre_Operation     => Look_For_Modular_Component_Pre_Op,
      Post_Operation    => Empty_Bool_Post_Op);
   --  Looks for a component declaration that defines a component of a modular
   --  type. If such a component declaration is foubd sets State ON, otherwise
   --  State is set OFF.

   procedure Check_For_Discr_Reference
     (Element :        Asis.Element;
      Control : in out Traverse_Control;
      State   : in out Boolean);
   --  If Element is An_Identifier, checks if it is reference to discriminant;
   --  and if it is - sets State ON and terminates traversing

   procedure Check_For_Discriminant_Reference is new Traverse_Element
     (State_Information => Boolean,
      Pre_Operation     => Check_For_Discr_Reference,
      Post_Operation    => Empty_Bool_Post_Op);
   --  Checks if Element has a reference to a discriminant

   function Is_Ancestor
     (Ancestor : Entity_Id;
      Source   : Entity_Id)
      return     Boolean;
   --  Assumes that both arguments are interface type entities. Check if
   --  Ancestor is indeed an ancestor of Source

   --  The folowing stuff is needed for No_Inherited_Classwide_Pre rule. The
   --  implementation of the rule is far from being good, so this definitely
   --  needs revision at some point. At least to eliminate code duplications.

   type List_Of_Nodes is array (Natural range <>) of Node_Id;
   function Get_Overridden_Ops (Op : Asis.Element) return List_Of_Nodes;
   function Get_Overridden_Ops (Op : Entity_Id) return List_Of_Nodes;
   --  Assuming that Op is a declaration of an overridding operation, gets a
   --  full list of operations (their entity nodes) that are overridden or
   --  implemented by this declaration.
   --
   --  The Get_Overridden_Ops function that works directly on operation entity
   --  node does NOT include in the result the operation pointed by the
   --  Overridden_Operation (Op) link

   function Primitive_Owner (Op : Entity_Id) return Entity_Id;
   --  Assuming that Op is a discpatching operation, returns the type for that
   --  this operation is defined. In case of private types/extensions a private
   --  view is returned.

   function Overridden_Interface_Ops
     (Type_Entity : Entity_Id;
      Op_Entity   : Entity_Id)
      return   List_Of_Nodes;
   --  Assuming that Type_Entity is a tagged type entity node, and Op_Entity
   --  is an entity node of a primitive of this type, returns the list of
   --  interface primitibes that are implemented by Op_Entity.

   function Has_Class_Wide_Pre (Op : Entity_Id) return Boolean;
   --  Checks if Op has an explicitly defined or inherited Pre'Class attribute
   --  specified for it.

   function Contract_Contains_Pre_Class (C : Node_Id) return Boolean;
   --  Assuming that C is of N_Contract kind checks if it defines class-wide
   --  Precondition

   function Add_Scope_Name (El : Asis.Element) return String;
   --  Assumes that El is an Element where
   --  Gnatcheck.Rules.Traversing.All_Rules_Pre_Op stands now (needed to apply
   --  fast Get_Enclosing_Element instead of standard Enclosing_Element query).
   --  returns the full Ada name of the inermost scope that encloses El.
   --  Returns an empty string if there is no such scope (library-level
   --  renaming or instantiation, compilation pragmas etc.)

   function Is_Named_Scope (E : Asis.Element) return Boolean;
   --  Checks if E is a named scope (in a sense needed to add the scope name
   --  in the diagnostic message if -dJ is specified).

   function Encl_Scope_Full_Name (El : Asis.Element) return String;
   --  Returns the full expanded Ada name of a scope that encloses El (not
   --  counting El itself if El is also a scope) with a dot character appended.
   --  Return an empty string if El is a library-level declaration

   function Overloading_Index (El : Asis.Element) return String;
   --  Assuming that Is_Named_Scope (El), and that El is a subprogram body,
   --  checks if there are other subprograms in the same declarative region
   --  that overloads this subprogram and precedes this one. If there are some,
   --  returns string of the forms "#n" where 'n' is a positional number of
   --  this subprogram in sequence of overloaded subprograms, otherwise an
   --  empty string is returned. Note, that when computing this chain and
   --  detecting this number, we do not consider subprogram body/stub/renaming
   --  declarations that are completions of other declarations, that is we are
   --  trying to follow the compiler's way of computing this suffix if -gnatdJ
   --  is set.

   --------------------
   -- Add_Scope_Name --
   --------------------

   function Add_Scope_Name (El : Asis.Element) return String is
      Step_Up         : Elmt_Idx     := 0;
      Enclosing_Scope : Asis.Element := Nil_Element;
      EE              : Asis.Element := El;
   begin
      while not Is_Nil (EE) loop
         if Is_Named_Scope (EE) then
            Enclosing_Scope := EE;
            exit;
         end if;

         EE      := Get_Enclosing_Element (Step_Up);
         Step_Up := Step_Up + 1;
      end loop;

      if Is_Nil (Enclosing_Scope) then
         return "";
      else
         return Encl_Scope_Full_Name (Enclosing_Scope)                 &
                To_String
                  (Defining_Name_Image (First_Name (Enclosing_Scope))) &
                Overloading_Index (Enclosing_Scope)                    &
                ":" & Build_GNAT_Location (Enclosing_Scope, 0, 0);
      end if;

   end Add_Scope_Name;

   ---------------------------
   -- Can_Cause_Side_Effect --
   ---------------------------

   function Can_Cause_Side_Effect (El : Asis.Element) return Boolean is
      Arg_Kind : constant Flat_Element_Kinds := Flat_Element_Kind (El);
      Result   :          Boolean := False;
   begin
      --  !!! Only partial implementation for now!!!

      case Arg_Kind is
         when An_Assignment_Statement    |
              A_Procedure_Call_Statement |
              A_Function_Call            =>
            --  What about entry calls???
            Result := True;
--         when =>
         when others =>
            null;
      end case;

      return Result;
   end Can_Cause_Side_Effect;

   ----------------------------------------------
   -- Call_To_Complicated_Cuncurrent_Structure --
   ----------------------------------------------

   function Call_To_Complicated_Cuncurrent_Structure
     (Call : Asis.Element)
      return Boolean
   is
      Arg_Kind    : constant Flat_Element_Kinds := Flat_Element_Kind (Call);
      Result      : Boolean                     := True;
      Called_Pref : Asis.Element                := Nil_Element;
      Called_Obj  : Asis.Element                := Nil_Element;
      Tmp_El      : Asis.Element;
   begin

      case Arg_Kind is
         when An_Entry_Call_Statement    |
             A_Procedure_Call_Statement =>
            Called_Pref := Called_Name (Call);

            if Arg_Kind = An_Entry_Call_Statement
             and then
               Flat_Element_Kind (Called_Pref) = An_Indexed_Component
            then
               --  Call to an entry from an entry family
               Called_Pref := Prefix (Called_Pref);
            end if;

         when A_Function_Call =>
            Called_Pref := Prefix (Call);
         when others =>
            null;
      end case;

      --  Called_Pref should be of A_Selected_Component kind. We are interested
      --  in task or protected object now

      if Flat_Element_Kind (Called_Pref) = A_Selected_Component then
         Called_Pref := Prefix (Called_Pref);

         if Flat_Element_Kind (Called_Pref) = A_Selected_Component then
            Called_Pref := Selector (Called_Pref);
         end if;

      end if;

      if Expression_Kind (Called_Pref) = An_Identifier then

         begin
            Called_Obj := Corresponding_Name_Definition (Called_Pref);
         exception
            when others =>
               Called_Obj := Nil_Element;
         end;

      end if;

      if not Is_Nil (Called_Obj) then
         Tmp_El := Enclosing_Element (Called_Obj);

         case Declaration_Kind (Tmp_El) is
            when A_Single_Task_Declaration .. A_Single_Protected_Declaration =>
               Result := False;

            when A_Variable_Declaration | A_Constant_Declaration =>
               Tmp_El := Object_Declaration_View (Tmp_El);

               Tmp_El := Asis.Definitions.Subtype_Mark (Tmp_El);

               if Expression_Kind (Tmp_El) = A_Selected_Component then
                  Tmp_El := Selector (Tmp_El);
               end if;

               Tmp_El := Corresponding_Name_Declaration (Tmp_El);

               --  Now we check that the type of the object is a task or
               --  protected type

               Tmp_El := Corresponding_First_Subtype (Tmp_El);

               --  We can n0t have a private type here.

               if Declaration_Kind (Tmp_El) in
                 A_Task_Type_Declaration .. A_Protected_Type_Declaration
               then
                  Result := False;
               else
                  Tmp_El := Type_Declaration_View (Tmp_El);

                  if Asis.Elements.Type_Kind (Tmp_El) =
                    A_Derived_Type_Definition
                  then
                     Tmp_El := Corresponding_Root_Type (Tmp_El);

                     if Declaration_Kind (Tmp_El) in
                       A_Task_Type_Declaration .. A_Protected_Type_Declaration
                     then
                        Result := False;
                     end if;

                  end if;
               end if;

            when others =>
               null;
         end case;

      end if;

      return Result;
   end Call_To_Complicated_Cuncurrent_Structure;

   -----------------------------------
   -- Can_Be_Replaced_With_Function --
   -----------------------------------

   function Can_Be_Replaced_With_Function
     (Decl : Asis.Element)
      return Boolean
   is
      Out_Par : Asis.Element := Nil_Element;
      Result  : Boolean := False;
   begin

      case Declaration_Kind (Decl) is
         when A_Procedure_Declaration         |
              A_Procedure_Body_Declaration    |
              A_Procedure_Body_Stub           |
              A_Generic_Procedure_Declaration |
              A_Formal_Procedure_Declaration  =>

            declare
               Params : constant Asis.Element_List := Parameter_Profile (Decl);
            begin

               for J in Params'Range loop

                  case Mode_Kind (Params (J)) is
                     when An_Out_Mode =>

                        if Names (Params (J))'Length > 1 then
                           Result := False;
                           exit;
                        end if;

                        if Is_Nil (Out_Par) then
                           Out_Par := Object_Declaration_View (Params (J));

                           if Definition_Kind (Out_Par) =
                                 An_Access_Definition
                           then
                              Result := True;
                           else
                              --  If we are here, Out_Par represents a subtype
                              --  mark
                              Result := not Is_Limited (Out_Par);

                              exit when not Result;

                           end if;

                        else
                           Result := False;
                           exit;
                        end if;

                     when An_In_Out_Mode =>
                        Result := False;
                        exit;
                     when others =>
                        null;
                  end case;

               end loop;

            end;

         when others =>
            null;
      end case;

      return Result;
   end Can_Be_Replaced_With_Function;

   ---------------------
   -- Changed_Element --
   ---------------------

   function Changed_Element (El : Asis.Element) return Asis.Element is
      Arg_Elem :          Asis.Element       := El;
      Arg_Kind : constant Flat_Element_Kinds := Flat_Element_Kind (El);
      Result   :          Asis.Element       := Nil_Element;
   begin

      --  Problem with access types!!!???

      case Arg_Kind is
         when An_Identifier =>
            --  Nothing to do:
            null;
         when A_Selected_Component =>
            Arg_Elem := Get_Whole_Object (Arg_Elem);

         when An_Indexed_Component    |
              A_Slice                 |
              An_Explicit_Dereference =>

            while not (Expression_Kind (Arg_Elem) = A_Selected_Component
                   or else
                       Expression_Kind (Arg_Elem) = An_Identifier)
            loop
               Arg_Elem := Prefix (Arg_Elem);
            end loop;

            if Expression_Kind (Arg_Elem) = A_Selected_Component then
               Arg_Elem := Get_Whole_Object (Arg_Elem);
            end if;

         when A_Type_Conversion =>
            return Changed_Element (Converted_Or_Qualified_Expression (El));

--         when  =>
         when others =>
            pragma Assert (False);
            null;
      end case;

      if Expression_Kind (Arg_Elem) = An_Identifier then
         Result := Corresponding_Name_Definition (Arg_Elem);
      else
         Result := Arg_Elem;
      end if;

      return Result;
   end Changed_Element;

   -----------------------------------
   -- Check_Classwide_Pre_Vioaltion --
   -----------------------------------

   procedure Check_Classwide_Pre_Vioaltion
     (Op       : Asis.Element;
      Detected : out Boolean;
      At_SLOC  : out String_Loc)
   is
      Overridden_Ops : constant List_Of_Nodes := Get_Overridden_Ops (Op);
      Template_El    : Asis.Element := Nil_Element;
   begin
      Detected := False;
      At_SLOC := Nil_String_Loc;

      for J in Overridden_Ops'Range loop
         if not Has_Class_Wide_Pre (Overridden_Ops (J)) then
            Detected := True;
            Set_Encl_Unit_Id (Template_El, Encl_Unit_Id (Op));
            Set_Node (Template_El, Overridden_Ops (J));
            At_SLOC :=
              Enter_String ("%1%" & Build_GNAT_Location (Template_El));
            exit;
         end if;
      end loop;

   end Check_Classwide_Pre_Vioaltion;

   -------------------------------
   -- Check_For_Discr_Reference --
   -------------------------------

   procedure Check_For_Discr_Reference
     (Element :        Asis.Element;
      Control : in out Traverse_Control;
      State   : in out Boolean)
   is
   begin

      case Expression_Kind (Element) is
         when An_Identifier =>

            begin
               if Declaration_Kind (Corresponding_Name_Declaration (Element)) =
                    A_Discriminant_Specification
               then
                  State   := True;
                  Control := Terminate_Immediately;
               end if;
            exception
               when Asis.Exceptions.ASIS_Inappropriate_Element =>
                  null;
            end;

         when Not_An_Expression =>
            null;
         when others =>
            Control := Abandon_Children;
      end case;

   end Check_For_Discr_Reference;

   ----------------------------------------
   -- Constraint_Depends_On_Discriminant --
   ----------------------------------------

   function Constraint_Depends_On_Discriminant
     (Constr : Asis.Element)
      return   Boolean
   is
      Control : Traverse_Control := Continue;
      Result  : Boolean          := False;
   begin

      if Constraint_Kind (Constr) in
           An_Index_Constraint | A_Discriminant_Constraint
      then
         Check_For_Discriminant_Reference
           (Element => Constr, Control => Control, State => Result);
      end if;

      return Result;
   end Constraint_Depends_On_Discriminant;

   -------------------
   -- Contains_Loop --
   -------------------

   function Contains_Loop (El : Asis.Element) return Boolean is
      Control : Traverse_Control := Continue;
      Result  : Boolean          := False;

      Comps : constant Asis.Element_List := Components (El);
   begin

      --  We can not just apply Look_For_Loop tp El - if El itself is a loop
      --  statement, then Result will alvays be True:
      for J in Comps'Range loop
         Look_For_Loop (Comps (J), Control, Result);
         exit when Result;
      end loop;

      return Result;

   end Contains_Loop;

   --------------------------------
   -- Contains_Modular_Component --
   --------------------------------

   function Contains_Modular_Component
     (Type_Decl : Asis.Element)
      return      Boolean
   is
      Result  : Boolean := False;
      Tmp     : Asis.Element;
      Control : Traverse_Control := Continue;
   begin
      if Declaration_Kind (Type_Decl) /= An_Ordinary_Type_Declaration then
         return False;
      end if;

      Tmp := Discriminant_Part (Type_Decl);

      if Definition_Kind (Tmp) = A_Known_Discriminant_Part then
         declare
            Discrs : constant Asis.Element_List := Discriminants (Tmp);
         begin
            for J in Discrs'Range loop
               Tmp := Object_Declaration_View (Discrs (J));

               if Flat_Element_Kind (Tmp) not in Flat_Access_Definition_Kinds
                 and then
                  Is_Modular_Type (Tmp)
               then
                  Result := True;
                  exit;
               end if;
            end loop;
         end;
      end if;

      if not Result then
         Tmp := Type_Declaration_View (Type_Decl);

         Look_For_Modular_Component
           (Element => Tmp,
           Control  => Control,
           State    => Result);
      end if;

      return Result;
   end Contains_Modular_Component;

   ---------------------------------
   -- Contract_Contains_Pre_Class --
   ---------------------------------

   function Contract_Contains_Pre_Class (C : Node_Id) return Boolean is
      Result   : Boolean := False;
      N_Pragma : Node_Id;
   begin
      pragma Assert (Nkind (C) = N_Contract);

      N_Pragma := Pre_Post_Conditions (C);

      while Present (N_Pragma) loop

         if Chars (Pragma_Identifier (N_Pragma)) = Name_Precondition
          and then
            Class_Present (N_Pragma)
         then
            Result := True;
            exit;
         end if;

         N_Pragma := Next_Pragma (N_Pragma);
      end loop;

      return Result;
   end Contract_Contains_Pre_Class;

   ------------------------------------
   -- Corresponding_Protected_Object --
   ------------------------------------

   function Corresponding_Protected_Object
     (Pref : Asis.Element)
      return Asis.Element
   is
      Tmp    : Asis.Element := Pref;
      Result : Asis.Element := Nil_Element;
   begin

      if Expression_Kind (Tmp) = A_Function_Call then
         Tmp := Prefix (Tmp);
      else
         Tmp := Called_Name (Tmp);
      end if;

      --  At the moment the simplest case only is implemented: we can process
      --  only the argument Element of the form P_Obj_Name.P_Op_Name

      if Expression_Kind (Tmp) = A_Selected_Component then
         Tmp := Prefix (Tmp);

         if Expression_Kind (Tmp) = A_Selected_Component then
            Tmp := Selector (Tmp);
         end if;

         pragma Assert (Expression_Kind (Tmp) = An_Identifier);

         Result := Corresponding_Name_Definition (Tmp);

         if Declaration_Kind (Enclosing_Element (Result)) =
            A_Single_Protected_Declaration
         then
            Result := Enclosing_Element (Result);
         end if;

      end if;

      pragma Assert (not Is_Nil (Result));

      return Result;

   end Corresponding_Protected_Object;

   -----------------------------------
   -- Declaration_Of_Renamed_Entity --
   -----------------------------------

   function Declaration_Of_Renamed_Entity
     (R    : Asis.Element)
      return Asis.Element
   is
      Arg_Element : Asis.Element := Renamed_Entity (R);
      Result      : Asis.Element := Nil_Element;
   begin

      if Expression_Kind (Arg_Element) = A_Selected_Component then
         Arg_Element := Selector (Arg_Element);
      end if;

      case Expression_Kind (Arg_Element) is
         when An_Identifier          |
              An_Operator_Symbol     |
              A_Character_Literal    |
              An_Enumeration_Literal =>
            Result := Corresponding_Name_Declaration (Arg_Element);
         when others =>
            null;
      end case;

      return Result;
   exception
      when others =>
         return Nil_Element;
   end Declaration_Of_Renamed_Entity;

   ------------------------
   -- Defines_Components --
   ------------------------

   function Defines_Components (Decl : Asis.Element) return Boolean is
      Type_Def : Asis.Element;
      Result   : Boolean := False;
   begin

      if Declaration_Kind (Decl) = An_Ordinary_Type_Declaration then

         Type_Def := Type_Declaration_View (Decl);

         case Asis.Elements.Type_Kind (Type_Def) is
            when A_Derived_Record_Extension_Definition |
                 A_Record_Type_Definition              |
                 A_Tagged_Record_Type_Definition       =>
               Result := True;
            when others =>
               null;
         end case;

      end if;

      return Result;

   end Defines_Components;

   ----------------------------
   -- Denotes_Access_Subtype --
   ----------------------------

   function Denotes_Access_Subtype (N : Asis.Element) return Boolean is
   begin
      return Ekind (Node (N)) in Access_Kind;
   end Denotes_Access_Subtype;

   --------------------------------
   -- Denotes_Class_Wide_Subtype --
   --------------------------------

   function Denotes_Class_Wide_Subtype (N : Asis.Element) return Boolean is
      E      : Entity_Id;
      Result : Boolean := False;
   begin

      E := R_Node (N);

      if Nkind (E) in  N_Expanded_Name | N_Identifier then
         E := Entity (E);

         if Present (E) then
            Result := Ekind (E) = E_Class_Wide_Subtype;
         end if;
      end if;

      return Result;
   end Denotes_Class_Wide_Subtype;

   ------------------------------------
   -- Denotes_Subtype_With_Predicate --
   ------------------------------------

   function Denotes_Subtype_With_Predicate
     (E    : Asis.Element)
      return Boolean
   is
      Result : Boolean := False;
      N      : Node_Id;
      E_Id   : Entity_Id;
   begin
      if Expression_Kind (E) not in An_Identifier | A_Selected_Component then
         return False;
      end if;

      N := R_Node (E);

      if Nkind (N) in N_Has_Entity then
         E_Id := Entity (N);

         Result := Present (E_Id)
                 and then
                   Is_Type (E_Id)
                 and then
                   Has_Predicate (E_Id);
      end if;

      return Result;
   end Denotes_Subtype_With_Predicate;

   ---------------------------
   -- Empty_Bool_Post_Op --
   ---------------------------

   procedure Empty_Bool_Post_Op
     (Element :        Asis.Element;
      Control : in out Traverse_Control;
      State   : in out Boolean)
   is
      pragma Unreferenced (Element, Control, State);
   begin
      null;
   end Empty_Bool_Post_Op;

   --------------------------
   -- Encl_Scope_Full_Name --
   --------------------------

   function Encl_Scope_Full_Name (El : Asis.Element) return String is
      Encl_Scope : Asis.Element := Enclosing_Element (El);
   begin
      while not Is_Nil (Encl_Scope)
          and then
            not Is_Named_Scope (Encl_Scope)
      loop
         Encl_Scope := Enclosing_Element (Encl_Scope);
      end loop;

      if Is_Nil (Encl_Scope) then
         return "";
      else
         return Encl_Scope_Full_Name (Encl_Scope)                         &
                To_String (Defining_Name_Image (First_Name (Encl_Scope))) &
                '.';
      end if;

   end Encl_Scope_Full_Name;

   --------------------
   -- Enclosing_List --
   --------------------

   function Enclosing_List return Asis.Element_List is
      EE : constant Asis.Element := Get_Enclosing_Element;
   begin

      if Is_Nil (EE) then
         return Nil_Element_List;
      else
         return Components (EE);
      end if;

   end Enclosing_List;

   --------------------------
   -- Entity_From_Rep_Item --
   --------------------------

   function Entity_From_Rep_Item
     (Rep_Item : Asis.Element)
      return     Asis.Element
   is
      Ent_Name : Asis.Element;
   begin
      if not Is_Representation_Item (Rep_Item) then
         Raise_ASIS_Inappropriate_Element
           (Diagnosis =>
              "Gnatcheck.ASIS_Utilities.Is_Representation_Item",
            Wrong_Kind => Int_Kind (Rep_Item));
      end if;

      if Clause_Kind (Rep_Item) = A_Representation_Clause then
         Ent_Name := Representation_Clause_Name (Rep_Item);
      else
         declare
            Params : constant Asis.Element_List :=
              Pragma_Argument_Associations (Rep_Item);
         begin
            Ent_Name := Params (Params'First);
            Ent_Name := Actual_Parameter (Ent_Name);
         end;

      end if;

      Ent_Name := Normalize_Reference (Ent_Name);

      return Corresponding_Name_Declaration (Ent_Name);
   end Entity_From_Rep_Item;

   ---------------------------------
   -- From_Subtype_With_Predicate --
   ---------------------------------

   function From_Subtype_With_Predicate
     (E    : Asis.Element)
      return Boolean
   is
      Result : Boolean := False;
      N      : Node_Id;
      E_Id   : Entity_Id;
   begin
      N := R_Node (E);

      if Nkind (N) in N_Has_Etype then
         E_Id := Etype (N);

         Result := Present (E_Id)
                 and then
                   Is_Type (E_Id)
                 and then
                   Has_Predicate (E_Id);
      end if;

      return Result;
   end From_Subtype_With_Predicate;

   -----------------------
   -- Full_View_Visible --
   -----------------------

   function Full_View_Visible
     (Type_Decl : Asis.Declaration;
      At_Place  : Asis.Element)
      return      Boolean
   is
      Result              : Boolean := False;
      Full_View           : Asis.Declaration;
      Enclosing_Pack_Spec : Asis.Declaration;
      Enclosing_Pack_Body : Asis.Declaration;

      Type_Spec_CU     : Asis.Compilation_Unit;
      Type_Body_CU     : Asis.Compilation_Unit := Nil_Compilation_Unit;
      Location_CU      : Asis.Compilation_Unit;
      Next_Parent      : Asis.Compilation_Unit;

      Stub_El          : Asis.Element;
   begin
      --  First, check if we have expected elements and return False if we
      --  do not.

      if Declaration_Kind (Type_Decl) not in
           A_Private_Type_Declaration .. A_Private_Extension_Declaration
        or else
         Is_Part_Of_Implicit (Type_Decl)
        or else
         Is_Part_Of_Implicit (At_Place)
        or else
         Is_Part_Of_Instance (Type_Decl)
        or else
         Is_Part_Of_Instance (At_Place)
      then
         return False;
      end if;

      Full_View           := Corresponding_Type_Declaration (Type_Decl);
      Enclosing_Pack_Spec := Enclosing_Element (Type_Decl);
      Enclosing_Pack_Body := Corresponding_Body (Enclosing_Pack_Spec);

      if Declaration_Kind (Enclosing_Pack_Body) = A_Package_Body_Stub then
         Enclosing_Pack_Body := Corresponding_Subunit (Enclosing_Pack_Body);
      end if;

      Type_Spec_CU := Enclosing_Compilation_Unit (Enclosing_Pack_Spec);
      Location_CU  := Enclosing_Compilation_Unit (At_Place);

      if not Is_Nil (Enclosing_Pack_Body) then
         Type_Body_CU := Enclosing_Compilation_Unit (Enclosing_Pack_Body);
      end if;

      --  Type declaration and location to check are in the same CU:

      if Is_Equal (Type_Spec_CU, Location_CU) then
         if In_Private_Part (Enclosing_Pack_Spec, At_Place) then
            Result := Before (Full_View, At_Place);
         elsif Is_Equal (Type_Body_CU, Location_CU) then
            Result :=
              Inclides (Whole => Enclosing_Pack_Body, Part => At_Place);
         end if;

         return Result;
      end if;

      --  If we are here, then type declaration and location to check are
      --  in different compilation units. First, check if location is in
      --  the body of the package that defines the type. (Subunits are a
      --  pain in this case)

      if not Is_Nil (Type_Body_CU) then

         if not Is_Equal (Type_Body_CU, Location_CU) then

            if Unit_Kind (Location_CU) in A_Subunit then
               Stub_El := Unit_Declaration (Location_CU);
               Stub_El := Corresponding_Body_Stub (Stub_El);
            end if;

            while Unit_Kind (Location_CU) in A_Subunit loop
               exit when Is_Equal (Type_Body_CU, Location_CU);

               Stub_El     := Unit_Declaration (Location_CU);
               Stub_El     := Corresponding_Body_Stub (Stub_El);
               Location_CU := Corresponding_Subunit_Parent_Body (Location_CU);

            end loop;

         else
            Stub_El := At_Place;
         end if;

         if Is_Equal (Type_Body_CU, Location_CU) then
            Result := Inclides (Whole => Enclosing_Pack_Body, Part => Stub_El);
            return Result;
         end if;

      end if;

      --  If we are here, the only possibility when the full view is visible
      --  at a given place is:
      --
      --  - Type_Decl is declared in a visible part of a library package
      --
      --  - At_Place is either in the child unit of this package - either in
      --    the body, or in the private part of the public child, or in the
      --    spec of a private child.

      if (Unit_Kind (Type_Spec_CU) = A_Package
         or else
          Unit_Kind (Type_Spec_CU) = A_Generic_Package)
        and then
          Is_Equal (Enclosing_Element (Type_Decl),
                    Unit_Declaration (Type_Spec_CU))
      then

         while Unit_Kind (Location_CU) in A_Subunit loop
            Location_CU := Corresponding_Subunit_Parent_Body (Location_CU);
         end loop;

         Next_Parent := Location_CU;

         while not Is_Nil (Next_Parent) loop
            exit when Is_Equal (Next_Parent, Type_Spec_CU);
            Next_Parent := Corresponding_Parent_Declaration (Next_Parent);
         end loop;

         if not Is_Equal (Next_Parent, Type_Spec_CU) then
            return False;
         elsif Unit_Kind (Location_CU) in A_Library_Unit_Body then
            return True;
         elsif Unit_Kind (Location_CU) in
                 A_Procedure          |
                 A_Function           |
                 A_Generic_Procedure  |
                 A_Generic_Function   |
                 A_Procedure_Instance |
                 A_Function_Instance  |
                 A_Package_Instance   |
                 A_Procedure_Renaming |
                 A_Function_Renaming
         then
            return False;

         elsif Unit_Kind (Location_CU) = A_Package
             or else
                Unit_Kind (Location_CU) = A_Generic_Package
         then
            if Unit_Class (Location_CU) = A_Private_Declaration
              and then
               Is_Equal (Corresponding_Parent_Declaration (Location_CU),
                         Type_Spec_CU)
            then
               return True;
            else
               Result :=
                 In_Private_Part (Pack    => Unit_Declaration (Location_CU),
                                  Element => At_Place);
               return Result;
            end if;
         end if;

         pragma Assert (False);
         return False;
      end if;

      return False;
   end Full_View_Visible;

   ----------------------
   -- Get_Associations --
   ----------------------

   function Get_Associations (El : Asis.Element) return Asis.Element_List is
   begin

      case Flat_Element_Kind (El) is
         when A_Record_Aggregate     |
              An_Extension_Aggregate =>
            return Record_Component_Associations (El);
         when A_Positional_Array_Aggregate |
              A_Named_Array_Aggregate      =>
            return Array_Component_Associations (El);
--         when  =>
--            return  (El);
         when others =>
            return Nil_Element_List;
      end case;

   end Get_Associations;

   ----------------------
   -- Get_Call_Element --
   ----------------------

   function Get_Call_Element return Asis.Element is
      Steps_Up     : Elmt_Idx := 0;
      Result       : Asis.Element := Get_Enclosing_Element (Steps_Up);
   begin
      loop
         exit when
            Expression_Kind (Result) = A_Function_Call
           or else
            Element_Kind (Result) /= An_Expression;

         Steps_Up := Steps_Up + 1;
         Result   := Get_Enclosing_Element (Steps_Up);
      end loop;

      return Result;
   end Get_Call_Element;

   ---------------------
   -- Get_Called_Task --
   ---------------------

   function Get_Called_Task (Call : Asis.Element) return Asis.Element is
      Result : Asis.Element := Nil_Element;
      Tmp    : Asis.Element;
      Tmp1   : Asis.Element;
   begin
      --  For now - the simplest case. We consider that the prefix has
      --  the form of Task_Name.Entry_Name

      Tmp := Called_Name (Call);

      if Expression_Kind (Tmp) = An_Indexed_Component then
         --  A call to an entry from an entry family
         Tmp := Prefix (Tmp);
      end if;

      if Expression_Kind (Tmp) = A_Selected_Component then
         Tmp := Prefix (Tmp);

         if Expression_Kind (Tmp) = A_Selected_Component then
            Tmp := Asis.Expressions.Selector (Tmp);
         end if;

         Tmp := Corresponding_Name_Definition (Tmp);

         if not Is_Nil (Tmp) then
            --  For a task declared by a single task declaration we return this
            --  single task declaration, otherwise we return a task defining
            --  identifier
            Tmp1 := Enclosing_Element (Tmp);

            if Declaration_Kind (Tmp1) = A_Single_Task_Declaration then
               Tmp := Tmp1;
            end if;

            Result := Tmp;
         end if;

      end if;

      pragma Assert (not Is_Nil (Result));
      --  A null result requires a special processing, so for the development
      --  period we just blow up

      return Result;
   end Get_Called_Task;

   -----------------
   -- Get_Choices --
   -----------------

   function Get_Choices (El : Asis.Element) return Asis.Element_List is
   begin

      case Association_Kind (El) is
         when An_Array_Component_Association =>
            return Array_Component_Choices (El);
         when A_Record_Component_Association =>
            return Record_Component_Choices (El);
         when others =>
            return Nil_Element_List;
      end case;

   end Get_Choices;

   ----------------------------------
   -- Get_Corresponding_Definition --
   ----------------------------------

   function Get_Corresponding_Definition
     (El   : Asis.Element)
      return Asis.Element
   is
      Arg_Kind : constant Expression_Kinds := Expression_Kind (El);
      Result   : Asis.Element;
   begin

      if not (Arg_Kind = An_Identifier
             or else
              Arg_Kind = An_Operator_Symbol
             or else
              Arg_Kind = A_Character_Literal
             or else
              Arg_Kind = An_Enumeration_Literal)
      then
         --  To avoid junk use of this query
         Raise_ASIS_Inappropriate_Element
           (Diagnosis =>
              "Gnatcheck.ASIS_Utilities.Get_Corresponding_Definition",
            Wrong_Kind => Int_Kind (El));
      end if;

      begin
         Result := Corresponding_Name_Definition (El);
      exception
         when Asis.Exceptions.ASIS_Inappropriate_Element =>
            Result := Nil_Element;
      end;

      return Result;
   end Get_Corresponding_Definition;

   -----------------------------
   -- Get_Encl_Protected_Body --
   -----------------------------

   function Get_Encl_Protected_Body return Asis.Element is
      Result  : Asis.Element := Nil_Element;
      Step_Up : Elmt_Idx     := 0;
      Tmp     : Asis.Element := Get_Enclosing_Element (Step_Up);
   begin

      while not Is_Nil (Tmp) loop

         if Declaration_Kind (Tmp) = A_Protected_Body_Declaration then
            Result := Tmp;
            exit;
         end if;

         Step_Up := Step_Up + 1;
         Tmp := Get_Enclosing_Element (Step_Up);

      end loop;

      return Result;
   end Get_Encl_Protected_Body;

   ------------------
   -- Get_Handlers --
   ------------------

   function Get_Handlers
     (El              : Asis.Element;
      Include_Pragmas : Boolean := False)
      return            Asis.Element_List
   is
   begin

      case Flat_Element_Kind (El) is
         when A_Procedure_Body_Declaration |
              A_Function_Body_Declaration  |
              A_Package_Body_Declaration   |
              An_Entry_Body_Declaration    |
              A_Task_Body_Declaration      =>
            return Body_Exception_Handlers (El, Include_Pragmas);

         when A_Block_Statement =>
            return Block_Exception_Handlers (El, Include_Pragmas);

         when An_Extended_Return_Statement =>
            return Extended_Return_Exception_Handlers (El, Include_Pragmas);

         when An_Accept_Statement =>
            return Accept_Body_Exception_Handlers (El, Include_Pragmas);

         when others =>
            return Nil_Element_List;
      end case;

   end Get_Handlers;

   -------------------------
   -- Get_Name_Definition --
   -------------------------

   function Get_Name_Definition (Ref : Asis.Element) return Asis.Element is
      Result : Asis.Element := Normalize_Reference (Ref);
   begin

      Result := Corresponding_Name_Definition (Result);

      if Declaration_Kind (Enclosing_Element (Result)) in
           A_Renaming_Declaration
      then
         Result := Corresponding_Base_Entity (Enclosing_Element (Result));
         Result := Normalize_Reference (Result);
         Result := Corresponding_Name_Definition (Result);
      end if;

      return Result;
   end Get_Name_Definition;

   -----------------
   -- Get_Obj_Dcl --
   -----------------

   function Get_Obj_Dcl (El : Asis.Element) return Asis.Element is
      Result : Asis.Element := Nil_Element;
   begin
      case Flat_Element_Kind (El) is
         when A_Function_Call =>
            null;
         when An_Identifier =>
            Result := Corresponding_Name_Declaration (El);
         when An_Explicit_Dereference |
              An_Indexed_Component    |
              A_Slice                 =>
            Result := Get_Obj_Dcl (Prefix (El));
         when A_Selected_Component =>
            --  The hard case: A.B may be the reference to the variable B
            --  declared in package A, or it may be the reference to the
            --  component B of a record object A

            Result := Corresponding_Name_Declaration (Selector (El));

            if Declaration_Kind (Result) = A_Component_Declaration then
               Result := Get_Obj_Dcl (Prefix (El));
            end if;

         when others =>
            pragma Assert (False);
      end case;

      if Declaration_Kind (Result) = An_Object_Renaming_Declaration then
         Result := Get_Obj_Dcl (Renamed_Entity (Result));
      end if;

      return Result;
   end Get_Obj_Dcl;

   ------------------------
   -- Get_Overridden_Ops --
   ------------------------

   function Get_Overridden_Ops (Op : Entity_Id) return List_Of_Nodes is
      Type_Entity : Entity_Id := Primitive_Owner (Op);
   begin
      if Ekind (Type_Entity) in E_Private_Type         |
                                E_Limited_Private_Type
      then
         Type_Entity := Full_View (Type_Entity);
      end if;

      return Overridden_Interface_Ops (Type_Entity, Op);
   end Get_Overridden_Ops;

   function Get_Overridden_Ops (Op : Asis.Element) return List_Of_Nodes is
      Result : List_Of_Nodes (1 .. 6000);
      --  6000 looks as infinity here
      Res_Last : Natural := 0;

      Op_Entity           : constant Entity_Id := R_Node (First_Name (Op));
      Directly_Overridded :          Entity_Id;
      Type_Entity         :          Entity_Id;

      Type_Def : Asis.Element;

   begin
      if not Is_Overriding_Operation (Op) then
         return Result (1 .. 0);
      end if;

      if Present (Overridden_Operation (Op_Entity)) then
         Directly_Overridded := Overridden_Operation (Op_Entity);
         Res_Last            := Res_Last + 1;
         Result (Res_Last)   := Directly_Overridded;
      end if;

      --  Check if we may have multiple inheritance:
      Type_Def := Primitive_Owner (Op);

      if Definition_Kind (Type_Def) in
           A_Private_Type_Definition |
           A_Private_Extension_Definition
      then
         Type_Def := Enclosing_Element (Type_Def);
         Type_Def := Corresponding_Type_Completion (Type_Def);
         Type_Def := Type_Declaration_View (Type_Def);
      end if;

      if Int_Kind (Type_Def) not in
           A_Derived_Record_Extension_Definition |
           A_Private_Extension_Definition        |
           Internal_Interface_Kinds              |
           A_Formal_Derived_Type_Definition      |
           Internal_Formal_Interface_Kinds
        or else
         Is_Nil (Definition_Interface_List (Type_Def))
      then
         --  Nothing else to do!
         return Result (1 .. Res_Last);
      end if;

      Type_Def    := First_Name (Enclosing_Element (Type_Def));
      Type_Entity := R_Node (Type_Def);

      return Result (1 .. Res_Last) &
             Overridden_Interface_Ops (Type_Entity, Op_Entity);
   end Get_Overridden_Ops;

   -------------------
   -- Get_Root_Type --
   -------------------

   function Get_Root_Type (Decl : Asis.Element) return Asis.Element is
      Arg_Kind : constant Flat_Element_Kinds := Flat_Element_Kind (Decl);
      Type_Def :          Asis.Element;
      Result   :          Asis.Element;
   begin

      case Arg_Kind is
         when A_Variable_Declaration |
              A_Constant_Declaration =>
            null;
         when others =>
            Raise_ASIS_Inappropriate_Element
              (Package_Name & "Get_Root_Type",
               Wrong_Kind => Int_Kind (Decl));
      end case;

      Result := Object_Declaration_View (Decl);
      Result := Asis.Definitions.Subtype_Mark (Result);

      if Expression_Kind (Result) = A_Selected_Component then
         Result := Selector (Result);
      end if;

      Result := Corresponding_Name_Declaration (Result);

      if Declaration_Kind (Result) = A_Subtype_Declaration then
         Result := Corresponding_First_Subtype (Result);
      end if;

      if Declaration_Kind (Result) = An_Ordinary_Type_Declaration then
         Type_Def := Type_Declaration_View (Result);

         if Asis.Elements.Type_Kind (Type_Def) in
            A_Derived_Type_Definition .. A_Derived_Record_Extension_Definition
         then
            Result := Corresponding_Root_Type (Type_Def);
         end if;

      end if;

      return Result;

   end Get_Root_Type;

   -------------------------
   -- Get_Type_Components --
   -------------------------

   function Get_Type_Components
     (El                    : Asis.Element;
      Include_Discriminants : Boolean)
      return                  Asis.Element_List
   is
      Type_Def : Asis.Element;

      procedure Add_Components (Comps : Asis.Element_List);
      --  Adds record components to the result, recursively going down into
      --  variant part(s)

      procedure Add_Components (Comps : Asis.Element_List) is
      begin

         for J in Comps'Range loop

            if Declaration_Kind (Comps (J)) = A_Component_Declaration then
               Gnatcheck_Element_Table.Append (Comps (J));
            elsif Definition_Kind (Comps (J)) = A_Variant_Part then

               declare
                  Vars : constant Asis.Element_List := Variants (Comps (J));
               begin
                  for K in Vars'Range loop
                     Add_Components (Record_Components (Vars (K)));
                  end loop;
               end;

            end if;

         end loop;

      end Add_Components;

   begin
      Gnatcheck_Element_Table.Init;

      if Include_Discriminants then

         Type_Def :=  Discriminant_Part (El);

         if Definition_Kind (Type_Def) = A_Known_Discriminant_Part then

            declare
               Discr_List : constant Asis.Element_List :=
                  Discriminants (Type_Def);
            begin

               for J in Discr_List'Range loop
                  Gnatcheck_Element_Table.Append (Discr_List (J));
               end loop;

            end;

         end if;

      end if;

      Type_Def := Type_Declaration_View (El);

      case Flat_Element_Kind (Type_Def) is
         when A_Protected_Definition =>

            declare
               Items : constant Asis.Element_List :=
                 Private_Part_Items (Type_Def);
            begin

               for J in Items'Range loop

                  if Declaration_Kind (Items (J)) =
                     A_Component_Declaration
                  then
                     Gnatcheck_Element_Table.Append (Items (J));
                  end if;

               end loop;

            end;

         when A_Derived_Type_Definition ..
              A_Derived_Record_Extension_Definition =>

            declare
               Items : constant Asis.Element_List :=
                 Implicit_Inherited_Declarations (Type_Def);
            begin

               for J in Items'Range loop

                  if Declaration_Kind (Items (J)) =
                     A_Component_Declaration
                  then
                     Gnatcheck_Element_Table.Append (Items (J));
                  end if;

               end loop;

            end;

         when others =>
            null;
      end case;

      --  Now add explicit record components, if any

      if Asis.Elements.Type_Kind (Type_Def) =
         A_Derived_Record_Extension_Definition
        or else
         Asis.Elements.Type_Kind (Type_Def) = A_Record_Type_Definition
        or else
         Asis.Elements.Type_Kind (Type_Def) = A_Tagged_Record_Type_Definition
      then
         Type_Def := Asis.Definitions.Record_Definition (Type_Def);

         if Definition_Kind (Type_Def) /= A_Null_Record_Definition then

            declare
               Comps : constant Asis.Element_List :=
                 Record_Components (Type_Def);
            begin
               Add_Components (Comps);
            end;

         end if;

      end if;

      return Asis.Element_List
        (Gnatcheck_Element_Table.Table (1 .. Gnatcheck_Element_Table.Last));
   end Get_Type_Components;

   -------------------------------------
   -- Get_Type_Decl_From_Subtype_Mark --
   -------------------------------------

   function Get_Type_Decl_From_Subtype_Mark
     (SM   : Asis.Element)
      return Asis.Element
   is
      Result : Asis.Element := SM;
   begin

      if Expression_Kind (Result) = A_Selected_Component then
         Result := Selector (Result);
      end if;

      Result := Corresponding_Name_Declaration (Result);

      if Declaration_Kind (Result) = A_Subtype_Declaration then
         Result := Corresponding_First_Subtype (Result);
      end if;

      if Declaration_Kind (Result) in
           A_Private_Type_Declaration .. A_Private_Extension_Declaration
      then
         Result := Corresponding_Type_Declaration (Result);
      end if;

      return Result;
   end Get_Type_Decl_From_Subtype_Mark;

   -------------------------
   -- Get_Underlying_Type --
   -------------------------

   function Get_Underlying_Type
     (SM              : Asis.Element;
      Stop_At_Private : Boolean := False)
      return            Asis.Element
   is
      Result : Asis.Element := Nil_Element;
      Tmp    : Asis.Element := SM;

   begin
      while Attribute_Kind (Tmp) = A_Base_Attribute loop
         Tmp := Prefix (Tmp);
      end loop;

      if Expression_Kind (Tmp) = A_Selected_Component then
         Tmp := Selector (Tmp);
      end if;

      if Expression_Kind (Tmp) = An_Identifier then
         begin
            Tmp := Corresponding_Name_Declaration (Tmp);
         exception
            when Asis.Exceptions.ASIS_Inappropriate_Element =>
               Tmp := Nil_Element;
         end;
      else
         Tmp := Nil_Element;
      end if;

      if Declaration_Kind (Tmp) in
           An_Ordinary_Type_Declaration         |
           A_Task_Type_Declaration              |
           A_Protected_Type_Declaration         |
           An_Incomplete_Type_Declaration       |
           A_Tagged_Incomplete_Type_Declaration |
           A_Private_Type_Declaration           |
           A_Private_Extension_Declaration      |
           A_Subtype_Declaration                |
           A_Formal_Type_Declaration            |
           A_Formal_Incomplete_Type_Declaration
      then
         Result := Unwind_Type (Tmp, Stop_At_Private);
      end if;

      return Result;
   end Get_Underlying_Type;

   ----------------------
   -- Get_Whole_Object --
   ----------------------

   function Get_Whole_Object (El : Asis.Element) return Asis.Element is
      Pref   : Asis.Element := El;
      --  Pref represents the (left) part of the argument name that has not
      --  been traversed yet

      Result : Asis.Element := Selector (El);
      --  The selector part of the current Pref

      procedure Step_To_The_Left;
      --  Resets the values of Pref and Result, moving them to the beginning
      --  (that is - to the left end) of the name represented by El: as a
      --  result of calling this procedure we should always have Result to be
      --  Selector (Prefix) except we are in the very beginning of El

      procedure Step_To_The_Left is
      begin
         case Expression_Kind (Pref) is
            when Not_An_Expression =>
               --  That is, Pref just is Nil_Element, and we have traversed the
               --  whole name represented by El

               Result := Nil_Element;

            when An_Identifier =>
               --  Approaching the left part of El
               Result := Pref;
               Pref   := Nil_Element;
            when A_Selected_Component =>
               Pref   := Prefix (Pref);

               if Expression_Kind (Pref) = An_Identifier then
                  Result := Pref;
                  Pref := Nil_Element;
               elsif Expression_Kind (Pref) = A_Selected_Component then
                  Result := Selector (Pref);
               else
                  pragma Warnings (Off);
                  Step_To_The_Left;
                  pragma Warnings (On);
               end if;

            when A_Slice                 |
                 An_Explicit_Dereference |
                 An_Indexed_Component    =>
               Pref := Prefix (Pref);

               pragma Warnings (Off);
               Step_To_The_Left;
               pragma Warnings (ON);

            when A_Function_Call =>
               --  A rather exotic case - a function call (or a component
               --  therteof) as a changen element...
               Result := Corresponding_Called_Function (Pref);

            when A_Type_Conversion =>

               Pref := Converted_Or_Qualified_Expression (Pref);

               pragma Warnings (Off);
               Step_To_The_Left;
               pragma Warnings (ON);

            when others =>
               Put_Line (Standard_Error, Debug_Image (Pref));

               if Is_Text_Available (Pref) then
                  Put_Line (Standard_Error, Element_Image (Pref));
               end if;

               pragma Assert (False);
         end case;

      end Step_To_The_Left;

   begin

      while not Is_Nil (Result) loop

         if Is_Function_Declaration (Result) then
            --  Actually, a more detailed analyzis is possible for this case
            exit;
         elsif No (Entity (R_Node (Result)))
           and then
            not Is_Nil (Pref)
         then
            --  We have a case of an expaded name - the Entity field is not
            --  set for a selector, but it is set for a whole expanded name.
            --  So what we now have in Result is what we are looking for:
            exit;

         elsif Is_Nil (Pref) then
            --  That means that we get to the beginning (rightmost identifier)
            --  in the expanded name. It can not be a subcomponent, so:
            exit;
         end if;

         Step_To_The_Left;

      end loop;

      return Result;
   end Get_Whole_Object;

   ------------------------
   -- Has_Address_Clause --
   ------------------------

   function Has_Address_Clause (Def_Name : Asis.Element) return Boolean is
      Object_Decl : constant Asis.Element := Enclosing_Element (Def_Name);

      Corr_Rep_Clauses : constant Asis.Element_List :=
        Corresponding_Representation_Clauses (Object_Decl);

      Result : Boolean := False;
   begin

      for J in Corr_Rep_Clauses'Range loop

         if Representation_Clause_Kind (Corr_Rep_Clauses (J)) =
            An_Attribute_Definition_Clause
           and then
             Attribute_Kind
               (Representation_Clause_Name (Corr_Rep_Clauses (J))) =
            An_Address_Attribute
           and then
             Is_Equal
               (Corresponding_Name_Definition
                 (Prefix (Representation_Clause_Name
                   (Corr_Rep_Clauses (J)))),
                Def_Name)
         then
            Result := True;
            exit;
         end if;

      end loop;

      return Result;
   end Has_Address_Clause;

   ------------------------
   -- Has_Class_Wide_Pre --
   ------------------------

   function Has_Class_Wide_Pre (Op : Entity_Id) return Boolean is
      Result : Boolean := False;
      Directly_Overridden_Op : Entity_Id := Empty;
   begin
      if Present (Contract (Op)) then
         Result := Contract_Contains_Pre_Class (Contract (Op));
      end if;

      if not Result then
         Directly_Overridden_Op := Overridden_Operation (Op);

         if Present (Directly_Overridden_Op) then
            Result := Has_Class_Wide_Pre (Directly_Overridden_Op);
         end if;

      end if;

      if not Result then

         declare
            Overridden_Ops : constant List_Of_Nodes := Get_Overridden_Ops (Op);
         begin
            for J in Overridden_Ops'Range loop
               if Overridden_Ops (J) /= Directly_Overridden_Op then
                  Result := Has_Class_Wide_Pre (Overridden_Ops (J));
               end if;

               exit when Result;
            end loop;
         end;

      end if;

      return Result;
   end Has_Class_Wide_Pre;

   -----------------------
   -- Has_One_Parameter --
   -----------------------

   function Has_One_Parameter (El : Asis.Element) return Boolean is
      Template_El : Asis.Element;
      Call_Node   : Node_Id;
      Result      : Boolean := False;
   begin

      if Expression_Kind (El) = A_Function_Call
        or else
         Statement_Kind (El) = A_Procedure_Call_Statement
        or else
         Statement_Kind (El) = An_Entry_Call_Statement
      then
         Call_Node := Node (El);

         if Nkind (Call_Node) = N_Attribute_Reference then

            if Sinfo.Expressions (Call_Node) /= No_List
              and then
               List_Length (Sinfo.Expressions (Call_Node)) = 1
            then
               Result := True;
            end if;

         else

            if Parameter_Associations (Call_Node) /= No_List
              and then
               List_Length (Parameter_Associations (Call_Node)) = 1
            then
               Result := True;
            end if;

         end if;

      elsif Declaration_Kind (El) in A_Generic_Instantiation then
         Template_El := Normalize_Reference (Generic_Unit_Name (El));
         Template_El := Corresponding_Name_Declaration (Template_El);

         if Declaration_Kind (Template_El) in
              A_Generic_Package_Renaming_Declaration ..
              A_Generic_Function_Renaming_Declaration
         then
            Template_El := Corresponding_Base_Entity (Template_El);
            Template_El := Normalize_Reference (Template_El);
            Template_El := Corresponding_Name_Declaration (Template_El);
         end if;

         Result := Generic_Formal_Part (Template_El)'Length = 1;
      end if;

      return Result;
   end Has_One_Parameter;

   --------------------------------
   -- Has_Positional_Association --
   --------------------------------

   function Has_Positional_Association (El : Asis.Element) return Boolean is
      Result : Boolean := False;
   begin

      if Expression_Kind (El) in
           A_Record_Aggregate .. An_Extension_Aggregate
         --  The condition can be extended
      then

         declare
            Associations : constant Asis.Element_List := Get_Associations (El);
         begin
            if Associations'Length > 0 then
               Result := Is_Positional (Associations (Associations'First));
            end if;
         end;

      end if;

      return Result;
   end Has_Positional_Association;

   -------------------
   -- Has_Predicate --
   -------------------

   function Has_Predicate (Type_E : Entity_Id) return Boolean is
      N      : Node_Id;
      Result : Boolean := False;
   begin
      if Has_Dynamic_Predicate_Aspect (Type_E) then
         Result := True;
      elsif Present (First_Rep_Item (Type_E)) then
         N := First_Rep_Item (Type_E);

         while Present (N) loop
            if Nkind (N) = N_Aspect_Specification
              and then
               Chars (Sinfo.Identifier (N)) in
                 Name_Dynamic_Predicate | Name_Static_Predicate
            then
               Result := True;
               exit;
            end if;

            N := Next_Rep_Item (N);
         end loop;
      end if;

      return Result;
   end Has_Predicate;

   -----------------------------
   -- Has_Range_Specification --
   -----------------------------

   function Has_Range_Specification (El : Asis.Element) return Boolean is
      Result : Boolean;
      Tmp    : Asis.Element := Type_Declaration_View (El);
      Constr : Asis.Element;
   begin
      --  The hardest case is a derived type declaration - we have to check
      --  all the chain of derivation and subtyping up to ansestor to see if
      --  there is a range constraint somewhere. So, if we use the recursion,
      --  we may have three kinds of arguments:
      --
      --   * a floating point type or a decimal fixed point type declaration;
      --   * a derived type declaration
      --   * a subtype declaration

      case Flat_Element_Kind (Tmp) is
         when A_Floating_Point_Definition      |
              A_Decimal_Fixed_Point_Definition =>
            Result := not Is_Nil (Real_Range_Constraint (Tmp));

         when A_Subtype_Indication =>
            Constr := Subtype_Constraint (Tmp);

            if Constraint_Kind (Constr) = A_Simple_Expression_Range then
               Result := True;
            else
               Tmp    := Corresponding_First_Subtype (El);
               Result := Has_Range_Specification (Tmp);
            end if;

         when A_Derived_Type_Definition =>
            Constr := Parent_Subtype_Indication (Tmp);
            Constr := Subtype_Constraint (Constr);

            if Constraint_Kind (Constr) = A_Simple_Expression_Range then
               Result := True;
            else
               Tmp    := Corresponding_Parent_Subtype (Tmp);
               Result := Has_Range_Specification (Tmp);
            end if;

         when others =>
            pragma Assert (False);
      end case;

      return Result;
   end Has_Range_Specification;

   ------------------------------
   -- Has_Statements_And_Decls --
   ------------------------------

   function Has_Statements_And_Decls (Decl : Asis.Element) return Boolean is
      Result    : Boolean := False;
   begin

      Result := not Is_Nil (Body_Statements (Decl))
              and then
                not Is_Nil (Body_Declarative_Items (Decl));

      return Result;
   end Has_Statements_And_Decls;

   ------------------------------
   -- Is_Address_Specification --
   ------------------------------

   function Is_Address_Specification (El : Asis.Element) return Boolean is
      Result : Boolean := False;
      Tmp    : Asis.Element;
   begin
      case Flat_Element_Kind (El) is
         when An_Attribute_Definition_Clause =>
            Tmp    := Representation_Clause_Name (El);
            Result := Attribute_Kind (Tmp) = An_Address_Attribute;

         when An_At_Clause =>
            Result := True;

         when An_Aspect_Specification =>
            Tmp := Aspect_Mark (El);

            if Expression_Kind (Tmp) = An_Identifier then
               Result := To_Lower_Case (Asis.Expressions.Name_Image (Tmp)) =
                           "address";
            end if;

         when others =>
            null;
      end case;

      return Result;
   end Is_Address_Specification;

   -----------------
   -- Is_Ancestor --
   -----------------

   function Is_Ancestor
     (Ancestor : Entity_Id;
      Source   : Entity_Id)
      return     Boolean
   is
      Result   : Boolean := False;
      Type_Def : Node_Id;
      Next_T   : Entity_Id;
   begin
      Result := Source = Ancestor;

      if not Result then
         --  Starting from source, go to its definition (and it should be an
         --  interface type definition!) and check Is_Ancestor for all the
         --  interfaces listed in this definition
         Next_T := Parent (Source);

         if Nkind (Next_T) = N_Private_Type_Declaration then
            --  nothing to analyze!
            return False;
         end if;

         pragma Assert (Nkind (Next_T) = N_Full_Type_Declaration);

         Type_Def := Sinfo.Type_Definition (Next_T);

         if Nkind (Type_Def) = N_Derived_Type_Definition then
            --  First element from the interface list can be reached by the
            --  Subtype_Indication (Type_Def) link, the other (if any) are
            --  the members of Interface_List (Type_Def) list

            Next_T := Sinfo.Subtype_Indication (Type_Def);
            Next_T := Entity (Next_T);

            while Present (Next_T)
                and then
                  Nkind (Parent (Next_T)) /= N_Full_Type_Declaration
                and then
                  Etype (Next_T) /= Next_T
            loop
               Next_T := Etype (Next_T);
            end loop;

            pragma Assert (Nkind (Parent (Next_T)) = N_Full_Type_Declaration);

            Result := Is_Ancestor (Ancestor, Next_T);

            if not Result
              and then
               not Is_Empty_List (Interface_List (Type_Def))
            then
               Next_T := First (Interface_List (Type_Def));

               while Present (Next_T) loop

                  Next_T := Entity (Next_T);

                  while Present (Next_T)
                      and then
                        Nkind (Parent (Next_T)) /= N_Full_Type_Declaration
                      and then
                        Etype (Next_T) /= Next_T
                  loop
                     Next_T := Etype (Next_T);
                  end loop;

                  pragma Assert (Nkind (Parent (Next_T)) =
                                 N_Full_Type_Declaration);

                  Result := Is_Ancestor (Ancestor, Next_T);

                  exit when Result;

                  Next_T := Next (Next_T);
               end loop;
            end if;

         end if;

      end if;

      return Result;
   end Is_Ancestor;

   -------------
   -- Is_Body --
   -------------

   function Is_Body (El : Asis.Element) return Boolean is
      Result : Boolean := False;
   begin

      case Flat_Element_Kind (El) is
         when A_Procedure_Body_Declaration |
              A_Function_Body_Declaration  |
              A_Package_Body_Declaration   |
              A_Task_Body_Declaration      |
              An_Entry_Body_Declaration    =>
            Result := True;
         when  others =>
            null;
      end case;

      return Result;

   end Is_Body;

   ---------------------------
   -- Is_Boolean_Logical_Op --
   ---------------------------

   function Is_Boolean_Logical_Op (Op : Asis.Element) return Boolean is
      Entity_N : Entity_Id;
      Call     : Asis.Element;
      Arg_Node : Node_Id := Node (Op);
      Result   : Boolean := False;
   begin

      if Operator_Kind (Op) in An_And_Operator .. An_Xor_Operator then

         Call := Enclosing_Element (Op);

         if Is_Prefix_Call (Call) then
            Arg_Node := R_Node (Call);
         end if;

         if Nkind (Arg_Node) in N_Type_Conversion | N_Qualified_Expression
           and then
            not Comes_From_Source (Arg_Node)
         then
            --  Implicit conversion/qyulification added by front-end
            Arg_Node := Sinfo.Expression (Arg_Node);
         end if;

         if Nkind (Arg_Node) = N_Op_And
           or else
            Nkind (Arg_Node) = N_Op_Or
           or else
            Nkind (Arg_Node) = N_Op_Xor
         then
            Entity_N := Entity (Arg_Node);

            if Present (Entity_N)
              and then
               Sloc (Entity_N) <= Standard_Location
              and then
               Ekind (Etype (Arg_Node)) = E_Enumeration_Type
            then
               Result := True;
            end if;
         end if;

      end if;

      return Result;
   end Is_Boolean_Logical_Op;

   ----------------------------------
   -- Is_Call_To_Operator_Function --
   ----------------------------------

   function Is_Call_To_Operator_Function (El : Asis.Element) return Boolean is
      Pref   : Asis.Element;
      Result : Boolean := False;
   begin

      if Expression_Kind (El) = A_Function_Call then

         if not Is_Prefix_Call (El) then
            Result := True;
         else
            Pref := Prefix (El);

            if Expression_Kind (Pref) = A_Selected_Component then
               Pref := Selector (Pref);
            end if;

            Result := Expression_Kind (Pref) = An_Operator_Symbol;

         end if;

      end if;

      return Result;
   end Is_Call_To_Operator_Function;

   ---------------
   -- Is_Caller --
   ---------------

--   function Is_Caller (El : Asis.Element) return Boolean is
--      Spec_El : Asis.Element;
--      Result  : Boolean := False;
--   begin
--      --  Implementation is incomplete!!! ???
--      --  Protected operations is a huge hole!!!

--      case Flat_Element_Kind (El) is
--         when A_Procedure_Declaration |
--              A_Function_Declaration  =>

--            Result := Trait_Kind (El) /= An_Abstract_Trait;

--         when An_Entry_Body_Declaration =>

--            Result := True;

--         when A_Procedure_Body_Declaration |
--              A_Function_Body_Declaration  |
--              A_Procedure_Body_Stub        |
--              A_Function_Body_Stub         =>

--            Spec_El := El;

--            if Is_Subunit (El) then
--               Spec_El := Corresponding_Body_Stub (El);
--            end if;

--            Spec_El := Corresponding_Declaration (El);

--            Result :=
--              Declaration_Kind (Spec_El) not in
--                A_Generic_Procedure_Declaration ..
--                A_Generic_Function_Declaration;

--         when An_Entry_Declaration =>

--            if Definition_Kind (Get_Enclosing_Element) =
--               A_Protected_Definition
--            then
--               Result := True;
--            end if;

--         when others =>
--            null;
--      end case;

--      return Result;
--   end Is_Caller;

   -----------------
   -- Is_Constant --
   -----------------

   function Is_Constant (E : Asis.Element) return Boolean is
      Result : Boolean := False;
   begin
      if Defining_Name_Kind (E) = A_Defining_Identifier then
         Result := Ekind (Node (E)) = E_Constant;
      end if;

      return Result;
   end Is_Constant;

   ---------------------------------
   -- Is_Constr_Error_Declaration --
   ---------------------------------

   function Is_Constr_Error_Declaration (Decl : Asis.Element) return Boolean is
      Result : Boolean := False;
   begin

      if Declaration_Kind (Decl) = An_Exception_Declaration
        and then
         Is_Standard (Enclosing_Compilation_Unit (Decl))
        and then
         Defining_Name_Image (First_Name (Decl)) = "Constraint_Error"
      then
         Result := True;
      end if;

      return Result;
   end Is_Constr_Error_Declaration;

   -------------------------
   -- Is_Constraint_Error --
   -------------------------

   function Is_Constraint_Error (Ref : Asis.Element) return Boolean is
      Next_Exception_Decl : Asis.Element;

      Result : Boolean := False;
   begin
      Next_Exception_Decl := Corresponding_Name_Declaration (Ref);

      while not Is_Nil (Next_Exception_Decl) loop

         if Is_Constr_Error_Declaration (Next_Exception_Decl) then
            Result := True;
            exit;
         elsif Is_Num_Error_Declaration (Next_Exception_Decl) then
            exit;
         elsif Declaration_Kind (Next_Exception_Decl) =
               An_Exception_Renaming_Declaration
         then
            Next_Exception_Decl := Renamed_Entity (Next_Exception_Decl);
            Next_Exception_Decl := Normalize_Reference (Next_Exception_Decl);
            Next_Exception_Decl :=
              Corresponding_Name_Declaration (Next_Exception_Decl);
         else
            exit;
         end if;

      end loop;

      return Result;
   end Is_Constraint_Error;

   --------------------
   -- Is_Constructor --
   --------------------

   function Is_Constructor (Element : Asis.Element) return Boolean is
      Name     : Asis.Element;
      N        : Node_Id;
      Result   : Boolean := False;

   begin

      if Declaration_Kind (Element) in
            A_Function_Declaration             |
            An_Expression_Function_Declaration |
            A_Function_Body_Declaration        |
            A_Function_Renaming_Declaration    |
            A_Function_Body_Stub
        and then
         Is_Dispatching_Operation (Element)
      then
         Name := First_Name (Element);
         N    := Node (Name);

         if Has_Controlling_Result (N) then
            --  The last thing to check is that we do not have any controlling
            --  parameter
            Result := True;

            declare
               Pars : constant Asis.Element_List :=
                 Parameter_Profile (Element);
            begin
               if Pars'Length > 0 then
                  --  All we have to check in a legal code is if we have at
                  --  least one parameter of a tagged type, and this type is
                  --  not a class-wide type.

                  for J in Pars'Range loop
                     Name := First_Name (Pars (J));
                     N    := Node (Name);
                     N    := Etype (N);

                     if Ekind (N) in
                          E_Access_Type          |
                          E_Access_Subtype       |
                          E_Anonymous_Access_Type
                     then
                        N := Directly_Designated_Type (N);
                     end if;

                     if Is_Tagged_Type (N)
                       and then
                        Ekind (N) not in
                          E_Class_Wide_Type | E_Class_Wide_Subtype
                     then
                        --  We already have a controlling result, so in a legal
                        --  code this type should be the same as the result
                        --  type. Therefore:
                        Result := False;
                        exit;
                     end if;
                  end loop;
               end if;
            end;
         end if;
      end if;

      return Result;
   end Is_Constructor;

   --------------------------
   -- Is_Control_Structure --
   --------------------------

   function Is_Control_Structure (Stmt : Asis.Element) return Boolean is
      Result : Boolean := False;
   begin

      case Statement_Kind (Stmt) is
         when An_If_Statement                    |
              A_Case_Statement                   |
              A_Loop_Statement                   |
              A_While_Loop_Statement             |
              A_For_Loop_Statement               |
              A_Selective_Accept_Statement       |
              A_Timed_Entry_Call_Statement       |
              A_Conditional_Entry_Call_Statement |
              An_Asynchronous_Select_Statement   =>
            Result := True;
         when others =>
            null;
      end case;

      return Result;
   end Is_Control_Structure;

   ---------------------------------
   -- Is_Downward_View_Conversion --
   ---------------------------------

   function Is_Downward_View_Conversion
     (Element : Asis.Element)
      return    Boolean
   is
      Result : Boolean := False;
      Source : Asis.Element;
      Target : Asis.Element;

      Source_T : Entity_Id;
      Target_T : Entity_Id;
   begin
      if Expression_Kind (Element) /= A_Type_Conversion then
         return False;
      end if;

      Source   := Converted_Or_Qualified_Expression (Element);
      Source_T := R_Node (Source);
      Source_T := Etype (Source_T);

      while Ekind (Source_T) in
              E_Class_Wide_Subtype          |
              E_Record_Subtype              |
              E_Record_Subtype_With_Private |
              E_Private_Subtype             |
              E_Limited_Private_Subtype
      loop
         Source_T := Etype (Source_T);
      end loop;

      --  We are interested in view conversions in the context of
      --  Downward_View_Conversions gnatcheck rule, so both source and target
      --  types should be tagged

      if not Is_Tagged_Type (Source_T) then
         return False;
      end if;

      Target   := Converted_Or_Qualified_Subtype_Mark (Element);
      Target_T := R_Node (Target);
      Target_T := Etype (Target_T);

      while Ekind (Target_T) in
              E_Class_Wide_Subtype          |
              E_Record_Subtype              |
              E_Record_Subtype_With_Private |
              E_Private_Subtype             |
              E_Limited_Private_Subtype
      loop
         Target_T := Etype (Target_T);
      end loop;

      if Ekind (Source_T) = E_Class_Wide_Type then
         Source_T := Etype (Source_T);
      end if;

      if Ekind (Target_T) = E_Class_Wide_Type then
         Target_T := Etype (Target_T);
      end if;

      --  Conversion of non-interface type into interface type is always OK

      if Is_Interface (Target_T) and then not Is_Interface (Source_T) then
         return False;
      end if;

      --  Simple case - both source and target are not interfaces

      if not Is_Interface (Target_T) and then not Is_Interface (Source_T) then

         Result := True;

         loop

            if Source_T = Target_T then
               Result := False;
               exit;
            end if;

            exit when Etype (Source_T) = Source_T;

            Source_T := Etype (Source_T);
         end loop;

      end if;

      if Is_Interface (Target_T) and then Is_Interface (Source_T) then
         --  The hardest case - both source and target are interfaces.
         Result := not Is_Ancestor (Target_T, Source_T);
      end if;

      return Result;
   end Is_Downward_View_Conversion;

   --------------
   -- Is_Frame --
   --------------

   function Is_Frame (El : Asis.Element) return Boolean is
      Result : Boolean := False;
   begin

      case Flat_Element_Kind (El) is
         when A_Procedure_Body_Declaration |
              A_Function_Body_Declaration  |
              A_Package_Body_Declaration   |
              An_Entry_Body_Declaration    |
              A_Task_Body_Declaration      |
              A_Block_Statement            |
              An_Extended_Return_Statement |
              An_Accept_Statement          =>

            Result := True;
         when others =>
            null;
      end case;

      return Result;
   end Is_Frame;

   ----------------------
   -- Is_From_Standard --
   ----------------------

   function Is_From_Standard (El : Asis.Element) return Boolean is
      Result : Boolean := False;
   begin

      if not Is_Nil (El) then
         Result := Sloc (Node (El)) <= Standard_Location;
      end if;

      return Result;
   end Is_From_Standard;

   -----------------------------
   -- Is_Function_Declaration --
   -----------------------------

   function Is_Function_Declaration (El : Asis.Element) return Boolean is
      Result : Boolean := False;
   begin

      case Declaration_Kind (El) is
         when A_Function_Declaration          |
              A_Function_Body_Declaration     |
              A_Function_Body_Stub            |
              A_Function_Renaming_Declaration |
              A_Function_Instantiation        |
              A_Formal_Function_Declaration   |
              A_Generic_Function_Declaration  =>

            Result := True;

         when others =>
            null;
      end case;

      return Result;
   end Is_Function_Declaration;

   ---------------------
   -- Is_Dynamic_Call --
   ---------------------

   function Is_Dynamic_Call (Call : Asis.Element) return Boolean is
      Tmp    : Asis.Element;
      Result : Boolean := False;
   begin

      if Expression_Kind (Call) = A_Function_Call then
         Tmp := Prefix (Call);
      else
         Tmp := Called_Name (Call);
      end if;

      if Expression_Kind (Tmp) = An_Explicit_Dereference
        or else
         Is_True_Expression (Tmp)
      then
         --  If the prefix of a (procedure or function) call is a true
         --  expression that is, if it has a type, the only possibility for
         --  this prefix is to be of an access to procedure/function type, so
         Result := True;
      end if;

      return Result;
   end Is_Dynamic_Call;

   ------------------------------
   -- Is_Enum_Literal_Renaming --
   ------------------------------

   function Is_Enum_Literal_Renaming (El : Asis.Element) return Boolean is
      Result         : Boolean := False;
      Renamed_Entity : Entity_Id;
   begin
      if Declaration_Kind (El) = A_Function_Renaming_Declaration then

         Renamed_Entity := Sinfo.Name (Node (El));
         Renamed_Entity := Entity (Renamed_Entity);

         if Present (Renamed_Entity)
           and then
            Ekind (Renamed_Entity) = E_Enumeration_Literal
         then
            Result := True;
         end if;

      end if;

      return Result;
   end Is_Enum_Literal_Renaming;

   --------------
   -- Is_Fixed --
   --------------

   function Is_Fixed (Expr : Asis.Element) return Boolean is
      Result      : Boolean := False;
      Type_Entity : Entity_Id;
   begin

      if Asis.Extensions.Is_True_Expression (Expr) then
         Type_Entity := Etype (R_Node (Expr));
         Result      := Ekind (Type_Entity) in Fixed_Point_Kind;
      end if;

      return Result;

   end Is_Fixed;

   --------------
   -- Is_Float --
   --------------

   function Is_Float (Expr : Asis.Element) return Boolean is
      Result      : Boolean := False;
      Type_Entity : Entity_Id;
   begin

      if Asis.Extensions.Is_True_Expression (Expr) then
         Type_Entity := Etype (R_Node (Expr));

         while Present (Type_Entity)
             and then
               Ekind (Type_Entity) in E_Private_Type | E_Private_Subtype
         loop
            Type_Entity := Full_View (Type_Entity);
         end loop;

         Result := Present (Type_Entity)
                  and then
                   Ekind (Type_Entity) in Float_Kind;
      end if;

      return Result;

   end Is_Float;

   ----------------
   -- Is_Handled --
   ----------------

   function Is_Handled
     (Exc  : Asis.Element;
      By   : Asis.Element_List)
      return Boolean
   is
      Exc_To_Catch : Asis.Element := Exc;
      Result       : Boolean  := False;
      Last_Handler : Boolean := True;
   begin

      if By'Length > 0 then

         if Declaration_Kind (Enclosing_Element (Exc_To_Catch)) =
            An_Exception_Renaming_Declaration
         then
            Exc_To_Catch :=
              Get_Name_Definition
                (Renamed_Entity (Enclosing_Element (Exc_To_Catch)));
         end if;

         Traverse_Handlers : for J in reverse By'Range loop

            declare
               Handled_Excs : constant Asis.Element_List :=
                 Exception_Choices (By (J));
            begin

               if Last_Handler
                 and then
                  Definition_Kind (Handled_Excs (Handled_Excs'Last)) =
                  An_Others_Choice
               then
                  Result := True;
                  exit Traverse_Handlers;
               end if;

               Last_Handler := False;

               for K in Handled_Excs'Range loop

                  if Is_Equal
                       (Get_Name_Definition (Handled_Excs (K)),
                        Exc_To_Catch)
                  then
                     Result := True;
                     exit Traverse_Handlers;
                  end if;

               end loop;

            end;

         end loop Traverse_Handlers;

      end if;

      return Result;
   end Is_Handled;

   --------------------------
   -- Is_Interrupt_Handler --
   --------------------------

   function Is_Interrupt_Handler (Proc : Asis.Element) return Boolean is
      Result : Boolean := False;
      Tmp    : Asis.Element;
   begin
      if Declaration_Kind (Proc) = A_Procedure_Declaration
        and then
         Definition_Kind (Enclosing_Element (Proc)) = A_Protected_Definition
        and then
         Parameter_Profile (Proc)'Length = 0
      then

         --  Check for aspects Attach_Handler or Interrupt_Handler first
         declare
            Asps : constant Asis.Element_List := Aspect_Specifications (Proc);
         begin
            for J in Asps'Range loop
               Tmp := Aspect_Mark (Asps (J));

               if To_Lower_Case (Asis.Expressions.Name_Image (Tmp)) in
                    "attach_handler" | "interrupt_handler"
               then
                  Result := True;
                  exit;
               end if;
            end loop;
         end;

         if not Result then
            --  Check for pragmas Attach_Handler or Interrupt_Handler
            declare
               Dcls : constant Asis.Element_List :=
                 Pragmas (Enclosing_Element (Proc));
            begin
               for J in Dcls'Range loop
                  if To_Lower_Case (Pragma_Name_Image (Dcls (J))) in
                       "attach_handler" | "interrupt_handler"
                  then
                     declare
                        Pars : constant Asis.Element_List :=
                          Pragma_Argument_Associations (Dcls (J));
                     begin
                        Tmp := Actual_Parameter (Pars (Pars'First));

                        if To_Lower_Case (Asis.Expressions.Name_Image (Tmp)) =
                           To_Lower_Case
                             (Defining_Name_Image (First_Name (Proc)))
                        then
                           Result := True;
                           exit;
                        end if;
                     end;
                  end if;
               end loop;
            end;
         end if;

      end if;

      return Result;
   end Is_Interrupt_Handler;

   ----------------
   -- Is_Limited --
   ----------------

   function Is_Limited (SM : Asis.Element) return Boolean is
      Type_Entity : Entity_Id;
      Result      : Boolean := False;
   begin

      case Expression_Kind (SM) is
         when An_Identifier          |
              A_Selected_Component   |
              An_Attribute_Reference =>

            Type_Entity := Etype (R_Node (SM));

            Result :=
              Is_Limited_Type (Type_Entity)
             or else
              (Is_Interface (Type_Entity)
              and then
               Is_Limited_Interface (Type_Entity));

         when others =>
            null;
      end case;

      return Result;
   end Is_Limited;

   --------------------
   -- Is_Local --
   --------------------

   function Is_Local
     (Dcl            : Asis.Element;
      Protected_Body : Asis.Element)
      return Boolean
   is
      Result         :          Boolean      := False;
      Encl_El        :          Asis.Element := Enclosing_Element (Dcl);
      Protected_Spec : constant Asis.Element :=
        Corresponding_Declaration (Protected_Body);
   begin
      while not Is_Nil (Encl_El) loop
         if Is_Equal (Encl_El, Protected_Body)
           or else
            Is_Equal (Encl_El, Protected_Spec)
         then
            Result := True;
            exit;
         end if;

         Encl_El := Enclosing_Element (Encl_El);
      end loop;

      return Result;
   end Is_Local;

   --------------------
   -- Is_Named_Scope --
   --------------------

   function Is_Named_Scope (E : Asis.Element) return Boolean is
   begin

      return Declaration_Kind (E) in
               A_Task_Type_Declaration                  |
               A_Protected_Type_Declaration             |
               A_Procedure_Body_Declaration             |
               A_Function_Body_Declaration              |
               A_Package_Declaration                    |
               A_Package_Body_Declaration               |
               A_Task_Body_Declaration                  |
               A_Protected_Body_Declaration             |
               A_Generic_Package_Declaration            |
               A_Procedure_Declaration                  |
               An_Expression_Function_Declaration       |
               A_Function_Declaration                   |
               A_Package_Renaming_Declaration           |
               A_Procedure_Renaming_Declaration         |
               A_Function_Renaming_Declaration          |
               A_Generic_Package_Renaming_Declaration   |
               A_Generic_Procedure_Renaming_Declaration |
               A_Generic_Function_Renaming_Declaration  |
               A_Generic_Procedure_Declaration          |
               A_Generic_Function_Declaration;

   end Is_Named_Scope;

   ------------------------------
   -- Is_Num_Error_Declaration --
   ------------------------------

   function Is_Num_Error_Declaration (Decl : Asis.Element) return Boolean is
      Result : Boolean := False;
   begin

      if Declaration_Kind (Decl) = An_Exception_Renaming_Declaration
        and then
         Is_Standard (Enclosing_Compilation_Unit (Decl))
        and then
         Defining_Name_Image (First_Name (Decl)) = "Numeric_Error"
      then
         Result := True;
      end if;

      return Result;
   end Is_Num_Error_Declaration;

   ----------------------
   -- Is_Numeric_Error --
   ----------------------

   function Is_Numeric_Error (Ref : Asis.Element) return Boolean is
      Next_Exception_Decl : Asis.Element;

      Result : Boolean := False;
   begin
      Next_Exception_Decl := Corresponding_Name_Declaration (Ref);

      while not Is_Nil (Next_Exception_Decl) loop

         if Is_Num_Error_Declaration (Next_Exception_Decl) then
            Result := True;
            exit;
         elsif Declaration_Kind (Next_Exception_Decl) =
               An_Exception_Renaming_Declaration
         then
            Next_Exception_Decl := Renamed_Entity (Next_Exception_Decl);
            Next_Exception_Decl := Normalize_Reference (Next_Exception_Decl);
            Next_Exception_Decl :=
              Corresponding_Name_Declaration (Next_Exception_Decl);
         else
            exit;
         end if;

      end loop;

      return Result;
   end Is_Numeric_Error;

   -------------------------------------
   -- Is_Object_Address_Specification --
   -------------------------------------

   function Is_Object_Address_Specification
     (El   : Asis.Element)
      return Boolean
   is
      Result : Boolean := False;
      Tmp    : Asis.Element;
   begin

      if Is_Address_Specification (El) then

         if Definition_Kind (El) = An_Aspect_Specification then
            Tmp := Enclosing_Element (El);
         else
            --  an 'Address definition clause
            Tmp := Entity_From_Rep_Item (El);
         end if;

         Result := Declaration_Kind (Tmp) in
                     A_Variable_Declaration |
                     A_Constant_Declaration |
                     A_Deferred_Constant_Declaration;
      end if;

      return Result;
   end Is_Object_Address_Specification;

   -------------------
   -- Is_Positional --
   -------------------

   function Is_Positional (El : Asis.Element) return Boolean is
      Result : Boolean := False;
   begin

      if not Is_Normalized (El) then

         case Association_Kind (El) is
            when A_Pragma_Argument_Association |
                 A_Parameter_Association       |
                 A_Generic_Association         =>
               Result := Is_Nil (Formal_Parameter (El));
            when A_Discriminant_Association =>
               Result := Is_Nil (Discriminant_Selector_Names (El));
            when A_Record_Component_Association =>
               Result := Is_Nil (Record_Component_Choices (El));
            when An_Array_Component_Association =>
               Result := Is_Nil (Array_Component_Choices (El));
            when others =>
               null;
         end case;

      end if;

      return Result;
   end Is_Positional;

   -------------------
   -- Is_Predefined --
   -------------------

   function Is_Predefined (Operation : Asis.Element) return Boolean is
      Tmp_Element : Asis.Element;
      Op_Entity   : Entity_Id := Empty;
      Result      : Boolean := False;
   begin

      if Expression_Kind (Operation) = An_Operator_Symbol
        and then
         Is_Uniquely_Defined (Operation)
      then

         Tmp_Element := Corresponding_Name_Definition (Operation);

         if Is_Nil (Tmp_Element) then
            --  This also includes the case of "/=" implicitly declared by
            --  an explicit declaration of "="

            Tmp_Element := Enclosing_Element (Operation);

            if Expression_Kind (Tmp_Element) = A_Selected_Component then
               Op_Entity := R_Node (Tmp_Element);
            else
               Op_Entity := R_Node (Operation);
            end if;

            if Nkind (Op_Entity) = N_Raise_Constraint_Error then
               Op_Entity := Node (Operation);
            end if;

            case Nkind (Op_Entity) is
               when N_Function_Call =>
                  Op_Entity := Sinfo.Name (Op_Entity);
               when N_Type_Conversion =>
                  Op_Entity := Sinfo.Expression (Op_Entity);
               when others =>
                  null;
            end case;

            Op_Entity := Entity (Op_Entity);

            Result := Sloc (Op_Entity) = Standard_Location;

         end if;
      end if;

      return Result;

   end Is_Predefined;

   --------------------------
   -- Is_Predefined_String --
   --------------------------

   function Is_Predefined_String (Type_Decl : Asis.Element) return Boolean is
      Type_Entity : Entity_Id;
      Result      : Boolean := False;
   begin

      if Declaration_Kind (Type_Decl) = An_Ordinary_Type_Declaration
        or else
         Declaration_Kind (Type_Decl) = A_Subtype_Declaration
      then
         Type_Entity := R_Node (Names (Type_Decl) (1));

         while Etype (Type_Entity) /= Type_Entity loop
            Type_Entity := Etype (Type_Entity);
         end loop;

         Result := Type_Entity = Stand.Standard_String;

      end if;

      return Result;

   end Is_Predefined_String;

   ----------------------------------
   -- Is_Prefix_Notation_Exception --
   ----------------------------------

   function Is_Prefix_Notation_Exception
     (El                 : Asis.Element;
      Exclude_Second_Par : Boolean)
      return Boolean
   is
      Call_Node      : Node_Id;
      Par_Node       : Node_Id;
      Firts_Par_Node : Node_Id;

      Result    : Boolean := False;
   begin
      Call_Node := Parent (R_Node (El));

      --  We can be sure, that El is a subprogram call that has at least one
      --  parameter, so Parameter_Associations (Call_Node) definitely presents.
      if List_Length (Parameter_Associations (Call_Node)) = 1 then
         Result := True;
      else
         Par_Node       := R_Node (El);
         Firts_Par_Node := First (Parameter_Associations (Call_Node));

         if Par_Node = Firts_Par_Node then
            Result := True;
         elsif List_Length (Parameter_Associations (Call_Node)) = 2
            and then
             Exclude_Second_Par
         then
            Result := Par_Node = Next (Firts_Par_Node);
         end if;

      end if;

      return Result;
   end Is_Prefix_Notation_Exception;

   ---------------------------------
   -- Is_Protected_Operation_Call --
   ---------------------------------

   function Is_Protected_Operation_Call (Call : Asis.Element) return Boolean is
      Tmp_Node : Node_Id;
      Result   : Boolean := False;
   begin
      Tmp_Node := R_Node (Call);

      if Nkind (Tmp_Node) = N_Entry_Call_Statement then
         Tmp_Node := Prefix (Sinfo.Name (Tmp_Node));
         Tmp_Node := Etype (Tmp_Node);

         if Ekind (Tmp_Node) in Private_Kind then
            Tmp_Node := Full_View (Tmp_Node);
         end if;

         Result := Ekind (Tmp_Node) in Protected_Kind;
      end if;

      return Result;
   end Is_Protected_Operation_Call;

   ------------------------------------
   -- Is_Ref_To_Standard_Num_Subtype --
   ------------------------------------

   function Is_Ref_To_Standard_Num_Subtype
     (Ref  : Asis.Element)
      return Boolean
   is
      Result     : Boolean := False;
      Arg_Entity : Entity_Id;
   begin
      Arg_Entity := Node (Ref);

      if Nkind (Arg_Entity) in N_Has_Entity then

         if No (Entity (Arg_Entity))
           and then
            Nkind (Parent (Arg_Entity)) = N_Expanded_Name
           and then
            Arg_Entity = Selector_Name (Parent (Arg_Entity))
         then
            Arg_Entity := Parent (Arg_Entity);
         end if;

         Arg_Entity := Entity (Arg_Entity);

         if Present (Arg_Entity)
           and then
            Sloc (Arg_Entity) = Standard_Location
           and then
            Ekind (Arg_Entity) in Numeric_Kind
         then
            Result := True;
         end if;

      end if;

      return Result;

   end Is_Ref_To_Standard_Num_Subtype;

   ---------------
   -- Is_Public --
   ---------------

   function Is_Public (Def_Name : Asis.Element) return Boolean is
      Result : Boolean := False;
   begin

      case Defining_Name_Kind (Def_Name) is
         when A_Defining_Identifier .. A_Defining_Operator_Symbol =>
            Result := not Is_Hidden (Node (Def_Name));
         when A_Defining_Expanded_Name =>
            Result := not Is_Hidden (Node (Defining_Selector (Def_Name)));
         when others =>
            null;
      end case;

      return Result;
   end Is_Public;

   -----------------
   -- Is_Renaming --
   -----------------

   function Is_Renaming (El : Asis.Element) return Boolean is
      Result : Boolean := False;
   begin
      --  A very simple test at the moment

      case Flat_Element_Kind (El) is
         when A_Procedure_Renaming_Declaration |
              A_Function_Renaming_Declaration  =>
            Result := True;
         when others =>
            null;
      end case;

      return Result;
   end Is_Renaming;

   ----------------------------
   -- Is_Representation_Item --
   ----------------------------

   function Is_Representation_Item (El : Asis.Element) return Boolean is
   begin
      return Clause_Kind (El) = A_Representation_Clause
           or else
             (Element_Kind (El) = A_Pragma
             and then
              To_Lower_Case (Pragma_Name_Image (El)) in
                "atomic"                 |
                "atomic_components"      |
                "independent"            |
                "independent_components" |
                "pack"                   |
                "unchecked_union"        |
                "volatile)"              |
                "volatile_components");
   end Is_Representation_Item;

   -------------------------
   -- Is_Standard_Boolean --
   -------------------------

   function Is_Standard_Boolean (Expr : Asis.Element) return Boolean is
      Result      : Boolean := False;
      Type_Entity : Entity_Id;
   begin

      if Asis.Extensions.Is_True_Expression (Expr) then
         Type_Entity := Etype (R_Node (Expr));

         while Present (Type_Entity)
            and then
               Type_Entity /= Etype (Type_Entity)
            and then
               Ekind (Type_Entity) /= E_Enumeration_Type
         loop
            Type_Entity := Etype (Type_Entity);
         end loop;

         Result      := Type_Entity = Standard_Boolean;
      end if;

      return Result;

   end Is_Standard_Boolean;

   ----------------------
   -- Is_Task_Creation --
   ----------------------

   function Is_Task_Creation (El : Asis.Element) return Boolean is
      Arg_Kind : constant Flat_Element_Kinds := Flat_Element_Kind (El);
      Result   :          Boolean := False;
   begin

      case Arg_Kind is
         when A_Variable_Declaration |
              A_Constant_Declaration =>
            Result := Is_Task_Object_Declaration (El);
         when A_Single_Task_Declaration =>
            Result := True;
         when others =>
            null;
      end case;

      return Result;
   end Is_Task_Creation;

   ------------------------
   -- Is_Task_Entry_Call --
   ------------------------

   function Is_Task_Entry_Call (Call : Asis.Element) return Boolean is
      Pref_Node      : Node_Id;
      Pref_Type_Node : Entity_Id;
      Result         : Boolean   := False;
   begin

      if Statement_Kind (Call) = An_Entry_Call_Statement then
         Pref_Node      := Node (Called_Name (Call));

         if Nkind (Pref_Node) = N_Indexed_Component then
            --  Call to an entry from an entrty family
            Pref_Node := Prefix (Pref_Node);
         end if;

         Pref_Type_Node := Etype (Pref_Node);

         if (No (Pref_Type_Node)
            or else
             Ekind (Pref_Type_Node) = E_Void)
           and then
             Nkind (Pref_Node) = N_Selected_Component
         then
            Pref_Node      := Sinfo.Prefix (Pref_Node);
            Pref_Type_Node := Etype (Pref_Node);
         end if;

         if Present (Pref_Type_Node)
           and then
            Ekind (Pref_Type_Node) in
              E_Private_Type         |
              E_Private_Subtype      |
              E_Limited_Private_Type |
              E_Limited_Private_Subtype
         then
            Pref_Type_Node := Full_View (Pref_Type_Node);
         end if;

         Result := Ekind (Pref_Type_Node) in Task_Kind;
      end if;

      return Result;
   end Is_Task_Entry_Call;

   --------------------------------
   -- Is_Task_Object_Declaration --
   --------------------------------

   function Is_Task_Object_Declaration (Expr : Asis.Element) return Boolean is
      N      : Node_Id;
      Result : Boolean := False;
   begin

      case Flat_Element_Kind (Expr) is
         when A_Variable_Declaration |
              A_Constant_Declaration =>

            N := Defining_Identifier (R_Node (Expr));
            N := Etype (N);

            Result := Ekind (N) in Task_Kind;
         when others =>
            null;
      end case;

      return Result;
   end Is_Task_Object_Declaration;

   ------------------------
   -- Is_Template_Caller --
   ------------------------

   function Is_Template_Caller (El : Asis.Element) return Boolean is
      Result : Boolean := False;
   begin
      case Flat_Element_Kind (El) is
         when A_Task_Type_Declaration =>
            Result := True;
         when others =>
            null;
      end case;

      return Result;
   end Is_Template_Caller;

   ----------------------------
   -- Is_Unconstrained_Array --
   ----------------------------

   function Is_Unconstrained_Array (Type_Decl : Asis.Element) return Boolean is
      Type_Entity : Entity_Id;
      Result      : Boolean := False;
   begin

      if Declaration_Kind (Type_Decl) = An_Ordinary_Type_Declaration
        or else
         Declaration_Kind (Type_Decl) = A_Subtype_Declaration
      then
         Type_Entity := R_Node (Names (Type_Decl) (1));

         if Is_Array_Type (Type_Entity)
           and then
            not Is_Constrained (Type_Entity)
         then
            Result := True;
         end if;

      end if;

      return Result;

   end Is_Unconstrained_Array;

   --------------------------
   -- Look_For_Loop_Pre_Op --
   --------------------------

   procedure Look_For_Loop_Pre_Op
     (Element :        Asis.Element;
      Control : in out Traverse_Control;
      State   : in out Boolean)
   is
   begin

      case Element_Kind (Element) is
         when A_Statement =>

            case Statement_Kind (Element) is
               when An_If_Statement                    |
                    A_Case_Statement                   |
                    A_Block_Statement                  |
                    An_Extended_Return_Statement       |
                    An_Accept_Statement                |
                    A_Selective_Accept_Statement       |
                    A_Timed_Entry_Call_Statement       |
                    A_Conditional_Entry_Call_Statement |
                    An_Asynchronous_Select_Statement   =>
                  null;
               when A_Loop_Statement       |
                    A_While_Loop_Statement |
                    A_For_Loop_Statement   =>

                  State   := True;
                  Control := Terminate_Immediately;

               when others =>
                  Control := Abandon_Children;
            end case;

         when A_Path =>
            null;
         when others =>
            Control := Abandon_Children;
      end case;

   end Look_For_Loop_Pre_Op;

   ---------------------------------------
   -- Look_For_Modular_Component_Pre_Op --
   ---------------------------------------

   procedure Look_For_Modular_Component_Pre_Op
     (Element :        Asis.Element;
      Control : in out Traverse_Control;
      State   : in out Boolean)
   is
      S_Mark : Asis.Element;
   begin

      case Declaration_Kind (Element) is
         when A_Component_Declaration =>
            S_Mark := Object_Declaration_View (Element);
            S_Mark := Component_Definition_View (S_Mark);

            if Definition_Kind (S_Mark) = A_Subtype_Indication then
               S_Mark := Asis.Definitions.Subtype_Mark (S_Mark);

               if Is_Modular_Type (S_Mark) then
                  State   := True;
                  Control := Terminate_Immediately;
               else
                  Control := Abandon_Children;
               end if;
            else
               --  Anonymous access definition
               Control := Abandon_Children;
            end if;

         when others =>
            null;
      end case;

   end Look_For_Modular_Component_Pre_Op;

   ----------------------
   -- Needs_Completion --
   ----------------------

   function Needs_Completion (El : Asis.Element) return Boolean is
      Arg_Kind : constant Flat_Element_Kinds := Flat_Element_Kind (El);
      Result   : Boolean                     := False;
      Entity_N : Entity_Id;
   begin

      case Arg_Kind is
         when A_Task_Type_Declaration        |
              A_Protected_Type_Declaration   |
              A_Single_Task_Declaration      |
              A_Single_Protected_Declaration |
              A_Procedure_Body_Stub          |
              A_Function_Body_Stub           |
              A_Package_Body_Stub            |
              A_Task_Body_Stub               |
              A_Protected_Body_Stub          =>
            Result := True;

         when A_Package_Declaration         |
              A_Generic_Package_Declaration =>

            --  Now we make the check for library packages only!

            if Is_Nil (Enclosing_Element (El)) then
               Result :=
                 Asis.Compilation_Units.Is_Body_Required
                   (Enclosing_Compilation_Unit (El));
            end if;

         when A_Generic_Procedure_Declaration |
              A_Generic_Function_Declaration  |
              A_Procedure_Declaration         |
              A_Function_Declaration          =>

            Entity_N := Defining_Unit_Name (Specification (Node (El)));

            if Nkind (Entity_N) = N_Defining_Program_Unit_Name then
               Entity_N := Defining_Identifier (Entity_N);
            end if;

            if not (Is_Intrinsic_Subprogram (Entity_N)
                 or else
                    Is_Imported (Entity_N))
            then
               Result := True;
            end if;

         when others =>
            null;
      end case;

      return Result;
   end Needs_Completion;

   ---------------------------------
   -- Needs_Real_Range_Definition --
   ---------------------------------

   function Needs_Real_Range_Definition (El : Asis.Element) return Boolean is
      Result : Boolean := False;
      Tmp    : Asis.Element;
      Ent    : Entity_Id;
   begin
      if Declaration_Kind (El) = An_Ordinary_Type_Declaration then
         Tmp := First_Name (El);
         Ent := R_Node (Tmp);

         Result := Ekind (Ent) in Digits_Kind;
      end if;

      return Result;
   end Needs_Real_Range_Definition;

   -----------------------
   -- Overloading_Index --
   -----------------------

   function Overloading_Index (El : Asis.Element) return String is
      Dcl      : Asis.Element;
      Dcl_N    : Asis.Element;
      Dcl_Name : Program_Text_Access;

      Res : Positive := 1;

      Scope     : Asis.Element := Nil_Element;
      Dcl_Scope : Asis.Element := Nil_Element;

      Detected : Boolean := False;

      procedure Get_Overloding_Index (Dcls : Asis.Element_List);
      --  Parses its argument and counts in Res declarations that overloads El.
      --  If founds the declaration that Is_Equal to El, sets Detected ON and
      --  returns (without increasing Res).
      procedure Get_Overloding_Index (Dcls : Asis.Element_List) is
      begin
         for J in Dcls'Range loop
            if Is_Equal (Dcls (J), Dcl) then
               Detected := True;
               exit;
            end if;

            if Declaration_Kind (Dcls (J)) in
                  A_Procedure_Declaration            |
                  An_Entry_Declaration               |
                  A_Procedure_Instantiation          |
                  A_Function_Declaration             |
                  A_Function_Instantiation
               or else
                (Declaration_Kind (Dcls (J)) in
                   A_Procedure_Body_Declaration       |
                   A_Null_Procedure_Declaration       |
                   A_Procedure_Body_Stub              |
                   A_Function_Body_Declaration        |
                   A_Function_Body_Stub               |
                   An_Expression_Function_Declaration
                 and then
                Acts_As_Spec (Dcls (J)))
               or else
                (Declaration_Kind (Dcls (J)) in
                  A_Procedure_Renaming_Declaration   |
                  A_Function_Renaming_Declaration
                 and then
                not Is_Renaming_As_Body (Dcls (J)))
            then
               if To_Lower_Case (Defining_Name_Image (First_Name (Dcls (J)))) =
                    Dcl_Name.all
               then
                  Res := Res + 1;
               end if;
            end if;

         end loop;
      end Get_Overloding_Index;

   begin
      if Declaration_Kind (El) in
           A_Procedure_Body_Declaration |
           A_Function_Body_Declaration
      then
         Dcl := Corresponding_Declaration (El);

         if Is_Nil (Dcl) then
            if Acts_As_Spec (El) then
               Dcl := El;
            elsif Is_Subunit (El) then
               Dcl := Corresponding_Body_Stub (El);

               if not Acts_As_Spec (Dcl) then
                  Dcl := Corresponding_Declaration (Dcl);
               end if;
            else
               pragma Assert (False);
            end if;
         end if;
      else
         return "";
      end if;

      if Declaration_Kind (Dcl) in
           A_Generic_Procedure_Declaration |
           A_Generic_Function_Declaration
      then
         return "";
      end if;

      Dcl_N := First_Name (Dcl);

      if Defining_Name_Kind (Dcl_N) = A_Defining_Expanded_Name
        or else
         not Has_Homonym (R_Node (Dcl_N))
      then
         return "";
      end if;

      Scope    := Enclosing_Element (Dcl);

      Dcl_Name := new Program_Text'(To_Lower_Case
                        (Defining_Name_Image (Dcl_N)));

      if Is_Nil (Scope) then
         --  We are at the library level
         return "";
      end if;

      if Declaration_Kind (Scope) in
           A_Package_Body_Declaration |
           A_Protected_Body_Declaration
      then
         Dcl_Scope := Corresponding_Declaration (Scope);
      end if;

      if Declaration_Kind (Dcl_Scope) in
           A_Protected_Type_Declaration |
           A_Single_Protected_Declaration
      then
         Get_Overloding_Index
          (Visible_Part_Items (
             (if Declaration_Kind (Dcl_Scope) =
                   A_Protected_Type_Declaration
              then
                 Type_Declaration_View (Dcl_Scope)
              else
                 Object_Declaration_View (Dcl_Scope))));
         if not Detected then
            Get_Overloding_Index
             (Private_Part_Items (
                (if Declaration_Kind (Dcl_Scope) =
                      A_Protected_Type_Declaration
                 then
                    Type_Declaration_View (Dcl_Scope)
                 else
                    Object_Declaration_View (Dcl_Scope))));
         end if;
      elsif Declaration_Kind (Dcl_Scope) in
              A_Package_Declaration |
              A_Generic_Package_Declaration
      then
         Get_Overloding_Index (Visible_Part_Declarative_Items (Dcl_Scope));

         if not Detected then
            Get_Overloding_Index (Private_Part_Declarative_Items (Dcl_Scope));
         end if;
      end if;

      if not Detected then

         if Declaration_Kind (Scope) = A_Protected_Body_Declaration then
            Get_Overloding_Index (Protected_Operation_Items (Scope));

         elsif Declaration_Kind (Scope) in
                 A_Package_Declaration |
                 A_Generic_Package_Declaration
         then
            Get_Overloding_Index (Visible_Part_Declarative_Items (Scope));

         elsif Declaration_Kind (Scope) in
                 A_Procedure_Body_Declaration |
                 A_Function_Body_Declaration  |
                 A_Package_Body_Declaration   |
                 A_Task_Body_Declaration
         then
            Get_Overloding_Index (Body_Declarative_Items (Scope));

         elsif Definition_Kind (Scope) = A_Protected_Definition then
            Get_Overloding_Index (Visible_Part_Items (Scope));

         elsif Statement_Kind (Scope) = A_Block_Statement then
            Get_Overloding_Index (Block_Declarative_Items (Scope));
         else
            pragma Assert (False);
         end if;

         if not Detected then
            if Declaration_Kind (Scope) in
                 A_Package_Declaration |
                 A_Generic_Package_Declaration
            then
               Get_Overloding_Index (Private_Part_Declarative_Items (Scope));
            elsif Definition_Kind (Scope) = A_Protected_Definition then
               Get_Overloding_Index (Private_Part_Items (Scope));
            end if;
         end if;
      end if;

      Free (Dcl_Name);

      if Res = 1 then
         return "";
      else
         return '#' & Image (Res);
      end if;
   end Overloading_Index;

   ------------------------------
   -- Overridden_Interface_Ops --
   ------------------------------

   function Overridden_Interface_Ops
     (Type_Entity : Entity_Id;
      Op_Entity   : Entity_Id)
      return   List_Of_Nodes
   is
      Result : List_Of_Nodes (1 .. 6000);
      --  6000 looks as infinity here
      Res_Last : Natural := 0;

      Next_El             :          Elmt_Id;
      Next_Overridden     :          Entity_Id;
   begin
      Next_El := First_Elmt (Direct_Primitive_Operations (Type_Entity));

      while Present (Next_El) loop
         Next_Overridden := Node (Next_El);

         if Present (Interface_Alias (Next_Overridden))
           and then
            Alias (Next_Overridden) = Op_Entity
         then
            Res_Last            := Res_Last + 1;
            Result (Res_Last)   := Interface_Alias (Next_Overridden);
         end if;

         Next_El := Next_Elmt (Next_El);
      end loop;

      return Result (1 .. Res_Last);

   end Overridden_Interface_Ops;

   ---------------------
   -- Primitive_Owner --
   ---------------------

   function Primitive_Owner (Op : Entity_Id) return Entity_Id is
      Res_Node : Entity_Id := Empty;
      Op_Def   : Node_Id;
      Par_Node : Node_Id;
   begin
      --  Two cases should be processed separately - explicit and implicit
      --  primitives

      if Comes_From_Source (Op) then
         Op_Def := Parent (Op);
         pragma Assert (Nkind (Op_Def) in
                        N_Procedure_Specification | N_Function_Specification);

         if Nkind (Op_Def) = N_Function_Specification
           and then
            Has_Controlling_Result (Op)
         then
            Res_Node := Sinfo.Result_Definition (Op_Def);

            if Nkind (Res_Node) = N_Access_Definition then
               Res_Node := Sinfo.Subtype_Mark (Res_Node);
            end if;

            Res_Node := Entity (Res_Node);
         end if;

         if No (Res_Node) then
            --  This means that we do not have a function with controlling
            --  result, so we have to go through the formal parameter list,
            --  and it can not be No_List or empty

            Par_Node := First (Parameter_Specifications (Op_Def));

            while Present (Par_Node) loop

               if Is_Controlling_Formal
                    (Defining_Identifier (Par_Node))
               then

                  if Nkind (Parameter_Type (Par_Node)) =
                     N_Access_Definition
                  then
                     Res_Node :=
                        Sinfo.Subtype_Mark (Parameter_Type (Par_Node));
                  else
                     Res_Node := Defining_Identifier (Par_Node);
                  end if;

                  Res_Node := Etype (Res_Node);

                  exit;
               end if;

               Par_Node := Next (Par_Node);
            end loop;

         end if;

         pragma Assert (Present (Res_Node));

         if Nkind (Original_Node (Parent (Res_Node))) =
              N_Subtype_Declaration
         then
            Res_Node := Etype (Res_Node);
         end if;

         if Ekind (Res_Node) = E_Incomplete_Type
           and then
            Present (Full_View (Res_Node))
         then
            Res_Node := Full_View (Res_Node);
         end if;

      else
         Res_Node := Parent (Op);

         pragma Assert (Nkind (Res_Node) in
                          N_Private_Extension_Declaration |
                          N_Private_Type_Declaration      |
                          N_Full_Type_Declaration);

         Res_Node := Defining_Identifier (Res_Node);
      end if;

      return Res_Node;
   end Primitive_Owner;

   ----------------------
   -- Raises_Exception --
   ----------------------

   function Raises_Exception (El : Asis.Element) return Boolean is
      Result          : Boolean := False;
      First_Handler   : Boolean := Element_Kind (El) = An_Exception_Handler;
      First_Body_Decl : Boolean :=
        Declaration_Kind (El) in
          A_Procedure_Body_Declaration .. A_Function_Body_Declaration;

      procedure Check_Construct
        (Element :        Asis.Element;
         Control : in out Traverse_Control;
         State   : in out Boolean);
      --  Checks if we have a raise statement or a construct that should be
      --  skipped in the analysis;
      procedure No_Op
        (Element :        Asis.Element;
         Control : in out Traverse_Control;
         State   : in out Boolean);

      procedure Check_For_Raise_Statement is new Traverse_Element
        (Pre_Operation     => Check_Construct,
         Post_Operation    => No_Op,
         State_Information => Boolean);

      Control : Traverse_Control := Continue;

      procedure Check_Construct
        (Element :        Asis.Element;
         Control : in out Traverse_Control;
         State   : in out Boolean)
      is
      begin
         case Element_Kind (Element) is
            when A_Declaration =>

               case Declaration_Kind (Element) is
                  when A_Procedure_Body_Declaration |
                       A_Function_Body_Declaration  =>

                     if First_Body_Decl then
                        First_Body_Decl := False;
                     else
                        Control := Abandon_Children;
                     end if;

                  when others =>
                     Control := Abandon_Children;
               end case;

            when A_Statement =>
               if Statement_Kind (Element) = A_Raise_Statement then
                  State   := True;
                  Control := Terminate_Immediately;
               end if;
            when A_Path =>
               null;
            when An_Exception_Handler =>
               if First_Handler then
                  First_Handler := False;
               else
                  Control := Abandon_Children;
               end if;

            when others =>
               Control := Abandon_Children;
         end case;
      end Check_Construct;

      procedure No_Op
        (Element :        Asis.Element;
         Control : in out Traverse_Control;
         State   : in out Boolean)
      is
      begin
         null;
      end No_Op;

   begin
      Check_For_Raise_Statement (El, Control, Result);

      return Result;
   end Raises_Exception;

   ----------------
   -- Scope_Name --
   ----------------

   function Scope_Name (El : Asis.Element) return String is
   begin
      if not ASIS_UL.Debug.Debug_Flag_JJ
        or else Is_Nil (El)
      then
         return "";
      else
         declare
            Result : constant String := Add_Scope_Name (El);
         begin
            if Result = "" then
               return "";
            else
               return Result & ": ";
            end if;
         end;
      end if;

   end Scope_Name;

   -------------------------------
   -- Self_Ref_Discr_Constraint --
   -------------------------------

   function Self_Ref_Discr_Constraint
     (Constr : Asis.Element)
      return   Boolean
   is
      Result    : Boolean := False;
      Type_Name : Asis.Element;
   begin

      if Constraint_Kind (Constr) /= A_Discriminant_Constraint then
         return False;
      end if;

      Type_Name :=
        Enclosing_Element (Enclosing_Element (Enclosing_Element (Constr)));

      if Declaration_Kind (Type_Name) /= A_Component_Declaration then
         return False;
      end if;

      while Declaration_Kind (Type_Name) /= An_Ordinary_Type_Declaration loop
         Type_Name := Enclosing_Element (Type_Name);
      end loop;

      Type_Name := First_Name (Type_Name);

      declare
         D_Associations : constant Asis.Element_List :=
           Discriminant_Associations (Constr);
         D_Value : Asis.Element;
         Pref    : Asis.Element;
      begin
         for J in D_Associations'Range loop
            D_Value := Discriminant_Expression (D_Associations (J));

            if Is_Access_Attribute (D_Value) then
               Pref := Prefix (D_Value);

               if Expression_Kind (Pref) = An_Identifier then
                  Pref := Corresponding_Name_Declaration (Pref);

                  while Declaration_Kind (Pref) in
                          An_Incomplete_Type_Declaration       |
                          A_Tagged_Incomplete_Type_Declaration |
                          A_Private_Type_Declaration            |
                          A_Private_Extension_Declaration
                  loop
                     Pref := Corresponding_Type_Completion (Pref);
                  end loop;

                  if Declaration_Kind (Pref) =
                       An_Ordinary_Type_Declaration
                  then
                     Pref := First_Name (Pref);

                     if Is_Equal (Pref, Type_Name) then
                        Result := True;
                        exit;
                     end if;
                  end if;

               end if;
            end if;

         end loop;
      end;

      return Result;
   end Self_Ref_Discr_Constraint;

   -------------------------------------
   -- Storage_Order_Defined_By_Pragma --
   -------------------------------------

   function Storage_Order_Defined_By_Pragma
     (E    : Asis.Element)
      return Boolean
   is
      Type_Entity : Entity_Id;
      Next_Pragma : Node_Id;
      Pragma_Arg  : Node_Id;
      Result      : Boolean := False;
   begin
      Type_Entity := R_Node (E);
      Next_Pragma := Next (Type_Entity);
      Type_Entity := Defining_Identifier (Type_Entity);

      while Present (Next_Pragma) loop
         if Nkind (Next_Pragma) = N_Attribute_Definition_Clause
          and then
            Is_Rewrite_Substitution (Next_Pragma)
          and then
            Nkind (Original_Node (Next_Pragma)) = N_Pragma
          and then
            Chars (Next_Pragma) = Name_Scalar_Storage_Order
         then
            Pragma_Arg := Sinfo.Name (Next_Pragma);

            if Nkind (Pragma_Arg) = N_Identifier
              and then
               Entity (Pragma_Arg) = Type_Entity
            then
               Result := True;
               exit;
            end if;
         end if;

         Next_Pragma := Next (Next_Pragma);
      end loop;

      return Result;
   end Storage_Order_Defined_By_Pragma;

   -----------------
   -- Unwind_Type --
   -----------------

   function Unwind_Type
     (D               : Asis.Element;
      Stop_At_Private : Boolean := False)
      return            Asis.Element
   is
      Result : Asis.Element := D;
      T_Def  : Asis.Element;
   begin
      case Declaration_Kind (Result) is
         when A_Task_Type_Declaration              |
              A_Protected_Type_Declaration         |
              An_Incomplete_Type_Declaration       |
              A_Tagged_Incomplete_Type_Declaration |
              A_Formal_Type_Declaration            |
              A_Formal_Incomplete_Type_Declaration =>
            null;

         when An_Ordinary_Type_Declaration =>
            T_Def := Type_Declaration_View (Result);

            if Asis.Elements.Type_Kind (T_Def) in
                 A_Derived_Type_Definition             |
                 A_Derived_Record_Extension_Definition
            then
               Result := Unwind_Type (Corresponding_Root_Type (T_Def));
            end if;

         when A_Private_Type_Declaration      |
              A_Private_Extension_Declaration =>
            if not Stop_At_Private then
               Result := Unwind_Type (Corresponding_Type_Completion (Result));
            end if;

         when A_Subtype_Declaration =>
            Result := Unwind_Type (Corresponding_First_Subtype (Result));

         when others =>
            pragma Assert (False);
            null;
      end case;

      return Result;
   end Unwind_Type;

   -------------------------------
   -- Used_To_Pass_Actual_Subpr --
   -------------------------------

   function Used_To_Pass_Actual_Subpr (El : Asis.Element) return Boolean is
      Result : Boolean := False;
   begin

      if Declaration_Kind (El) in A_Procedure_Renaming_Declaration ..
        A_Function_Renaming_Declaration
      then
         Result := Pass_Generic_Actual (Node (El));
      end if;

      return Result;
   end Used_To_Pass_Actual_Subpr;

end Gnatcheck.ASIS_Utilities;
------------------------------------------------
