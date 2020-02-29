------------------------------------------------------------------------------
--                                                                          --
--                          GNATCHECK COMPONENTS                            --
--                                                                          --
--       A S I S _ U L . G L O B A L _ S T A T E . U T I L I T I E S        --
--                                                                          --
--                                 B o d y                                  --
--                                                                         --
--                     Copyright (C) 2007-2018, AdaCore                     --
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

with Ada.Characters.Handling;    use Ada.Characters.Handling;

with Asis;                       use Asis;
with Asis.Clauses;               use Asis.Clauses;
with Asis.Declarations;          use Asis.Declarations;
with Asis.Definitions;           use Asis.Definitions;
with Asis.Elements;              use Asis.Elements;
with Asis.Expressions;           use Asis.Expressions;
with Asis.Extensions;            use Asis.Extensions;
with Asis.Extensions.Flat_Kinds; use Asis.Extensions.Flat_Kinds;
with Asis.Iterator;
with Asis.Statements;            use Asis.Statements;
with Asis.Exceptions;            use Asis.Exceptions;

with Asis.Set_Get;               use Asis.Set_Get;

with A4G.A_Sem;                  use A4G.A_Sem;
with A4G.A_Types;                use A4G.A_Types;
with A4G.Asis_Tables;            use A4G.Asis_Tables;
with A4G.Contt.UT;               use A4G.Contt.UT;
with A4G.Int_Knds;               use A4G.Int_Knds;
with A4G.Mapping;                use A4G.Mapping;

with ASIS_UL.Utilities;          use ASIS_UL.Utilities;

with Atree;                      use Atree;
with Einfo;                      use Einfo;
with Elists;                     use Elists;
with Nlists;                     use Nlists;
with Sinfo;                      use Sinfo;

package body ASIS_UL.Global_State.Utilities is

   ------------------------
   --  Local subprograms --
   ------------------------

   function Is_Default_Initialization (Expr : Asis.Element) return Boolean;
   pragma Unreferenced (Is_Default_Initialization); -- Needed???
   --  Checks that the argument Element is a default initialization expression
   --  Returns False for any unexpected Element. Also returns false in case if
   --  this is an initialization expression from a single protected declaration
   --  (becauce this expression is from executable context, see the use of this
   --  function)
   --
   --  Expected Declaration_Kinds:
   --     A_Discriminant_Specification
   --     A_Component_Declaration
   --     A_Parameter_Specification

   procedure Check_Component_Definition
     (Element :        Asis.Element;
      Control : in out Traverse_Control;
      State   : in out Boolean);
   --  If Element is of A_Component_Declaration kind, checks if it contains
   --  an initialization expression, and if it does, sets State ON and
   --  terminates traversing. Skips (ny setting Control to Abandon_Children)
   --  parts of the Element that cannot contain component declarations)

   procedure No_Op
     (Element :        Asis.Element;
      Control : in out Traverse_Control;
      State   : in out Boolean);
   --  Does nothing

   procedure Check_Components is new Asis.Iterator.Traverse_Element
     (State_Information => Boolean,
      Pre_Operation     => Check_Component_Definition,
      Post_Operation    => No_Op);
   --  Checks if the argument type definition needs and initialization
   --  procedure.

   function Get_Type (S : Node_Id) return Entity_Id;
   --  Supposing that S is a subtype mark, tries to get its actual type by
   --  unvinding subtyping and derivations. In case of private or incomplete
   --  type, gets to the full view.

   ----------------------------------
   -- Can_Be_Embedded_In_Equiality --
   ----------------------------------

   function Can_Be_Embedded_In_Equiality (El : Asis.Element) return Boolean is
      Result      : Boolean := False;
      Entity_El   : Asis.Element;
      Entity_Node : Entity_Id;
      Next_Par    : Entity_Id;
   begin
      if Is_Bool_Eq_Declaration (El) then
         Entity_El   := First_Name (El);
         Entity_Node := Node (Entity_El);

         if Is_Primitive (Entity_Node) then
            Entity_Node := Parent (Entity_Node);
            Entity_Node := First (Parameter_Specifications (Entity_Node));
            Next_Par    := Next (Entity_Node);

            Entity_Node := Parameter_Type (Entity_Node);
            Next_Par    := Parameter_Type (Next_Par);

            Entity_Node := Get_Type (Entity_Node);
            Next_Par    := Get_Type (Next_Par);

         end if;

         Result := Entity_Node = Next_Par;

      end if;

      return Result;
   end Can_Be_Embedded_In_Equiality;

   ----------------------
   -- Can_Create_Tasks --
   ----------------------

   function Can_Create_Tasks (El : Asis.Element) return Boolean is
   begin
      return
        Declaration_Kind (El) in
          A_Variable_Declaration .. A_Constant_Declaration
       or else
        Expression_Kind (El) = An_Allocation_From_Subtype;
   end Can_Create_Tasks;

   ---------------------------
   -- Corresponding_Element --
   ---------------------------

   function Corresponding_Element (El : Asis.Element) return Asis.Element is
      Result  : Asis.Element := El;
   begin

      case Flat_Element_Kind (Result) is
         when A_Task_Body_Declaration =>

            if Is_Subunit (Result) then
               Result := Corresponding_Body_Stub (Result);
            end if;

            Result := Corresponding_Declaration (Result);

         when A_Procedure_Declaration |
              A_Function_Declaration  =>

--            if Is_Part_Of_Inherited (Result) then
--               Result := Corresponding_Declaration (Result);
--               Result := Corresponding_Element (Result);
--            els
            if Is_Implicit_Neq_Declaration (Result) then
               Result := Corresponding_Equality_Operator (Result);
               Result := Corresponding_Element (Result);
            end if;

            if Special_Case (Result) = Expanded_Subprogram_Instantiation then
               Result := Enclosing_Element (Result);
            end if;

         when A_Package_Body_Declaration |
              An_Entry_Body_Declaration  =>
            Result := Corresponding_Declaration (Result);

         when A_Procedure_Body_Declaration       |
              A_Function_Body_Declaration        |
              An_Expression_Function_Declaration |
              A_Procedure_Body_Stub              |
              A_Function_Body_Stub               =>

            Result := Corresponding_Declaration (Result);

            if Is_Nil (Result) then

               if Is_Subunit (El) then
                  Result :=
                    Corresponding_Element (Corresponding_Body_Stub (El));
               else
                  --  No explicit spec
                  Result := El;

               end if;

            end if;

            if Special_Case (Result) = Expanded_Subprogram_Instantiation then
               Result := Enclosing_Element (Result);
            end if;

         when A_Procedure_Renaming_Declaration |
              A_Function_Renaming_Declaration =>
            Result := Get_Renamed_Subprogram (El);

         when An_Accept_Statement =>
            Result :=
              Corresponding_Name_Declaration
                (Accept_Entry_Direct_Name (Result));

         when others =>
            null;
      end case;

      return Result;
   end Corresponding_Element;

   ----------------------------------------
   -- Can_Create_Reference_To_Subprogram --
   ----------------------------------------

   function Can_Create_Reference_To_Subprogram
     (El   : Asis.Element)
      return Boolean
   is
      Result : Boolean := False;
   begin

      case Attribute_Kind (El) is
         when An_Access_Attribute           |
              An_Unchecked_Access_Attribute |
              An_Address_Attribute          =>
            Result := True;
         when An_Implementation_Defined_Attribute =>
            Result :=
              To_Lower (To_String
                (Name_Image (Attribute_Designator_Identifier (El)))) =
              "unrestricted_access";
         when others =>
            null;
      end case;

      return Result;
   end Can_Create_Reference_To_Subprogram;

   --------------------------------
   -- Check_Component_Definition --
   --------------------------------

   procedure Check_Component_Definition
     (Element :        Asis.Element;
      Control : in out Traverse_Control;
      State   : in out Boolean)
   is
   begin
      case Flat_Element_Kind (Element) is
         when A_Component_Declaration =>
            if not Is_Nil (Initialization_Expression (Element)) then
               State   := True;
               Control := Terminate_Immediately;
            end if;
         when A_Protected_Definition                |
              A_Derived_Record_Extension_Definition |
              A_Record_Type_Definition              |
              A_Tagged_Record_Type_Definition       |
              A_Record_Definition                   |
              A_Variant_Part                        |
              A_Variant                             =>
            null;
         when others =>
            Control := Abandon_Children;
      end case;
   end Check_Component_Definition;

   ---------------------
   -- Enclosing_Scope --
   ---------------------

   function Enclosing_Scope (El : Asis.Element) return Asis.Element is
      Result : Asis.Element := El;
   begin

      while not Is_Nil (Result) loop

         if Is_Subunit (Result) then
            Result := Corresponding_Body_Stub (Result);
         else
            Result := Enclosing_Element (Result);
            begin
               --  For subprogram parameters, consider the scope of the
               --  corresponding body
               if Compute_Global_Objects_Accessed then
                  Result := Corresponding_Body (Result);
               end if;
            exception
               when ASIS_Inappropriate_Element =>
                  null;  --  No corresponding body defined
            end;
         end if;

         exit when Is_Scope (Result);
      end loop;

      return Result;
   end Enclosing_Scope;

   -----------------------
   -- Get_Defining_Name --
   -----------------------

   function Get_Defining_Name (El : Asis.Element) return Asis.Element is
      Result : Asis.Element := First_Name (El);
   begin
      if Defining_Name_Kind (Result) = A_Defining_Expanded_Name then
         Result := Defining_Selector (Result);
      end if;

      return Result;
   end Get_Defining_Name;

   ----------------------------
   -- Get_Renamed_Subprogram --
   ----------------------------

   function Get_Renamed_Subprogram (El : Asis.Element) return Asis.Element is
      Result : Asis.Element;
   begin
      Result := Corresponding_Base_Entity (El);

      if Expression_Kind (Result) = A_Selected_Component then
         Result := Selector (Result);
      end if;

      case Expression_Kind (Result) is
         when An_Identifier |
              An_Operator_Symbol =>
            Result := Corresponding_Name_Declaration (Result);

            if Declaration_Kind (Result) in A_Procedure_Renaming_Declaration ..
                 A_Function_Renaming_Declaration
            then
               Result := Get_Renamed_Subprogram (Result);
            end if;

         when An_Attribute_Reference |
              An_Enumeration_Literal =>
            null;
         when others =>
            Result := Nil_Element;
      end case;

      return Result;
   end Get_Renamed_Subprogram;

   --------------
   -- Get_Type --
   --------------

   function Get_Type (S : Node_Id) return Entity_Id is
      Result : Entity_Id := S;
   begin
      if Nkind (S) in N_Has_Etype then
         Result := Etype (Result);

         while Etype (Result) /= Result loop
            Result := Etype (Result);
         end loop;

         while Ekind (Result) in E_Incomplete_Type | E_Private_Type
             and then
               Present (Full_View (Result))
         loop
            Result := Full_View (Result);
         end loop;

      end if;

      return Result;
   end Get_Type;

   -------------------------
   -- Has_Discr_Init_Proc --
   -------------------------

   function Has_Discr_Init_Proc (El : Asis.Element) return Boolean is
      Discr_Part : Asis.Element;
      Result     : Boolean := False;
   begin
      if Declaration_Kind (El) in
           An_Ordinary_Type_Declaration .. A_Protected_Type_Declaration
      then
         Discr_Part := Discriminant_Part (El);

         if Definition_Kind (Discr_Part) = A_Known_Discriminant_Part then
            Discr_Part := Discriminants (Discr_Part) (1);
            Result := not Is_Nil (Initialization_Expression (Discr_Part));
         elsif Is_Nil (Discr_Part) then
            Discr_Part := Type_Declaration_View (El);

            if Asis.Elements.Type_Kind (Discr_Part) in
              A_Derived_Type_Definition ..
                A_Derived_Record_Extension_Definition
            then
               Discr_Part := Corresponding_Parent_Subtype (Discr_Part);

               if Declaration_Kind (Discr_Part) = A_Subtype_Declaration then
                  Discr_Part := Corresponding_First_Subtype (Discr_Part);
               end if;

               Result := Has_Discr_Init_Proc (Discr_Part);
            end if;
         end if;

      end if;

      return Result;
   end Has_Discr_Init_Proc;

   ------------------------
   -- Has_Type_Init_Proc --
   ------------------------

   function Has_Type_Init_Proc (El : Asis.Element) return Boolean is
      Tmp     : Asis.Element;
      Control : Traverse_Control := Continue;
      Result  : Boolean          := False;
   begin

      if Declaration_Kind (El) in
           An_Ordinary_Type_Declaration |
           A_Protected_Type_Declaration
      then
         Tmp := Type_Declaration_View (El);

         case Flat_Element_Kind (Tmp) is

            when A_Derived_Type_Definition =>
               Tmp    := Parent_Subtype_Indication (Tmp);
               Tmp    := Get_Subtype_Structure (Tmp);
               Result := Has_Type_Init_Proc (Tmp);

            when An_Unconstrained_Array_Definition |
                 A_Constrained_Array_Definition    =>
               Tmp := Array_Component_Definition (Tmp);
               Tmp := Component_Definition_View  (Tmp);

               if Definition_Kind (Tmp) = A_Subtype_Indication then
                  --  we are not interested in components that are defined
                  --  by An_Access_Definition
                  Tmp    := Get_Subtype_Structure (Tmp);
                  Result := Has_Type_Init_Proc (Tmp);
               end if;

            when A_Protected_Definition                |
                 A_Derived_Record_Extension_Definition |
                 A_Record_Type_Definition              |
                 A_Tagged_Record_Type_Definition       =>

               Check_Components (Tmp, Control, Result);

               if not Result
                 and then
                   Asis.Elements.Type_Kind (Tmp) =
                   A_Derived_Record_Extension_Definition
               then
                  Tmp    := Parent_Subtype_Indication (Tmp);
                  Tmp    := Get_Subtype_Structure (Tmp);
                  Result := Has_Type_Init_Proc (Tmp);
               end if;

            when others =>
               null;
         end case;

      end if;

      return Result;
   end Has_Type_Init_Proc;

   ----------------------------
   -- Implemented_Operations --
   ----------------------------

   function Implemented_Operations
     (Op   : Asis.Element)
      return Asis.Element_List
   is
      Arg_Node       : Node_Id;
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

      if not Is_Part_Of_Inherited (Op) then
         Arg_Node := Node (Op);
         Arg_Node := Defining_Unit_Name (Specification (Arg_Node));

         if Comes_From_Source (Arg_Node) then
            Type_Node := R_Node (Enclosing_Element (Primitive_Owner (Op)));
         else
            Type_Node := Parent (Arg_Node); --  ???
         end if;
      else
         Arg_Node  := Node_Field_1 (Op);
         Type_Node := Parent (Arg_Node); --  ???
--         pragma Assert
--           (False,
--            "case of implicit inherited primitives, not implemented yet");
--         return Nil_Element_List;
      end if;

      --  First, we have to get the list of primitive operations Op belongs
      --  to:

      Type_Node := Defining_Identifier (Type_Node);

      if Ekind (Type_Node) in Private_Kind
        and then
         not Is_Tagged_Type (Type_Node)
      then
         --  Non-tagged private type completed by a tagged type
         Type_Node := Full_View (Type_Node);
      end if;

      --  pragma Assert (Has_Primitive_Operations (Type_Node)); ???

      Asis_Element_Table.Init;

      Prim_Ops  := Primitive_Operations (Type_Node);

      Next_Pr_El := First_Elmt (Prim_Ops);

      while Present (Next_Pr_El) loop
         Next_Primitive := Node (Next_Pr_El);

         if (Present (Interface_Alias (Next_Primitive))
            and then
             Alias (Next_Primitive) = Arg_Node)
           or else
             (Next_Primitive = Arg_Node
            and then
             Present (Overridden_Operation (Next_Primitive)))
           or else
             (Next_Primitive = Arg_Node
               --  This part of the condition most probably should be changed
               --  for multiple inheritance
            and then
              not Comes_From_Source (Next_Primitive)
               --  Is this check enough?
             )
         then

            if Present (Interface_Alias (Next_Primitive)) then
               Next_Primitive := Interface_Alias (Next_Primitive);
            elsif Present (Overridden_Operation (Next_Primitive)) then
               Next_Primitive := Overridden_Operation (Next_Primitive);
            else
               --  Case of implicit inherited operation
               Next_Primitive := Alias (Next_Primitive);
            end if;

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
                                    (Encl_Cont_Id (Op),
                                     Next_Primitive));

            if Present (Res_NF_1) then

               if Is_From_Instance (Res_NF_1) then
                  Set_From_Instance (Res_Element, True);
               else
                  Set_From_Instance (Res_Element, False);
               end if;

            end if;

--            Asis_Element_Table.Append (Res_Element);
            Add_New_Element (Res_Element);
         end if;

         Next_Pr_El := Next_Elmt (Next_Pr_El);
      end loop;

      return Asis.Element_List
               (Asis_Element_Table.Table (1 .. Asis_Element_Table.Last));
   end Implemented_Operations;

   -------------
   -- Is_Call --
   -------------

   function Is_Call (El : Asis.Element) return Boolean is
      Result : Boolean := False;
   begin
      case Flat_Element_Kind (El) is
         when A_Function_Call            |
              A_Procedure_Call_Statement =>

            Result := True;
         when An_Entry_Call_Statement =>
            Result := True;
         when An_Indexed_Component =>
            Result := Is_Generalized_Indexing (El);
         when others =>
            null;
      end case;

      return Result;
   end Is_Call;

   ---------------------------------------
   -- Is_Call_To_Default_Null_Procedure --
   ---------------------------------------

   function Is_Call_To_Default_Null_Procedure
     (El   : Asis.Element)
      return Boolean
   is
      Result : Boolean := False;
      Tmp    : Asis.Element;
   begin

      if Statement_Kind (El) = A_Procedure_Call_Statement
        and then
         Is_Nil (Corresponding_Called_Entity (El))
      then
         Tmp := Called_Name (El);

         if Expression_Kind (Tmp) = A_Selected_Component then
            Tmp := Normalize_Reference (Tmp);
         end if;

         if Expression_Kind (Tmp) = An_Identifier then
            Result := Is_Default_For_Null_Procedure (Tmp);
         end if;

      end if;

      return Result;
   end Is_Call_To_Default_Null_Procedure;

   -------------------------------------
   -- Is_Call_To_Predefined_Operation --
   -------------------------------------

   function Is_Call_To_Predefined_Operation
     (Call : Asis.Element)
      return Boolean
   is
      Result    : Boolean := False;
      Pref_Node : Node_Id;

      function Is_Call_To_Predefined_Op_Of_User_Type
        (N    : Node_Id)
         return Boolean;
      --  This function covers the cases not covered by
      --  A4G.A_Sem.Defined_In_Standard. For example, a predefined
      --  concatenation for a user-defined one-dimentioal array type

      function Is_Call_To_Predefined_Op_Of_User_Type
        (N    : Node_Id)
         return Boolean
      is
         N_Entity : Node_Id := Empty;
         Result   : Boolean := False;
      begin

         if Nkind (N) in N_Has_Entity then
            N_Entity := Entity (N);
         elsif Nkind (N) in Sinfo.N_Entity then
            N_Entity := N;
         end if;

         Result :=
           Present (N_Entity)
          and then
           not Comes_From_Source (N_Entity)
          and then
           No (Parent (N_Entity))
          and then
           Is_Intrinsic_Subprogram (N_Entity);

         return Result;

      end Is_Call_To_Predefined_Op_Of_User_Type;

   begin

      if Is_Static (Call) then
         Result := True;
      elsif Expression_Kind (Call) = A_Function_Call
        and then
         Function_Call_Parameters (Call)'Length in 1 .. 2
      then
         --  We use the direct access into the GNAT tree
         Pref_Node := R_Node (Call);

         if Nkind (Pref_Node) not in N_Op then
            Pref_Node := Node (Call);
         end if;

         if Nkind (Pref_Node) in N_Op
          and then
            (Defined_In_Standard (Pref_Node)
            or else
             Is_Call_To_Predefined_Op_Of_User_Type (Pref_Node))
         then
            Result := True;
         end if;

      end if;

      return Result;

   end Is_Call_To_Predefined_Operation;

   -------------------------------
   -- Is_Default_Initialization --
   -------------------------------

   function Is_Default_Initialization (Expr : Asis.Element) return Boolean is
      Encl_El : Asis.Element;
      Result  : Boolean := False;
   begin

      if Element_Kind (Expr) = An_Expression then
         Encl_El := Enclosing_Element (Expr);

         if (Declaration_Kind (Encl_El) in
               A_Discriminant_Specification .. A_Component_Declaration
              or else
             Declaration_Kind (Encl_El) = A_Parameter_Specification)
          and then
            Is_Equal (Expr, Initialization_Expression (Encl_El))
         then
            Encl_El := Enclosing_Element (Enclosing_Element (Encl_El));

            if Declaration_Kind (Encl_El) /=
               A_Single_Protected_Declaration
            then
               Result := True;
            end if;

         end if;

      end if;

      return Result;
   end Is_Default_Initialization;

   ---------------------------------------
   -- Is_Declaration_Of_Callable_Entity --
   ---------------------------------------

   function Is_Declaration_Of_Callable_Entity
     (El   : Asis.Element)
      return Boolean
   is
      Result : Boolean := False;
   begin
      case Declaration_Kind (El) is
         when A_Procedure_Instantiation |
              A_Function_Instantiation  |
              A_Task_Type_Declaration   |
              A_Single_Task_Declaration |
              An_Entry_Declaration      =>
            Result := True;

         when A_Procedure_Declaration      |
              A_Null_Procedure_Declaration |
              A_Function_Declaration       =>
            Result := True;

         when A_Procedure_Body_Stub |
              A_Function_Body_Stub  =>

            if Declaration_Kind (Corresponding_Declaration (El)) not in
                 A_Generic_Declaration
            then
               Result := True;
            end if;

         when others =>
            null;
      end case;

      if Compute_Global_Objects_Accessed and then
        (Declaration_Kind (El) = A_Package_Declaration or else
           Declaration_Kind (El) = A_Generic_Package_Declaration or else
           Declaration_Kind (El) = A_Formal_Procedure_Declaration or else
           Declaration_Kind (El) = A_Formal_Function_Declaration)
      then
         Result := True;
      end if;

      return Result;
   end Is_Declaration_Of_Callable_Entity;

   ---------------------------------
   -- Is_Non_Executable_Construct --
   ---------------------------------

   function Is_Non_Executable_Construct (El : Asis.Element) return Boolean is
      Corr_Decl : Asis.Element;
      Tmp       : Asis.Element;
      Result    : Boolean := False;
   begin

      --  Some nonexecutable constructs may include some other non-executable
      --  constructs. From the performance point of view, the more code we
      --  exclude as non-executable when building the call graph - the better.
      --  That's why we consider the whole task type declaration as a
      --  non-executable context instead of excluding separately the profiles
      --  in the entry declarations. But we cannot exclude the tack object -
      --  it may contain function calls in representation clauses

      case Flat_Element_Kind (El) is
         when A_Private_Type_Declaration      |
              A_Parameter_Specification       | -- in a body declaration
              A_Generic_Procedure_Declaration |
              A_Generic_Function_Declaration  |
              A_Generic_Package_Declaration =>
            Result := True;

         when A_Procedure_Body_Declaration       |
              A_Function_Body_Declaration        |
              An_Expression_Function_Declaration |
              A_Package_Body_Declaration         |
              A_Task_Body_Declaration            |
              A_Protected_Body_Declaration       =>

            if Is_Subunit (El) then

               --  We have to traverse a possible chain of "nested" subunits
               Corr_Decl := Corresponding_Body_Stub (El);
               Tmp       := Corresponding_Declaration (Corr_Decl);

               if not Is_Nil (Tmp) then
                  Corr_Decl := Tmp;
               end if;

               if Flat_Element_Kind (Corr_Decl) in
                  A_Generic_Procedure_Declaration ..
                    A_Generic_Package_Declaration
               then
                  Result := True;
               else
                  --  We are in some unit, and we do not know if this
                  --  unit is an executable unit
                  Corr_Decl := Enclosing_Element (Corr_Decl);

                  while not Is_Nil (Corr_Decl) loop
                     Result := Is_Non_Executable_Construct (Corr_Decl);

                     if Result then
                        exit;
                     else
                        Corr_Decl := Enclosing_Element (Corr_Decl);
                     end if;

                  end loop;

               end if;

            else

               if Flat_Element_Kind (Corresponding_Declaration (El)) in
                  A_Generic_Procedure_Declaration ..
                    A_Generic_Package_Declaration
               then
                  Result := True;
               end if;

            end if;

         when others =>
            null;
      end case;

      return Result;
   end Is_Non_Executable_Construct;

   --------------------------------------
   -- Is_Predefined_Operation_Renaming --
   --------------------------------------

   function Is_Predefined_Operation_Renaming
     (Ren  : Asis.Element)
      return Boolean
   is
      Decl : Asis.Element := Ren;

      Op_Entity : Entity_Id;
      Result    : Boolean := False;
   begin
      if Is_Implicit_Neq_Declaration (Decl) then
         Decl := Corresponding_Equality_Operator (Decl);
      end if;

      if Declaration_Kind (Decl) = A_Function_Renaming_Declaration then
         Op_Entity := Defining_Unit_Name (Specification (Node (Decl)));

         if Nkind (Op_Entity) /= N_Defining_Program_Unit_Name
           and then
            Ekind (Op_Entity) = E_Function
         then

            while Present (Alias (Op_Entity)) loop
               Op_Entity := Alias (Op_Entity);
            end loop;

            Result := Defined_In_Standard (Op_Entity);

         end if;

      end if;

      return Result;
   end Is_Predefined_Operation_Renaming;

   --------------------------------------
   -- Is_Renaming_Of_Null_Proc_Default --
   --------------------------------------

   function Is_Renaming_Of_Null_Proc_Default
     (El   : Asis.Element)
      return Boolean
   is
      Result : Boolean := False;
      Tmp    : Asis.Element;
   begin

      if Declaration_Kind (El) = A_Procedure_Renaming_Declaration then
         Tmp := Corresponding_Base_Entity (El);

         if Expression_Kind (Tmp) = A_Selected_Component then
            Tmp := Selector (Tmp);
         end if;

         if Expression_Kind (Tmp) = An_Identifier then
            Result := Is_Default_For_Null_Procedure (Tmp);
         end if;

      end if;

      return Result;
   end Is_Renaming_Of_Null_Proc_Default;

   --------------
   -- Is_Scope --
   --------------

   function Is_Scope (El : Asis.Element) return Boolean is
      Result : Boolean := False;
   begin

      case Flat_Element_Kind (El) is
         when A_Procedure_Body_Declaration       |
              A_Function_Body_Declaration        |
              An_Expression_Function_Declaration |
              A_Task_Body_Declaration            |
              An_Entry_Body_Declaration          |
              An_Accept_Statement                =>
            Result := True;
         when others =>
            null;
      end case;

      if Compute_Global_Objects_Accessed and then
        (Flat_Element_Kind (El) = A_Package_Declaration or else
           Flat_Element_Kind (El) = A_Package_Body_Declaration or else
           Flat_Element_Kind (El) = A_Generic_Package_Declaration)
      then
         Result := True;
      end if;

      return Result;
   end Is_Scope;

   --------------------------------------
   -- Is_Stream_Attribute_Redefinition --
   --------------------------------------

   function Is_Stream_Attribute_Redefinition
     (Element : Asis.Element)
      return    Boolean
   is
      Result          : Boolean := False;
      Attr_Designator : Asis.Element;
   begin
      if Representation_Clause_Kind (Element) =
         An_Attribute_Definition_Clause
      then
         Attr_Designator := Representation_Clause_Name (Element);

         case Attribute_Kind (Attr_Designator) is
            when An_Input_Attribute  |
                 An_Output_Attribute |
                 A_Read_Attribute    |
                 A_Write_Attribute   =>
               Result := True;
            when others =>
               null;
         end case;

      end if;

      return Result;
   end Is_Stream_Attribute_Redefinition;

   -----------
   -- No_Op --
   -----------

   procedure No_Op
     (Element :        Asis.Element;
      Control : in out Traverse_Control;
      State   : in out Boolean)
   is
   begin
      null;
   end No_Op;

end ASIS_UL.Global_State.Utilities;
