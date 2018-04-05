------------------------------------------------------------------------------
--                                                                          --
--                 ASIS-for-GNAT IMPLEMENTATION COMPONENTS                  --
--                                                                          --
--                     A S I S . D E F I N I T I O N S                      --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--            Copyright (C) 1995-2015, Free Software Foundation, Inc.       --
--                                                                          --
-- ASIS-for-GNAT is free software; you can redistribute it and/or modify it --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software  Foundation;  either version 3,  or (at your option)  any later --
-- version.  ASIS-for-GNAT  is  distributed  in  the  hope  that it will be --
-- useful,  but  WITHOUT ANY WARRANTY; without even the implied warranty of --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     --
--                                                                          --
--                                                                          --
--                                                                          --
--                                                                          --
--                                                                          --
-- You should have  received  a copy of the  GNU General Public License and --
-- a copy of the  GCC Runtime Library Exception  distributed with GNAT; see --
-- the files COPYING3 and COPYING.RUNTIME respectively.  If not, see        --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
-- ASIS-for-GNAT was originally developed  by the ASIS-for-GNAT team at the --
-- Software  Engineering  Laboratory  of  the Swiss  Federal  Institute  of --
-- Technology (LGL-EPFL) in Lausanne,  Switzerland, in cooperation with the --
-- Scientific  Research  Computer  Center of  Moscow State University (SRCC --
-- MSU), Russia,  with funding partially provided  by grants from the Swiss --
-- National  Science  Foundation  and  the  Swiss  Academy  of  Engineering --
-- Sciences.  ASIS-for-GNAT is now maintained by  AdaCore                   --
-- (http://www.adacore.com).                                                --
--                                                                          --
------------------------------------------------------------------------------

pragma Ada_2012;

with Asis.Declarations; use Asis.Declarations;
with Asis.Elements;     use Asis.Elements;
with Asis.Errors;       use Asis.Errors;
with Asis.Exceptions;   use Asis.Exceptions;
with Asis.Expressions;  use Asis.Expressions;
with Asis.Extensions;   use Asis.Extensions;

with Asis.Set_Get;      use  Asis.Set_Get;

with A4G.A_Sem;         use A4G.A_Sem;
with A4G.Asis_Tables;   use A4G.Asis_Tables;
with A4G.Contt.UT;      use A4G.Contt.UT;
with A4G.Mapping;       use A4G.Mapping;
with A4G.Norm;          use A4G.Norm;
with A4G.Stand;         use A4G.Stand;
with A4G.Vcheck;        use A4G.Vcheck;

with Atree;             use Atree;
with Einfo;             use Einfo;
with Namet;             use Namet;
with Nlists;            use Nlists;
with Sinfo;             use Sinfo;
with Snames;            use Snames;

package body Asis.Definitions is

   Package_Name : constant String := "Asis.Definitions.";

------------------------------------------------------------------------------

   ---------------------------
   -- ASIS 2005 Draft stuff --
   ---------------------------

   ---------------------------------------------
   -- Anonymous_Access_To_Object_Subtype_Mark --
   ---------------------------------------------

   function Anonymous_Access_To_Object_Subtype_Mark
     (Definition : Asis.Definition)
      return       Asis.Expression
   is
      Arg_Kind : constant Internal_Element_Kinds := Int_Kind (Definition);
      Arg_Node :          Node_Id;
   begin
      Check_Validity
        (Definition, Package_Name & "Anonymous_Access_To_Object_Subtype_Mark");

      if not (Arg_Kind = An_Anonymous_Access_To_Variable or else
              Arg_Kind = An_Anonymous_Access_To_Constant)
      then
         Raise_ASIS_Inappropriate_Element
           (Diagnosis => Package_Name &
                         "Anonymous_Access_To_Object_Subtype_Mark",
            Wrong_Kind => Arg_Kind);
      end if;

      Arg_Node := Node (Definition);

      return Node_To_Element_New
               (Node             => Subtype_Mark (Arg_Node),
                Starting_Element => Definition);

   exception
      when ASIS_Inappropriate_Element =>
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information
              (Argument   => Definition,
               Outer_Call => Package_Name &
                             "Anonymous_Access_To_Object_Subtype_Mark");
         end if;

         raise;
      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name  => Package_Name &
                           "Anonymous_Access_To_Object_Subtype_Mark",
            Ex          => Ex,
            Arg_Element => Definition);
   end Anonymous_Access_To_Object_Subtype_Mark;

   -------------------------------
   -- Component_Definition_View --
   -------------------------------

   function Component_Definition_View
     (Component_Definition : Asis.Component_Definition)
      return                 Asis.Definition
   is
      Arg_Kind : constant Internal_Element_Kinds :=
        Int_Kind (Component_Definition);

      Res_Node    : Node_Id;
      Result_Kind : Internal_Element_Kinds := Not_An_Element;
   begin
      Check_Validity
        (Component_Definition,
         Package_Name & "Component_Definition_View");

      if not (Arg_Kind = A_Component_Definition) then
         Raise_ASIS_Inappropriate_Element
           (Diagnosis => Package_Name & "Component_Definition_View",
            Wrong_Kind => Arg_Kind);
      end if;

      Res_Node := Node (Component_Definition);

      if Present (Access_Definition (Res_Node)) then
         Res_Node := Access_Definition (Res_Node);
      else
         Result_Kind := A_Subtype_Indication;
         Res_Node    := Sinfo.Subtype_Indication (Res_Node);
      end if;

      return Node_To_Element_New
               (Node             => Res_Node,
                Starting_Element => Component_Definition,
                Internal_Kind    => Result_Kind);

   exception
      when ASIS_Inappropriate_Element =>
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information
              (Argument   => Component_Definition,
               Outer_Call => Package_Name & "Component_Definition_View");
         end if;

         raise;
      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name  => Package_Name & "Component_Definition_View",
            Ex          => Ex,
            Arg_Element => Component_Definition);
   end Component_Definition_View;

   -------------------------------
   -- Definition_Interface_List --
   -------------------------------

   function Definition_Interface_List
      (Type_Definition : Asis.Definition)
       return            Asis.Expression_List
   is
      Arg_Kind : constant Internal_Element_Kinds := Int_Kind (Type_Definition);
      Arg_Node :          Node_Id;
      First_I  :          Node_Id;
      I_Kind   :          Internal_Element_Kinds;
   begin
      Check_Validity
        (Type_Definition, Package_Name & "Definition_Interface_List");

      if not (Arg_Kind =  A_Derived_Record_Extension_Definition or else
              Arg_Kind =  A_Private_Extension_Definition        or else
              Arg_Kind in Internal_Interface_Kinds              or else
              Arg_Kind =  A_Formal_Derived_Type_Definition      or else
              Arg_Kind in Internal_Formal_Interface_Kinds)
      then
         Raise_ASIS_Inappropriate_Element
           (Diagnosis  => Package_Name & "Definition_Interface_List",
            Wrong_Kind => Arg_Kind);
      end if;

      Arg_Node := Node (Type_Definition);

      if Nkind (Arg_Node) = N_Record_Definition
        and then
         not Interface_Present (Arg_Node)
      then
         return Nil_Element_List;
      elsif Nkind (Arg_Node) = N_Derived_Type_Definition
        and then
            Interface_Present (Arg_Node)
      then
         --  The first interface name in the list is represented as
         --  Subtype_Indication field in N_Derived_Type_Definition node
         First_I := Sinfo.Subtype_Indication (Arg_Node);

         if Nkind (First_I) = N_Identifier then
            I_Kind := An_Identifier;
         else
            I_Kind := A_Selected_Component;
         end if;

         return
           Node_To_Element_New
                  (Node             => First_I,
                   Starting_Element => Type_Definition,
                   Internal_Kind    => I_Kind)
          &
           N_To_E_List_New (List             => Interface_List (Arg_Node),
                            Starting_Element => Type_Definition);
      else
         return N_To_E_List_New (List             => Interface_List (Arg_Node),
                                 Starting_Element => Type_Definition);
      end if;

   exception
      when ASIS_Inappropriate_Element =>
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information
              (Argument   => Type_Definition,
               Outer_Call => Package_Name & "Definition_Interface_List");
         end if;

         raise;
      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name  => Package_Name & "Definition_Interface_List",
            Ex          => Ex,
            Arg_Element => Type_Definition);
   end Definition_Interface_List;

   ---------------------------
   -- ASIS 2012 Draft stuff --
   ---------------------------

   -----------------------
   -- Aspect_Definition --
   -----------------------

   function Aspect_Definition
     (Aspect_Specification : Asis.Element)
      return                Asis.Element
   is
      Arg_Kind : constant Internal_Element_Kinds :=
        Int_Kind (Aspect_Specification);
      Res_Node : Node_Id;
   begin
      Check_Validity
        (Aspect_Specification, Package_Name & "Aspect_Definition");

      if Arg_Kind /= An_Aspect_Specification then
         Raise_ASIS_Inappropriate_Element
           (Diagnosis => Package_Name & "Aspect_Definition",
            Wrong_Kind => Arg_Kind);
      end if;

      Res_Node := Sinfo.Expression (Node (Aspect_Specification));

      if Chars (Sinfo.Identifier (Node (Aspect_Specification))) in
           Name_Abstract_State  |
           Name_Contract_Cases  |
           Name_Depends         |
           Name_Global          |
           Name_Initializes     |
           Name_Refined_Depends |
           Name_Refined_Global  |
           Name_Refined_State
      then
         if Nkind (Res_Node) = N_Aggregate
           and then
            List_Length (Sinfo.Expressions (Res_Node)) = 1
           and then
            Nkind (First (Sinfo.Expressions (Res_Node))) /=
              N_Component_Association
         then
            --  The front-end represents cases like
            --
            --     with Initializes => A_Varialbe

            --  as
            --
            --     with Initializes => (A_Variable)
            --
            --  that is, it creates N_Aggregate for A_Varialbe in both cases,
            --  but in ASIS we do not want to deal with an aggregate element
            --  here

            Res_Node := First (Sinfo.Expressions (Res_Node));
         end if;
      end if;

      return Node_To_Element_New
               (Node             => Res_Node,
                Starting_Element => Aspect_Specification);

   exception
      when ASIS_Inappropriate_Element =>
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information
              (Argument   => Aspect_Specification,
               Outer_Call => Package_Name & "Aspect_Definition");
         end if;

         raise;
      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name  => Package_Name & "Aspect_Definition",
            Ex          => Ex,
            Arg_Element => Aspect_Specification);
   end Aspect_Definition;

   -----------------
   -- Aspect_Mark --
   -----------------

   function Aspect_Mark
     (Aspect_Specification : Asis.Element)
      return                Asis.Element
   is
      Arg_Kind : constant Internal_Element_Kinds :=
        Int_Kind (Aspect_Specification);
      Res_Node : Node_Id;
      Res_Kind : Internal_Element_Kinds := An_Identifier;
   begin
      Check_Validity
        (Aspect_Specification, Package_Name & "Aspect_Mark");

      if Arg_Kind /= An_Aspect_Specification then
         Raise_ASIS_Inappropriate_Element
           (Diagnosis => Package_Name & "Aspect_Mark",
            Wrong_Kind => Arg_Kind);
      end if;

      Res_Node := Node (Aspect_Specification);

      if Class_Present (Res_Node) then
         Res_Kind := A_Class_Attribute;
      end if;

      Res_Node := Sinfo.Identifier (Res_Node);

      return Node_To_Element_New
               (Node             => Res_Node,
                Starting_Element => Aspect_Specification,
                Internal_Kind    => Res_Kind);

   exception
      when ASIS_Inappropriate_Element =>
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information
              (Argument   => Aspect_Specification,
               Outer_Call => Package_Name & "Aspect_Mark");
         end if;

         raise;
      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name  => Package_Name & "Aspect_Mark",
            Ex          => Ex,
            Arg_Element => Aspect_Specification);
   end Aspect_Mark;

------------------------------------------------------------------------------

   function Corresponding_Type_Operators
     (Type_Definition : Asis.Type_Definition)
      return            Asis.Declaration_List
   is
      Arg_Kind : constant Internal_Element_Kinds := Int_Kind (Type_Definition);
   begin
      Check_Validity (Type_Definition,
               Package_Name & "Corresponding_Type_Operators");

      if not (Arg_Kind in Internal_Type_Kinds        or else
              Arg_Kind in Internal_Formal_Type_Kinds or else
              Arg_Kind in A_Private_Type_Definition ..
                          A_Protected_Definition)
      then
         Raise_ASIS_Inappropriate_Element
           (Diagnosis  => Package_Name & "Corresponding_Type_Operators",
            Wrong_Kind => Arg_Kind);
      end if;

      return Inherited_Type_Operators (Type_Definition) &
             Explicit_Type_Operators (Type_Definition);

   exception
      when ASIS_Inappropriate_Element =>
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information
              (Argument   => Type_Definition,
               Outer_Call => Package_Name & "Corresponding_Type_Operators");
         end if;

         raise;
      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name  => Package_Name & "Corresponding_Type_Operators",
            Ex          => Ex,
            Arg_Element => Type_Definition);
   end Corresponding_Type_Operators;
-----------------------------------------------------------------------------
   function Parent_Subtype_Indication
     (Type_Definition : Asis.Type_Definition)
      return            Asis.Subtype_Indication
   is
      Arg_Kind : constant Internal_Element_Kinds := Int_Kind (Type_Definition);
      Arg_Node : Node_Id;
   begin
      Check_Validity
        (Type_Definition, Package_Name & "Parent_Subtype_Indication");

      if not (Arg_Kind = A_Derived_Type_Definition or else
              Arg_Kind = A_Derived_Record_Extension_Definition)
      then
         Raise_ASIS_Inappropriate_Element
           (Diagnosis  => Package_Name & "Parent_Subtype_Indication",
            Wrong_Kind => Arg_Kind);
      end if;

      Arg_Node := Node (Type_Definition);

      return Node_To_Element_New
        (Node             => Sinfo.Subtype_Indication (Arg_Node),
         Starting_Element => Type_Definition,
         Internal_Kind    => A_Subtype_Indication);
   exception
      when ASIS_Inappropriate_Element =>
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information
              (Argument   => Type_Definition,
               Outer_Call => Package_Name & "Parent_Subtype_Indication");
         end if;

         raise;
      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name  => Package_Name & "Parent_Subtype_Indication",
            Ex          => Ex,
            Arg_Element => Type_Definition);
   end Parent_Subtype_Indication;
-----------------------------------------------------------------------------
   function Record_Definition
     (Type_Definition : Asis.Type_Definition)
      return            Asis.Record_Definition
   is
      Arg_Kind : constant Internal_Element_Kinds := Int_Kind (Type_Definition);

      Arg_Node    : Node_Id;
      Result_Kind : Internal_Element_Kinds;
      Result_Node : Node_Id;
   begin
      Check_Validity (Type_Definition, Package_Name & "Record_Definition");

      if not (Arg_Kind = A_Derived_Record_Extension_Definition or else
              Arg_Kind = A_Record_Type_Definition              or else
              Arg_Kind = A_Tagged_Record_Type_Definition)
      then
         Raise_ASIS_Inappropriate_Element
           (Diagnosis  => Package_Name & "Record_Definition",
            Wrong_Kind => Arg_Kind);
      end if;

      Arg_Node := Node (Type_Definition);

      if Arg_Kind = A_Derived_Record_Extension_Definition then
         Result_Node := Record_Extension_Part (Arg_Node);
      else
         Result_Node := Arg_Node;
      end if;

      if Null_Present (Result_Node) then
         Result_Kind := A_Null_Record_Definition;
      else
         Result_Kind := A_Record_Definition;
      end if;

      return Node_To_Element_New
               (Node             => Result_Node,
                Starting_Element => Type_Definition,
                Internal_Kind    => Result_Kind);
   exception
      when ASIS_Inappropriate_Element =>
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information
              (Argument   => Type_Definition,
               Outer_Call => Package_Name & "Record_Definition");
         end if;

         raise;
      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name  => Package_Name & "Record_Definition",
            Ex          => Ex,
            Arg_Element => Type_Definition);
   end Record_Definition;
------------------------------------------------------------------------------

   function Implicit_Inherited_Declarations
     (Definition : Asis.Definition)
      return       Asis.Declaration_List
   is
      Arg_Kind : constant Internal_Element_Kinds := Int_Kind (Definition);

      Type_Entity_Node : Node_Id;
      Type_Decl_Node   : Node_Id;
      Inherit_Discrims : Boolean := True;
   begin
      Check_Validity
        (Definition, Package_Name & "Implicit_Inherited_Declarations");

      if not (Arg_Kind = A_Private_Extension_Definition        or else
              Arg_Kind = A_Derived_Type_Definition             or else
              Arg_Kind = A_Derived_Record_Extension_Definition or else
              Arg_Kind = A_Formal_Derived_Type_Definition)
      then
         Raise_ASIS_Inappropriate_Element
           (Diagnosis  => Package_Name & "Implicit_Inherited_Declarations",
            Wrong_Kind => Arg_Kind);
      end if;

      Type_Entity_Node := R_Node (Definition);

      if Nkind (Type_Entity_Node) /= N_Private_Extension_Declaration then
         Type_Entity_Node := Parent (Type_Entity_Node);
      end if;

      Type_Entity_Node := Defining_Identifier (Type_Entity_Node);

      if not (Is_Record_Type      (Type_Entity_Node) or else
              Is_Enumeration_Type (Type_Entity_Node) or else
              Is_Task_Type        (Type_Entity_Node) or else
              Is_Protected_Type   (Type_Entity_Node))
      then
         return Nil_Element_List;
      end if;

      Type_Decl_Node := R_Node (Definition);

      if Nkind (Type_Decl_Node) /= N_Private_Extension_Declaration then
         Type_Decl_Node := Parent (Type_Decl_Node);
      end if;

      if Present (Discriminant_Specifications
                    (Original_Node (Type_Decl_Node)))
      then
         Inherit_Discrims := False;
      end if;

      if Is_Record_Type (Type_Entity_Node) then
         Set_Inherited_Components (Definition, Inherit_Discrims);

      elsif Is_Concurrent_Type (Type_Entity_Node) then
         Set_Concurrent_Inherited_Components (Definition, Inherit_Discrims);

      elsif Is_Enumeration_Type (Type_Entity_Node) then

         if Present (First_Literal (Type_Entity_Node)) then
            Set_Inherited_Literals (Definition);
         else
            --  Type derived (directly or indirectly) from Standard.Character
            --  or Standard.Wide_Character
            return  Standard_Char_Decls
                      (Type_Definition => Definition,
                       Implicit        => True);
         end if;

      else
         Not_Implemented_Yet
           (Diagnosis => Package_Name & "Implicit_Inherited_Declarations");
      end if;

      for J in 1 .. Asis_Element_Table.Last loop
         Set_From_Implicit  (Asis_Element_Table.Table (J), True);
         Set_From_Inherited (Asis_Element_Table.Table (J), True);
         Set_Node_Field_1   (Asis_Element_Table.Table (J), Type_Decl_Node);
      end loop;

      return Asis.Declaration_List
               (Asis_Element_Table.Table (1 .. Asis_Element_Table.Last));

   exception
      when ASIS_Inappropriate_Element =>
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information
              (Argument   => Definition,
               Outer_Call => Package_Name & "Implicit_Inherited_Declarations");
         end if;

         raise;
      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name  => Package_Name & "Implicit_Inherited_Declarations",
            Ex          => Ex,
            Arg_Element => Definition);
   end Implicit_Inherited_Declarations;
------------------------------------------------------------------------------

   function Implicit_Inherited_Subprograms
     (Definition : Asis.Definition)
      return       Asis.Declaration_List
   is
      Arg_Kind : constant Internal_Element_Kinds := Int_Kind (Definition);

      Type_Entity_Node   : Node_Id;
      Type_Dcl_Node      : Node_Id;
      Next_Subpr_Node    : Node_Id;
      Next_Expl_Subpr    : Node_Id;
      Expl_Subpr_Name    : Node_Id;
      Next_Subpr_Kind    : Internal_Element_Kinds;
      Next_Subpr_Element : Element;
      Result_Unit        : Compilation_Unit;

      function Is_Inherited_Invariant (Subpr_E : Entity_Id) return Boolean;
      --  Checks if the argument is an artificial procedure that implements an
      --  (inherited) type invariant

      function Is_Inherited_Invariant (Subpr_E : Entity_Id) return Boolean is
         Alias_E : Entity_Id;
         Result  : Boolean := False;
      begin
         if Is_Invariant_Procedure (Subpr_E) then
            Result := True;
         else
            Alias_E := Alias (Subpr_E);

            while Present (Alias_E) loop
               if Is_Invariant_Procedure (Alias_E) then
                  Result := True;
                  exit;
               end if;

               Alias_E := Alias (Alias_E);
            end loop;
         end if;

         return Result;
      end Is_Inherited_Invariant;

   begin

      Check_Validity (Definition,
                     Package_Name & "Implicit_Inherited_Subprograms");

      if not (Arg_Kind =  A_Private_Extension_Definition        or else
              Arg_Kind =  A_Derived_Type_Definition             or else
              Arg_Kind =  A_Derived_Record_Extension_Definition or else
              Arg_Kind =  A_Formal_Derived_Type_Definition      or else
              Arg_Kind in Internal_Interface_Kinds              or else
              Arg_Kind in Internal_Formal_Interface_Kinds)
      then
         Raise_ASIS_Inappropriate_Element
           (Diagnosis  => Package_Name & "Implicit_Inherited_Subprograms",
            Wrong_Kind => Arg_Kind);
      end if;

      Type_Dcl_Node := R_Node (Definition);

      if Nkind (Type_Dcl_Node) /= N_Private_Extension_Declaration then
         Type_Dcl_Node := Parent (Type_Dcl_Node);
      end if;

      Type_Entity_Node := Defining_Identifier (Type_Dcl_Node);

      if Arg_Kind in
           A_Derived_Type_Definition |
           A_Derived_Record_Extension_Definition
        and then
         Present (Full_View (Type_Entity_Node))
        and then
         Present (Next_Entity (Full_View (Type_Entity_Node)))
      then
         --  This is a case of a type derived from a formal private type
         Type_Entity_Node := Full_View (Type_Entity_Node);
      end if;

      Result_Unit := Encl_Unit (Definition);

      Asis_Element_Table.Init;

      Next_Subpr_Node := Next_Entity (Type_Entity_Node);
      --  All the inherited subprograms can be *after* the type entity only

      while Present (Next_Subpr_Node) loop

         if (Ekind (Next_Subpr_Node) = E_Procedure or else
             Ekind (Next_Subpr_Node) = E_Function)
           and then
             Parent (Next_Subpr_Node) = Type_Dcl_Node
           and then
             not (Is_Hidden (Next_Subpr_Node)
                and then
                  Present (Interface_Alias (Next_Subpr_Node)))
           and then
              Not_Overriden_By_Explicit (Next_Subpr_Node)
           and then
              (not Has_Invariants (Type_Entity_Node)
             or else
              not Is_Inherited_Invariant (Next_Subpr_Node))
         then
            --  This entity node represents the user-defined inherited
            --  subprogram for Type_Dcl_Node

            Next_Expl_Subpr := Explicit_Parent_Subprogram (Next_Subpr_Node);
            Expl_Subpr_Name := Next_Expl_Subpr;

            if Is_Generic_Instance (Expl_Subpr_Name) then

               --  Go to the instantiation entity node, because for the
               --  expanded subprogram the front-end creates an artificial
               --  name:

               while Nkind (Expl_Subpr_Name) /= N_Package_Declaration loop
                  Expl_Subpr_Name := Parent (Expl_Subpr_Name);
               end loop;

               while Nkind (Expl_Subpr_Name) not in
                       N_Generic_Instantiation
               loop
                  Expl_Subpr_Name := Next (Expl_Subpr_Name);
               end loop;

               Expl_Subpr_Name := Defining_Unit_Name (Expl_Subpr_Name);

            end if;

            if Chars (Next_Subpr_Node) = Chars (Expl_Subpr_Name) then
               --  For this condition, see the discussion in 8215-007

               Next_Expl_Subpr := Parent (Next_Expl_Subpr);

               if Ekind (Next_Subpr_Node) = E_Function then
                  Next_Subpr_Kind := A_Function_Declaration;

                  if Nkind (Original_Node (Parent (Next_Expl_Subpr))) =
                       N_Expression_Function
                  then
                     Next_Subpr_Kind := An_Expression_Function_Declaration;
                  end if;

               else
                  Next_Subpr_Kind := A_Procedure_Declaration;
               end if;

               Next_Expl_Subpr := Parent (Next_Expl_Subpr);

               Next_Subpr_Element :=
                  Node_To_Element_New (Node          => Next_Expl_Subpr,
                                       Node_Field_1  => Next_Subpr_Node,
                                       Internal_Kind => Next_Subpr_Kind,
                                       Inherited     => True,
                                       In_Unit       => Result_Unit);

                  --  See the comment in the body of
                  --  A4G.A_Sem.Get_Corr_Called_Entity

                  if Is_From_Instance (Next_Subpr_Node) then
                     Set_From_Instance (Next_Subpr_Element, True);
                  else
                     Set_From_Instance (Next_Subpr_Element, False);
                  end if;

                  Asis_Element_Table.Append (Next_Subpr_Element);
            end if;

         end if;

         Next_Subpr_Node := Next_Entity (Next_Subpr_Node);
      end loop;

      return Asis.Declaration_List
               (Asis_Element_Table.Table (1 .. Asis_Element_Table.Last));

   exception
      when ASIS_Inappropriate_Element =>
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information
              (Argument   => Definition,
               Outer_Call => Package_Name & "Implicit_Inherited_Subprograms");
         end if;

         raise;
      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name  => Package_Name & "Implicit_Inherited_Subprograms",
            Ex          => Ex,
            Arg_Element => Definition);
   end Implicit_Inherited_Subprograms;
-----------------------------------------------------------------------------
   function Corresponding_Parent_Subtype
    (Type_Definition : Asis.Type_Definition)
     return            Asis.Declaration
   is
      Arg_Kind : constant Internal_Element_Kinds := Int_Kind (Type_Definition);

      Type_Mark_Node : Node_Id;
      Result_Node    : Node_Id;
      Result_Unit    : Asis.Compilation_Unit;
      Result         : Asis.Element := Nil_Element;
   begin
      Check_Validity (Type_Definition,
                     Package_Name & "Corresponding_Parent_Subtype");
      if not (Arg_Kind = A_Derived_Type_Definition or else
              Arg_Kind = A_Derived_Record_Extension_Definition)
      then
         Raise_ASIS_Inappropriate_Element
          (Diagnosis  => Package_Name & "Corresponding_Parent_Subtype",
           Wrong_Kind => Arg_Kind);
      end if;

      Type_Mark_Node := Sinfo.Subtype_Indication (Node (Type_Definition));

      if Nkind (Type_Mark_Node) = N_Subtype_Indication then
         Type_Mark_Node := Sinfo.Subtype_Mark (Type_Mark_Node);
      end if;

      if Nkind (Original_Node (Type_Mark_Node)) /= N_Attribute_Reference then

         Result_Node := Entity (Type_Mark_Node);
         Result_Node := Parent (Result_Node);

         Result_Unit :=
           Enclosing_Unit (Encl_Cont_Id (Type_Definition), Result_Node);

         Result := Node_To_Element_New (Node    => Result_Node,
                                        In_Unit => Result_Unit);
      end if;

      return Result;

   exception
      when ASIS_Inappropriate_Element =>
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information
              (Argument   => Type_Definition,
               Outer_Call => Package_Name & "Corresponding_Parent_Subtype");
         end if;

         raise;
      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name  => Package_Name & "Corresponding_Parent_Subtype",
            Ex          => Ex,
            Arg_Element => Type_Definition);
   end Corresponding_Parent_Subtype;
-----------------------------------------------------------------------------
   function Corresponding_Root_Type
     (Type_Definition : Asis.Type_Definition)
      return            Asis.Declaration
   is
      Arg_Kind : constant Internal_Element_Kinds := Int_Kind (Type_Definition);
      Result_El   : Asis.Declaration;
      Result_Kind : Internal_Element_Kinds;
      Def_El      : Asis.Type_Definition;
      Def_Kind    : Internal_Element_Kinds;
   begin
      Check_Validity
        (Type_Definition, Package_Name & "Corresponding_Root_Type");

      if not (Arg_Kind = A_Derived_Type_Definition or else
              Arg_Kind = A_Derived_Record_Extension_Definition)
      then
         Raise_ASIS_Inappropriate_Element
           (Diagnosis  => Package_Name & "Corresponding_Root_Type",
            Wrong_Kind => Arg_Kind);
      end if;

      Result_El := Corresponding_Parent_Subtype_Unwind_Base (Type_Definition);

      loop
         Result_Kind := Int_Kind (Result_El);

         if Result_Kind = A_Subtype_Declaration then
            Result_El := Corresponding_First_Subtype (Result_El);
         else
            --  Result_El can be of An_Ordinary_Type_Declaration,
            --  A_Task_Type_Declaration, A_Protected_Type_Declaration,
            --  A_Private_Type_Declaration, A_Private_Extension_Declaration
            --  or A_Formal_Type_Declaration only
            if Result_Kind = An_Ordinary_Type_Declaration or else
               Result_Kind = A_Formal_Type_Declaration
            then
               Def_El   := Type_Declaration_View (Result_El);
               Def_Kind := Int_Kind (Def_El);
               if Def_Kind = A_Derived_Type_Definition or else
                  Def_Kind = A_Derived_Record_Extension_Definition
               then
                  Result_El :=
                    Corresponding_Parent_Subtype_Unwind_Base (Def_El);
               else
                  exit;
               end if;
            else
               exit;
            end if;

         end if;

      end loop;

      return Result_El;

   exception
      when ASIS_Inappropriate_Element =>
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information
               (Argument  => Type_Definition,
               Outer_Call => Package_Name & "Corresponding_Root_Type");
         end if;

         raise;
      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name  => Package_Name & "Corresponding_Root_Type",
            Ex          => Ex,
            Arg_Element => Type_Definition);
   end Corresponding_Root_Type;
------------------------------------------------------------------------------

   function Corresponding_Type_Structure
     (Type_Definition : Asis.Type_Definition)
      return            Asis.Declaration
   is
      Arg_Kind : constant Internal_Element_Kinds := Int_Kind (Type_Definition);

      Result_El       : Asis.Element;
      Type_Def_El     : Asis.Element;
      Res_Entity_Node : Node_Id;
      Tmp_Node        : Node_Id;
   begin
      Check_Validity
        (Type_Definition, Package_Name & "Corresponding_Type_Structure");

      if not (Arg_Kind = A_Derived_Type_Definition or else
              Arg_Kind = A_Derived_Record_Extension_Definition)
      then
         Raise_ASIS_Inappropriate_Element
           (Diagnosis  => Package_Name & "Corresponding_Type_Structure",
            Wrong_Kind => Arg_Kind);
      end if;

      --  The implementation approach:
      --  1. We are considering, that the following things change the
      --     type structure (type representation):
      --     (1) adding the new component to a tagged record type;
      --     (2) applying any representation pragma or representation
      --         clause to a type in the derivation chain
      --     ??? What about adding a new primitive operation in case of a
      --     ??? tagged type? It changes the representation of the tag.
      --
      --  2. The implementation is based on other semantic queries from
      --     this package. The idea is to make the implementation more
      --     stable and to isolate the code which depends on processing of
      --     implicit types in the tree

      Result_El       := Enclosing_Element (Type_Definition);
      Res_Entity_Node := Defining_Identifier (Node (Result_El));

      Derivation_Chain : loop

         --  In this loop we are iterating through the derivation chain.
         --  There are three reasons to exit the loop:
         --  1. Result_El has representation items;
         --  2. Result_El is not a derived type
         --  3. Result_El defines a new component

         Tmp_Node := First_Rep_Item (Res_Entity_Node);

         while Present (Tmp_Node) loop

            if not Is_Derived_Rep_Item (Res_Entity_Node, Tmp_Node) then
               exit Derivation_Chain;
            end if;

            Tmp_Node := Next_Rep_Item (Tmp_Node);

         end loop;

         Type_Def_El := Type_Declaration_View (Result_El);

         case Int_Kind (Type_Def_El) is

            when A_Derived_Type_Definition |
                 A_Formal_Derived_Type_Definition =>
               null;

            when A_Derived_Record_Extension_Definition =>
               --  Here we are iterating through the list of the components
               --  checking if there is a new, non-inherited component:
               Tmp_Node := First_Entity (Res_Entity_Node);

               while Present (Tmp_Node) loop

                  if (Ekind (Tmp_Node) = E_Component or else
                      Ekind (Tmp_Node) = E_Discriminant)
                    and then
                     Original_Record_Component (Tmp_Node) = Tmp_Node
                  then
                     --  Note that we can have implicit (sub)types in the chain
                     exit Derivation_Chain;
                  end if;

                  Tmp_Node := Next_Entity (Tmp_Node);
               end loop;

            when others =>
               exit Derivation_Chain;

         end case;

         Result_El := Type_Declaration_View (Result_El);

         if Int_Kind (Result_El) = A_Formal_Derived_Type_Definition then
            Result_El := Asis.Definitions.Subtype_Mark (Result_El);
            Result_El := Normalize_Reference (Result_El);
            Result_El := Corresponding_Name_Declaration (Result_El);
         else
            if Is_Nil (Corresponding_Parent_Subtype (Result_El)) then
               --  Derivation from T'Base
               Result_El := Parent_Subtype_Indication (Result_El);
               Result_El := Asis.Definitions.Subtype_Mark (Result_El);
               Result_El := Normalize_Reference (Result_El);
               Result_El := Corresponding_Name_Declaration (Result_El);
            else
               Result_El := Corresponding_Parent_Subtype (Result_El);
            end if;
         end if;

         if Int_Kind (Result_El) = A_Subtype_Declaration then
            Result_El := Corresponding_First_Subtype (Result_El);
         end if;

         if Declaration_Kind (Result_El) in
              A_Private_Type_Declaration | A_Private_Extension_Declaration
         then
            Result_El := Corresponding_Type_Completion (Result_El);
         end if;

         Res_Entity_Node := Defining_Identifier (Node (Result_El));
      end loop Derivation_Chain;

      return Result_El;

   exception
      when ASIS_Inappropriate_Element =>
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information
              (Argument   => Type_Definition,
               Outer_Call => Package_Name & "Corresponding_Type_Structure");
         end if;

         raise;
      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name  => Package_Name & "Corresponding_Type_Structure",
            Ex          => Ex,
            Arg_Element => Type_Definition);
   end Corresponding_Type_Structure;
------------------------------------------------------------------------------

   function Enumeration_Literal_Declarations
     (Type_Definition : Asis.Type_Definition)
     return             Asis.Declaration_List
   is
      Arg_Kind : constant Internal_Element_Kinds := Int_Kind (Type_Definition);
      Arg_Node : Node_Id;
   begin
      Check_Validity
        (Type_Definition, Package_Name & "Enumeration_Literal_Declarations");

      if not (Arg_Kind = An_Enumeration_Type_Definition) then
         Raise_ASIS_Inappropriate_Element
           (Diagnosis  => Package_Name & "Enumeration_Literal_Declarations",
            Wrong_Kind => Arg_Kind);
      end if;

      Arg_Node := Node (Type_Definition);

      if Is_Standard_Char_Type (Arg_Node) then
         --  There is no Literals list for standard char types, so a special
         --  processing is needed
         return Standard_Char_Decls (Type_Definition);
      else
         return N_To_E_List_New
                  (List             => Literals (Arg_Node),
                   Starting_Element => Type_Definition,
                   Internal_Kind    => An_Enumeration_Literal_Specification);
      end if;

   exception
      when ASIS_Inappropriate_Element =>
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information
              (Argument   => Type_Definition,
               Outer_Call => Package_Name &
                             "Enumeration_Literal_Declarations");
         end if;

         raise;
      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name  => Package_Name & "Enumeration_Literal_Declarations",
            Ex          => Ex,
            Arg_Element => Type_Definition);
   end Enumeration_Literal_Declarations;
------------------------------------------------------------------------------
--  OPEN PROBLEMS:
--
--  1. Standard.Character and Standard.Whide_Character types have
--     to be processed specifically (See Sinfo.ads item for
--     N_Enumeration_Type_Definition Node. This is not implemented yet.
------------------------------------------------------------------------------

   function Integer_Constraint
     (Type_Definition : Asis.Type_Definition)
      return            Asis.Range_Constraint
   is
      Arg_Kind : constant Internal_Element_Kinds := Int_Kind (Type_Definition);
      Arg_Node : Node_Id;
   begin
      Check_Validity
        (Type_Definition, Package_Name & "Integer_Constraint");

      if not (Arg_Kind = A_Signed_Integer_Type_Definition) then
         Raise_ASIS_Inappropriate_Element
           (Diagnosis  => Package_Name & "Integer_Constraint",
            Wrong_Kind => Arg_Kind);
      end if;

      Arg_Node := Node (Type_Definition);

      return Node_To_Element_New
               (Node             => Arg_Node,
                Starting_Element => Type_Definition,
                Internal_Kind    => A_Simple_Expression_Range);
   exception
      when ASIS_Inappropriate_Element =>
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information
              (Argument   => Type_Definition,
               Outer_Call => Package_Name & "Integer_Constraint");
         end if;

         raise;
      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name  => Package_Name & "Integer_Constraint",
            Ex          => Ex,
            Arg_Element => Type_Definition);
   end Integer_Constraint;
-----------------------------------------------------------------------------

   function Mod_Static_Expression
     (Type_Definition : Asis.Type_Definition)
      return            Asis.Expression
   is
      Arg_Kind : constant Internal_Element_Kinds := Int_Kind (Type_Definition);
      Arg_Node : Node_Id;
   begin
      Check_Validity
        (Type_Definition, Package_Name & "Mod_Static_Expression");

      if not (Arg_Kind = A_Modular_Type_Definition) then
         Raise_ASIS_Inappropriate_Element
           (Diagnosis  => Package_Name & "Mod_Static_Expression",
            Wrong_Kind => Arg_Kind);
      end if;

      Arg_Node := Node (Type_Definition);

      return Node_To_Element_New
               (Node             => Sinfo.Expression (Arg_Node),
                Starting_Element => Type_Definition);
   exception
      when ASIS_Inappropriate_Element =>
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information
              (Argument   => Type_Definition,
               Outer_Call => Package_Name & "Mod_Static_Expression");
         end if;

         raise;
      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name  => Package_Name & "Mod_Static_Expression",
            Ex          => Ex,
            Arg_Element => Type_Definition);
   end Mod_Static_Expression;
-----------------------------------------------------------------------------

   function Digits_Expression
     (Definition : Asis.Definition)
      return       Asis.Expression
   is
      Arg_Kind : constant Internal_Element_Kinds := Int_Kind (Definition);
      Arg_Node : Node_Id;
   begin
      Check_Validity (Definition, Package_Name & "Digits_Expression");

      if not (Arg_Kind = A_Floating_Point_Definition      or else
              Arg_Kind = A_Decimal_Fixed_Point_Definition or else
              Arg_Kind = A_Digits_Constraint)
      then
         Raise_ASIS_Inappropriate_Element
           (Diagnosis  => Package_Name & "Digits_Expression",
            Wrong_Kind => Arg_Kind);
      end if;

      Arg_Node := Node (Definition);

      return Node_To_Element_New
              (Node             => Digits_Expression (Arg_Node),
               Starting_Element => Definition);
   exception
      when ASIS_Inappropriate_Element =>
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information
              (Argument   => Definition,
               Outer_Call => Package_Name & "Digits_Expression");
         end if;

         raise;
      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name  => Package_Name & "Digits_Expression",
            Ex          => Ex,
            Arg_Element => Definition);
   end Digits_Expression;
-----------------------------------------------------------------------------

   function Delta_Expression
     (Definition : Asis.Definition)
      return       Asis.Expression
   is
      Arg_Kind : constant Internal_Element_Kinds := Int_Kind (Definition);
      Arg_Node : Node_Id;
   begin
      Check_Validity (Definition, Package_Name & "Delta_Expression");

      if not (Arg_Kind = An_Ordinary_Fixed_Point_Definition or else
              Arg_Kind = A_Decimal_Fixed_Point_Definition   or else
              Arg_Kind = A_Delta_Constraint)
      then
         Raise_ASIS_Inappropriate_Element
           (Diagnosis  => Package_Name & "Delta_Expression",
            Wrong_Kind => Arg_Kind);
      end if;

      Arg_Node := Node (Definition);

      return Node_To_Element_New
               (Node             => Delta_Expression (Arg_Node),
                Starting_Element => Definition);
   exception
      when ASIS_Inappropriate_Element =>
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information
              (Argument   => Definition,
               Outer_Call => Package_Name & "Delta_Expression");
         end if;

         raise;
      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name  => Package_Name & "Delta_Expression",
            Ex          => Ex,
            Arg_Element => Definition);
   end Delta_Expression;
-----------------------------------------------------------------------------
   function Real_Range_Constraint
     (Definition : Asis.Definition)
      return       Asis.Range_Constraint
   is
      Arg_Kind : constant Internal_Element_Kinds := Int_Kind (Definition);
      Arg_Node : Node_Id;
      Result_Node : Node_Id;
   begin
      Check_Validity (Definition, Package_Name & "Real_Range_Constraint");

      if not (Arg_Kind = A_Floating_Point_Definition        or else
              Arg_Kind = An_Ordinary_Fixed_Point_Definition or else
              Arg_Kind = A_Decimal_Fixed_Point_Definition   or else
              Arg_Kind = A_Digits_Constraint                or else
              Arg_Kind = A_Delta_Constraint)
      then
         Raise_ASIS_Inappropriate_Element
           (Diagnosis  => Package_Name & "Real_Range_Constraint",
            Wrong_Kind => Arg_Kind);
      end if;

      Arg_Node := Node (Definition);

      if Arg_Kind = A_Floating_Point_Definition        or else
         Arg_Kind = An_Ordinary_Fixed_Point_Definition or else
         Arg_Kind = A_Decimal_Fixed_Point_Definition
      then
         Result_Node :=  Real_Range_Specification (Arg_Node);
      else
         --  Arg_Kind = A_Digits_Constraint or Arg_Kind = A_Delta_Constraint
         Result_Node :=  Sinfo.Range_Constraint (Arg_Node);
      end if;

      if No (Result_Node) then
         return Nil_Element;
      else
         return Node_To_Element_New
                  (Node             => Result_Node,
                   Starting_Element => Definition,
                   Internal_Kind    => A_Simple_Expression_Range);
      end if;
   exception
      when ASIS_Inappropriate_Element =>
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information (
               Argument => Definition,
               Outer_Call => Package_Name & "Real_Range_Constraint");
         end if;

         raise;
      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name  => Package_Name & "Real_Range_Constraint",
            Ex          => Ex,
            Arg_Element => Definition);
   end Real_Range_Constraint;
-----------------------------------------------------------------------------
   function Index_Subtype_Definitions
     (Type_Definition : Asis.Type_Definition)
      return            Asis.Expression_List
   is
      Arg_Kind : constant Internal_Element_Kinds := Int_Kind (Type_Definition);
      Arg_Node : Node_Id;
   begin
      Check_Validity (Type_Definition,
                     Package_Name & "Index_Subtype_Definitions");

      if not (Arg_Kind = An_Unconstrained_Array_Definition or else
              Arg_Kind = A_Formal_Unconstrained_Array_Definition)
      then
         Raise_ASIS_Inappropriate_Element
           (Diagnosis  => Package_Name & "Index_Subtype_Definitions",
            Wrong_Kind => Arg_Kind);
      end if;

      Arg_Node := Node (Type_Definition);

      return N_To_E_List_New (List             => Subtype_Marks (Arg_Node),
                              Starting_Element => Type_Definition);

   exception
      when ASIS_Inappropriate_Element =>
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information (
               Argument => Type_Definition,
               Outer_Call => Package_Name & "Index_Subtype_Definitions");
         end if;

         raise;
      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name  => Package_Name & "Index_Subtype_Definitions",
            Ex          => Ex,
            Arg_Element => Type_Definition);
   end Index_Subtype_Definitions;
-----------------------------------------------------------------------------

   function Discrete_Subtype_Definitions
     (Type_Definition : Asis.Type_Definition)
      return            Asis.Definition_List
   is
      Arg_Kind : constant Internal_Element_Kinds := Int_Kind (Type_Definition);
      Arg_Node : Node_Id;
   begin
      Check_Validity
         (Type_Definition, Package_Name & "Discrete_Subtype_Definitions");

      if not (Arg_Kind = A_Constrained_Array_Definition or else
              Arg_Kind = A_Formal_Constrained_Array_Definition)
      then
         Raise_ASIS_Inappropriate_Element
           (Diagnosis  => Package_Name & "Discrete_Subtype_Definitions",
            Wrong_Kind => Arg_Kind);
      end if;

      Arg_Node := Node (Type_Definition);

      return N_To_E_List_New (
            List             => Discrete_Subtype_Definitions (Arg_Node),
            Starting_Element => Type_Definition);
   exception
      when ASIS_Inappropriate_Element =>
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information
               (Argument  => Type_Definition,
               Outer_Call => Package_Name & "Discrete_Subtype_Definitions");
         end if;

         raise;
      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name  => Package_Name & "Discrete_Subtype_Definitions",
            Ex          => Ex,
            Arg_Element => Type_Definition);
   end Discrete_Subtype_Definitions;

   --------------------------------
   -- Array_Component_Definition --
   --------------------------------

   function Array_Component_Definition
     (Type_Definition : Asis.Type_Definition)
      return            Asis.Component_Definition
   is
      Arg_Kind : constant Internal_Element_Kinds := Int_Kind (Type_Definition);
      Arg_Node : Node_Id;
   begin
      Check_Validity
        (Type_Definition, Package_Name & "Array_Component_Definition");

      if not (Arg_Kind = An_Unconstrained_Array_Definition       or else
              Arg_Kind = A_Constrained_Array_Definition          or else
              Arg_Kind = A_Formal_Unconstrained_Array_Definition or else
              Arg_Kind = A_Formal_Constrained_Array_Definition)
      then
         Raise_ASIS_Inappropriate_Element
           (Diagnosis  => Package_Name & "Array_Component_Definition",
            Wrong_Kind => Arg_Kind);
      end if;

      Arg_Node := Node (Type_Definition);

      return Node_To_Element_New
               (Node             => Sinfo.Component_Definition (Arg_Node),
                Starting_Element => Type_Definition,
                Internal_Kind    => A_Component_Definition);
   exception
      when ASIS_Inappropriate_Element =>
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information
              (Argument   => Type_Definition,
               Outer_Call => Package_Name & "Array_Component_Definition");
         end if;

         raise;
      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name  => Package_Name & "Array_Component_Definition",
            Ex          => Ex,
            Arg_Element => Type_Definition);
   end Array_Component_Definition;
-----------------------------------------------------------------------------

   function Access_To_Object_Definition
     (Type_Definition : Asis.Type_Definition)
      return            Asis.Subtype_Indication
   is
      Arg_Kind : constant Internal_Element_Kinds := Int_Kind (Type_Definition);
      Arg_Node : Node_Id;
   begin
      Check_Validity
        (Type_Definition, Package_Name & "Access_To_Object_Definition");

      if not (Arg_Kind = A_Pool_Specific_Access_To_Variable        or else
              Arg_Kind = An_Access_To_Variable                     or else
              Arg_Kind = An_Access_To_Constant                     or else
              Arg_Kind = A_Formal_Pool_Specific_Access_To_Variable or else
              Arg_Kind = A_Formal_Access_To_Variable               or else
              Arg_Kind = A_Formal_Access_To_Constant)
      then
         Raise_ASIS_Inappropriate_Element
           (Diagnosis  => Package_Name & "Access_To_Object_Definition",
            Wrong_Kind => Arg_Kind);
      end if;

      Arg_Node := Node (Type_Definition);

      return Node_To_Element_New
               (Node             => Sinfo.Subtype_Indication (Arg_Node),
                Starting_Element => Type_Definition,
                Internal_Kind    => A_Subtype_Indication);
   exception
      when ASIS_Inappropriate_Element =>
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information
              (Argument   => Type_Definition,
               Outer_Call => Package_Name & "Access_To_Object_Definition");
         end if;

         raise;
      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name  => Package_Name & "Access_To_Object_Definition",
            Ex          => Ex,
            Arg_Element => Type_Definition);
   end Access_To_Object_Definition;
-----------------------------------------------------------------------------

   function Access_To_Subprogram_Parameter_Profile
     (Type_Definition : Asis.Type_Definition)
      return            Asis.Parameter_Specification_List
   is
      Arg_Kind : constant Internal_Element_Kinds := Int_Kind (Type_Definition);

      Arg_Node    : Node_Id;
      Result_List : List_Id;
   begin
      Check_Validity
        (Type_Definition,
         Package_Name & "Access_To_Subprogram_Parameter_Profile");

      if not (Arg_Kind = An_Access_To_Procedure                 or else
              Arg_Kind = An_Access_To_Protected_Procedure       or else
              Arg_Kind = An_Access_To_Function                  or else
              Arg_Kind = An_Access_To_Protected_Function        or else
              Arg_Kind = A_Formal_Access_To_Procedure           or else
              Arg_Kind = A_Formal_Access_To_Protected_Procedure or else
              Arg_Kind = A_Formal_Access_To_Function            or else
              Arg_Kind = A_Formal_Access_To_Protected_Function  or else
--  --|A2005 start
              Arg_Kind = An_Anonymous_Access_To_Procedure           or else
              Arg_Kind = An_Anonymous_Access_To_Protected_Procedure or else
              Arg_Kind = An_Anonymous_Access_To_Function            or else
              Arg_Kind = An_Anonymous_Access_To_Protected_Function)
--  --|A2005 end
      then
         Raise_ASIS_Inappropriate_Element
           (Diagnosis =>
              Package_Name & "Access_To_Subprogram_Parameter_Profile",
            Wrong_Kind => Arg_Kind);
      end if;

      Arg_Node := Node (Type_Definition);

--  --|A2005 start
      if Nkind (Arg_Node) = N_Access_Definition then
         Arg_Node := Sinfo.Access_To_Subprogram_Definition (Arg_Node);
      end if;
--  --|A2005 end

      Result_List := Parameter_Specifications (Arg_Node);

      if No (Result_List) then
         return Nil_Element_List;
      else
         return N_To_E_List_New
           (List              => Result_List,
            Starting_Element  => Type_Definition,
            Internal_Kind     => A_Parameter_Specification);
      end if;

   exception
      when ASIS_Inappropriate_Element =>
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information
              (Argument   => Type_Definition,
               Outer_Call => Package_Name &
                             "Access_To_Subprogram_Parameter_Profile");
         end if;

         raise;
      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name  => Package_Name &
                           "Access_To_Subprogram_Parameter_Profile",
            Ex          => Ex,
            Arg_Element => Type_Definition);
   end Access_To_Subprogram_Parameter_Profile;
-----------------------------------------------------------------------------
   function Access_To_Function_Result_Profile
     (Type_Definition : Asis.Type_Definition)
      return            Asis.Expression
   is
      Arg_Kind : constant Internal_Element_Kinds := Int_Kind (Type_Definition);
      Arg_Node : Node_Id;
   begin
      Check_Validity
        (Type_Definition, Package_Name & "Access_To_Function_Result_Profile");

      if not (Arg_Kind = An_Access_To_Function                 or else
              Arg_Kind = An_Access_To_Protected_Function       or else
              Arg_Kind = A_Formal_Access_To_Function           or else
              Arg_Kind = A_Formal_Access_To_Protected_Function or else
--  --|A2005 start
              Arg_Kind = An_Anonymous_Access_To_Function     or else
              Arg_Kind = An_Anonymous_Access_To_Protected_Function)
--  --|A2005 end
      then
         Raise_ASIS_Inappropriate_Element
           (Diagnosis  => Package_Name & "Access_To_Function_Result_Profile",
            Wrong_Kind => Arg_Kind);
      end if;

      Arg_Node := Node (Type_Definition);

--  --|A2005 start
      if Nkind (Arg_Node) = N_Access_Definition then
         Arg_Node := Sinfo.Access_To_Subprogram_Definition (Arg_Node);
      end if;
--  --|A2005 end

      return Node_To_Element_New
              (Node             => Sinfo.Result_Definition (Arg_Node),
               Starting_Element => Type_Definition);
   exception
      when ASIS_Inappropriate_Element =>
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information
              (Argument   => Type_Definition,
               Outer_Call => Package_Name &
                             "Access_To_Function_Result_Profile");
         end if;

         raise;
      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name  => Package_Name & "Access_To_Function_Result_Profile",
            Ex          => Ex,
            Arg_Element => Type_Definition);
   end Access_To_Function_Result_Profile;
-----------------------------------------------------------------------------
   function Subtype_Mark
     (Definition : Asis.Definition)
      return       Asis.Expression
   is
      Arg_Kind    : constant Internal_Element_Kinds := Int_Kind (Definition);
      Arg_Node    : Node_Id;
      Result_Node : Node_Id;
      Result_Kind : Internal_Element_Kinds := Not_An_Element;
   begin
      Check_Validity (Definition, Package_Name & "Subtype_Mark");

      if not (Arg_Kind = A_Subtype_Indication             or else
              Arg_Kind = A_Discrete_Subtype_Indication    or else
              Arg_Kind = A_Formal_Derived_Type_Definition or else
              Arg_Kind = A_Discrete_Subtype_Indication_As_Subtype_Definition)
      then
         Raise_ASIS_Inappropriate_Element
           (Diagnosis  => Package_Name & "Subtype_Mark",
            Wrong_Kind => Arg_Kind);
      end if;

      Arg_Node := Node (Definition);

      if Nkind (Arg_Node) =  N_Subtype_Indication or else
         Nkind (Arg_Node) =  N_Formal_Derived_Type_Definition
      then
         Result_Node := Sinfo.Subtype_Mark (Arg_Node);
      else
         Result_Node := R_Node (Definition);
      end if;

      if Nkind (Original_Node (Result_Node)) = N_Identifier and then
         not Is_Rewrite_Substitution (Result_Node)
      then

         if Is_Part_Of_Instance (Definition) then

            if Represents_Class_Wide_Type_In_Instance (Result_Node) then
               Result_Kind := A_Class_Attribute;
            elsif Represents_Base_Type_In_Instance (Result_Node) then
               Result_Kind := A_Base_Attribute;
            else
               Result_Kind := An_Identifier;
            end if;

         else
            Result_Kind := An_Identifier;
         end if;

      elsif Nkind (Original_Node (Result_Node)) = N_Expanded_Name then
         Result_Kind := A_Selected_Component;
      end if;

      return Node_To_Element_New
        (Node             => Result_Node,
         Starting_Element => Definition,
         Internal_Kind    => Result_Kind);
   exception
      when ASIS_Inappropriate_Element =>
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information
              (Argument   => Definition,
               Outer_Call => Package_Name & "Subtype_Mark");
         end if;

         raise;
      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name  => Package_Name & "Subtype_Mark",
            Ex          => Ex,
            Arg_Element => Definition);
   end Subtype_Mark;
-----------------------------------------------------------------------------
   function Subtype_Constraint
     (Definition : Asis.Definition)
      return       Asis.Constraint
   is
      Arg_Kind    : constant Internal_Element_Kinds := Int_Kind (Definition);
      Arg_Node    : Node_Id;
      Result_Node : Node_Id := Empty;
      Result_Kind : Internal_Element_Kinds := Not_An_Element;
   begin
      Check_Validity (Definition, Package_Name & "Subtype_Constraint");

      if not (Arg_Kind = A_Subtype_Indication          or else
              Arg_Kind = A_Discrete_Subtype_Indication or else
              Arg_Kind = A_Discrete_Subtype_Indication_As_Subtype_Definition)
      then
         Raise_ASIS_Inappropriate_Element
           (Diagnosis  => Package_Name & "Subtype_Constraint",
            Wrong_Kind => Arg_Kind);
      end if;

      Arg_Node := Node (Definition);

      if Nkind (Arg_Node) =  N_Subtype_Indication then
         Result_Node := Sinfo.Constraint (Arg_Node);

      elsif Sloc (Arg_Node) <= Standard_Location and then
            Nkind (Parent (Arg_Node)) = N_Subtype_Declaration
      then
         --  This is either Standard.Positive or Standard.Natural,
         --  they have the constraint information not in
         --  N_Subtype_Declaration node, but in N_Defining_Identifier node

         Result_Node := Scalar_Range (Defining_Identifier (Parent (Arg_Node)));
         Result_Kind := A_Simple_Expression_Range;
      end if;

      return Node_To_Element_New
               (Node             => Result_Node,
                Starting_Element => Definition,
                Internal_Kind    => Result_Kind);
   exception
      when ASIS_Inappropriate_Element =>
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information
              (Argument   => Definition,
               Outer_Call => Package_Name & "Subtype_Constraint");
         end if;

         raise;
      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name  => Package_Name & "Subtype_Constraint",
            Ex          => Ex,
            Arg_Element => Definition);
   end Subtype_Constraint;
-----------------------------------------------------------------------------
   function Lower_Bound
     (Constraint : Asis.Range_Constraint)
      return       Asis.Expression
   is
      Arg_Kind    : constant Internal_Element_Kinds := Int_Kind (Constraint);
      Arg_Node    : Node_Id;
      Result_Node : Node_Id;
   begin
      Check_Validity (Constraint, Package_Name & "Lower_Bound");

      if not (Arg_Kind = A_Simple_Expression_Range or else
              Arg_Kind = A_Discrete_Simple_Expression_Range or else
              Arg_Kind =
                 A_Discrete_Simple_Expression_Range_As_Subtype_Definition)
      then
         Raise_ASIS_Inappropriate_Element
           (Diagnosis  => Package_Name & "Lower_Bound",
            Wrong_Kind => Arg_Kind);
      end if;

      Arg_Node := Node (Constraint);

      if Nkind (Arg_Node) = N_Range_Constraint then
         Result_Node := Low_Bound (Range_Expression (Arg_Node));
      elsif Nkind (Arg_Node) = N_Component_Clause then
         Result_Node := First_Bit (Arg_Node);
      else
         --  Nkind (Arg_Node) = N_Range or else
         --  Nkind (Arg_Node) = N_Real_Range_Specification
         Result_Node := Low_Bound (Arg_Node);
      end if;

      return Node_To_Element_New
               (Node             => Result_Node,
                Starting_Element => Constraint);
   exception
      when ASIS_Inappropriate_Element =>
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information
              (Argument   => Constraint,
               Outer_Call => Package_Name & "Lower_Bound");
         end if;

         raise;
      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name  => Package_Name & "Lower_Bound",
            Ex          => Ex,
            Arg_Element => Constraint);
   end Lower_Bound;
-----------------------------------------------------------------------------
   function Upper_Bound
     (Constraint : Asis.Range_Constraint)
      return       Asis.Expression
   is
      Arg_Kind    : constant Internal_Element_Kinds := Int_Kind (Constraint);
      Arg_Node    : Node_Id;
      Result_Node : Node_Id;
   begin
      Check_Validity (Constraint, Package_Name & "Upper_Bound");

      if not (Arg_Kind = A_Simple_Expression_Range          or else
              Arg_Kind = A_Discrete_Simple_Expression_Range or else
              Arg_Kind =
                 A_Discrete_Simple_Expression_Range_As_Subtype_Definition)
      then
         Raise_ASIS_Inappropriate_Element
           (Diagnosis  => Package_Name & "Upper_Bound",
            Wrong_Kind => Arg_Kind);
      end if;

      Arg_Node := Node (Constraint);

      if Nkind (Arg_Node) = N_Range_Constraint then
         Result_Node := High_Bound (Range_Expression (Arg_Node));
      elsif Nkind (Arg_Node) = N_Component_Clause then
         Result_Node := Last_Bit (Arg_Node);
      else
         Result_Node := High_Bound (Arg_Node);
      end if;

      return Node_To_Element_New
               (Node    => Result_Node,
                Starting_Element => Constraint);
   exception
      when ASIS_Inappropriate_Element =>
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information
              (Argument   => Constraint,
               Outer_Call => Package_Name & "Upper_Bound");
         end if;

         raise;
      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name  => Package_Name & "Upper_Bound",
            Ex          => Ex,
            Arg_Element => Constraint);
   end Upper_Bound;
-----------------------------------------------------------------------------
   function Range_Attribute
     (Constraint : Asis.Range_Constraint)
      return        Asis.Expression
   is
      Arg_Kind    : constant Internal_Element_Kinds := Int_Kind (Constraint);
      Arg_Node    : constant Node_Id := Node (Constraint);
      Result_Node : Node_Id;
   begin
      Check_Validity (Constraint, Package_Name & "Range_Attribute");

      if not (Arg_Kind = A_Range_Attribute_Reference          or else
              Arg_Kind = A_Discrete_Range_Attribute_Reference or else
              Arg_Kind =
                 A_Discrete_Range_Attribute_Reference_As_Subtype_Definition)
      then
         Raise_ASIS_Inappropriate_Element
           (Diagnosis  => Package_Name & "Range_Attribute",
            Wrong_Kind => Arg_Kind);
      end if;

      if Nkind (Arg_Node) = N_Range_Constraint then
         --  one step down to N_Attruibute_Reference node
         Result_Node := Range_Expression (Arg_Node);
      else
         Result_Node := R_Node (Constraint);
      end if;

      return Node_To_Element_New
        (Starting_Element         => Constraint,
         Node                     => Result_Node,
         Internal_Kind            => A_Range_Attribute);
   exception
      when ASIS_Inappropriate_Element =>
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information
              (Argument   => Constraint,
               Outer_Call => Package_Name & "Range_Attribute");
         end if;

         raise;
      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name  => Package_Name & "Range_Attribute",
            Ex          => Ex,
            Arg_Element => Constraint);
   end Range_Attribute;
-------------------------------------------------------------------------

   function Discrete_Ranges
     (Constraint : Asis.Constraint)
      return       Asis.Discrete_Range_List
   is
      Arg_Kind : constant Internal_Element_Kinds := Int_Kind (Constraint);
      Arg_Node : Node_Id;
   begin
      Check_Validity (Constraint, Package_Name & "Discrete_Ranges");

      if not (Arg_Kind = An_Index_Constraint) then
         Raise_ASIS_Inappropriate_Element
           (Diagnosis  => Package_Name & "Discrete_Ranges",
            Wrong_Kind => Arg_Kind);
      end if;

      Arg_Node := Node (Constraint);

      return N_To_E_List_New (List             => Constraints (Arg_Node),
                              Starting_Element => Constraint);
   exception
      when ASIS_Inappropriate_Element =>
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information
              (Argument   => Constraint,
               Outer_Call => Package_Name & "Discrete_Ranges");
         end if;

         raise;
      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name  => Package_Name & "Discrete_Ranges",
            Ex          => Ex,
            Arg_Element => Constraint);
   end Discrete_Ranges;
------------------------------------------------------------------------------
--  ??? PARTIALLY IMPLEMENTED, CANNOT PROCESS THE CASE WHEN
--  ??? NORMALIZED = TRUE

   function Discriminant_Associations
     (Constraint : Asis.Constraint;
      Normalized : Boolean := False)
      return       Asis.Discriminant_Association_List
   is
      Arg_Kind : constant Internal_Element_Kinds := Int_Kind (Constraint);
      Arg_Node : Node_Id;
   begin
      Check_Validity
        (Constraint, Package_Name & "Discriminant_Associations");

      if not (Arg_Kind = A_Discriminant_Constraint) then
         Raise_ASIS_Inappropriate_Element
           (Diagnosis  => Package_Name & "Discriminant_Associations",
            Wrong_Kind => Arg_Kind);
      end if;

      Arg_Node := Node (Constraint);

      if Normalized then

         return Normalized_Discriminant_Associations (
                   Constr_Elem => Constraint,
                   Constr_Node => Arg_Node);

      else

         return N_To_E_List_New
                 (List             => Constraints (Arg_Node),
                  Internal_Kind    => A_Discriminant_Association,
                  Starting_Element => Constraint);
      end if;

   exception
      when ASIS_Inappropriate_Element =>
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information
              (Argument   => Constraint,
               Bool_Par   => Normalized,
               Outer_Call => Package_Name & "Discriminant_Associations");
         end if;

         raise;
      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name  => Package_Name & "Discriminant_Associations",
            Ex          => Ex,
            Arg_Element => Constraint,
            Bool_Par_ON => Normalized);
   end Discriminant_Associations;
-----------------------------------------------------------------------------
   function Component_Subtype_Indication
     (Component_Definition : Asis.Definition)
      return                 Asis.Definition
   is
      Arg_Kind : constant Internal_Element_Kinds :=
         Int_Kind (Component_Definition);
      Arg_Node : Node_Id;
   begin
      Check_Validity
        (Component_Definition, Package_Name & "Component_Subtype_Indication");

      if not (Arg_Kind = A_Component_Definition) then
         Raise_ASIS_Inappropriate_Element
           (Diagnosis  => Package_Name & "Component_Subtype_Indication",
            Wrong_Kind => Arg_Kind);
      end if;

      Arg_Node := Sinfo.Subtype_Indication (R_Node (Component_Definition));

      return Node_To_Element_New
               (Node             => Arg_Node,
                Starting_Element => Component_Definition,
                Internal_Kind    => A_Subtype_Indication);
   exception
      when ASIS_Inappropriate_Element =>
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information
              (Argument   => Component_Definition,
               Outer_Call => Package_Name & "Component_Subtype_Indication");
         end if;

         raise;
      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name  => Package_Name & "Component_Subtype_Indication",
            Ex          => Ex,
            Arg_Element => Component_Definition);
   end Component_Subtype_Indication;
-----------------------------------------------------------------------------
   function Discriminants
     (Definition : Asis.Definition)
      return       Asis.Discriminant_Specification_List
   is
      Arg_Kind : constant Internal_Element_Kinds := Int_Kind (Definition);
      Arg_Node : Node_Id;
   begin
      Check_Validity (Definition, Package_Name & "Discriminations");
      if not (Arg_Kind = A_Known_Discriminant_Part) then
         Raise_ASIS_Inappropriate_Element
           (Diagnosis  => Package_Name & "Discriminations",
            Wrong_Kind => Arg_Kind);
      end if;

      Arg_Node := Node (Definition);

      return N_To_E_List_New
               (List             => Discriminant_Specifications (Arg_Node),
                Starting_Element => Definition,
                Internal_Kind    => A_Discriminant_Specification);
   exception
      when ASIS_Inappropriate_Element =>
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information
              (Argument   => Definition,
               Outer_Call => Package_Name & "Discriminations");
         end if;

         raise;
      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name  => Package_Name & "Discriminations",
            Ex          => Ex,
            Arg_Element => Definition);
   end Discriminants;
-----------------------------------------------------------------------------
   function Record_Components
     (Definition        : Asis.Record_Definition;
      Include_Pragmas   : Boolean := False)
      return              Asis.Record_Component_List
   is
      Arg_Kind : constant Internal_Element_Kinds := Int_Kind (Definition);

      Arg_Node            : Node_Id;
      Component_List_Node : Node_Id;
      Result_List         : List_Id; -- All nodes except the Variant Node
      Variant_Part_Node   : Node_Id;
   begin
      Check_Validity
        (Definition, Package_Name & "Record_Components");

      if not (Arg_Kind = A_Record_Definition or else
              Arg_Kind = A_Variant)
      then
         Raise_ASIS_Inappropriate_Element
           (Diagnosis  => Package_Name & "Record_Components",
            Wrong_Kind => Arg_Kind);
      end if;

      Arg_Node := Node (Definition);

      Component_List_Node := Component_List (Arg_Node);

      --  first, we should check the null record case:
      if Null_Present (Component_List_Node) then
         return Element_List'(1 =>
            Node_To_Element_New (Node              => Arg_Node,
                                 Starting_Element  => Definition,
                                 Internal_Kind     => A_Null_Component));
      end if;

      Result_List         := Component_Items (Component_List_Node);
      Variant_Part_Node   := Variant_Part    (Component_List_Node);

      if No (Variant_Part_Node) then
         return N_To_E_List_New (List             => Result_List,
                                 Include_Pragmas  => Include_Pragmas,
                                 Starting_Element => Definition);
      else
         return (
               N_To_E_List_New (List             => Result_List,
                                Include_Pragmas  => Include_Pragmas,
                                Starting_Element => Definition)
            &
               Element_List'(1 =>
                  Node_To_Element_New (Node              => Variant_Part_Node,
                                       Starting_Element  => Definition,
                                       Internal_Kind     => A_Variant_Part))
               );
      end if;
   exception
      when ASIS_Inappropriate_Element =>
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information
              (Argument   => Definition,
               Bool_Par   => Include_Pragmas,
               Outer_Call => Package_Name & "Record_Components");
         end if;

         raise;
      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name  => Package_Name & "Record_Components",
            Ex          => Ex,
            Arg_Element => Definition,
            Bool_Par_ON => Include_Pragmas);
   end Record_Components;
------------------------------------------------------------------------------
--  NOT IMPLEMENTED

   function Implicit_Components
      (Definition : Asis.Record_Definition)
       return       Asis.Record_Component_List
   is
   begin
      Check_Validity
        (Definition, Package_Name & "Implicit_Components");

      Not_Implemented_Yet
        (Diagnosis => Package_Name & "Implicit_Components");
      --  ASIS_Failed is raised, Not_Implemented_Error status is set

      return Nil_Element_List; -- to make the code syntactically correct

   exception
      when ASIS_Inappropriate_Element =>
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information
              (Argument   => Definition,
               Outer_Call => Package_Name & "Implicit_Components");
         end if;

         raise;
      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name  => Package_Name & "Implicit_Components",
            Ex          => Ex,
            Arg_Element => Definition);
   end Implicit_Components;
-----------------------------------------------------------------------------
   function Discriminant_Direct_Name
     (Variant_Part : Asis.Record_Component)
      return         Asis.Name
   is
      Arg_Kind : constant Internal_Element_Kinds := Int_Kind (Variant_Part);
      Arg_Node : Node_Id;
   begin
      Check_Validity
        (Variant_Part, Package_Name & "Discriminant_Direct_Name");

      if not (Arg_Kind = A_Variant_Part) then
         Raise_ASIS_Inappropriate_Element
           (Diagnosis  => Package_Name & "Discriminant_Direct_Name",
            Wrong_Kind => Arg_Kind);
      end if;

      Arg_Node := Node (Variant_Part);

      return Node_To_Element_New
               (Node             => Sinfo.Name (Arg_Node),
                Starting_Element => Variant_Part,
                Internal_Kind    => An_Identifier);
   exception
      when ASIS_Inappropriate_Element =>
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information
              (Argument   => Variant_Part,
               Outer_Call => Package_Name & "Discriminant_Direct_Name");
         end if;

         raise;
      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name  => Package_Name & "Discriminant_Direct_Name",
            Ex          => Ex,
            Arg_Element => Variant_Part);
   end Discriminant_Direct_Name;
-----------------------------------------------------------------------------
   function Variants
     (Variant_Part    : Asis.Record_Component;
      Include_Pragmas : Boolean := False)
      return            Asis.Variant_List
   is
      Arg_Kind : constant Internal_Element_Kinds := Int_Kind (Variant_Part);
      Arg_Node : Node_Id;
   begin
      Check_Validity (Variant_Part, Package_Name & "Variants");

      if not (Arg_Kind = A_Variant_Part) then
         Raise_ASIS_Inappropriate_Element
           (Diagnosis  => Package_Name & "Variants",
            Wrong_Kind => Arg_Kind);
      end if;

      Arg_Node := Node (Variant_Part);

      return N_To_E_List_New (List             => Variants (Arg_Node),
                              Include_Pragmas  => Include_Pragmas,
                              Starting_Element => Variant_Part);

   exception
      when ASIS_Inappropriate_Element =>
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information
              (Argument   => Variant_Part,
               Bool_Par   => Include_Pragmas,
               Outer_Call => Package_Name & "Variants");
         end if;

         raise;
      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name  => Package_Name & "Variants",
            Ex          => Ex,
            Arg_Element => Variant_Part,
            Bool_Par_ON => Include_Pragmas);
   end Variants;
-----------------------------------------------------------------------------

   function Variant_Choices
     (Variant : Asis.Variant)
      return    Asis.Element_List
   is
      Arg_Kind : constant Internal_Element_Kinds := Int_Kind (Variant);
      Arg_Node : Node_Id;
   begin
      Check_Validity (Variant, Package_Name & "Variant_Choices");

      if not (Arg_Kind = A_Variant) then
         Raise_ASIS_Inappropriate_Element
           (Diagnosis  => Package_Name & "Variant_Choices",
            Wrong_Kind => Arg_Kind);
      end if;

      Arg_Node := Node (Variant);

      return Discrete_Choice_Node_To_Element_List
              (Choice_List      => Discrete_Choices (Arg_Node),
               Starting_Element => Variant);
   exception
      when ASIS_Inappropriate_Element =>
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information
              (Argument   => Variant,
               Outer_Call => Package_Name & "Variant_Choices");
         end if;

         raise;
      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name  => Package_Name & "Variant_Choices",
            Ex          => Ex,
            Arg_Element => Variant);
   end Variant_Choices;
------------------------------------------------------------------------------
--  OPEN PROBLEMS:
--
--  1. Is using of the special list construction function
--     Discrete_Choice_Node_To_Element_List really necessary here? We should
--     try to replace it by non-special (trivial) constructor (all
--     necessary local mapping items for Nodes in the Node List have
--     already been defined - ???).
--
--     IT SEEMS TO BE NOT ONLY OK, BUT REALLY NECESSARY HERE (03.11.95)
------------------------------------------------------------------------------
   function Ancestor_Subtype_Indication
     (Definition : Asis.Definition)
      return       Asis.Definition
   is
      Arg_Kind : constant Internal_Element_Kinds := Int_Kind (Definition);
      Arg_Node : Node_Id;
   begin
      Check_Validity
        (Definition, Package_Name & "Ancestor_Subtype_Indication");

      if not (Arg_Kind = A_Private_Extension_Definition) then
         Raise_ASIS_Inappropriate_Element
           (Diagnosis  => Package_Name & "Ancestor_Subtype_Indication",
            Wrong_Kind => Arg_Kind);
      end if;

      Arg_Node := Node (Definition);

      return Node_To_Element_New
               (Node             => Sinfo.Subtype_Indication (Arg_Node),
                Starting_Element => Definition,
                 Internal_Kind   => A_Subtype_Indication);
   exception
      when ASIS_Inappropriate_Element =>
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information
              (Argument   => Definition,
               Outer_Call => Package_Name & "Ancestor_Subtype_Indication");
         end if;

         raise;
      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name  => Package_Name & "Ancestor_Subtype_Indication",
            Ex          => Ex,
            Arg_Element => Definition);
   end Ancestor_Subtype_Indication;
-----------------------------------------------------------------------------
   function Visible_Part_Items
     (Definition      : Asis.Definition;
      Include_Pragmas : Boolean := False)
      return            Asis.Definition_List
   is
      Arg_Kind : constant Internal_Element_Kinds := Int_Kind (Definition);
      Arg_Node : Node_Id;
   begin
      Check_Validity (Definition, Package_Name & "Visible_Part_Items");

      if not (Arg_Kind = A_Task_Definition or else
              Arg_Kind = A_Protected_Definition)
      then
         Raise_ASIS_Inappropriate_Element
           (Package_Name & "Visible_Part_Items",
            Wrong_Kind => Arg_Kind);
      end if;

      Arg_Node := Node (Definition);

      return N_To_E_List_New
               (List             => Visible_Declarations (Arg_Node),
                Include_Pragmas  => Include_Pragmas,
                Starting_Element => Definition);

   exception
      when ASIS_Inappropriate_Element =>
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information
              (Argument   => Definition,
               Bool_Par   => Include_Pragmas,
               Outer_Call => Package_Name & "Visible_Part_Items");
         end if;

         raise;
      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name  => Package_Name & "Visible_Part_Items",
            Ex          => Ex,
            Arg_Element => Definition,
            Bool_Par_ON => Include_Pragmas);
   end Visible_Part_Items;
-----------------------------------------------------------------------------

   function Private_Part_Items
     (Definition      : Asis.Definition;
      Include_Pragmas : Boolean := False)
      return            Asis.Definition_List
   is
      Arg_Kind : constant Internal_Element_Kinds := Int_Kind (Definition);
      Arg_Node : Node_Id;
   begin
      Check_Validity (Definition, Package_Name & "Private_Part_Items");

      if not (Arg_Kind = A_Task_Definition or else
              Arg_Kind = A_Protected_Definition)
      then
         Raise_ASIS_Inappropriate_Element
           (Package_Name & "Private_Part_Items",
            Wrong_Kind => Arg_Kind);
      end if;

      Arg_Node := Node (Definition);

      return N_To_E_List_New
               (List            => Private_Declarations (Arg_Node),
                Include_Pragmas => Include_Pragmas,
                Starting_Element => Definition);
   exception
      when ASIS_Inappropriate_Element =>
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information
              (Argument   => Definition,
               Bool_Par   => Include_Pragmas,
               Outer_Call => Package_Name & "Private_Part_Items");
         end if;

         raise;
      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name  => Package_Name & "Private_Part_Items",
            Ex          => Ex,
            Arg_Element => Definition,
            Bool_Par_ON => Include_Pragmas);
   end Private_Part_Items;
-----------------------------------------------------------------------------
   function Is_Private_Present
     (Definition : Asis.Definition)
      return       Boolean
   is
      Arg_Kind : constant Internal_Element_Kinds := Int_Kind (Definition);
      Arg_Node : Node_Id;
   begin
      Check_Validity (Definition, Package_Name & "Is_Private_Present");

      if not (Arg_Kind = A_Task_Definition or else
              Arg_Kind = A_Protected_Definition)
      then
         --  unexpected element
         return False;
      end if;
      Arg_Node := Node (Definition);

      return Present (Private_Declarations (Arg_Node));
   exception
      when ASIS_Inappropriate_Element =>
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information
              (Argument   => Definition,
               Outer_Call => Package_Name & "Is_Private_Present");
         end if;

         raise;
      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name  => Package_Name & "Is_Private_Present",
            Ex          => Ex,
            Arg_Element => Definition);
   end Is_Private_Present;
-----------------------------------------------------------------------------
end Asis.Definitions;
