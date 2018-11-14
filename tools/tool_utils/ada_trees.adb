------------------------------------------------------------------------------
--                                                                          --
--                            GNAT2XML COMPONENTS                           --
--                                                                          --
--                    G N A T 2 X M L . A D A _ T R E E S                   --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                     Copyright (C) 2012-2017, AdaCore                     --
--                                                                          --
-- Gnat2xml is free software; you can redistribute it and/or modify it      --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software Foundation;  either version 2,  or  (at your option)  any later --
-- version. Gnat2xml is distributed  in the hope  that it will be useful,   --
-- but WITHOUT ANY WARRANTY; without even the implied warranty of MER-      --
-- CHANTABILITY or  FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General  --
-- Public License for more details. You should have received a copy of the  --
-- GNU General Public License distributed with GNAT; see file COPYING. If   --
-- not, write to the Free Software Foundation, 59 Temple Place Suite 330,   --
-- Boston, MA 02111-1307, USA.                                              --
-- The gnat2xml tool was derived from the Avatox sources.                   --
------------------------------------------------------------------------------

pragma Ada_2012;

with System.Address_Image;

with ASIS_UL.Formatted_Output;

with Asis.Elements;
with Asis.Set_Get;

package body Ada_Trees is

   function Addr_Img (X : System.Address) return String;
   --  Image of X without leading '0's

   function Addr_Img (X : System.Address) return String is
      Result : constant String := System.Address_Image (X);
      pragma Assert (Result'First = 1);
      First : Positive := 1;
   begin
      while First < Result'Last and then Result (First) = '0' loop
         First := First + 1;
      end loop;
      pragma Assert (First = Result'Last or else Result (First) /= '0');
      return Result (First .. Result'Last);
   end Addr_Img;

   function T_Img (Tree : Ada_Tree_Base) return String is
   begin
      return
        (if Tree = null then "null"
         else Addr_Img (Tree.all'Address) & " " & Image (Tree.Kind));
   end T_Img;

   function Ekind (Element : Asis.Element) return Opt_ASIS_Elems is
   begin
      return Opt_ASIS_Elems (Asis.Set_Get.Int_Kind (Element));
   end Ekind;

   function Span (Element : Asis.Element) return Asis.Text.Span is
      use Asis.Elements;
   begin
      if Ekind (Element) in Flat_Element_Kinds then
         if ASIS_UL.Debug.Debug_Flag_2 and then not Is_Nil (Element) then
            --  Attempt to defeat bugs in ASIS.Text that prevent debugging
            --  printouts from working.
            return (First_Line   => 1,
                    First_Column => 1,
                    Last_Line    => 1,
                    Last_Column  => 1);
         else
            return Asis.Text.Element_Span (Element);
         end if;
      else
         return Asis.Text.Nil_Span;
      end if;
   end Span;

   function Ada_Tree_Rec_OK
     (X    : Ada_Tree_Rec)
      return Boolean is
     (X.Kind in Flat_List_Kinds
      or else X.Subtree_Count = Num_Queries (X.Kind));

   function Empty
     (Kind : Flat_List_Kinds;
      Sloc : Asis.Text.Span := Asis.Text.Nil_Span)
      return Ada_Tree
   is
   begin
      return
        Result : constant Ada_Tree :=
          new Ada_Tree_Rec (Kind, Subtree_Count => 0)
      do
         Result.Sloc := Sloc;
         Result.Checks := Asis.Extensions.Empty_Check_Set;
         pragma Assert (Result.Subtrees = Empty_Tree_Array);
      end return;
   end Empty;

   function Ref (T : Ada_Tree) return Name_Id is
   begin
      case T.Kind is
         when Usage_Names =>
            return T.Ref;

         when A_Selected_Component =>
            return T.Subtrees (2).Ref; -- Ref of the Selector

         when others =>
            raise Program_Error;
      end case;
   end Ref;

   procedure Free_Tree (T : Ada_Tree_Base) is
      Temp   : Ada_Tree_Base := T;
      Temp_2 : Ada_Tree_Base;
   begin
      if T = The_Nil'Access then
         return;
      end if;

      for Subtree of Temp.Subtrees loop
         Temp_2 := Subtree;
         Free_Tree (Temp_2);
      end loop;
      Free_Tree_Rec (Temp);
   end Free_Tree;

   procedure Free_Subtrees (A : Ada_Tree_Array_Ref) is
      Temp   : Ada_Tree_Array_Ref := A;
      Temp_2 : Ada_Tree_Base;
   begin
      for Subtree of Temp.all loop
         Temp_2 := Subtree;
         Free_Tree (Temp_2);
      end loop;
      Free_Tree_Array (Temp);
   end Free_Subtrees;

   ----------------

   type Kinds_In_Class_Table is array (Flat_Abstract_Classes) of Kind_Set;
   --  Table mapping classes to the set of kinds that are in that class.
   --  Kinds_In_Class (C) (K) is True if and only if kind K is in class C.

   function Init_Kinds_In_Class return Kinds_In_Class_Table;

   function Cardinality (Kinds : Kind_Set) return Natural is
   begin
      return Result : Natural := 0 do
         for K of Kinds loop
            if K then
               Result := Result + 1;
            end if;
         end loop;
      end return;
   end Cardinality;

   procedure Put_Kinds (Kinds : Kind_Set) is
      use Formatted_Output;
      First_Time : Boolean := True;

   begin
      for K in Kinds'Range loop
         if Kinds (K) then
            if First_Time then
               First_Time := False;

            else
               Put (" |\n");
            end if;

            Put ("\1", Capitalize (K'Img));
         end if;
      end loop;
   end Put_Kinds;

   function Init_Kinds_In_Class return Kinds_In_Class_Table is
   begin
      return Result : Kinds_In_Class_Table := (others => (others => False)) do
         declare
            generic
               --  Instantiating this package has the side effect of setting
               --  Result (Class) to the set of kinds in Class. It is
               --  instantiated below with each class, and the corresponding
               --  Flat subtype.

               Class : Flat_Abstract_Classes;
               type Kind_Subtype is new Ada_Tree_Kind;
            package C is
            end C;

            package body C is
            begin
               for K in Kind_Subtype loop
                  Result (Class) (Flat_Element_Kinds (K)) := True;
               end loop;
            end C;

            pragma Warnings (Off); -- following not referenced
            package An_Element_Class_Init is new C
              (An_Element_Class,
               Flat_Element_Kinds);
            package An_Association_Class_Init is new C
              (An_Association_Class,
               Flat_Association_Kinds);
            package A_Constraint_Class_Init is new C
              (A_Constraint_Class,
               Flat_Constraint_Kinds);
            package A_Context_Clause_Class_Init is new C
              (A_Context_Clause_Class,
               Flat_Context_Clause_Kinds);
            package A_Declaration_Class_Init is new C
              (A_Declaration_Class,
               Flat_Declaration_Kinds);
            package A_Declarative_Item_Class_Init is new C
              (A_Declarative_Item_Class,
               Flat_Declarative_Item_Kinds);
            package A_Definition_Class_Init is new C
              (A_Definition_Class,
               Flat_Definition_Kinds);
            package A_Discrete_Range_Class_Init is new C
              (A_Discrete_Range_Class,
               Flat_Discrete_Range_Kinds);
            package A_Discrete_Subtype_Definition_Class_Init is new C
              (A_Discrete_Subtype_Definition_Class,
               Flat_Discrete_Subtype_Definition_Kinds);
            package A_Defining_Name_Class_Init is new C
              (A_Defining_Name_Class,
               Flat_Defining_Name_Kinds);
            package An_Expression_Class_Init is new C
              (An_Expression_Class,
               Flat_Expression_Kinds);
            package A_Name_Class_Init is new C (A_Name_Class, Flat_Name_Kinds);
            package A_Path_Class_Init is new C (A_Path_Class, Flat_Path_Kinds);
            package A_Pragma_Element_Class_Init is new C
              (A_Pragma_Element_Class,
               Flat_Pragma_Kinds);
            package A_Range_Constraint_Class_Init is new C
              (A_Range_Constraint_Class,
               Flat_Range_Constraint_Kinds);
            package A_Record_Component_Class_Init is new C
              (A_Record_Component_Class,
               Flat_Record_Component_Kinds);
            package A_Statement_Class_Init is new C
              (A_Statement_Class,
               Flat_Statement_Kinds);
            pragma Warnings (On);

         begin
            null;
         end;

         --  We allow nils everywhere, because they are needed in boolean
         --  elements. Pragmas are allowed in all sorts of places in the Ada
         --  syntax, so we simply allow them everywhere. We also allow comments
         --  everywhere.

         for C in Flat_Abstract_Classes loop
            Result (C) (Not_An_Element)    := True;
            Result (C) (Flat_Pragma_Kinds) := (others => True);
            Result (C) (A_Comment) := True;
         end loop;

         Result (A_Pragma_Element_Class) (Not_An_Element) := False;
         Result (A_Pragma_Element_Class) (A_Comment) := False;

         --  Some kinds are missing from the Asis documentation, and from the
         --  flat kind subtypes:

         Result (A_Definition_Class) (An_Identifier)        := True;
         Result (A_Definition_Class) (A_Selected_Component) := True;
         Result (A_Definition_Class) (A_Base_Attribute)     := True;
         Result (A_Definition_Class) (A_Class_Attribute)    := True;
         Result (A_Name_Class) (Flat_Attribute_Reference_Kinds) :=
           (others => True);
         Result (An_Expression_Class) (An_Others_Choice) := True;

         --  The following are for the Array_Component_Choices query, which is
         --  documented to return an Expression_List:

         Result (An_Expression_Class) (A_Discrete_Simple_Expression_Range) :=
           True;
         Result (An_Expression_Class) (A_Discrete_Range_Attribute_Reference) :=
           True;
      end return;
   end Init_Kinds_In_Class;

   The_Kinds_In_Class : constant Kinds_In_Class_Table := Init_Kinds_In_Class;

   function Kinds_In_Class (Class : Flat_Element_Kinds'Base) return Kind_Set is
   begin
      if Class in Flat_Abstract_Classes then
         return The_Kinds_In_Class (Class);

      else
         return Result : Kind_Set := (others => False) do
            Result (Class)             := True;
            Result (Not_An_Element)    := True;
            Result (Flat_Pragma_Kinds) := (others => True);
         end return;
      end if;
   end Kinds_In_Class;

   function Kind_In_Class
     (Kind  : Opt_ASIS_Elems;
      Class : Flat_Abstract_Classes)
      return  Boolean
   is
   begin
      return Result : constant Boolean := Kinds_In_Class (Class) (Kind);
   end Kind_In_Class;

   subtype Elem_Query_Index is Query_Index range 1 .. 100;
   --  Index for a query on an element. Query_Index is also used for lists,
   --  which can be arbitrarily long, but elements never have more than a
   --  handful of queries; 100 should be plenty. We'd like it to fit in 1
   --  byte, so Query_Offset_Table isn't too big.

   subtype Elem_Query_Count is
     Elem_Query_Index'Base range 0 .. Elem_Query_Index'Last;

   type Query_Offset_Table is
     array (Opt_ASIS_Elems, Structural_Queries) of Elem_Query_Count;
   --  Query_Offsets (K, Q) is the offset of query Q in elements of kind K.
   --  Zero means query Q is not allowed for kind K, or for Boolean queries,
   --  always returns False.

   function Init_Query_Offset_Table return Query_Offset_Table;

   function Init_Query_Offset_Table return Query_Offset_Table is
   begin
      return Result : Query_Offset_Table := (others => (others => 0)) do
         for K in Opt_ASIS_Elems loop
            declare
               Qs : Query_List renames Appropriate_Queries (K).all;

            begin
               for Q_Index in Qs'Range loop
                  Result (K, Qs (Q_Index)) := Q_Index;
               end loop;
            end;
         end loop;
      end return;
   end Init_Query_Offset_Table;

   Query_Offsets : constant Query_Offset_Table := Init_Query_Offset_Table;

   function Get (Tree : Ada_Tree; Q : Structural_Queries) return Ada_Tree is
   begin
      return Tree.Subtrees (Query_Offsets (Tree.Kind, Q));
   end Get;

   procedure Set
     (Tree    : Ada_Tree;
      Q       : Structural_Queries;
      Subtree : Ada_Tree)
   is
   begin
      Tree.Subtrees (Query_Offsets (Tree.Kind, Q)) := Subtree;
   end Set;

   function Generic_Getter (Tree : Ada_Tree) return Result_Type is
   begin
      return Result_Type (Get (Tree, Query));
   end Generic_Getter;

   procedure Generic_Setter (Tree : Ada_Tree; Subtree : Result_Type) is
   begin
      Set (Tree, Query, Ada_Tree (Subtree));
   end Generic_Setter;

   function Make
     (Kind     : Opt_ASIS_Elems;
      Subtrees : Assoc_List     := (1 .. 0 => <>);
      Sloc     : Asis.Text.Span := Asis.Text.Nil_Span)
      return     Ada_Tree
   is
      Result : constant Ada_Tree_Base :=
        new Ada_Tree_Rec (Kind, Subtree_Count => Subtrees'Length);
   begin
      Result.Sloc := Sloc;
      Result.Checks := Asis.Extensions.Empty_Check_Set;

      for Q_Index in Subtrees'Range loop
         declare
            Q : constant Structural_Queries := Subtrees (Q_Index).Query;

         begin
            pragma Assert (Query_Offsets (Kind, Q) = Q_Index);
            --  Assert that the subtrees are in order

            Set (Result, Q, Subtrees (Q_Index).Subtree);
         end;
      end loop;

      return Result;
   end Make;

   function Make_List
     (Kind     : Flat_List_Kinds;
      Subtrees : Ada_Tree_Array := Empty_Tree_Array;
      Sloc     : Asis.Text.Span := Asis.Text.Nil_Span)
      return     Ada_Tree
   is
      Result : constant Ada_Tree_Base :=
        new Ada_Tree_Rec (Kind, Subtree_Count => Subtrees'Length);
   begin
      Result.Sloc     := Sloc;
      Result.Checks := Asis.Extensions.Empty_Check_Set;
      Result.Subtrees := Subtrees;
      return Result;
   end Make_List;

   function Clone (Tree : Ada_Tree) return Ada_Tree is
   begin
      return Result : constant Ada_Tree := new Ada_Tree_Rec'(Tree.all) do
         for Subtree in Result.Subtrees'Range loop
            Result.Subtrees (Subtree) := Clone (Result.Subtrees (Subtree));
         end loop;
      end return;
   end Clone;

   function Hash (Key : Name_Id) return Ada.Containers.Hash_Type is
   begin
      return Ada.Containers.Hash_Type (Key);
   end Hash;

   procedure Resolve_Symbols (Tree : Ada_Tree) is
      procedure Walk (Decl : Ada_Tree; T : Ada_Tree);
      procedure Walk (Decl : Ada_Tree; T : Ada_Tree) is
         Pos : Symbol_Tables.Cursor;
         Ins : Boolean;
      begin
         if T.Kind in Def_Names then
            Insert (Symtab, T.Def, (Decl, T), Pos, Ins);
            --  There can be duplicates; for example, a formal parameter
            --  is duplicated on the spec and body of the procedure, and
            --  both have the same Def. We take the first one, here.
         end if;
         for Subtree of T.Subtrees loop
            Walk
              (Decl =>
                 (if T.Kind in Flat_Declaration_Kinds then T else Decl),
               T => Subtree);
         end loop;
      end Walk;
   begin
      Walk (Decl => Tree, T => Tree);
   end Resolve_Symbols;

   function Decl_Of_Def_Kind
     (Symtab : Symbol_Table;
      Def_Id : Ada_Tree)
      return   Opt_ASIS_Elems
   is
   begin
      return Decl_Of_Def (Symtab, Def_Id).Kind;
   end Decl_Of_Def_Kind;

   function Decl_Of_Def
     (Symtab : Symbol_Table; Def_Id : Ada_Tree) return Ada_Tree is
   begin
      --  Note that Symtab is empty when running xml2gnat, so this is written
      --  to survive "not found".
      if Contains (Symtab, Def_Id.Def) then
         return Symtab (Def_Id.Def).Decl;
      else
         return Nil;
      end if;
   end Decl_Of_Def;

   function Spec_Of_Body
     (Symtab : Symbol_Table; Body_Def : Ada_Tree) return Ada_Tree is
      The_Body : constant Ada_Tree := Decl_Of_Def (Symtab, Body_Def);
      Def : constant String := Get_Name_String (Body_Def.Def);
      Result : Ada_Tree := Body_Def;
   begin
      pragma Assert (Has_Prefix (Def, Prefix => "ada://"));
      case The_Body.Kind is
         --  A task body's Def contains "task_body", but shares the unique id
         --  with the space. Replace "task_body" with "task_type" and
         --  with "single_task" and try looking those up in Symtab.

         when A_Task_Body_Declaration =>
            pragma Assert (Has_Prefix (Def, Prefix => "ada://task_body"));
            declare
               Type_Def : constant Name_Id :=
                 Name_Find
                   ("ada://task_type" &
                    Strip_Prefix (Def, Prefix => "ada://task_body"));
               Obj_Def : constant Name_Id :=
                 Name_Find
                   ("ada://single_task" &
                    Strip_Prefix (Def, Prefix => "ada://task_body"));
            begin
               if Contains (Symtab, Type_Def) then
                  Result := Symtab (Type_Def).Def_Id;
                  pragma Assert
                    (Decl_Of_Def_Kind (Symtab, Result) =
                       A_Task_Type_Declaration);
               elsif Contains (Symtab, Obj_Def) then
                  Result := Symtab (Obj_Def).Def_Id;
                  pragma Assert
                    (Decl_Of_Def_Kind (Symtab, Result) =
                       A_Single_Task_Declaration);
               else
                  --  The unique_id is wrong in case of a private type
                  --  completed by a task. Possible bug in
                  --  Corresponding_First_Definition? So return Body_Def in
                  --  that case.
                  null; -- pragma Assert (False);
               end if;
            end;

         when A_Protected_Body_Declaration =>
            pragma Assert (Has_Prefix (Def, Prefix => "ada://protected_body"));
            declare
               Type_Def : constant Name_Id :=
                 Name_Find
                   ("ada://protected_type" &
                    Strip_Prefix (Def, Prefix => "ada://protected_body"));
               Obj_Def : constant Name_Id :=
                 Name_Find
                   ("ada://single_protected" &
                    Strip_Prefix (Def, Prefix => "ada://protected_body"));
            begin
               if Contains (Symtab, Type_Def) then
                  Result := Symtab (Type_Def).Def_Id;
                  pragma Assert
                    (Decl_Of_Def_Kind (Symtab, Result) =
                       A_Protected_Type_Declaration);
               elsif Contains (Symtab, Obj_Def) then
                  Result := Symtab (Obj_Def).Def_Id;
                  pragma Assert
                    (Decl_Of_Def_Kind (Symtab, Result) =
                       A_Single_Protected_Declaration);
               else
                  null; -- pragma Assert (False);
               end if;
            end;

         when others =>
            pragma Assert (False);
            --  ???Other cases not yet implemented; not needed so far.
      end case;

      return Result;
   end Spec_Of_Body;

end Ada_Trees;
