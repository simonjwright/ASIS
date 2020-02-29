------------------------------------------------------------------------------
--                                                                          --
--                            GNAT2XML COMPONENTS                           --
--                                                                          --
--       G N A T 2 X M L . A D A _ T R E E S . A S I S _ T O _ T R E E      --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                      Copyright (C) 2013-2017, AdaCore                    --
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

with Ada.Characters.Handling;
with Ada.Characters.Conversions; use Ada.Characters.Conversions;
with Ada.Directories;
with Ada.Strings.Wide_Fixed;     use Ada.Strings.Wide_Fixed;

with Asis.Elements;
with Asis.Text;
with Asis.Compilation_Units;
with Asis.Declarations;
with Asis.Definitions;
with Asis.Expressions;
with Asis.Extensions; use Asis.Extensions;
with Asis.Set_Get;

with Text_IO;      use Text_IO;
with A4G.A_Output; use A4G.A_Output; -- ???
with A4G.Int_Knds;
with A4G.Queries;
with A4G.Mapping;

with ASIS_UL.Options;

with Ada_Trees.Rep_Clauses;

package body Ada_Trees.Asis_To_Tree is

   subtype Structural_Queries is A4G.Queries.Structural_Queries;

   Generic_Nesting_Count : Natural := 0;
   --  Number of generic units we are inside of. This is needed because we
   --  can't generate representation clauses for types in generic units. They
   --  have no representation, and for example, DD.Size returns 0.

   function In_Generic return Boolean is (Generic_Nesting_Count > 0);

   function Decl_Of_Def_Id (Def_Id : Defining_Name) return Asis.Declaration;
   --  Return the declaration of the defining name

   function Decl_Kind (Def_Id : Defining_Name) return String;
   --  Return the "kind" string for the defining name. This is the 'Image of
   --  the Kind of Decl_Of_Def_Id (Def_Id), with "A_", "An_", "_Declaration",
   --  "_Specification" stripped off.

   function Enclosing_Def_Id (Def_Id : Defining_Name) return Asis.Element;
   --  Return the Defining_Name of the innermost enclosing declaration of
   --  Def_Id, or Nil_Element if Def_Id is a root library unit or Standard.
   --  If we have spec/body, we return the first (spec) one.

   function Unique_Id (Def_Id : Defining_Name) return String;
   --  Return a unique string globally identifying Def_Id

   function Ref_Value (Ref_Id : Asis.Name) return String;
   function Ref_Value (Ref_Id : Asis.Name) return Name_Id is
     (Name_Find (Ref_Value (Ref_Id)));

   function Ref_Name_Value (Ref_Id : Asis.Name) return String;
   function Ref_Name_Value (Ref_Id : Asis.Name) return Name_Id is
     (Name_Find (Ref_Name_Value (Ref_Id)));

   function Type_Value (Elem : Asis.Element) return String;
   function Type_Value (Elem : Asis.Element) return Name_Id is
     (Name_Find (Type_Value (Elem)));

   procedure Set_Usage_Name_Attrs
     (Q      : Structural_Queries;
      Ref_Id : Asis.Name;
      Result : in out Ada_Tree_Rec);
   --  Ref_Id is a usage name. Sets the Ref_Name_Value, Ref_Value, Decl_Kind,
   --  and Is_Predef attributes of the node.

   function Name_Definition
     (Reference : Asis.Expression)
      return      Asis.Defining_Name;
   --  Wrapper to work around bugs in Corresponding_Name_Definition

   function Name_Definition
     (Reference : Asis.Expression)
      return      Asis.Defining_Name
   is
      use Expressions;
   begin
      return Corresponding_Name_Definition (Reference);
   exception
      --  In production mode, if Corresponding_Name_Definition crashes, we just
      --  return Nil. The only thing we will do wrong is possibly print
      --  identifiers in the wrong case.

      when others =>
         if Assert_Enabled then
            raise;
         else
            return Nil_Element;
         end if;
   end Name_Definition;

   ----------------------------------------------------------------------------

   function Decl_Of_Def_Id (Def_Id : Defining_Name) return Asis.Declaration is
      Result : Asis.Element := Def_Id;
      use Asis.Elements;

   begin
      loop
         Result := Enclosing_Element (Result);
         if Ekind (Result) in Flat_Declaration_Kinds then
            return Result;
         end if;
      end loop;
   end Decl_Of_Def_Id;

   function Decl_Kind (Def_Id : Defining_Name) return String is
      use Ada.Characters.Handling;
      Decl : constant Asis.Declaration       := Decl_Of_Def_Id (Def_Id);
      Kind : constant Flat_Declaration_Kinds := Ekind (Decl);
      S1   : constant String                 := To_Lower (Kind'Img);
      S2   : constant String                 := Strip_Article (S1);
      S3   : constant String := Strip_Suffix (S2, Suffix => "_Declaration");
      S4   : constant String := Strip_Suffix (S3, Suffix => "_Specification");

   begin
      return S4;
   end Decl_Kind;

   ----------------------------------------------------------------------------

   function Enclosing_Def_Id (Def_Id : Defining_Name) return Asis.Element is
      use Asis.Elements;
      pragma Assert
        (Is_Identical (Def_Id, Corresponding_First_Definition (Def_Id)));
      Decl : Asis.Declaration := Decl_Of_Def_Id (Def_Id);

   begin
      --  Here, Decl is the Declaration of Def_Id. We loop upwards to find the
      --  innermost enclosing Decl.
      loop
         Decl := Enclosing_Element (Decl);
         case Ekind (Decl) is
            when Flat_Declaration_Kinds =>
               return Corresponding_First_Definition (First_Name (Decl));

            when Not_An_Element =>
               return Nil_Element;

            when others =>
               null; -- keep looping
         end case;
      end loop;
   end Enclosing_Def_Id;

   ----------------------------------------------------------------------------

   function Unique_Id (Def_Id : Defining_Name) return String is
      use Ada.Strings, Asis.Text, Asis.Declarations, Asis.Elements;

      D : constant Defining_Name := Corresponding_First_Definition (Def_Id);

      Span : constant Asis.Text.Span := Element_Span (D);
      L    : constant Wide_String    :=
        Line_Number_Positive'Wide_Image (Span.First_Line);
      LL : constant Wide_String := Trim (L, Left); -- remove annoying blank
      C  : constant Wide_String :=
        Character_Position_Positive'Wide_Image (Span.First_Column);
      CC : constant Wide_String := Trim (C, Left); -- remove annoying blank

      CU : constant Compilation_Unit := Enclosing_Compilation_Unit (D);
      Unit_Kind : constant Asis.Unit_Kinds :=
        Compilation_Units.Unit_Kind (CU);
      U : constant Wide_String :=
        (if Unit_Kind in A_Library_Unit_Body then "+" else "-");
      --  It is possible to have two declarations in the same scope that have
      --  the same Sloc, if one is in the spec and the other in the body, so
      --  we include a different character for spec vs. body to handle this
      --  unlikely case.

      Simple_Name : constant String :=
        To_UTF8 (Defining_Name_Image (D) & U & LL & ":" & CC);
      Enclosing : constant Asis.Element := Enclosing_Def_Id (D);

      --  If it is a subunit, then we need to include the parent name in
      --  order to make it unique, because two subunits in different files
      --  could have the same name. This only applies if the subunit is a
      --  subprogram body with no spec. This is not necessary for child
      --  units, because their Defining_Name_Image already includes the
      --  parent.

      use Asis.Compilation_Units;
      Subunit_Parent : constant Wide_String :=
        (if True and then Unit_Class (CU) = A_Separate_Body
           and then Is_Equal (Def_Id, Names (Unit_Declaration (CU)) (1))
         then Unit_Full_Name (Corresponding_Subunit_Parent_Body (CU)) & "."
         else "");

   begin
      if Is_Nil (Enclosing) then
         return To_UTF8 (Subunit_Parent) & Simple_Name;
      else
         return Unique_Id (Enclosing) & "/" &
           To_UTF8 (Subunit_Parent) & Simple_Name;
      end if;
   end Unique_Id;

   function Def_Value (Def_Id : Defining_Name) return String is
   begin
      --  Use To_Utf8 here and elsewhere???
      return "ada://" & Decl_Kind (Def_Id) & "/" & Unique_Id (Def_Id);
   end Def_Value;

   function Def_Name_Value (Def_Id : Defining_Name) return String is
      use Asis.Declarations;

      D : constant Defining_Name := Corresponding_First_Definition (Def_Id);

   begin
      if Ekind (D) = A_Defining_Expanded_Name then
         return Def_Name_Value (Defining_Selector (D));
      else
         return To_UTF8 (Defining_Name_Image (D));
      end if;
   end Def_Name_Value;

   function Ref_Value (Ref_Id : Asis.Name) return String is

   begin
      if Is_Uniquely_Defined (Ref_Id) then
         declare
            Def_Id : constant Asis.Element := Name_Definition (Ref_Id);

         begin
            case Ekind (Def_Id) is
               when Not_An_Element =>
                  return "implicit declaration";

               when Def_Names =>
                  return Def_Value (Def_Id);

               when others =>
                  raise Program_Error;
            end case;
         end;

      else
         --  It's something like a pragma or attribute name, so there is no
         --  corresponding name definition.
         return "null";
      end if;
   end Ref_Value;

   function Ref_Name_Value (Ref_Id : Asis.Name) return String is
      use Expressions;
   begin
      --  If there is a corresponding name definition, we return that name, so
      --  that casing is normalized (if you declare Mumble, and refer to it as
      --  mumble, the ref_name will be Mumble). If the declaration is implicit,
      --  or there is none, we return the name as written.

      if Is_Uniquely_Defined (Ref_Id) then
         declare
            Def_Id : constant Asis.Element := Name_Definition (Ref_Id);

         begin
            case Ekind (Def_Id) is
               when Not_An_Element =>
                  null;

               when Def_Names =>
                  return Def_Name_Value (Def_Id);

               when others =>
                  raise Program_Error;
            end case;
         end;

      else
         --  It's something like a pragma or attribute name, so there is no
         --  corresponding name definition.
         null;
      end if;

      declare
         Name_Im : constant W_Str  := Name_Image (Ref_Id);
         Result  : constant String := To_UTF8 (Name_Im);
      begin
         return Result;
      end;
   end Ref_Name_Value;

   ----------------------------------------------------------------------------

   function Type_Value (Elem : Asis.Element) return String is
      use Asis.Elements, Asis.Expressions, Asis.Declarations;
      Kind : constant Opt_ASIS_Elems := Ekind (Elem);

   begin
      case Kind is
         when Not_An_Element =>
            return "null";
         --  We need this in case Kind_Fixup below returned A_Box_Expression
         --  or A_Null_Literal.

         when Def_Names =>
            declare
               Decl : constant Asis.Declaration := Decl_Of_Def_Id (Elem);

            begin
               if Ekind (Decl) in A_Flat_Object_Declaration then
                  declare
                     Ident : Asis.Element := Object_Declaration_View (Decl);

                  begin
                     if Ekind (Ident) in A_Subtype_Indication then
                        Ident := Asis.Definitions.Subtype_Mark (Ident);
                     end if;

                     if Ekind (Ident) in A_Selected_Component then
                        Ident := Selector (Ident);
                     end if;

                     return Ref_Value (Ident);
                  end;

               else
                  return "null";
               end if;
            end;

         when Flat_Expression_Kinds =>
            if Is_True_Expression (Elem) then
               declare
                  Type_Decl : constant Asis.Declaration :=
                    Corresponding_Expression_Type (Elem);

               begin
                  if Is_Nil (Type_Decl) then
                     return "anonymous subtype";

                  elsif Asis.Set_Get.Is_Root_Num_Type (Type_Decl) then
                     --  We put a blank in the names of root and universal
                     --  numeric types (instead of an underscore) to
                     --  distinguish them from a user-defined type with
                     --  the same name.

                     case Root_Type_Kind (Type_Declaration_View (Type_Decl)) is
                        when Not_A_Root_Type_Definition =>
                           raise Program_Error;

                        when A_Root_Integer_Definition =>
                           return "root integer";

                        when A_Root_Real_Definition =>
                           return "root real";

                        when A_Universal_Integer_Definition =>
                           return "universal integer";

                        when A_Universal_Real_Definition =>
                           return "universal real";

                        when A_Universal_Fixed_Definition =>
                           return "universal fixed";
                     end case;

                  else
                     return Def_Value
                         (Corresponding_First_Definition
                            (First_Name (Type_Decl)));
                  end if;
               end;

            else
               return "null";
            end if;

         when others =>
            raise Program_Error;
      end case;
   end Type_Value;

   ----------------------------------------------------------------------------

   procedure Set_Usage_Name_Attrs
     (Q      : Structural_Queries;
      Ref_Id : Asis.Name;
      Result : in out Ada_Tree_Rec)
   is
   begin
      Result.Ref_Name := Ref_Name_Value (Ref_Id);
      Result.Ref      := Ref_Value (Ref_Id);

      if Is_Uniquely_Defined (Ref_Id) then
         declare
            use Compilation_Units, Elements;
            Def_Id : constant Asis.Element := Name_Definition (Ref_Id);
         begin
            case Ekind (Def_Id) is
               when Not_An_Element =>
                  null;

               when Def_Names =>
                  declare
                     Decl : constant Asis.Declaration :=
                       Decl_Of_Def_Id (Def_Id);
                     Decl_Kind : constant Flat_Declaration_Kinds :=
                       Ekind (Decl);
                  begin
                     Result.Decl_Kind := Decl_Kind;
                  end;

                  if Unit_Origin (Enclosing_Compilation_Unit (Def_Id)) /=
                    An_Application_Unit
                  then
                     Result.Is_Predef := True;
                  end if;

               when others =>
                  raise Program_Error;
            end case;
         end;

      else
         --  It's something like a pragma or attribute name, so there is no
         --  corresponding name definition.

         if Q = Attribute_Designator_Identifier then
            Result.Decl_Kind := An_Unknown_Attribute;
         end if;
      end if;
   end Set_Usage_Name_Attrs;

   ----------------------------------------------------------------------------

   Stop_Kinds : array (Opt_ASIS_Elems) of Boolean :=
     (An_If_Expression => True, others => False);
   pragma Export (Ada, Stop_Kinds);
   --  For debugging. E.g., set Stop_Kinds(A_Component_Declaration) := True
   --  in gdb to stop when Pre is passed A_Component_Declaration. And set a
   --  breakpoint on Breakpoint.

   procedure Breakpoint;

   procedure Breakpoint is
   begin
      null;
   end Breakpoint;

   ----------------------------------------------------------------------------

   function Compilation_Unit_To_Tree
     (The_Unit : Asis.Compilation_Unit;
      Gen_Regions : Scanner.Token_Vector_Ptr := null)
     return Ada_Tree
   is
      use Compilation_Units, Expressions;

      Enclosing_Formal_Subp : Asis.Element := Asis.Nil_Element;
      --  See Kind_Fixup below

      function Element_To_Tree
        (Q       : Structural_Queries;
         Element : Asis.Element)
         return    Ada_Tree;

      function List_To_Tree
        (Q    : Structural_Queries;
         List : Asis.Element_List)
         return Ada_Tree;

      function Pre
        (Q       : Structural_Queries;
         Element : Asis.Element)
         return    Ada_Tree;

      function In_Gen_Regions (Element : Asis.Element) return Boolean;
      --  Wrapper for Scanner.In_Gen_Regions

      function In_Gen_Regions (Element : Asis.Element) return Boolean is
         use Scanner;
      begin
         if Gen_Regions /= null then
            return Result : constant Boolean :=
              In_Gen_Regions (Span (Element).First_Line, Gen_Regions.all)
            do
               pragma Assert
                 (Result = In_Gen_Regions
                             (Span (Element).Last_Line, Gen_Regions.all));
               --  If it's inside the region, then it's entirely inside.
            end return;
         else
            return False;
         end if;
      end In_Gen_Regions;

      function Pre
        (Q       : Structural_Queries;
         Element : Asis.Element)
         return    Ada_Tree
      is

         function Kind_Fixup return Opt_ASIS_Elems;
         --  This works around the handling of generic formal subprogram
         --  defaults. In ASIS, the Formal_Subprogram_Default query doesn't
         --  work very well. If the default is "is Some_Name" it returns
         --  Some_Name, but if it's "is <>" or "is null", then the version in
         --  Declarations raises an exception, and the version in Extensions
         --  returns a Nil_Element. Neither behavior is very useful, so this
         --  function detects these cases, and changes the Kind accordingly.
         --
         --  In almost all cases, this just returns Ekind (Element).

         function Kind_Fixup return Opt_ASIS_Elems is
            Result : Opt_ASIS_Elems := Ekind (Element);

         begin
            pragma Assert
              (if
                 Q = Formal_Subprogram_Default
               then
                 Ekind (Enclosing_Formal_Subp) in
                   A_Formal_Procedure_Declaration |
                     A_Formal_Function_Declaration);

            if Q = Formal_Subprogram_Default then
               if Result = Not_An_Element then
                  --  We can't just look at Enclosing_Element, because it
                  --  doesn't work for Nil.

                  case Asis.Elements.Default_Kind (Enclosing_Formal_Subp) is
                     when Not_A_Default =>
                        --  Can't get here, because the Enclosing_Formal_Subp
                        --  must be an appropriate element for Default_Kind.
                        raise Program_Error;

                     when A_Name_Default =>
                        --  Can't get here, because Result would not be
                        --  Not_An_Element in this case.
                        raise Program_Error;

                     when A_Box_Default =>
                        Result := A_Box_Expression;

                     when A_Null_Default =>
                        Result := A_Null_Literal;

                     when A_Nil_Default =>
                        --  Result = Not_An_Element is correct in this case
                        null;
                  end case;
               end if;

               Enclosing_Formal_Subp := Nil_Element;
            end if;

            return Result;
         end Kind_Fixup;

         Kind : constant Opt_ASIS_Elems := Kind_Fixup;

         use Asis.Elements;

      --  Start of processing for Pre

      begin
         if Stop_Kinds (Kind) then
            Breakpoint;
         end if;

         if Kind in
             A_Formal_Procedure_Declaration |
               A_Formal_Function_Declaration
         then
            Enclosing_Formal_Subp := Element;
         end if;

         return
           Result : constant Ada_Tree :=
             new Ada_Tree_Rec (Kind, Num_Queries (Kind))
         do
            Result.Sloc := Span (Element);

            if Kind = Not_An_Element then
               Result.Checks := Empty_Check_Set;
            else
               Result.Checks := Needed_Checks (Element);
            end if;

            case Kind is
               when A_Compilation_Unit | Def_Names =>
                  Result.Def_Name := Def_Name_Value (Element);

                  case Kind is
                     when A_Compilation_Unit =>
                        raise Program_Error;

                     when Def_Names =>
                        Result.Def       := Def_Value (Element);
                        Result.Decl_Type := Type_Value (Element);

                     when others =>
                        null;
                  end case;

               when Flat_Expression_Kinds =>
                  Result.Expr_Type := Type_Value (Element);

                  case Kind is
                     when Usage_Names =>
                        Set_Usage_Name_Attrs (Q, Element, Result.all);

                     when An_Integer_Literal |
                       A_Real_Literal        |
                       A_String_Literal      =>
                        Result.Lit_Val :=
                          Name_Find (To_UTF8 (Value_Image (Element)));

                     when others =>
                        null;
                  end case;

               when Flat_Pragma_Kinds =>
                  pragma Assert
                    (Asis.Elements.Pragma_Name_Image (Element) /= "");
                  Result.Pragma_Name :=
                    Name_Find
                      (To_UTF8 (Asis.Elements.Pragma_Name_Image (Element)));

               when A_Parameter_Specification | A_Formal_Object_Declaration =>
                  Result.Mode := Mode_Kind (Element);

               when A_Comment =>
                  raise Program_Error;
                  --  These are never created by ASIS, only as Ada_Trees

               when others =>
                  null;
            end case;
         end return;
      end Pre;

      function Element_To_Tree
        (Q       : Structural_Queries;
         Element : Asis.Element)
         return    Ada_Tree
      is

         function Child_To_Tree (Q : Structural_Queries) return Ada_Tree;

         function Child_To_Tree (Q : Structural_Queries) return Ada_Tree is
            FE : constant Func_Elem := Get_Func_Elem (Q);

         begin
            case FE.Query_Kind is
               when Bug | CU_Query_Kinds =>
                  raise Program_Error;

               --  For Boolean_Query, we concoct a dummy element with the
               --  appropriate kind to pass to Element_To_Tree.

               when Boolean_Query =>
                  declare
                     Val : constant Boolean       := FE.Func_Boolean (Element);
                     Dummy_Kind : constant Boolean_Elems :=
                       Query_Result_Types (Q);

                     use A4G.Mapping;
                     Dummy_Child : constant Asis.Element :=
                       (if
                          Val
                        then
                          Node_To_Element_New
                            (Node          => Asis.Set_Get.Node (Element),
                             Internal_Kind =>
                               A4G.Int_Knds.Internal_Element_Kinds
                                 (Dummy_Kind),
                             In_Unit => The_Unit)
                        else Nil_Element);

                  begin
                     return Element_To_Tree (Q, Element => Dummy_Child);
                  end;

               when Single_Element_Query =>
                  declare
                     Child : constant Asis.Element := FE.Func_Simple (Element);

                  begin
                     return Element_To_Tree (Q, Element => Child);
                  end;

               when Element_List_Query =>
                  declare
                     Child_List : constant Asis.Element_List :=
                       FE.Func_List (Element);

                  begin
                     return List_To_Tree (Q, Child_List);
                  end;

               when Element_List_Query_With_Boolean =>
                  declare
                     Child_List : constant Asis.Element_List :=
                       FE.Func_List_Boolean (Element, FE.Bool);

                  begin
                     return List_To_Tree (Q, Child_List);
                  end;
            end case;
         end Child_To_Tree;

         Kind : constant Opt_ASIS_Elems := Ekind (Element);
         use Asis.Declarations;
         Qs : Query_List renames Appropriate_Queries (Kind).all;
         Result : Ada_Tree_Base;
         Gen : constant Boolean :=
             Kind in Flat_Declaration_Kinds and then Is_Generic (Element);

      --  Start of processing for Element_To_Tree

      begin
         if Debug_Mode then
            begin
               Write_Element (Element);
            exception
               when others =>
                  Put_Line ("Bug in Write_Element");
            end;

            if Kind in Def_Names then
               Put_Line
                 ("Defining_Name_Image = " &
                  To_UTF8 (Defining_Name_Image (Element)));
            end if;

            if Kind in Flat_Usage_Name_Kinds then
               Put_Line ("Name_Image = " & To_UTF8 (Name_Image (Element)));
            end if;
         end if;

         if Gen then
            Generic_Nesting_Count := Generic_Nesting_Count + 1;
         end if;

         Result := Pre (Q, Element);

         for Index in Qs'Range loop
            Result.Subtrees (Index) := Child_To_Tree (Qs (Index));
         end loop;

         if Gen then
            Generic_Nesting_Count := Generic_Nesting_Count - 1;
         end if;

         return Result;
      end Element_To_Tree;

      -------------------------------------------------------------------------

      function List_To_Tree
        (Q    : Structural_Queries;
         List : Asis.Element_List)
         return Ada_Tree
      is
         pragma Assert (List'First = 1);
         L : Ada_Tree_Vector;

         use Ada_Tree_Vectors;

      begin
         for Index in List'Range loop
            if In_Gen_Regions (List (Index)) then
               null; -- skip generated code in input
            else
               declare
                  E : constant Asis.Element := List (Index);
                  T : constant Ada_Tree := Element_To_Tree (Q, Element => E);
                  use ASIS_UL.Options;
               begin
                  Append (L, T);

                  if Generate_Representation_Clauses then
                     --  Types declared inside generic units have no
                     --  representation.

                     if not In_Generic then
                        Rep_Clauses.Append_Rep_Clauses (L, E, T);
                     end if;
                  end if;
               end;
            end if;
         end loop;

         return
            Result : constant Ada_Tree :=
              new Ada_Tree_Rec (Query_Result_Types (Q), Last_Index (L))
         do
            Result.Sloc := Asis.Text.Nil_Span;
            Result.Subtrees := Elems (L) (1 .. Last_Index (L));
         end return;
      end List_To_Tree;

      Name : constant String := To_String (Unit_Full_Name (The_Unit));
      Src  : constant String :=
        Ada.Directories.Simple_Name (To_String (Text_Name (The_Unit)));
      Unit_Span : constant Asis.Text.Span :=
        Asis.Text.Compilation_Unit_Span
          (Asis.Elements.Unit_Declaration (The_Unit));

      Cont_Clause_Elements : constant Element_List :=
        Asis.Elements.Context_Clause_Elements
          (Compilation_Unit => The_Unit,
           Include_Pragmas  => True);
      Unit_Element : constant Asis.Element :=
        Asis.Elements.Unit_Declaration (The_Unit);
      Pragmas : constant Element_List :=
        Asis.Extensions.Pragmas_After (Compilation_Unit => The_Unit);

   --  Start of processing for Compilation_Unit_To_Tree

   begin
      return Result : constant Ada_Tree :=
        new Ada_Tree_Rec'
          (Kind           => A_Compilation_Unit,
           Subtree_Count  => 3,
           Checks         => Asis.Extensions.Empty_Check_Set,
           Sloc           => Unit_Span,
           Unit_Kind      => Unit_Kind (The_Unit),
           Unit_Class     => Unit_Class (The_Unit),
           Unit_Origin    => Unit_Origin (The_Unit),
           Unit_Full_Name => Name_Find (To_UTF8 (Unit_Full_Name (The_Unit))),
           Def_Name       => Name_Find (Name),
           Source_File    => Name_Find (Src),

           Subtrees =>
             (1 =>
                List_To_Tree
                  (Q    => Context_Clause_Elements,
                   List => Cont_Clause_Elements),

              2 =>
                Element_To_Tree
                  (Q       => Unit_Declaration,
                   Element => Unit_Element),

              3 => List_To_Tree (Q => Pragmas_After, List => Pragmas)))
      do
         pragma Assert (Generic_Nesting_Count = 0);
      end return;
   end Compilation_Unit_To_Tree;

end Ada_Trees.Asis_To_Tree;
