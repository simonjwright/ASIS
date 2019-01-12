------------------------------------------------------------------------------
--                                                                          --
--                            GNAT2XML COMPONENTS                           --
--                                                                          --
--       G N A T 2 X M L . A D A _ T R E E S . R E P _ C L A U S E S        --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                   Copyright (C) 2014-2018, AdaCore                       --
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

with Asis.Data_Decomposition;
with Asis.Data_Decomposition.Debug; use Asis.Data_Decomposition.Debug;
with Asis.Declarations;             use Asis.Declarations;

with Ada_Trees.Asis_To_Tree;
with Ada_Trees.Factory; use Ada_Trees.Factory;
with Pp.Scanner;

package body Ada_Trees.Rep_Clauses is
   use Ada_Tree_Vectors;

   package DD renames Asis.Data_Decomposition;

   function "+" (S : String) return Name_Id renames Name_Find;
   function Nil return Asis.Text.Span is (Asis.Text.Nil_Span);

   --  The following Do_... procedures are called by Append_Rep_Clauses (except
   --  Do_Rec_Comp_Clause, which is called by Do_Record_Clause) to generate a
   --  particular kind of clause. Def_Id is the defining id of the type, and
   --  Defn is the type definition.

   procedure Do_Size_Clause
     (Clauses : in out Ada_Tree_Vector;
      Def_Id  :        Ada_Tree;
      Defn    :        Asis.Element);

   procedure Do_Comp_Size_Clause
     (Clauses : in out Ada_Tree_Vector;
      Def_Id  :        Ada_Tree;
      Defn    :        Asis.Element);

   procedure Do_Rec_Comp_Clause
     (Comp_Clauses : in out Ada_Tree_Vector;
      Component    :        DD.Record_Component);

   procedure Do_Record_Clause
     (Clauses : in out Ada_Tree_Vector;
      Def_Id  :        Ada_Tree;
      Defn    :        Asis.Element);

   procedure Do_Comp_Size_Clause
     (Clauses : in out Ada_Tree_Vector;
      Def_Id  :        Ada_Tree;
      Defn    :        Asis.Element)
   is
      function Component_Size
        (Type_Definition : Asis.Type_Definition) return Asis.ASIS_Natural is
         (DD.Size (DD.Array_Components (Type_Definition)));
      --  There is no direct support in DD for the Component_Size attribute,
      --  but we can get that information from the Size of the
      --  Array_Components.

      Component_Size_Clause : constant Ada_Tree :=
        Attribute_Definition_Clause
          (Representation_Clause_Name =>
             Component_Size_Attribute
               (Expr_Type => +"null",
                Prefix    =>
                  Identifier
                    (Expr_Type => +"null",
                     Ref_Name  => Def_Id.Def_Name,
                     Ref       => Def_Id.Def,
                     Sloc      => Nil),
                Attribute_Designator_Identifier =>
                  Identifier
                    (Expr_Type => +"null",
                     Ref_Name  => +"Component_Size",
                     Ref       => +"null",
                     Sloc      => Nil),
                Sloc => Nil),
           Representation_Clause_Expression =>
             Integer_Literal
               (Expr_Type => +"universal integer",
                Lit_Val   => +Image (Component_Size (Defn)),
                Sloc      => Nil),
           Sloc => Nil);
   begin
      Append (Clauses, Component_Size_Clause);
   end Do_Comp_Size_Clause;

   procedure Do_Rec_Comp_Clause
     (Comp_Clauses : in out Ada_Tree_Vector;
      Component    :        DD.Record_Component)
   is
      Comp_Decl : constant Asis.Declaration :=
        DD.Component_Declaration (Component);
      pragma Assert
        (Ekind (Comp_Decl) in
           A_Discriminant_Specification | A_Component_Declaration);
      Comp_Def : constant Asis.Defining_Name := Names (Comp_Decl) (1);
      pragma Assert (Ekind (Comp_Def) = A_Defining_Identifier);

      Comp_Clause : constant Component_Clause_Tree :=
        Component_Clause
          (Representation_Clause_Name =>
             Identifier
               (Expr_Type => +"null",
                Ref_Name  => Asis_To_Tree.Def_Name_Value (Comp_Def),
                Ref       => Asis_To_Tree.Def_Value (Comp_Def),
                Sloc      => Nil),
           Component_Clause_Position =>
             Integer_Literal
               (Expr_Type => +"universal integer",
                Lit_Val   => +Image (DD.Position (Component)),
                Sloc      => Nil),
           Component_Clause_Range =>
             Discrete_Simple_Expression_Range
               (Lower_Bound =>
                  Integer_Literal
                    (Expr_Type => +"universal integer",
                     Lit_Val   => +Image (DD.First_Bit (Component)),
                     Sloc      => Nil),
                Upper_Bound =>
                  Integer_Literal
                    (Expr_Type => +"universal integer",
                     Lit_Val   => +Image (DD.Last_Bit (Component)),
                     Sloc      => Nil),
                Sloc => Nil),
           Sloc => Nil);
   begin
      Append (Comp_Clauses, Comp_Clause);
   end Do_Rec_Comp_Clause;

   procedure Do_Record_Clause
     (Clauses : in out Ada_Tree_Vector;
      Def_Id  :        Ada_Tree;
      Defn    :        Asis.Element)
   is
      Comps : constant DD.Record_Component_List := DD.Record_Components (Defn);
      Comp_Clauses : Ada_Tree_Vector;
   begin
      for Comp of Comps loop
         Do_Rec_Comp_Clause (Comp_Clauses, Comp);
      end loop;

      Append
        (Clauses,
         Record_Representation_Clause
           (Representation_Clause_Name =>
              Identifier
                (Expr_Type => +"null",
                 Ref_Name  => Def_Id.Def_Name,
                 Ref       => Def_Id.Def,
                 Sloc      => Nil),
            Mod_Clause_Expression => Nil,
            Component_Clauses     =>
              Component_Clause_List (To_Array (Comp_Clauses), Sloc => Nil),
            Sloc => Nil));
   end Do_Record_Clause;

   procedure Do_Size_Clause
     (Clauses : in out Ada_Tree_Vector;
      Def_Id  :        Ada_Tree;
      Defn    :        Asis.Element)
   is
      Size_Clause : constant Ada_Tree :=
        Attribute_Definition_Clause
          (Representation_Clause_Name =>
             Size_Attribute
               (Expr_Type => +"null",
                Prefix    =>
                  Identifier
                    (Expr_Type => +"null",
                     Ref_Name  => Def_Id.Def_Name,
                     Ref       => Def_Id.Def,
                     Sloc      => Nil),
                Attribute_Designator_Identifier =>
                  Identifier
                    (Expr_Type => +"null",
                     Ref_Name  => +"Size",
                     Ref       => +"null",
                     Sloc      => Nil),
                Sloc => Nil),
           Representation_Clause_Expression =>
             Integer_Literal
               (Expr_Type => +"universal integer",
                Lit_Val   => +Image (DD.Size (Defn)),
                Sloc      => Nil),
           Sloc => Nil);
   begin
      Append (Clauses, Size_Clause);
   end Do_Size_Clause;

   procedure Append_Rep_Clauses
     (L : in out Ada_Tree_Vector;
      E :        Asis.Element;
      T :        Ada_Tree)
   is
      --  This generates the following kinds of representation clauses, where
      --  appropriate:
      --
      --     Size clause
      --     Component_Size clause
      --     record_representation_clause

      pragma Assert
        ((Ekind (E) = An_Ordinary_Type_Declaration)
          = (T.Kind = An_Ordinary_Type_Declaration));

      Defn    : Asis.Element; -- the type definition
      Def_Id  : Ada_Tree_Base; -- the defining id of the type
      Clauses : Ada_Tree_Vector;
      --  Build up the rep clauses in this, and append this to L at the end

   begin
      if Ekind (E) = An_Ordinary_Type_Declaration then
         Def_Id := Names (T).Subtrees (1);
         Defn   := Type_Declaration_View (E);

         if DD.Type_Model_Kind (Defn) in DD.A_Simple_Static_Model then
            --  Generate Size clause

            Do_Size_Clause (Clauses, Def_Id, Defn);

            --  Generate Component_Size clause if it's an array

            if Ekind (Defn) in
                An_Unconstrained_Array_Definition |
                  A_Constrained_Array_Definition
              or else Is_Derived_From_Array (Defn)
            then
               Do_Comp_Size_Clause (Clauses, Def_Id, Defn);
            end if;

            --  Generate record_representation_clause if it's a record

            if Ekind (Defn) = A_Record_Type_Definition
              or else Is_Derived_From_Record (Defn)
            then
               Do_Record_Clause (Clauses, Def_Id, Defn);
            end if;

            pragma Assert (not Is_Empty (Clauses));
            Append (L, Comment (W_Name_Find (Scanner.Gen_Plus)));
            Append (L, To_Array (Clauses));
            Append (L, Comment (W_Name_Find (Scanner.Gen_Minus)));
         end if;
      end if;
   end Append_Rep_Clauses;

end Ada_Trees.Rep_Clauses;
