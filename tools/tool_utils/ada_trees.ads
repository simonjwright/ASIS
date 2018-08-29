------------------------------------------------------------------------------
--                                                                          --
--                            GNAT2XML COMPONENTS                           --
--                                                                          --
--                    G N A T 2 X M L . A D A _ T R E E S                   --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                     Copyright (C) 2012-2018, AdaCore                     --
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

--  This package provides a data structure Ada_Tree for representing Ada syntax
--  trees. An Ada_Tree has basically the same structure as the ASIS tree, but
--  represented as a data type rather than various query functions. Unlike an
--  ASIS tree, an Ada_Tree can be created and modified, as well as queried.

with Unchecked_Deallocation;
with Ada.Containers.Hashed_Maps;

with Namet; use Namet;

with A4G.Queries; use A4G;

with Asis.Text;
with Asis.Extensions.Flat_Kinds; use Asis.Extensions.Flat_Kinds;

with ASIS_UL.Debug;
with ASIS_UL.Utilities;
with ASIS_UL.Vectors;
with ASIS_UL.String_Utilities; use ASIS_UL.String_Utilities;

pragma Warnings (Off); -- imported for children
with ASIS_UL.Dbg_Out;
with Ada.Wide_Characters.Handling; use Ada.Wide_Characters.Handling;
use Ada;
with Pp; use Pp;
pragma Warnings (On);

package Ada_Trees is

   use ASIS_UL;

   subtype Classes is
     Flat_Element_Kinds'Base range Not_An_Element .. A_Statement_Class;
   --  All classes, including the abstract ones, the lists, and the singleton
   --  element kinds

   subtype Opt_ASIS_Elems is
     Flat_Element_Kinds'Base range Not_An_Element .. A_Compilation_Unit;

   function Ekind (Element : Asis.Element) return Opt_ASIS_Elems;

   use type A4G.Queries.Query_Index;

   function Span (Element : Asis.Element) return Asis.Text.Span;
   --  Return Nil_Element for gnat2xml-specific kinds

   subtype ASIS_Elems is Opt_ASIS_Elems with
        Predicate => ASIS_Elems /= Not_An_Element;

   subtype Def_Names is Flat_Defining_Name_Kinds;
   --  Defining occurences, such as A_Defining_Identifier

   subtype Usage_Names is Flat_Usage_Name_Kinds;
   --  References to defining occurrences, such as An_Identifier

   subtype Name_Elems is ASIS_Elems with
        Predicate => Name_Elems in Def_Names | Usage_Names;

   subtype Boolean_Elems is
     ASIS_Elems'Base range An_Aliased .. An_Is_Prefix_Notation;

   subtype Other_Elems is ASIS_Elems with
        Predicate => Other_Elems not in Name_Elems | Boolean_Elems;

   subtype Unit_Kinds is Asis.Unit_Kinds;
   subtype Unit_Classes is Asis.Unit_Classes;
   subtype Unit_Origins is Asis.Unit_Origins;
   use all type Unit_Kinds, Unit_Classes, Unit_Origins;

   Main_Done : Boolean renames ASIS_UL.Utilities.Main_Done;

   Debug_Mode : Boolean renames ASIS_UL.Debug.Debug_Flag_9;

   ----------------

   use A4G.Queries;

   type Ada_Tree_Rec;

   type Ada_Tree_Base is access all Ada_Tree_Rec;
   subtype Ada_Tree is Ada_Tree_Base with
     Predicate => Ada_Tree_Rec_OK (Ada_Tree.all);

   type Ada_Tree_Array is array (Query_Index range <>) of Ada_Tree; --??? with
--     Predicate => Ada_Tree_Array'First = 1;

   function Image (X : Query_Count) return String is (Image (Integer (X)));

   subtype Ada_Tree_Kind is
     ASIS_Elems'Base range ASIS_Elems'Base'First .. A_Variant_List;

   function Image
     (Kind : Ada_Tree_Kind)
      return String is
     (Capitalize (Kind'Img));

   function Is_Null (Tree : Ada_Tree_Base) return Boolean is (Tree = null);
   function T_Img (Tree : Ada_Tree_Base) return String;

   type Ada_Tree_Rec
     (Kind          : Ada_Tree_Kind;
      Subtree_Count : Query_Count)
   is record
      Sloc     : Asis.Text.Span := Asis.Text.Nil_Span;
      Checks   : Asis.Extensions.Run_Time_Check_Set :=
        Asis.Extensions.Empty_Check_Set;
      Subtrees : Ada_Tree_Array (1 .. Subtree_Count);

      --  Changes to node kinds and subtrees are typically benign here; handled
      --  automatically by tables in asis. However when the following variant
      --  part changes, various corresponding changes need to be done by hand.
      --  In particular, the following files generally need to be visited:
      --
      --     tools/tool_utils/ada_trees-asis_to_tree.adb
      --     tools/tool_utils/ada_trees-formatting-tree_to_ada.adb
      --     tools/tool_utils/ada_trees-generate_factory.adb
      --     tools/tool_utils/ada_trees-self_rep.adb
      --     tools/gnat2xml/gnat2xml-xsd.adb
      --     tools/gnat2xml/gnat2xml-xml.adb
      --     tools/gnat2xml/gnat2xml-xml2tree.adb

      case Kind is
         when A_Compilation_Unit | Def_Names =>
            Def_Name : Name_Id;

            case Kind is
               when A_Compilation_Unit =>
                  Unit_Kind      : Unit_Kinds;
                  Unit_Class     : Unit_Classes;
                  Unit_Origin    : Unit_Origins;
                  Unit_Full_Name : Name_Id;
                  Source_File    : Name_Id;

               when Def_Names =>
                  Def       : Name_Id;
                  Decl_Type : Name_Id;
               --  Type of declared name; corresponds to "type" attribute

               when others =>
                  null;
            end case;

         when Flat_Expression_Kinds =>
            Expr_Type : Name_Id;
            --  Type of expression; also corresponds to "type" attribute

            case Kind is
               when Usage_Names =>
                  Ref_Name, Ref : Name_Id;
                  Decl_Kind     : Opt_ASIS_Elems := Not_An_Element;
                  --  If this node denotes a declaration, this is the kind of
                  --  declaration node. If this is an attribute name, this is
                  --  An_Unknown_Attribute. (We don't care which attribute it
                  --  is.) Otherwise nil.
                  Is_Predef : Boolean := False;
               --  True if this node denotes a declaration in the predefined
               --  environment (either standard Ada, or GNAT). Decl_Kind and
               --  Is_Predef are used in gnatpp, but not in gnat2xml.

               when An_Integer_Literal | A_Real_Literal | A_String_Literal =>
                  Lit_Val : Name_Id;

               when others =>
                  null;
            end case;

         when Flat_Pragma_Kinds =>
            Pragma_Name : Name_Id;

         when A_Parameter_Specification | A_Formal_Object_Declaration =>
            Mode : Asis.Mode_Kinds;

         when A_Comment =>
            Text : Name_Id;
            --  Text of the comment, including the leading "--"

         when others =>
            null;
      end case;
   end record; --??? with -- Ada_Tree_Rec
--         Predicate => Ada_Tree_Rec_OK (Ada_Tree_Rec);

   function Ada_Tree_Rec_OK (X : Ada_Tree_Rec) return Boolean;

   function Empty
     (Kind : Flat_List_Kinds;
      Sloc : Asis.Text.Span := Asis.Text.Nil_Span)
      return Ada_Tree;

   Empty_Tree_Array : constant Ada_Tree_Array := (1 .. 0 => <>);

   The_Nil : aliased Ada_Tree_Rec :=
     (Not_An_Element,
      Subtree_Count => 0,
      Sloc          => Asis.Text.Nil_Span,
      Checks        => Asis.Extensions.Empty_Check_Set,
      Subtrees      => Empty_Tree_Array);

   function Nil
     (Ignored_Sloc : Asis.Text.Span := Asis.Text.Nil_Span)
      return Ada_Tree is
     (The_Nil'Access);
--   is (new Ada_Tree_Rec'(The_Nil));

   function Is_Nil
     (Tree : Ada_Tree)
      return Boolean is
     (Tree.Kind = Not_An_Element);

   function Ref (T : Ada_Tree) return Name_Id;
   --  For a name that statically denotes something, returns the unique id of
   --  that thing. This means taking apart selected components, so for X.Y.Z,
   --  we return the unique id of Z.

   type Ada_Tree_Array_Ref is access Ada_Tree_Array;

   procedure Free_Tree_Rec is new Unchecked_Deallocation
     (Ada_Tree_Rec, Ada_Tree_Base);
   --  Free a single tree node
   procedure Free_Tree (T : Ada_Tree_Base);
   --  Free the tree along with all subtrees
   procedure Free_Tree_Array is new Unchecked_Deallocation
     (Ada_Tree_Array, Ada_Tree_Array_Ref);
   --  Free a single array
   procedure Free_Subtrees (A : Ada_Tree_Array_Ref);
   --  Free the array along with all subtrees

   function Get_Type (T : Ada_Tree) return Name_Id is
      (case T.Kind is
         when Def_Names => T.Decl_Type,
         when Flat_Expression_Kinds => T.Expr_Type,
         when others => raise Program_Error);
   --  Returns the "type" attribute

   package Ada_Tree_Vectors is new ASIS_UL.Vectors
     (Query_Index, Ada_Tree, Ada_Tree_Array);
   subtype Ada_Tree_Vector is Ada_Tree_Vectors.Vector;

   ----------------

   type Kind_Set is array (Ada_Tree_Kind) of Boolean with
        Pack => True;

   function Kinds_In_Class (Class : Flat_Element_Kinds'Base) return Kind_Set;

   function Cardinality (Kinds : Kind_Set) return Natural;
   --  Number of elements in Kinds

   procedure Put_Kinds (Kinds : Kind_Set);
   --  Print something like "This | That | The_Other" to standard output

   function Kind_In_Class
     (Kind  : Opt_ASIS_Elems;
      Class : Flat_Abstract_Classes)
      return  Boolean;
   --  True if Kind is in the Class

   function Get (Tree : Ada_Tree; Q : Structural_Queries) return Ada_Tree;
   procedure Set (Tree : Ada_Tree; Q : Structural_Queries; Subtree : Ada_Tree);
   --  Getters and setters

   generic
      Query : Structural_Queries;
      type Result_Type is new Ada_Tree;
   function Generic_Getter (Tree : Ada_Tree) return Result_Type;
   --  An instance will return Get (Tree, Query), returning the appropriate
   --  subtype. For example, instantiate like this:
   --     function Discriminant_Part is new Generic_Getter
   --       (Discriminant_Part, Definition_Class);
   --  to get an instance like this:
   --     function Discriminant_Part (Tree : Ada_Tree) return Definition_Class;

   generic
      Query : Structural_Queries;
      type Result_Type is new Ada_Tree;
   procedure Generic_Setter (Tree : Ada_Tree; Subtree : Result_Type);
   --  An instance will do Set (Tree, Query, Subtree).

   type Assoc is record
      Query   : Structural_Queries;
      Subtree : Ada_Tree;
   end record;

   type Assoc_List is array (Query_Index range <>) of Assoc;

   function Make
     (Kind     : Opt_ASIS_Elems;
      Subtrees : Assoc_List     := (1 .. 0 => <>);
      Sloc     : Asis.Text.Span := Asis.Text.Nil_Span)
      return     Ada_Tree with
      Pre => Subtrees'First = 1 and then Subtrees'Last = Num_Queries (Kind);
      --  Make a new Ada_Tree with the given Kind, Subtrees, and Sloc. Other
      --  components (the ones in the variant part) are not filled in.

   function Make_List
     (Kind     : Flat_List_Kinds;
      Subtrees : Ada_Tree_Array := Empty_Tree_Array;
      Sloc     : Asis.Text.Span := Asis.Text.Nil_Span)
      return     Ada_Tree;
   --  Make a new list with the given Kind, Subtrees, and Sloc.

   function Clone (Tree : Ada_Tree) return Ada_Tree;
   --  Returns a deep copy of Tree

   function Q_Name
     (Q    : Structural_Queries)
      return String is
     (Capitalize (Strip_Article (Q'Img)));
   --  Name of the Query function in the Factory child package

   function Constructor_Name
     (Class : Ada_Tree_Kind)
      return  String is
     ((if Class in Boolean_Elems then "Make_" else "") &
      Capitalize (Strip_Article (Class'Img)));
   --  Name of the constructor function in the Factory child package. We
   --  prepend a "Make_" prefix for the booleans, because some of those
   --  are reserved words.

   List_Component_Type : constant array (Flat_List_Kinds) of Classes :=
   --  Mapping from list kinds to their component kinds. For example a
   --  A_Declarative_Item_List is a list of A_Declarative_Item_Class elements.

     (An_Element_List                   => An_Element_Class,
      An_Association_List               => An_Association_Class,
      A_Component_Clause_List           => A_Component_Clause,
      A_Context_Clause_List             => A_Context_Clause_Class,
      A_Declaration_List                => A_Declaration_Class,
      A_Declarative_Item_List           => A_Declarative_Item_Class,
      A_Definition_List                 => A_Definition_Class,
      A_Discrete_Range_List             => A_Discrete_Range_Class,
      A_Discriminant_Association_List   => A_Discriminant_Association,
      A_Discriminant_Specification_List => A_Discriminant_Specification,
      A_Defining_Name_List              => A_Defining_Name_Class,
      An_Exception_Handler_List         => An_Exception_Handler,
      An_Expression_List                => An_Expression_Class,
      A_Name_List                       => A_Name_Class,
      A_Parameter_Specification_List    => A_Parameter_Specification,
      A_Path_List                       => A_Path_Class,
      A_Record_Component_List           => A_Record_Component_Class,
      A_Statement_List                  => A_Statement_Class,
      A_Variant_List                    => A_Variant);

   function Hash (Key : Name_Id) return Ada.Containers.Hash_Type;

   --  Symbol_Table is a mapping from Def to Symbol_Table_Entry. For now, this
   --  only works within a single unit. The Def_Id is the defining name that
   --  has that Def_Name, and Decl is the declaration that declares it (i.e.
   --  the innermost enclosing declaration).

   type Symbol_Table_Entry is record
      Decl   : Ada_Tree;
      Def_Id : Ada_Tree;
   end record;

   package Symbol_Tables is new Ada.Containers.Hashed_Maps
     (Key_Type        => Name_Id,
      Element_Type    => Symbol_Table_Entry,
      Hash            => Hash,
      Equivalent_Keys => "=",
      "="             => "=");

   use Symbol_Tables;
   subtype Symbol_Table is Symbol_Tables.Map;

   Symtab : Symbol_Table;

   procedure Resolve_Symbols (Tree : Ada_Tree);
   --  Insert entries for Tree into Symtab

   function Decl_Of_Def
     (Symtab : Symbol_Table; Def_Id : Ada_Tree) return Ada_Tree with
       Pre => Def_Id.Kind in Def_Names;
   --  Returns the declaration containing the given defining name. So for
   --  "package P is..." this takes you from P to the package declaration.

   function Decl_Of_Def_Kind
     (Symtab : Symbol_Table;
      Def_Id : Ada_Tree)
      return   Opt_ASIS_Elems with
      Post => Decl_Of_Def_Kind'Result in
        Flat_Declaration_Kinds | Not_An_Element;
      --  Same as "Decl_Of_Def (Symtab, Def_Id)", except Nil if there is none.

   function Spec_Of_Body
     (Symtab : Symbol_Table; Body_Def : Ada_Tree) return Ada_Tree with
       Pre => Body_Def.Kind in Def_Names;
   --  Given the defining name of a body, returns the defining name of the
   --  corresponding first declaration.

end Ada_Trees;
