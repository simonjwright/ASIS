------------------------------------------------------------------------------
--                                                                          --
--                           AVATOX COMPONENTS                              --
--                                                                          --
--                        VATOX (Via Asis To Xml)                           --
--                                                                          --
--                                                                          --
--                Copyright (c) 2006, McKae Technologies.                   --
--                                                                          --
-- Avatox is free software; you can redistribute it and/or modify it        --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software Foundation;  either version 2,  or  (at your option)  any later --
-- version. Avatox is distributed in the hope  that it will be useful,      --
-- but WITHOUT ANY WARRANTY; without even the implied warranty of MER-      --
-- CHANTABILITY or  FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General  --
-- Public License for more details. You should have received a copy of the  --
-- GNU General Public License distributed with GNAT; see file COPYING. If   --
-- not, write to the Free Software Foundation, 59 Temple Place Suite 330,   --
-- Boston, MA 02111-1307, USA.                                              --
--                                                                          --
-- Avatox is based off the Display_Source software distributed as part of   --
-- the ASIS implementation for GNAT, and therefore inherits its GPL         --
-- licensing.  Ada Core Technologies maintains the Display_Source program   --
-- and its copyright is held by the Free Software Foundation.               --
--                                                                          --
-- Avatox is now maintained by McKae Technologies (http://www.mckae.com)    --                                               --
------------------------------------------------------------------------------

with Ada.Characters.Conversions; use Ada.Characters.Conversions;
with Ada.Strings.Unbounded;      use Ada.Strings.Unbounded;
with Ada.Strings.Wide_Fixed;     use Ada.Strings.Wide_Fixed;
with Ada.Strings.Wide_Maps;      use Ada.Strings.Wide_Maps;
with Ada.Strings.Wide_Unbounded; use Ada.Strings.Wide_Unbounded;
with Ada.Unchecked_Deallocation;

with Asis.Elements;
with Asis.Text;
with Asis.Compilation_Units;
with Asis.Declarations;
with Asis.Definitions;
with Asis.Exceptions;
with Asis.Expressions;
with Asis.Implementation;
with Asis.Statements;

with Gnatvsn;

with Mckae.Text.Lexicals;
with Mckae.Xml.Ez_Out;

with Strings_Edit.Utf8.Handling;

with Vatox.Axf_Points.Options;
with Vatox.Axf_Points.References.Ada_Refs;
with Vatox.Axf_Points.Terminal_Nomenclature.Ada_Terms;
with Vatox.Axf_Pedigrees;
with Vatox.Xml_File_Output;  use Vatox.Xml_File_Output;

with Avatox_Versioning;

with Text_Io; use Text_IO;

package body Vatox.Traversal is

   use Asis;
   --  to make all the literals from Element classification hierarchy
   --  directly visible

   use Mckae.Text;

   Dq_Map : Wide_Character_Set := To_Set ("'""");

   ------------------------------------------------------------------------------

   -- These declarations associate an empty string with a False boolean
   -- attribute value, and the string "true" with a True value. It's done this
   -- way to take advantage of XML EZ Out's default behavior of omitting
   -- attributes that have an empty string as their value.
   type Boolean_Attr_Values is array (Boolean) of Unbounded_String;
   Boolean_Attr_Value : constant Boolean_Attr_Values
     := (False => Null_Unbounded_String,
         True  => To_Unbounded_String("true"));

   ------------------------------------------------------------------------------

   -- Pedigrees for the elements that are produced
   Asis_Pedigree_Id        : constant String := "asis";
   Asis_Product_Name       : constant String
     := "Ada Semantic Interface Specification (ASIS)";

   Avatox_Asis_Pedigree_Id : constant String := "avatoxAsis";

   Asis_Pedigree        : constant Attribute_Value_Pairs
     := "pedigree" = Asis_Pedigree_Id;
   Avatox_Asis_Pedigree : constant Attribute_Value_Pairs
     := "pedigree" = Avatox_Asis_Pedigree_Id;

   -----------------------------------------------------------------------------

   procedure Free_Private_Decls is new Ada.Unchecked_Deallocation
     (Asis.Declarative_Item_List, Private_Decls_List);

   -----------------------------------------------------------------------------

   -- Identify whether the given ASIS element appears in a sequence of
   -- statements.
   function Mark_As_A_Statement
     (Asis_Element : Asis.Element) return Boolean;

   -----------------------------------------------------------------------------

   function To_Trimmed_String (W : Wide_String) return Wide_String is
      S : Natural := W'First;
      First_Char  : Wide_Character := W(S);
      L : Natural := W'Last;
      Last_Char   : Wide_Character := W(L);
   begin
      if Is_In (First_Char, Dq_Map) then
         S := S + 1;
      end if;
      if Is_In (Last_Char, Dq_Map) then
         L := L - 1;
      end if;
      return W(S .. L);
   end To_Trimmed_String;

   Comment_Start_Line : Asis.Text.Line_Number := 0;

   ------------------------------------------------------------------------------

   function Rep (N : Natural) return Wide_String is
      W : constant Wide_String := Natural'Wide_Image(N);
   begin
      return W (2 .. W'Last);
   end Rep;

   ------------------------------------------------------------------------------

   function T (S          : String;
               Xml_Casing : Boolean) return String is
   begin
      if Xml_Casing then
         return Lexicals.Transform (S,
                                    Lexicals.Xml_Common,
                                    Remove_Underscores => True);
      else
         return S;
      end if;
   end T;

   ------------------------------------------------------------------------------

   function Span_To_Attributes
     (Element_Span : Asis.Text.Span)
      return         Attributes_List
   is
      use Ada.Strings;
      use Asis.Text;

   begin
      return
        (("startLine" =
           To_String (Trim (Line_Number_Positive'Wide_Image
             (Element_Span.First_Line), Left))),
         ("startCol" =
            To_String (Trim (Character_Position_Positive'Wide_Image
              (Element_Span.First_Column),
              Left))),
         ("endLine" =
            To_String (Trim (Line_Number'Wide_Image
              (Element_Span.Last_Line), Left))),
         ("endCol" =
            To_String (Trim (Character_Position'Wide_Image
              (Element_Span.Last_Column), Left))));
   end Span_To_Attributes;

   -----------------------------------------------------------------------------

   function Get_Private_Decls (Private_Container_Decl : Asis.Declaration)
                               return Asis.Declarative_Item_List is
      use Asis;

      Container_Kind : constant Declaration_Kinds
        := Elements.Declaration_Kind(Private_Container_Decl);
      Prot_Defn : Definition;

   begin
      case Container_Kind is
         when A_Package_Declaration |
              A_Generic_Package_Declaration =>
            return Declarations.Private_Part_Declarative_Items
              (Private_Container_Decl, Include_Pragmas => True);
         when A_Protected_Type_Declaration =>
            Prot_Defn := Declarations.Type_Declaration_View (Private_Container_Decl);
            return Definitions.Private_Part_Items
              (Prot_Defn, Include_Pragmas => True);
         when A_Single_Protected_Declaration =>
            Prot_Defn := Declarations.Object_Declaration_View (Private_Container_Decl);
            return Definitions.Private_Part_Items
              (Prot_Defn, Include_Pragmas => True);
         when others =>
            -- Shouldn't be calling this procedure with anything that can't
            -- possibly have a private part.
            raise Constraint_Error;
      end case;
   end Get_Private_Decls;

   -----------------------------------------------------------------------------

   procedure Append_Private_Declarations
     (Private_Container_Decl : in     Asis.Declaration;
      State        : in out Info_Node) is
      -- Four kinds of entities (declarations) can have private parts:
      --  Packages, generic packages, protected objects, and protected types.
      -- And even though the source constructs are syntatically similar,
      -- different mechanisms are used to get to their private parts.
      use Asis;
      Curr_Decls : Private_Decls_List := State.Private_Decls;
      Container_Kind : constant Asis.Declaration_Kinds
        := Elements.Declaration_Kind(Private_Container_Decl);
      Private_Decls : Asis.Declarative_Item_List
        := Get_Private_Decls(Private_Container_Decl);

   begin
      -- If there aren't any private_decls, then just skip this
      if Private_Decls'Length > 0 then
         -- Package_Decl is either A_Package_Declaration or
         -- A_Generic_Package_Declaration
         if Curr_Decls /= null then
            -- You've got a package with a private part inside a package, so
            -- just combine the list of private items. (And then go redesign
            -- your code, okay? Stop doing stuff like that.)
            State.Private_Decls :=
              new Asis.Declarative_Item_List'(Curr_Decls.all & Private_Decls);
            Free_Private_Decls (Curr_Decls);
         else
            State.Private_Decls := new Asis.Declarative_Item_List'(Private_Decls);
         end if;
      end if;
   end Append_Private_Declarations;

   -----------------------------------------------------------------------------

   function Could_Be_In_Private_Part (Elem : Asis.Element;
                                      State : Info_Node) return Boolean is
      Elem_Kind      : Asis.Element_Kinds := Elements.Element_Kind(Elem);
      Enclosing_Unit : Asis.Unit_Kinds
        := Compilation_Units.Unit_Kind
          (Elements.Enclosing_Compilation_Unit (Elem));

      May_Be_In_Private : Boolean :=
        ((Enclosing_Unit = Asis.A_Package)
         or (Enclosing_Unit = Asis.A_Generic_Package)
         or (Enclosing_Unit in Asis.A_Library_Unit_Body)
         or (Enclosing_Unit in Asis.A_Subunit))
        and (State.Private_Decls /= null);
   begin
      return ((Elem_Kind = A_Declaration)
              or (Elem_Kind = A_Clause)
              or (Elem_Kind = A_Pragma))
        and May_Be_In_Private;
   end Could_Be_In_Private_Part;

   -----------------------------------------------------------------------------

   function Is_A_Private_Library_Unit (Elem : Asis.Element;
                                       State : Info_Node) return Boolean is
      -- (If it's not a declaration, this will be 'Not_A_Declaration'
      Decl_Kind : Declaration_Kinds := Elements.Declaration_Kind(Elem);
   begin
      return ((Decl_Kind = A_Package_Declaration) or
                (Decl_Kind = A_Package_Renaming_Declaration))
        and then (Compilation_Units.Unit_Class
                  (Elements.Enclosing_Compilation_Unit (Elem))
                  = A_Private_Declaration);
   end Is_A_Private_Library_Unit;

   -----------------------------------------------------------------------------

   function Get_Accessibility (Elem  : Asis.Element;
                               State : Info_Node) return String is

   begin
      if Could_Be_In_Private_Part (Elem, State) then
         -- Ada package (and generic package) components can have two types of
         -- visibility: "packaged", if they're in the publicly visible part, and
         -- "internal", if they're in the private part.

         -- Check the list of private declarations in the unit currently being
         -- processed. If it's not there, then this is visible, ergo packaged.
         for I in State.Private_Decls'Range loop
            if Elements.Is_Identical (Elem, State.Private_Decls (I)) then
               return "internal";
            end if;
         end loop;
         return "packaged";
      elsif Is_A_Private_Library_Unit (Elem, State) then
         return "internal";
      else
         return "";
      end if;
   end Get_Accessibility;

   -----------------------------------------------------------------------------

   procedure Output_Scope_References
     (Element : in Asis.Element;
      State   : in Info_Node) is

      use Vatox.Axf_Points.References;
      use Strings_Edit.Utf8.Handling;

      Refs : Ref_List := Ada_Refs.Get_Scope_Refs (Element);
      Ref_Base : constant Natural := Refs'First;

   begin
      -- Output the scope element and the ID generated from the nested scopes
      Start_Element
        (State.XML_File.all,
         Vatox.Axf_Points.Axf_Point_Tag,
         Vatox.Axf_Points.Axf_Points_Pedigree &
         (Vatox.Axf_Points.References.Axf_Scope_ID_Attr =
              To_Utf8 (Ada_Refs.Get_Scope_Sequence (Element))));

      -- Generate individual entries for the scopes
      for I in Refs'Range loop
         Output_Tag
           (State.XML_File.all,
            Vatox.Axf_Points.Axf_Point_Tag,
            Vatox.Axf_Points.Axf_Points_Pedigree &
            (Vatox.Axf_Points.References.Axf_Scope_Name_Attr =
                 To_Utf8 (To_Wide_String(Refs (I))),
            Vatox.Axf_Points.References.Axf_Scope_Level_Attr =
           	To_Utf8 (Rep(I - Ref_Base))));
      end loop;

      End_Element (State.Xml_File.all, Vatox.Axf_Points.Axf_Point_Tag);
   end Output_Scope_References;

   -----------------------------------------------------------------------------

   procedure Output_Cross_Reference
     (Element : in Asis.Expression;
      State   : in Info_Node) is

      use Vatox.Axf_Points.References;
      use Strings_Edit.Utf8.Handling;

      Ref_Decl : Asis.Element :=
        Asis.Expressions.Corresponding_Name_Declaration(Element);
      Refs     : Ref_List := Ada_Refs.Get_Scope_Refs (Ref_Decl);
      Ref_Base : constant Natural := Refs'First;
   begin
      if Refs'Length > 0 then
         -- Output the scope element and the ID generated from the nested scopes
         Start_Element
           (State.XML_File.all,
            Vatox.Axf_Points.Axf_Point_Tag,
            Vatox.Axf_Points.Axf_Points_Pedigree &
            (Vatox.Axf_Points.References.Axf_Xref_ID_Attr =
                 To_Utf8 (Ada_Refs.Get_Scope_Sequence (Ref_Decl))));

         -- Generate individual entries for the scopes
         for I in Refs'Range loop
            Output_Tag
              (State.XML_File.all,
               Vatox.Axf_Points.Axf_Point_Tag,
               Vatox.Axf_Points.Axf_Points_Pedigree &
               (Vatox.Axf_Points.References.Axf_Xref_Name_Attr =
                    To_Utf8 (To_Wide_String (Refs (I))),
                  Vatox.Axf_Points.References.Axf_Xref_Level_Attr =
                    To_Utf8 (Rep (I - Ref_Base))));
         end loop;

         End_Element (State.Xml_File.all, Vatox.Axf_Points.Axf_Point_Tag);
      end if;
   end Output_Cross_Reference;

   -----------------------------------------------------------------------------

   procedure Output_Valid_Cross_Reference
     (Element : in Asis.Expression;
      State   : in Info_Node) is

      -- Some Ada designators do not have valid cross-references, such as
      -- attribute designators and various pragma constructs.
      -- There appears to be no ASIS mechanism to explicitly determine this, so
      -- instead attempt to get the corresponding name, and if no exception
      -- is raised, then its valid and a cross reference can be generated.

      Ref_Decl : Asis.Element;
   begin
      Ref_Decl := Asis.Expressions.Corresponding_Name_Declaration(Element);
      Output_Cross_Reference (Element, State);
   exception
      when Asis.Exceptions.ASIS_Inappropriate_Element =>
         -- This element has no corresponding name declaration
         null;
   end Output_Valid_Cross_Reference;

   -----------------------------------------------------------------------------

   procedure Output_Comment
     (Comments : in Asis.Text.Line_List;
      State    : in out Info_Node)
   is
      use Strings_Edit.Utf8.Handling;
      Comment_Span : Asis.Text.Span;
   begin
      for I in  Comments'Range loop
         declare
            Comment_Line : constant Wide_String :=
              Asis.Text.Comment_Image (Comments (I));
         begin
            if Comment_Line'Length > 0 then
               Comment_Span :=
                 (First_Line   => I,
                  First_Column =>
                    Asis.Text.Character_Position_Positive
                      (Index (Comment_Line, "--")),
                  Last_Line    => I,
                  Last_Column  => Comment_Line'Last);
               Output_Element
                 (State.XML_File.all,
                  T ("A_COMMENT", State.Xml_Style),
                  To_Utf8 (Trim (Comment_Line, Ada.Strings.Both)),
                  Avatox_Asis_Pedigree &
                  Span_To_Attributes (Comment_Span));
            end if;
         end;
      end loop;
   end Output_Comment;

   --------------------------------------------
   --                                        --
   --  Here is the pre procedure to provide  --
   --  to Traverse_Element to make a node    --
   --  display.                              --
   --                                        --
   --------------------------------------------

   procedure Pre_Procedure
     (Element : in     Asis.Element;
      Control : in out Asis.Traverse_Control;
      State   : in out Info_Node)
   is
      pragma Unreferenced (Control);
      use Strings_Edit.Utf8.Handling;
      use Vatox.Axf_Points;
      use Vatox.Axf_Points.Terminal_Nomenclature;

      function Operator_Kind (Element : Asis.Element) return String is
      begin
         case Asis.Elements.Defining_Name_Kind (Element) is
            when A_Defining_Operator_Symbol =>
               return Operator_Kinds'Image
                        (Asis.Elements.Operator_Kind (Element));
            when others =>
               return "";
         end case;
      end Operator_Kind;

      Element_Span   : Asis.Text.Span;

   begin
      Element_Span := Asis.Text.Element_Span (Element);

      -- Check for, and output, any intervening comments
      if Element_Span.First_Line > Comment_Start_Line then
         Output_Comment
           (Asis.Text.Lines
              (Element,
               Comment_Start_Line + 1,
               Element_Span.First_Line),
            State);
         Comment_Start_Line := Element_Span.First_Line;
      end if;

      Start_Element
        (State.XML_File.all,
         T (Element_Kinds'Image (Asis.Elements.Element_Kind (Element)),
           State.Xml_Style),
         Asis_Pedigree &
         ("accessibility" = Get_Accessibility (Element, State),
            "isAStatement" = Boolean_Attr_Value
              (Mark_As_A_Statement (Element))) &
         Span_To_Attributes (Element_Span));
      case Asis.Elements.Element_Kind (Element) is
         when A_Pragma =>                  -- Asis.Elements
            Output_Tag
              (State.XML_File.all,
               T (Pragma_Kinds'Image (Asis.Elements.Pragma_Kind (Element)),
                 State.Xml_Style),
               Asis_Pedigree &
               ("name" = To_Utf8 (To_Trimmed_String
                  (Asis.Elements.Pragma_Name_Image (Element)))) &
               Span_To_Attributes (Element_Span));
         when A_Defining_Name =>           -- Asis.Declarations
            Output_Tag
              (State.XML_File.all,
                 T (Defining_Name_Kinds'Image
                 (Asis.Elements.Defining_Name_Kind (Element)), State.Xml_Style),
                 Asis_Pedigree &
               ("name" =
                    To_Utf8 (To_Trimmed_String
                      (Asis.Declarations.Defining_Name_Image (Element))),
                  "operator" = To_Utf8 (Operator_Kind (Element))) &
               Span_To_Attributes (Element_Span));
         when A_Declaration =>             -- Asis.Declarations
            Start_Element
              (State.XML_File.all,
               T (Declaration_Kinds'Image
                 (Asis.Elements.Declaration_Kind (Element)), State.Xml_Style),
               Asis_Pedigree &
               Span_To_Attributes (Element_Span));

            case Asis.Elements.Declaration_Kind (Element) is
               when A_Private_Type_Declaration      |
                    A_Private_Extension_Declaration |
                    A_Variable_Declaration          |
                    A_Constant_Declaration          |
                    A_Deferred_Constant_Declaration |
                    A_Discriminant_Specification    |
                    A_Loop_Parameter_Specification  |
                    A_Procedure_Declaration         |
                    A_Function_Declaration          |
                    A_Parameter_Specification       =>
                  Output_Tag
                    (State.XML_File.all,
                     T (Trait_Kinds'Image (Asis.Elements.Trait_Kind (Element)),
                       State.Xml_Style),
                     Asis_Pedigree &
                     Span_To_Attributes (Element_Span));

               when A_Formal_Function_Declaration  |
                    A_Formal_Procedure_Declaration =>
                  Output_Tag
                    (State.XML_File.all,
                     T (Subprogram_Default_Kinds'Image
                       (Asis.Elements.Default_Kind (Element)), State.Xml_Style),
                     Asis_Pedigree &
                     Span_To_Attributes (Element_Span));

               when A_Package_Declaration |
                    A_Generic_Package_Declaration |
                    A_Protected_Type_Declaration |
                    A_Single_Protected_Declaration =>
                  Append_Private_Declarations(Element, State);
               when others =>
                  null;
            end case;

            case Asis.Elements.Declaration_Kind (Element) is
               when A_Parameter_Specification | A_Formal_Object_Declaration =>
                  Output_Tag
                    (State.XML_File.all,
                     T (Mode_Kinds'Image (Asis.Elements.Mode_Kind (Element)),
                       State.Xml_Style),
                     Asis_Pedigree &
                     Span_To_Attributes (Element_Span));
               when others =>
                  null;
            end case;

            if Options.Is_Set(State.Axf_Points, Options.Axf_Decl_Enclosures) then
               Output_Scope_References (Element, State);
            end if;

            End_Element (State.XML_File.all);

         when A_Definition =>              -- Asis.Definitions
            Start_Element
              (State.XML_File.all,
               T (Definition_Kinds'Image (Asis.Elements.Definition_Kind (Element)),
                 State.Xml_Style),
               Asis_Pedigree &
               Span_To_Attributes (Element_Span));
            case Asis.Elements.Definition_Kind (Element) is
               when A_Type_Definition =>
                  Start_Element
                    (State.XML_File.all,
                     T (Type_Kinds'Image (Asis.Elements.Type_Kind (Element)),
                       State.Xml_Style),
                     Asis_Pedigree &
                     Span_To_Attributes (Element_Span));
                  case Asis.Elements.Type_Kind (Element) is
                  when An_Access_Type_Definition =>
                     Output_Tag
                       (State.XML_File.all,
                        T (Access_Type_Kinds'Image
                          (Asis.Elements.Access_Type_Kind (Element)),
                          State.Xml_Style),
                        Asis_Pedigree &
                        Span_To_Attributes (Element_Span));
                  when A_Derived_Type_Definition             |
                       A_Derived_Record_Extension_Definition |
                       A_Record_Type_Definition              |
                       A_Tagged_Record_Type_Definition       =>
                     Output_Tag
                       (State.XML_File.all,
                        T (Trait_Kinds'Image
                          (Asis.Elements.Trait_Kind (Element)),
                          State.Xml_Style),
                        Asis_Pedigree &
                        Span_To_Attributes (Element_Span));
                  when others =>
                     null;
                  end case;
                  End_Element (State.XML_File.all);
               when A_Constraint =>
                  Output_Tag
                    (State.XML_File.all,
                     T (Constraint_Kinds'Image
                       (Asis.Elements.Constraint_Kind (Element)),
                       State.Xml_Style),
                     Asis_Pedigree &
                     Span_To_Attributes (Element_Span));
               when A_Formal_Type_Definition =>
                  Start_Element
                    (State.XML_File.all,
                     T (Formal_Type_Kinds'Image
                       (Asis.Elements.Formal_Type_Kind (Element)),
                       State.Xml_Style),
                     Asis_Pedigree &
                     Span_To_Attributes (Element_Span));
                  case Asis.Elements.Formal_Type_Kind (Element) is
                  when A_Formal_Access_Type_Definition =>
                     Output_Tag
                       (State.XML_File.all,
                        T (Access_Type_Kinds'Image
                          (Asis.Elements.Access_Type_Kind (Element)),
                          State.Xml_Style),
                        Asis_Pedigree &
                        Span_To_Attributes (Element_Span));
                  when A_Formal_Private_Type_Definition        |
                       A_Formal_Tagged_Private_Type_Definition |
                       A_Formal_Derived_Type_Definition        =>
                     Output_Tag
                       (State.XML_File.all,
                        T (Trait_Kinds'Image
                          (Asis.Elements.Trait_Kind (Element)),
                          State.Xml_Style),
                        Asis_Pedigree &
                        Span_To_Attributes (Element_Span));
                  when others =>
                     null;
                  end case;
                  End_Element (State.XML_File.all);
               when A_Discrete_Subtype_Definition | A_Discrete_Range =>
                  Output_Tag
                    (State.XML_File.all,
                     T (Discrete_Range_Kinds'Image
                       (Asis.Elements.Discrete_Range_Kind (Element)),
                       State.Xml_Style),
                     Asis_Pedigree &
                     Span_To_Attributes (Element_Span));
               when A_Component_Definition           |
                    A_Private_Type_Definition        |
                    A_Tagged_Private_Type_Definition |
                    A_Private_Extension_Definition   =>
                  Output_Tag
                    (State.XML_File.all,
                     T (Trait_Kinds'Image (Asis.Elements.Trait_Kind (Element)),
                       State.Xml_Style),
                     Asis_Pedigree &
                     Span_To_Attributes (Element_Span));
               when others =>
                  null;
            end case;
            End_Element (State.XML_File.all);

         when An_Expression =>             -- Asis.Expressions
            case Asis.Elements.Expression_Kind (Element) is
            when An_Attribute_Reference =>
               Output_Tag
                 (State.XML_File.all,
                  T (Expression_Kinds'Image
                    (Asis.Elements.Expression_Kind (Element)),
                    State.Xml_Style),
                  Asis_Pedigree &
                  ("attr" =
                       T (Attribute_Kinds'Image
                         (Asis.Elements.Attribute_Kind (Element)),
                         State.Xml_Style)) &
                  Span_To_Attributes (Element_Span));
            when An_Identifier =>
               Output_Tag
                 (State.XML_File.all,
                  T (Expression_Kinds'Image
                    (Asis.Elements.Expression_Kind (Element)),
                    State.Xml_Style),
                  Asis_Pedigree &
                  ("ident" =
                       To_Utf8 (To_Trimmed_String (Asis.Expressions.Name_Image (Element)))) &
                  Span_To_Attributes (Element_Span));
                  if Options.Is_Set (State.Axf_Points, Options.Axf_Cross_References) then
                     Output_Valid_Cross_Reference(Element, State);
                  end if;
            when An_Operator_Symbol =>
               declare
                  Tag : constant String :=
                    T (Expression_Kinds'Image
                       (Asis.Elements.Expression_Kind (Element)),
                       State.Xml_Style);
                  Oper_Rep   : constant String :=
                    To_Utf8 (To_Trimmed_String (Asis.Expressions.Name_Image (Element)));
                  Oper_Attrs : constant Attributes_List :=
                    Asis_Pedigree
                      & ("operator" = Oper_Rep)
                      & Span_To_Attributes (Element_Span);
               begin
                  Start_Element (State.Xml_File.all, Tag, Oper_Attrs);
                  if Options.Is_Set(State.Axf_Points, Options.Axf_Terminal_Reps) then
                     Output_Tag
                       (State.Xml_File.all,
                        Vatox.Axf_Points.Axf_Point_Tag,
                        Vatox.Axf_Points.Axf_Points_Pedigree &
                        (Vatox.Axf_Points.Terminal_Nomenclature.Axf_Oper_Attr =
                             Ada_Terms.Lookup_Operator (Oper_Rep)));
                  end if;
                  if Options.Is_Set (State.Axf_Points, Options.Axf_Cross_References) then
                     Output_Valid_Cross_Reference(Element, State);
                  end if;
                  End_Element (State.Xml_File.all, Tag);
               end;
            when A_Character_Literal | An_Enumeration_Literal =>
                  Output_Tag
                    (State.XML_File.all,
                     T (Expression_Kinds'Image
                    (Asis.Elements.Expression_Kind (Element)),
                    State.Xml_Style),
                  Asis_Pedigree &
                  ("literal" =
                       To_Utf8 (To_Trimmed_String (Asis.Expressions.Name_Image (Element)))) &
                  Span_To_Attributes (Element_Span));
                  if Options.Is_Set (State.Axf_Points, Options.Axf_Cross_References) then
                     Output_Valid_Cross_Reference(Element, State);
                  end if;

            when An_Integer_Literal | A_Real_Literal =>
               declare
                  Tag : constant String :=
                    T (Expression_Kinds'Image
                       (Asis.Elements.Expression_Kind (Element)),
                       State.Xml_Style);
                  Literal_Rep   : constant String :=
                    To_Utf8 (To_Trimmed_String (Asis.Expressions.Value_Image (Element)));
                  Literal_Attrs : constant Attributes_List :=
                    Asis_Pedigree
                      & ("literal" = Literal_Rep)
                    & Span_To_Attributes (Element_Span);
               begin
                  Start_Element (State.Xml_File.all, Tag, Literal_Attrs);
                  if Options.Is_Set(State.Axf_Points, Options.Axf_Terminal_Reps) then
                     Output_Tag
                       (State.Xml_File.all,
                        Vatox.Axf_Points.Axf_Point_Tag,
                        Vatox.Axf_Points.Axf_Points_Pedigree &
                        (Vatox.Axf_Points.Terminal_Nomenclature.Axf_Numeric_Attr =
                             Ada_Terms.Based_Representation (Literal_Rep)));
                  end if;
                  End_Element (State.Xml_File.all, Tag);
               end;

            when A_String_Literal =>
               Output_Tag
                 (State.XML_File.all,
                  T (Expression_Kinds'Image
                    (Asis.Elements.Expression_Kind (Element)),
                    State.Xml_Style),
                  Asis_Pedigree &
                  ("literal" =
                       To_Utf8 (To_Trimmed_String (Asis.Expressions.Value_Image (Element))))
                  & Span_To_Attributes (Element_Span));

            when A_Function_Call =>
               Output_Tag
                 (State.XML_File.all,
                  T (Expression_Kinds'Image
                    (Asis.Elements.Expression_Kind (Element)),
                    State.Xml_Style),
                  Asis_Pedigree &
                  ("prefixed" =
                       T (Boolean'Image (Asis.Expressions.Is_Prefix_Call (Element)),
                         State.Xml_Style),
                     "prefixNotation" = Boolean_Attr_Value
                       (Elements.Is_Prefix_Notation(Element)))
                  & Span_To_Attributes (Element_Span));
            when others =>
               Output_Tag
                 (State.XML_File.all,
                  T (Expression_Kinds'Image
                    (Asis.Elements.Expression_Kind (Element)),
                    State.Xml_Style),
                  Asis_Pedigree &
                  Span_To_Attributes (Element_Span));
            end case;

         when An_Association =>            -- Asis.Expressions
            Output_Tag
              (State.XML_File.all,
               T (Association_Kinds'Image
                 (Asis.Elements.Association_Kind (Element)),
                 State.Xml_Style),
               Asis_Pedigree &
               Span_To_Attributes (Element_Span));

         when A_Statement =>               -- Asis.Statements
            Output_Tag
              (State.XML_File.all,
               T (Statement_Kinds'Image (Asis.Elements.Statement_Kind (Element)),
                 State.Xml_Style),
               Asis_Pedigree
               & ("prefixNotation" = Boolean_Attr_Value
                 (Elements.Is_Prefix_Notation (Element)))
               & Span_To_Attributes (Element_Span));

         when A_Path =>                    -- Asis.Statements
            Output_Tag
              (State.XML_File.all,
               T (Path_Kinds'Image (Asis.Elements.Path_Kind (Element)),
                 State.Xml_Style),
               Asis_Pedigree &
               Span_To_Attributes (Element_Span));

         when A_Clause =>                  -- Asis.Clauses
            case Asis.Elements.Clause_Kind (Element) is
            when A_Representation_Clause =>
               Output_Tag
                 (State.XML_File.all,
                  T (Clause_Kinds'Image (Asis.Elements.Clause_Kind (Element)),
                    State.Xml_Style),
                  ("repClause" =
                     T(Representation_Clause_Kinds'Image
                       (Asis.Elements.Representation_Clause_Kind (Element)),
                         State.Xml_Style)) &
                  Asis_Pedigree &
                  Span_To_Attributes (Element_Span));
            when others =>
               Output_Tag
                 (State.XML_File.all,
                  T (Clause_Kinds'Image (Asis.Elements.Clause_Kind (Element)),
                    State.Xml_Style),
                  Asis_Pedigree &
                  Span_To_Attributes (Element_Span));
            end case;

         when others =>
            null;
      end case;

   end Pre_Procedure;

   -----------------------------------------------------------------------------

   procedure Post_Procedure
     (Element : in     Asis.Element;
      Control : in out Asis.Traverse_Control;
      State   : in out Info_Node)
   is
   begin
      pragma Unreferenced (Control);
      State.Last_Element := Element;
      End_Element (State.XML_File.all);
   end Post_Procedure;

   -----------------------------------------------------------------------------

   procedure Initiate_Node
     (Unit    : in Asis.Compilation_Unit;
      Control : in out Asis.Traverse_Control;
      State   : in out Info_Node)
   is
   begin
      pragma Unreferenced (Unit);
      pragma Unreferenced (Control);
      pragma Unreferenced (State);
      null;
   end Initiate_Node;

   -----------------------------------------------------------------------------

   procedure Terminate_Node
     (Control : in out Asis.Traverse_Control;
      State   : in out Info_Node)
   is
   begin
      pragma Unreferenced (Control);
      pragma Unreferenced (State);
      null;
   end Terminate_Node;

   -----------------------------------------------------------------------------

   procedure Write_Source_Info
     (State : in out Info_Node) is
   begin
      Output_Tag
        (State.XML_File.all,
         "sourceLanguage",
         ("name" = "Ada",
          "languageVersion" = 95,
          "compiler" = "GNAT " & Gnatvsn.Gnat_Version_String));
   end Write_Source_Info;

   -----------------------------------------------------------------------------

   procedure Register_Pedigrees is
      use Vatox.Axf_Pedigrees;
   begin
      Register (Asis_Pedigree_Id,
                Standard_Info,
                Asis_Product_Name,
                To_String (Asis.Implementation.ASIS_Implementor),
                To_String (Asis.Implementation.ASIS_Implementor_Version));
      Register (Avatox_Asis_Pedigree_Id,
                Processor_Info,
                Avatox_Versioning.Product_Name,
                Avatox_Versioning.Vendor,
                Avatox_Versioning.Version);
   end Register_Pedigrees;

   -----------------------------------------------------------------------------

   Current_State : Info_Node;

   procedure Output_Pedigree
     (Pedigree_Id         : in     String;
      Pedigree_Type       : in     Vatox.Axf_Pedigrees.Pedigree_Types;
      Pedigree_Producer   : in     String;
      Implementor         : in     String;
      Implementor_Version : in     String;
      Continue            :    out Boolean) is
   begin
      Start_Element
        (Current_State.XML_File.all,
         "pedigree",
         ("name" = Pedigree_Id,
          "xmlStyle" = T (Boolean'Image (Current_State.Xml_Style),
            Xml_Casing => True)));
      Output_Tag
        (Current_State.XML_File.all,
         T (Vatox.Axf_Pedigrees.Pedigree_Types'Image (Pedigree_Type),
           Xml_Casing => True),
         ("name" = Pedigree_Producer,
          "implementor" = Implementor,
          "implementorVersion" = Implementor_Version));
      End_Element (Current_State.XML_File.all, "pedigree");
      Continue := True;
   end Output_Pedigree;

   -----------------------------------------------------------------------------

   procedure Write_Pedigrees
     (State : in out Info_Node) is
   begin
      Start_Element (State.XML_File.all, "pedigrees");

      Current_State := State;
      Vatox.Axf_Pedigrees.Apply_Processor (Output_Pedigree'Access);
      State := Current_State;

      End_Element (State.XML_File.all, "pedigrees");
   end Write_Pedigrees;

   -----------------------------------------------------------------------------

   procedure Start_Representation
     (State : in out Info_Node)
   is
   begin
      if State.Krunch then
         Current_Format := McKae.XML.EZ_Out.Continuous_Stream;
      end if;

      Register_Pedigrees;

      -- XML container for code representation
      Output_XML_Header (State.XML_File.all);
      Start_Element
        (State.XML_File.all,
         "codeRepresentation");

      -- Output information about the source language being processed.
      Write_Source_Info (State);

      -- Output the pedigrees that Avatox uses.
      Write_Pedigrees (State);
   end Start_Representation;

   -----------------------------------------------------------------------------

   procedure Start_Unit
     (Unit  : in Asis.Compilation_Unit;
      State : in out Info_Node)
   is
   begin
      Comment_Start_Line := 0;

      Start_Element
        (State.XML_File.all,
         T (Asis.Unit_Kinds'Image (Asis.Compilation_Units.Unit_Kind (Unit)),
           State.Xml_Style),
         Asis_Pedigree &
         ("unitName" =
              To_String (Asis.Compilation_Units.Unit_Full_Name (Unit))) &
         Span_To_Attributes
           (Asis.Text.Compilation_Unit_Span
              (Asis.Elements.Unit_Declaration (Unit))));
   end Start_Unit;

   -----------------------------------------------------------------------------

   procedure Start_Xml
     (Unit  : in Asis.Compilation_Unit;
      State : in out Info_Node)
   is
   begin
      Start_Representation (State);
      Start_Unit (Unit, State);
   end Start_Xml;

   -----------------------------------------------------------------------------

   procedure End_Unit (Unit  : in     Asis.Compilation_Unit;
                       State : in out Info_Node) is
      Unit_Span : constant Asis.Text.Span
        := Asis.Text.Compilation_Unit_Span (Asis.Elements.Unit_Declaration (Unit));
   begin
      -- Output any comments falling after the last element of the source code.
      if Comment_Start_Line < Unit_Span.Last_Line then
         Output_Comment
           (Asis.Text.Lines
              (State.Last_Element,
               Comment_Start_Line + 1,
               Unit_Span.Last_Line),
            State);
      end if;

      End_Element (State.XML_File.all); -- Compilation unit

      -- Free the list of private declarations (if there were any), since we're
      -- completely done processing this unit
      Free_Private_Decls (State.Private_Decls);
      State.Private_Decls := null;
   end End_Unit;

   -----------------------------------------------------------------------------

   procedure End_Representation (State : in out Info_Node) is
   begin
      End_Element (State.XML_File.all, "codeRepresentation");
   end End_Representation;

   -----------------------------------------------------------------------------

   procedure End_Xml (Unit  : in     Asis.Compilation_Unit;
                      State : in out Info_Node) is
   begin
      End_Unit (Unit, State);
      End_Representation (State);
   end End_Xml;

   -----------------------------------------------------------------------------

   function Is_Element_In_Statement_List
     (Asis_Element : Asis.Element;
      Statements   : Asis.Statement_List)
      return boolean is
      use Asis.Elements;
   begin
      for I in Statements'Range loop
         if Is_Identical (Asis_Element, Statements (I)) then
            return True;
         end if;
      end loop;
      return False;
   end Is_Element_In_Statement_List;

   -----------------------------------------------------------------------------

   function Mark_As_A_Statement
     (Asis_Element : Asis.Element) return Boolean is

      use Asis.Elements;

      Parent      : Asis.Element := Enclosing_Element (Asis_Element);
      Parent_Kind : Asis.Element_Kinds := Element_Kind (Parent);
      Decl_Kind   : Asis.Declaration_Kinds;
      Stmt_Kind   : Asis.Statement_Kinds;

   begin
      if Element_Kind (Asis_Element) = A_Statement then
         -- Self-evidently a statement, so we don't need to mark it as such.
         return False;
      end if;

      case Parent_Kind is
         when Asis.A_Declaration =>
            Decl_Kind := Declaration_Kind (Parent);
            case Decl_Kind is
               when A_Function_Body_Declaration |
                    A_Procedure_Body_Declaration |
                    A_Package_Body_Declaration |
                    A_Task_Body_Declaration |
                    An_Entry_Body_Declaration =>
                  return Is_Element_In_Statement_List
                    (Asis_Element,
                     Declarations.Body_Statements (Parent,
                       Include_Pragmas => True));
               when others =>
                  null;
            end case;

         when Asis.A_Path =>
            return Is_Element_In_Statement_List
              (Asis_Element,
               Statements.Sequence_Of_Statements (Parent,
                 Include_Pragmas => True));
         when Asis.A_Statement =>
            Stmt_Kind := Statement_Kind (Parent);
            case Stmt_Kind is
               when A_Loop_Statement |
                    A_While_Loop_Statement |
                    A_For_Loop_Statement =>
                  return Is_Element_In_Statement_List
                    (Asis_Element,
                     Statements.Loop_Statements (Parent,
                       Include_Pragmas => True));
               when A_Block_Statement =>
                  return Is_Element_In_Statement_List
                    (Asis_Element,
                     Statements.Block_Statements (Parent,
                       Include_Pragmas => True));
               when An_Accept_Statement =>
                  return Is_Element_In_Statement_List
                    (Asis_Element,
                     Statements.Accept_Body_Statements (Parent,
                       Include_Pragmas => True));
               when others =>
                  null;
            end case;

         when Asis.An_Exception_Handler =>
            return Is_Element_In_Statement_List
              (Asis_Element,
               Statements.Handler_Statements (Parent,
                 Include_Pragmas => True));
         when others =>
            null;
      end case;
      return False;
   end Mark_As_A_Statement;

   -----------------------------------------------------------------------------

end Vatox.Traversal;
