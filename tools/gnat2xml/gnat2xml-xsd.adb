------------------------------------------------------------------------------
--                                                                          --
--                            GNAT2XML COMPONENTS                           --
--                                                                          --
--                          G N A T 2 X M L . X S D                         --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                      Copyright (C) 2012-2014, AdaCore                    --
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

with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Command_Line;

with ASIS_UL.Formatted_Output; use ASIS_UL.Formatted_Output;

with Gnat2xml_Versioning;
with Ada_Trees; use Ada_Trees;

package body Gnat2xml.Xsd is

   --  We don't worry about reclaiming storage, for now. We're just going to
   --  exit the program after building heap-allocated data structures, so it
   --  probably doesn't matter. It would be straightforward to use a storage
   --  pool to reclaim storage upon return from Generate_Schema. Just move all
   --  the types inside Generate_Schema and use a storage pool. Or use Ada 2012
   --  subpools.

   use A4G.Queries;

   --  Data structures for representing XML elements and attributes:

   type WStr_Ptr is access String;

   type Xml_Attribute is record
      Attr : WStr_Ptr;
      Val  : WStr_Ptr;
   end record;

   type Xml_Attribute_Index is new Positive;

   type Xml_Attribute_Seq is
     array (Xml_Attribute_Index range <>) of Xml_Attribute;

   type Xml_Attribute_Seq_Ptr is access Xml_Attribute_Seq;

   function Empty return Xml_Attribute_Seq;

   type Xml_Element_Seq;

   type Xml_Element_Seq_Ptr is access Xml_Element_Seq;

   type Xml_Element is record
      Tag          : WStr_Ptr;
      Attrs        : Xml_Attribute_Seq_Ptr;
      Sub_Elements : Xml_Element_Seq_Ptr;
   end record;

   type Xml_Element_Ptr is access Xml_Element;

   type Xml_Element_Index is new Positive;
   subtype Xml_Element_Count is
     Xml_Element_Index'Base range 0 .. Xml_Element_Index'Last;

   type Xml_Element_Seq is
     array (Xml_Element_Index range <>) of Xml_Element_Ptr;

   function Empty return Xml_Element_Seq;

   function "+" (X : Xml_Attribute) return Xml_Attribute_Seq is ((1 => X));
   function "+" (X : Xml_Element_Ptr) return Xml_Element_Seq is ((1 => X));
   --  Return singleton sequence

   function Attr (Attr, Val : String) return Xml_Attribute;
   --  Construct an XML attribute Attr="Val"

   function Xml_Elem
     (Tag          : String;
      Attrs        : Xml_Attribute_Seq;
      Sub_Elements : Xml_Element_Seq)
      return         Xml_Element_Ptr;
   --  Construct an XML element

   procedure Put_Comment (Comment : String);

   procedure C (Comment : String) renames Put_Comment;
   procedure P (T : Template);
   --  Shorthands

   procedure Put (X : Xml_Attribute);
   procedure Put (X : Xml_Attribute_Seq_Ptr);
   procedure Put (X : Xml_Element_Ptr);
   procedure Put (X : Xml_Element_Seq_Ptr);
   --  Procedures to generate XML as text

   ----------
   -- Attr --
   ----------

   function Attr (Attr, Val : String) return Xml_Attribute is
   begin
      return (new String'(Attr), new String'(Val));
   end Attr;

   -----------
   -- Empty --
   -----------

   function Empty return Xml_Attribute_Seq is
   begin
      return (1 .. 0 => Attr ("", ""));
   end Empty;

   function Empty return Xml_Element_Seq is
   begin
      return (1 .. 0 => null);
   end Empty;

   ---------------------
   -- Generate_Schema --
   ---------------------

   procedure Generate_Schema is

      Compact_XML : Boolean := False;

      Compact_Sloc_Attrs : constant Xml_Element_Seq :=
      --  These are the attributes of an element in Compact_XML mode

        +Xml_Elem
          ("xsd:attribute",
           (Attr ("name", "sloc"),
            Attr ("type", "Source_Location"),
            Attr ("use", "required")),
           Empty);

      Verbose_Sloc_Attrs : constant Xml_Element_Seq :=
      --  These are the attributes of the Source_Location element in
      --  non-Compact_XML mode. In this case, each element has a Sloc
      --  sub-element.

        (Xml_Elem
           ("xsd:attribute",
            (Attr ("name", "line"),
             Attr ("type", "xsd:positiveInteger"),
             Attr ("use", "required")),
            Empty),
         Xml_Elem
           ("xsd:attribute",
            (Attr ("name", "col"),
             Attr ("type", "xsd:positiveInteger"),
             Attr ("use", "required")),
            Empty),
         Xml_Elem
           ("xsd:attribute",
            (Attr ("name", "endline"),
             Attr ("type", "xsd:nonNegativeInteger"),
             Attr ("use", "required")),
            Empty),
         Xml_Elem
           ("xsd:attribute",
            (Attr ("name", "endcol"),
             Attr ("type", "xsd:nonNegativeInteger"),
             Attr ("use", "required")),
            Empty));

      function Element_Name (Kind : Flat_Element_Kinds'Base) return String;
      --  A_Defining_Identifier --> "defining_identifier"
      function Element_Name (Q : Structural_Queries) return String;
      --  Sequence_Of_Statements --> "sequence_of_statements"
      function Type_Name (Kind : Flat_Element_Kinds'Base) return String;
      --  A_Defining_Identifier --> "Defining_Identifier".

      function Queries (Qs : Query_List) return Xml_Element_Seq;
      --  Query elements corresponding to the Query_List

      procedure Put_Abstract_Class_Or_List (Kind : Flat_Element_Kinds'Base);
      --  Prints out the xsd:choice xsd:complexType, consisting of the union of
      --  all kinds in the class. If Kind is a list type, then we allow zero or
      --  more occurrences, otherwise just one.

      procedure Put_Group (Class : Flat_Element_Kinds'Base);
      --  Prints out the xsd:group element, currently used only for
      --  pragmas_group.

      procedure Put_ASIS_Elem (Kind : Opt_ASIS_Elems);
      --  Prints out the xsd:element for the specified Kind, along with its
      --  type, an xsd:complexType.

      procedure Process_Command_Line;

      ------------------
      -- Element_Name --
      ------------------

      function Element_Name (Kind : Flat_Element_Kinds'Base) return String is
      begin
         return To_Lower (Type_Name (Kind));
      end Element_Name;

      function Element_Name (Q : Structural_Queries) return String is
      begin
         return To_Lower (Structural_Queries'Image (Q));
      end Element_Name;

      --------------------------------
      -- Put_Abstract_Class_Or_List --
      --------------------------------

      Num_Pragmas : constant Xml_Element_Count :=
        Xml_Element_Count
          (Cardinality (Kinds_In_Class (A_Pragma_Element_Class)));

      procedure Put_Abstract_Class_Or_List (Kind : Flat_Element_Kinds'Base) is
         Class : constant Flat_Element_Kinds'Base :=
           (if Kind in Flat_List_Kinds then List_Component_Type (Kind)
            else Kind);
         Kinds   : constant Kind_Set := Kinds_In_Class (Class);
         Choices : Xml_Element_Seq
         (1 .. Xml_Element_Index (Cardinality (Kinds)) - Num_Pragmas + 1);
         --  1 for each Kind, minus the pragma Kinds, plus 1 for the pragma
         --  group.
         Last : Xml_Element_Count := 0;

         Occurs : constant Xml_Attribute_Seq :=
           (if
              Kind in Flat_List_Kinds
            then
              (Attr ("minOccurs", "0"), Attr ("maxOccurs", "unbounded"))
            else +Attr ("maxOccurs", "1"));

      begin
         --  We skip the pragma Kinds, and add them on at the end as a "group"

         for K in Kinds'Range loop
            if K not in Flat_Pragma_Kinds and then Kinds (K) then
               Last           := Last + 1;
               Choices (Last) :=
                 Xml_Elem
                   ("xsd:element",
                    +Attr ("ref", Element_Name (K)),
                    Empty);
            end if;
         end loop;

         Last           := Last + 1;
         Choices (Last) :=
           Xml_Elem ("xsd:group", +Attr ("ref", "pragmas_group"), Empty);

         pragma Assert (Last = Choices'Last);

         Put
           (Xml_Elem
              ("xsd:complexType",
               +Attr ("name", Type_Name (Kind)),
               +Xml_Elem ("xsd:choice", Occurs, Choices)));
         Put ("\n");
      end Put_Abstract_Class_Or_List;

      ---------------
      -- Put_Group --
      ---------------

      procedure Put_Group (Class : Flat_Element_Kinds'Base) is
         pragma Assert (Class = A_Pragma_Element_Class); -- for now
         Kinds   : constant Kind_Set := Kinds_In_Class (Class);
         Choices : Xml_Element_Seq
         (1 .. Xml_Element_Index (Cardinality (Kinds)));
         Last : Xml_Element_Count := 0;

      begin
         for K in Kinds'Range loop
            if Kinds (K) then
               Last           := Last + 1;
               Choices (Last) :=
                 Xml_Elem
                   ("xsd:element",
                    +Attr ("ref", Element_Name (K)),
                    Empty);
            end if;
         end loop;

         pragma Assert (Last = Choices'Last);

         Put
           (Xml_Elem
              ("xsd:group",
               +Attr ("name", "pragmas_group"),
               +Xml_Elem ("xsd:choice", Empty, Choices)));
         Put ("\n");
      end Put_Group;

      -------------------
      -- Put_ASIS_Elem --
      -------------------

      procedure Put_ASIS_Elem (Kind : Opt_ASIS_Elems) is
         Qs : Query_List renames Appropriate_Queries (Kind).all;
         pragma Assert (Qs'First = 1);
         QQ      : constant Xml_Element_Seq := Queries (Qs);
         QQ_Sloc : constant Xml_Element_Seq :=
           (if Compact_XML then QQ
            else
              Xml_Elem
                ("xsd:element",
                 (Attr ("name", "sloc"), Attr ("type", "Source_Location")),
                 Empty) &
              QQ);

         Subelements : constant Xml_Element_Seq :=
         --  The sequence of Sloc and query elements, except we want Empty if
         --  there are none.
           (if QQ_Sloc'Length = 0 then Empty
            else +Xml_Elem ("xsd:sequence", Empty, QQ_Sloc));

         Unit_Kind_Attrs : constant Xml_Element_Seq :=
           (if
              Kind in A_Compilation_Unit
            then
              +Xml_Elem
                ("xsd:attribute",
                 (Attr ("name", "unit_kind"),
                  Attr ("type", "xsd:string"),
                  Attr ("use", "required")),
                 Empty)
            else Empty);

         Unit_Class_Attrs : constant Xml_Element_Seq :=
           (if
              Kind in A_Compilation_Unit
            then
              +Xml_Elem
                ("xsd:attribute",
                 (Attr ("name", "unit_class"),
                  Attr ("type", "xsd:string"),
                  Attr ("use", "required")),
                 Empty)
            else Empty);

         Unit_Origin_Attrs : constant Xml_Element_Seq :=
           (if
              Kind in A_Compilation_Unit
            then
              +Xml_Elem
                ("xsd:attribute",
                 (Attr ("name", "unit_origin"),
                  Attr ("type", "xsd:string"),
                  Attr ("use", "required")),
                 Empty)
            else Empty);

         Unit_Full_Name_Attrs : constant Xml_Element_Seq :=
           (if
              Kind in A_Compilation_Unit
            then
              +Xml_Elem
                ("xsd:attribute",
                 (Attr ("name", "unit_full_name"),
                  Attr ("type", "xsd:string"),
                  Attr ("use", "required")),
                 Empty)
            else Empty);

         Def_Name_Attrs : constant Xml_Element_Seq :=
           (if
              Kind in A_Compilation_Unit | Def_Names
            then
              +Xml_Elem
                ("xsd:attribute",
                 (Attr ("name", "def_name"),
                  Attr ("type", "xsd:string"),
                  Attr ("use", "required")),
                 Empty)
            else Empty);

         Ref_Name_Attrs : constant Xml_Element_Seq :=
           (if
              Kind in Usage_Names
            then
              +Xml_Elem
                ("xsd:attribute",
                 (Attr ("name", "ref_name"),
                  Attr ("type", "xsd:string"),
                  Attr ("use", "required")),
                 Empty)
            else Empty);

         Source_File_Attrs : constant Xml_Element_Seq :=
           (case Kind is
              when A_Compilation_Unit =>
                +Xml_Elem
                  ("xsd:attribute",
                   (Attr ("name", "source_file"),
                    Attr ("type", "xsd:string"),
                    Attr ("use", "required")),
                   Empty),
              when others => Empty);

         Def_Attrs : constant Xml_Element_Seq :=
           (case Kind is
              when Not_An_Element => Empty,
         --  ???when Usage_Names | Boolean_Elems | Other_Elems => Empty,
         --  Triggers infinite loop in Is_From_Unknown_Pragma.
              when Def_Names =>
                +Xml_Elem
                  ("xsd:attribute",
                   (Attr ("name", "def"),
                    Attr ("type", "xsd:string"),
                    Attr ("use", "required")),
                   Empty),
              when others => Empty);

         Ref_Attrs : constant Xml_Element_Seq :=
           (case Kind is
              when Not_An_Element => Empty,
         --  ???when Def_Names | Other_Elems => Empty,
              when Usage_Names =>
                +Xml_Elem
                  ("xsd:attribute",
                   (Attr ("name", "ref"),
                    Attr ("type", "xsd:string"),
                    Attr ("use", "required")),
                   Empty),
              when others => Empty);

         Lit_Val_Attrs : constant Xml_Element_Seq :=
           (if
              Kind in An_Integer_Literal | A_Real_Literal | A_String_Literal
            then
              +Xml_Elem
                ("xsd:attribute",
                 (Attr ("name", "lit_val"),
                  Attr ("type", "xsd:string"),
                  Attr ("use", "required")),
                 Empty)
            else Empty);

         Type_Attrs : constant Xml_Element_Seq :=
           (if
              Kind in Def_Names | Flat_Expression_Kinds
            then
              +Xml_Elem
                ("xsd:attribute",
                 (Attr ("name", "type"),
                  Attr ("type", "xsd:string"),
                  Attr ("use", "required")),
                 Empty)
            else Empty);

         Pragma_Name_Attrs : constant Xml_Element_Seq :=
           (if
              Kind in Flat_Pragma_Kinds
            then
              +Xml_Elem
                ("xsd:attribute",
                 (Attr ("name", "pragma_name"),
                  Attr ("type", "xsd:string"),
                  Attr ("use", "required")),
                 Empty)
            else Empty);

         Mode_Attrs : constant Xml_Element_Seq :=
           (if
              Kind in A_Parameter_Specification | A_Formal_Object_Declaration
            then
              +Xml_Elem
                ("xsd:attribute",
                 (Attr ("name", "mode"),
                  Attr ("type", "xsd:string"),
         --  ???Could be more specific type

                  Attr ("use", "required")),
                 Empty)
            else Empty);

         Text_Attrs : constant Xml_Element_Seq :=
           (if
              Kind in A_Comment
            then
              +Xml_Elem
                ("xsd:attribute",
                 (Attr ("name", "text"),
                  Attr ("type", "xsd:string"),
                  Attr ("use", "required")),
                 Empty)
            else Empty);

         Checks_Attrs : constant Xml_Element_Seq :=
           (if Kind = Not_An_Element then Empty
            else +Xml_Elem
              ("xsd:attribute",
               (Attr ("name", "checks"),
                Attr ("type", "xsd:string")), -- optional
               Empty));

         Sloc_Attrs : constant Xml_Element_Seq :=
           (if Compact_XML then Compact_Sloc_Attrs else Empty);

         Attrs : constant Xml_Element_Seq :=
           Unit_Kind_Attrs &
           Unit_Class_Attrs &
           Unit_Origin_Attrs &
           Unit_Full_Name_Attrs &
           Def_Name_Attrs &
           Ref_Name_Attrs &
           Source_File_Attrs &
           Def_Attrs &
           Ref_Attrs &
           Lit_Val_Attrs &
           Type_Attrs &
           Pragma_Name_Attrs &
           Mode_Attrs &
           Text_Attrs &
           Checks_Attrs &
           Sloc_Attrs;

      begin

         Put ("\n");
         Put
           (Xml_Elem
              ("xsd:element",
               (Attr ("name", Element_Name (Kind)),
                Attr ("type", Type_Name (Kind))),
               Empty));

         Put ("\n");

         Put
           (Xml_Elem
              ("xsd:complexType",
               +Attr ("name", Type_Name (Kind)),
               Subelements & Attrs));

      end Put_ASIS_Elem;

      -------------
      -- Queries --
      -------------

      function Queries (Qs : Query_List) return Xml_Element_Seq is
         Result : Xml_Element_Seq (1 .. Xml_Element_Index'Base (Qs'Length));

      begin
         for J in Qs'Range loop
            declare
               JJ : constant Xml_Element_Index := Xml_Element_Index (J);

            begin
               declare
                  Q            : constant Structural_Queries := Qs (J);
                  Syntax       : constant Func_Elem := Get_Func_Elem (Q);
                  Query_Suffix : constant String             :=
                    (case Syntax.Query_Kind is
                       when Bug => "? ? ?", -- can't happen
                       when Single_Element_Query |
                         Single_Element_CU_Query |
                         Boolean_Query =>
                         "_q",
                       when Element_List_CU_Query |
                         Element_List_Query |
                         Element_List_Query_With_Boolean =>
                         "_ql");
                  Q_Type : constant Flat_Element_Kinds'Base :=
                    Query_Result_Types (Q);
                  Bool_Kind : Boolean_Elems;

               begin
                  if Q_Type in Boolean_Elems then
                     Bool_Kind   := Query_Result_Types (Qs (J));
                     Result (JJ) :=
                       Xml_Elem
                         ("xsd:element",
                          +Attr ("name", Element_Name (Qs (J)) & Query_Suffix),
                          +Xml_Elem
                            ("xsd:complexType",
                             Empty,
                             +Xml_Elem
                               ("xsd:choice",
                                Empty,
                                (Xml_Elem
                                   ("xsd:element",
                                    (Attr ("name", Element_Name (Bool_Kind)),
                                     Attr ("type", Type_Name (Bool_Kind))),
                                    Empty),
                                 Xml_Elem
                                   ("xsd:element",
                                    (Attr
                                       ("name",
                                        Element_Name (Not_An_Element)),
                                     Attr
                                       ("type",
                                        Type_Name (Not_An_Element))),
                                    Empty)))));

                  else
                     --  ???A_Component_Definition and A_Subtype_Indication
                     --  should really have their own classes.
                     Result (JJ) :=
                       Xml_Elem
                         ("xsd:element",
                          (Attr ("name", Element_Name (Qs (J)) & Query_Suffix),
                           Attr
                             ("type",
                              (if
                                 Q_Type in
                                   A_Component_Definition |
                                     A_Subtype_Indication
                               then
                                 "Element_Class"
                               else Type_Name (Q_Type)))),
                          Empty);
                  end if;
               end;
            end;
         end loop;

         return Result;
      end Queries;

      ---------------
      -- Type_Name --
      ---------------

      function Type_Name (Kind : Flat_Element_Kinds'Base) return String is
      begin
         return Strip_Article (Capitalize (Opt_ASIS_Elems'Image (Kind)));
      end Type_Name;

      procedure Process_Command_Line is
         use Ada.Command_Line;

      begin
         case Argument_Count is
            when 0 =>
               null;

            when 1 =>
               if Argument (1) = "--compact" then
                  Compact_XML := True;

               else
                  raise Program_Error
                    with "???Unrecognized argument: " & Argument (1);
               end if;

            when others =>
               raise Program_Error with "???Too many arguments";
               --  Above "raises" should be proper error messages
         end case;
      end Process_Command_Line;

   --  Start of processing for Generate_Schema

   begin
      Process_Command_Line;

      P ("<xsd:schema xmlns:xsd=""http://www.w3.org/2001/XMLSchema"">\n\n");
      Indent;

      P ("<xsd:annotation>\n");
      Indent;
      P ("<xsd:documentation xml:lang=""en"">\n");
      Indent;

      P ("\n");
      P ("XML Schema (XSD) for Ada.\n");
      P ("\n");
      P ("This file was generated by the gnat2xsd tool.\n");
      P ("\n");

      Gnat2xml_Versioning.Print_Version_Info
        (Tool_Name          => "gnat2xsd",
         First_Release_Year => "2012");

      P ("\n");
      P ("This schema is open source and is licensed " &
         "under the Eclipse Public\n");
      P ("License (EPL). See http://www.eclipse.org/legal/epl-v10.html\n");

      P ("\n");
      P ("\n");
      P ("There are several sections below:\n");
      P ("\n");
      P ("    Basic Types. General-purpose elements used everywhere.\n");
      P ("\n");
      P ("    Abstract Classes. Unions of concrete types.\n");
      P ("\n");
      P ("    List Types. Lists of abstract class types.\n");
      P ("\n");
      P ("    Boolean Elements. These indicate that the corresponding\n");
      P ("    query is True; False is indicated by Nil.\n");
      P ("\n");
      P ("    Defining Occurrences. Elements for defining occurrences,\n");
      P ("    such as Defining_Identifier.\n");
      P ("\n");
      P ("    Usage Occurrences. Elements for usage occurrences\n");
      P ("    (references to defining occurrences), such as Identifier.\n");
      P ("\n");
      P ("    Other Elements. All syntactic structures that don't fall\n");
      P ("    under defining or usage occurrences.\n");
      P ("\n");

      Outdent;
      P ("</xsd:documentation>\n");
      Outdent;
      P ("</xsd:annotation>\n");

      P ("\n");
      P ("\n");
      C ("================================================================");
      C ("Basic Types");
      P ("\n");

      if Compact_XML then
         Put
           (Xml_Elem
              ("xsd:simpleType",
               +Attr ("name", "Source_Location"),
               +Xml_Elem
                 ("xsd:restriction",
                  +Attr ("base", "xsd:string"),
                  +Xml_Elem
                    ("xsd:pattern",
                     +Attr ("value", "\d+:\d+"),
                     Empty))));

      else
         Put
           (Xml_Elem
              ("xsd:complexType",
               +Attr ("name", "Source_Location"),
               Verbose_Sloc_Attrs));
      end if;

      Put_ASIS_Elem (Not_An_Element);

      Put_ASIS_Elem (A_Compilation_Unit);

      P ("\n");
      C ("================================================================");
      C ("Abstract Classes");
      P ("\n");

      for Class in Flat_Abstract_Classes loop
         Put_Abstract_Class_Or_List (Class);
      end loop;

      Put_Group (A_Pragma_Element_Class);

      P ("\n");
      C ("================================================================");
      C ("List Types");
      P ("\n");

      for Kind in Flat_List_Kinds loop
         Put_Abstract_Class_Or_List (Kind);
      end loop;

      P ("\n");
      C ("================================================================");
      C ("Boolean Elements");

      for Kind in Boolean_Elems loop
         Put_ASIS_Elem (Kind);
      end loop;

      P ("\n");
      C ("================================================================");
      C ("Defining Occurrences");

      for Kind in Def_Names loop
         Put_ASIS_Elem (Kind);
      end loop;

      P ("\n");
      C ("================================================================");
      C ("Usage Occurrences");

      for Kind in Usage_Names loop
         Put_ASIS_Elem (Kind);
      end loop;

      P ("\n");
      C ("================================================================");
      C ("Other Elements");

      for Kind in Other_Elems loop
         if Kind /= A_Compilation_Unit then
            Put_ASIS_Elem (Kind);
         end if;
      end loop;

      P ("\n");
      Outdent;
      P ("</xsd:schema>\n");
   end Generate_Schema;

   -------
   -- P --
   -------

   procedure P (T : Template) is
   begin
      Put (T);
   end P;

   ---------
   -- Put --
   ---------

   procedure Put (X : Xml_Attribute) is
   begin
      Put ("\1=""\2""", X.Attr.all, X.Val.all);
   end Put;

   procedure Put (X : Xml_Attribute_Seq_Ptr) is
   begin
      for J in X'Range loop
         if J /= X'First then
            Put (" ");
         end if;
         Put (X (J));
      end loop;
   end Put;

   procedure Put (X : Xml_Element_Ptr) is
   begin
      --  If no subelements, use the short one-line form

      if X.Sub_Elements'Length = 0 then
         if X.Attrs'Length = 0 then
            Put ("<\1/>\n", X.Tag.all);

         else
            Put ("<\1 ", X.Tag.all);
            Put (X.Attrs);
            Put ("/>\n");
         end if;

      --  Otherwise (there are subelements), use the long form

      else
         if X.Attrs'Length = 0 then
            Put ("<\1>\n", X.Tag.all);

         else
            Put ("<\1 ", X.Tag.all);
            Put (X.Attrs);
            Put (">\n");
         end if;

         Indent;
         Put (X.Sub_Elements);
         Outdent;
         Put ("</\1>\n", X.Tag.all);
      end if;
   end Put;

   procedure Put (X : Xml_Element_Seq_Ptr) is
   begin
      for J in X'Range loop
         Put (X (J));
      end loop;
   end Put;

   -----------------
   -- Put_Comment --
   -----------------

   procedure Put_Comment (Comment : String) is
   begin
      Put ("<!-- \1 -->\n", Comment);
   end Put_Comment;

   --------------
   -- Xml_Elem --
   --------------

   function Xml_Elem
     (Tag          : String;
      Attrs        : Xml_Attribute_Seq;
      Sub_Elements : Xml_Element_Seq)
      return         Xml_Element_Ptr
   is
   begin
      return new Xml_Element'
          (new String'(Tag),
           new Xml_Attribute_Seq'(Attrs),
           new Xml_Element_Seq'(Sub_Elements));
   end Xml_Elem;

end Gnat2xml.Xsd;
