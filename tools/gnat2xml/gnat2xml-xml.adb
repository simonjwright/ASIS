------------------------------------------------------------------------------
--                                                                          --
--                          GNAT2XML COMPONENTS                             --
--                           AVATOX COMPONENTS                              --
--                                                                          --
--                        VATOX (Via Asis To Xml)                           --
--                                                                          --
--                                                                          --
--                Copyright (C) 2006, McKae Technologies.                   --
--                   Copyright (C) 2012-2017, AdaCore                       --
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
-- Avatox is now maintained by McKae Technologies (http://www.mckae.com)    --
-- The gnat2xml tool was derived from the Avatox sources.                   --
------------------------------------------------------------------------------

pragma Ada_2012;

with Opt;

with Ada.Characters.Conversions; use Ada.Characters.Conversions;
with Ada.Characters.Handling;
with Ada.Directories;
with Ada.Strings.Unbounded;
with Ada.Strings.Wide_Fixed;     use Ada.Strings.Wide_Fixed;

with Asis.Elements;
with Asis.Text;
with Asis.Compilation_Units;

with McKae.Text.Lexicals;
with McKae.XML.EZ_Out;

with Gnat2xml.Command_Line;    use Gnat2xml.Command_Line;
with Gnat2xml.Xml_File_Output; use Gnat2xml.Xml_File_Output;
with Ada_Trees.Asis_To_Tree; use Ada_Trees;
with Pp.Buffers;        use Pp.Buffers;
with Pp.Scanner;

with Text_IO; use Text_IO;
with ASIS_UL.Options;
with Namet;
with A4G.Queries;  use A4G.Queries;

package body Gnat2xml.Xml is

   subtype Structural_Queries is A4G.Queries.Structural_Queries;

   use McKae.Text;

   function St (Id : Namet.Name_Id) return String
     renames Namet.Get_Name_String;

   ----------------------------------------------------------------------------

   function T (S : String; Xml_Casing : Boolean) return String;

   function T (S : String; Xml_Casing : Boolean) return String is
   begin
      if True then -- ???
         declare
            Result : constant String :=
              Lexicals.Transform
                (S,
                 Lexicals.lower,
                 Remove_Underscores => False);

         begin
            return Strip_Article (Result);
         end;
      end if;

      if Xml_Casing then
         return Lexicals.Transform
             (S,
              Lexicals.XML_Common,
              Remove_Underscores => True);

      else
         return S;
      end if;
   end T;

   ----------------------------------------------------------------------------

   function Span_To_Compact_Attributes
     (Element_Span : Asis.Text.Span)
      return         Attributes_List;

   function Span_To_Compact_Attributes
     (Element_Span : Asis.Text.Span)
      return         Attributes_List
   is
      use Ada.Strings;
      use Asis.Text;

   begin
      if Gnat2xml_Options.Compact_XML then
         return
           (0 =>
              "sloc" =
              To_String
                  (Trim
                     (Line_Number_Positive'Wide_Image
                        (Element_Span.First_Line),
                      Left)) &
                ":" &
                To_String
                  (Trim
                     (Character_Position_Positive'Wide_Image
                        (Element_Span.First_Column),
                      Left)));

      else
         return No_Attributes;
      end if;
   end Span_To_Compact_Attributes;

   function Span_To_Verbose_Attributes
     (Element_Span : Asis.Text.Span)
      return         Attributes_List;

   function Span_To_Verbose_Attributes
     (Element_Span : Asis.Text.Span)
      return         Attributes_List
   is
      use Ada.Strings;
      use Asis.Text;

   begin
      --???pragma Assert (not Gnat2xml_Options.Compact_XML);
      return
        (("line" =
          To_String
            (Trim
               (Line_Number_Positive'Wide_Image (Element_Span.First_Line),
                Left))),
         ("col" =
          To_String
            (Trim
               (Character_Position_Positive'Wide_Image
                  (Element_Span.First_Column),
                Left))),
         ("endline" =
          To_String
            (Trim (Line_Number'Wide_Image (Element_Span.Last_Line), Left))),
         ("endcol" =
          To_String
            (Trim
               (Character_Position'Wide_Image (Element_Span.Last_Column),
                Left))));
   end Span_To_Verbose_Attributes;

   ----------------------------------------------------------------------------

   Pp_Off_On_Delimiters : constant Pp.Scanner.Pp_Off_On_Delimiters_Rec
     := (others => <>);

   Skip_Gen : Boolean renames ASIS_UL.Debug.Debug_Flag_3;

   procedure Process_Unit
     (The_Unit : Asis.Compilation_Unit;
      State    : Info_Node)
   is
      --  ???Much of the info in State could be local to here!

      use Compilation_Units;

      procedure Put_Src_Lines (Up_To : Asis.Text.Line_Number);
      --  Print out source lines up to the current Sloc, which is Up_To. This
      --  is for debugging the XML, and is only done in --compact mode.

      procedure Process_Elem
        (Q               : Structural_Queries;
         Tree            : Ada_Tree;
         Is_List_Element : Boolean);

      procedure Process_List
        (Q    : Structural_Queries;
         List : Ada_Tree);

      procedure Pre
        (Q               : Structural_Queries;
         Is_List_Element : Boolean;
         Tree            : Ada_Tree);

      procedure Pre
        (Q               : Structural_Queries;
         Is_List_Element : Boolean;
         Tree            : Ada_Tree)
      is

         function Checks return String;

         function Checks return String is
            use Ada.Strings.Unbounded, Ada.Characters.Handling;
            Result : Unbounded_String;
            First_Time : Boolean := True;
         begin
            for Check in Tree.Checks'Range loop
               if Tree.Checks (Check) then
                  if First_Time then
                     First_Time := False;
                  else
                     Append (Result, ",");
                  end if;
                  Append (Result, To_Lower (Check'Img));
               end if;
            end loop;

            return To_String (Result);
         end Checks;

         Kind : constant Opt_ASIS_Elems := Tree.Kind;

         Def_Name_Attrs : constant Attributes_List :=
           (if Kind in Def_Names then +("def_name" = St (Tree.Def_Name))
            else No_Attributes);

         Def_Attrs : constant Attributes_List :=
           (if Kind in Def_Names then +("def" = St (Tree.Def))
            else No_Attributes);

         Ref_Name_Attrs : constant Attributes_List :=
           (if
              Kind in Flat_Usage_Name_Kinds
            then
              +("ref_name" = St (Tree.Ref_Name))
            else No_Attributes);

         Ref_Attrs : constant Attributes_List :=
           (if
              Kind in Flat_Usage_Name_Kinds
            then
              +("ref" = St (Tree.Ref))
            else No_Attributes);

         Type_Attrs : constant Attributes_List :=
           (if
              Kind in Def_Names | Flat_Expression_Kinds
            then
              +("type" = St (Get_Type (Tree)))
            else No_Attributes);

         Pragma_Name_Attrs : constant Attributes_List :=
           (if
              Kind in Flat_Pragma_Kinds
            then
              +("pragma_name" = St (Tree.Pragma_Name))
            else No_Attributes);

         Lit_Val_Attrs : constant Attributes_List :=
           (if
              Kind in An_Integer_Literal | A_Real_Literal | A_String_Literal
            then
              +("lit_val" = St (Tree.Lit_Val))
            else No_Attributes);

         Mode_Attrs : constant Attributes_List :=
           (if
              Kind in A_Parameter_Specification | A_Formal_Object_Declaration
            then
              +("mode" = Tree.Mode'Img)
            else No_Attributes);

         Text_Attrs : constant Attributes_List :=
           (if
              Kind in A_Comment
            then
              +("text" = St (Tree.Text))
            else No_Attributes);

         Checks_Attrs : constant Attributes_List :=
           (if Kind not in Not_An_Element
            then
              +("checks" = Checks)
            else No_Attributes);

         Attrs : constant Attributes_List :=
           Span_To_Compact_Attributes (Tree.Sloc) &
           Def_Name_Attrs &
           Def_Attrs &
           Ref_Name_Attrs &
           Ref_Attrs &
           Type_Attrs &
           Pragma_Name_Attrs &
           Lit_Val_Attrs &
           Mode_Attrs &
           Text_Attrs &
           Checks_Attrs;

      --  Start of processing for Pre

      begin
         if Gnat2xml_Options.Compact_XML then
            Put_Src_Lines (Up_To => Tree.Sloc.First_Line);
         end if;

         if not Is_List_Element then
            Start_Element
              (State.XML_File.all,
               T (Structural_Queries'Image (Q) & "_q", State.Xml_Style));
         end if;

         if Tree.Subtree_Count = 0 and then Gnat2xml_Options.Compact_XML then
            Output_Tag
              (State.XML_File.all,
               T (ASIS_Elems'Image (Kind), State.Xml_Style),
               Attrs);

         else
            Start_Element
              (State.XML_File.all,
               T (ASIS_Elems'Image (Kind), State.Xml_Style),
               Attrs);
            if not Gnat2xml_Options.Compact_XML then
               Output_Tag
                 (State.XML_File.all,
                  T ("sloc", State.Xml_Style),
                  Span_To_Verbose_Attributes (Tree.Sloc));
            end if;
         end if;
      end Pre;

      procedure Process_Elem
        (Q               : Structural_Queries;
         Tree            : Ada_Tree;
         Is_List_Element : Boolean)
      is

         procedure Process_Children;

         procedure Process_Children is
            Qs : Query_List renames Appropriate_Queries (Tree.Kind).all;

         begin
            for Index in Qs'Range loop
               declare
                  Q     : constant Structural_Queries := Qs (Index);
                  Child : constant Ada_Tree := Tree.Subtrees (Index);
               begin
                  case Child.Kind is
                     when Opt_ASIS_Elems =>
                        Process_Elem (Q, Child, Is_List_Element => False);
                     when Flat_List_Kinds =>
                        Process_List (Q, Child);
                  end case;
               end;
            end loop;
         end Process_Children;

      --  Start of processing for Process_Elem

      begin
         if Debug_Mode then
            Put_Line (Tree.Kind'Img);

            if Tree.Kind in Def_Names then
               Put_Line
                 ("Defining_Name_Image = " & St (Tree.Def_Name));
            end if;

            if Tree.Kind in Flat_Usage_Name_Kinds then
               Put_Line ("Name_Image = " & St (Tree.Ref_Name));
            end if;
         end if;

         Pre (Q, Is_List_Element, Tree);

         Process_Children;

         --  If it's a leaf and we're in compact mode, then we didn't do
         --  Start_Element in Pre_Procedure, but put out the whole element
         --  in short form, so in that case we don't want to End_Element here.

         if Tree.Subtree_Count = 0 and then Gnat2xml_Options.Compact_XML then
            null;

         else
            End_Element (State.XML_File.all);
         end if;

         if not Is_List_Element then
            End_Element (State.XML_File.all);
         end if;
      end Process_Elem;

      -------------------------------------------------------------------------

      procedure Process_List
        (Q    : Structural_Queries;
         List : Ada_Tree)
      is
      begin
         Start_Element
           (State.XML_File.all,
            T (Structural_Queries'Image (Q) & "_ql",
               State.Xml_Style)); --  ???,

         for Subtree of List.Subtrees loop
            Process_Elem (Q, Subtree, Is_List_Element => True);
         end loop;

         End_Element (State.XML_File.all);
      end Process_List;

      Name : constant String := To_String (Unit_Full_Name (The_Unit));
      Src_File_Name : constant String := To_String (Text_Name (The_Unit));
      Src_File_Simple_Name  : constant String :=
        Ada.Directories.Simple_Name (Src_File_Name);
      Unit_Span : constant Asis.Text.Span :=
        Asis.Text.Compilation_Unit_Span
          (Asis.Elements.Unit_Declaration (The_Unit));

      Src_File : aliased Text_IO.File_Type;
      --  Original source file, used to intersperse source text in the compact
      --  XML form.

      Cur_Line : Natural := 0;
      --  Current source file line that has been printed

      procedure Put_Src_Lines (Up_To : Asis.Text.Line_Number) is
      begin
         while not End_Of_File (Src_File) and then Cur_Line < Up_To loop
            Put_Line (State.XML_File.all, Get_Line (Src_File));
            Cur_Line := Cur_Line + 1;
         end loop;
      end Put_Src_Lines;

      Root : Ada_Tree;

      Src_Buf : Buffer;
      --  Buffer containing the text of the original source file

      Src_Tokens : Pp.Scanner.Token_Vector;
      Src_Gen_Regions : aliased Pp.Scanner.Token_Vector;
      Gen_Regions : Pp.Scanner.Token_Vector_Ptr := null;
      --  Set to point to Src_Gen_Regions if necessary.

      Ignored_BOM_Seen : Boolean;

   --  Start of processing for Process_Unit

   begin
      if Gnat2xml_Options.Compact_XML then
         if ASIS_UL.Options.Verbose_Mode then
            Put_Line ("Opening source " & Src_File_Name);
         end if;
         Open (Src_File, In_File, Src_File_Name);
      end if;

      if Skip_Gen then
         Read_Ada_File (Src_Buf, Src_File_Name,
                        Opt.Wide_Character_Encoding_Method, Ignored_BOM_Seen,
                        Expand_Tabs => True);

         Pp.Scanner.Get_Tokens
           (Src_Buf, Src_Tokens, Pp_Off_On_Delimiters,
            Gen_Regions => Src_Gen_Regions'Unchecked_Access);
         Gen_Regions := Src_Gen_Regions'Unchecked_Access;
      end if;

      Root := Asis_To_Tree.Compilation_Unit_To_Tree (The_Unit, Gen_Regions);

      Start_Element
        (State.XML_File.all,
         T ("compilation_unit", State.Xml_Style),
         ("unit_kind" = Capitalize (Root.Unit_Kind'Img),
          "unit_class" = Capitalize (Root.Unit_Class'Img),
          "unit_origin" = Capitalize (Root.Unit_Origin'Img),
          "unit_full_name" = St (Root.Unit_Full_Name),
          "def_name" = Name,
          "source_file" = Src_File_Simple_Name) &
         Span_To_Compact_Attributes (Unit_Span));
      if not Gnat2xml_Options.Compact_XML then
         Output_Tag
           (State.XML_File.all,
            T ("sloc", State.Xml_Style),
            Span_To_Verbose_Attributes (Unit_Span));
      end if;

      declare
         Cont_Clause_Elements : constant Ada_Tree := Root.Subtrees (1);
         Unit_Element : constant Ada_Tree := Root.Subtrees (2);
         Pragmas : constant Ada_Tree := Root.Subtrees (3);

      begin
         Process_List
           (Q    => Context_Clause_Elements,
            List => Cont_Clause_Elements);

         Process_Elem
           (Q               => Unit_Declaration,
            Tree            => Unit_Element,
            Is_List_Element => False);

         Process_List (Q => Pragmas_After, List => Pragmas);
      end;

      End_Element (State.XML_File.all);

      if Gnat2xml_Options.Compact_XML then
         --  Force out the rest of the lines:
         Put_Src_Lines (Up_To => Natural'Last);
         Close (Src_File);
      end if;

      --  Termination, depending on the application
--???      Terminate_Node (The_Control, The_Node_Information);
   end Process_Unit;

   ----------------------------------------------------------------------------

   procedure Start_Representation (State : Info_Node) is
   begin
      if State.Krunch then
         Current_Format := McKae.XML.EZ_Out.Continuous_Stream;
      end if;

      --  XML container for code representation
      Output_XML_Header (State.XML_File.all);
   end Start_Representation;

end Gnat2xml.Xml;
