------------------------------------------------------------------------------
--                                                                          --
--                            GNAT2XML COMPONENTS                           --
--                                                                          --
--           G N A T 2 X M L . A D A _ T R E E S . S E L F _ R E P          --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                     Copyright (C) 2012-2016, AdaCore                     --
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

with Text_IO;

with ASIS_UL.Formatted_Output; use ASIS_UL.Formatted_Output;

package body Ada_Trees.Self_Rep is

   procedure Put_Head (Proc_Name : String; Tree : Ada_Tree);

   procedure Put_Ada_Tree (Tree : Ada_Tree) is
      pragma Assert (Ada_Tree_Rec_OK (Tree.all));

      Kind      : constant Ada_Tree_Kind := Tree.Kind;
      Has_Attrs : constant Boolean       :=
        Kind in
          A_Compilation_Unit |
          Def_Names |
          Flat_Expression_Kinds |
          Flat_Pragma_Kinds |
          A_Parameter_Specification |
          A_Formal_Object_Declaration |
          A_Comment;

      First_Time : Boolean := True;

      procedure Put_Name (At_Name : String; Name : Name_Id);
      procedure Put_Attr (At_Name, At_Val : String);

      procedure Put_Attr (At_Name, At_Val : String) is
      begin
         if First_Time then
            First_Time := False;

         else
            Put (",\n");
         end if;

         Put ("\1 => \2", At_Name, At_Val);
      end Put_Attr;

      procedure Put_Name (At_Name : String; Name : Name_Id) is
      begin
         Put_Attr
           (At_Name,
            "+""" & Escape_String_Literal (Get_Name_String (Name)) & """");
      end Put_Name;

   begin
      case Kind is
         when Not_An_Element =>
            Put ("Nil");
            pragma Assert (Tree.Subtree_Count = 0);
            pragma Assert (Asis.Text.Is_Nil (Tree.Sloc));

            return;

         when others =>
            null;
      end case;

      Put ("\1", Constructor_Name (Kind));

      if Tree.Subtree_Count = 0
        and then Asis.Text.Is_Nil (Tree.Sloc)
        and then not Has_Attrs
      then
         return;
      end if;

      Put ("\n\i(");

      case Kind is
         when A_Compilation_Unit | Def_Names =>
            Put_Name ("Def_Name", Tree.Def_Name);

            case Kind is
               when A_Compilation_Unit =>
                  Put_Attr ("Unit_Kind", Capitalize (Tree.Unit_Kind'Img));
                  Put_Attr ("Unit_Class", Capitalize (Tree.Unit_Class'Img));
                  Put_Attr ("Unit_Origin", Capitalize (Tree.Unit_Origin'Img));
                  Put_Name ("Unit_Full_Name", Tree.Unit_Full_Name);
                  Put_Name ("Source_File", Tree.Source_File);

               when Def_Names =>
                  Put_Name ("Def", Tree.Def);
                  Put_Name ("Decl_Type", Tree.Decl_Type);

               when others =>
                  null;
            end case;

         when Flat_Expression_Kinds =>
            Put_Name ("Expr_Type", Tree.Expr_Type);

            case Kind is
               when Usage_Names =>
                  Put_Name ("Ref_Name", Tree.Ref_Name);
                  Put_Name ("Ref", Tree.Ref);

               when An_Integer_Literal | A_Real_Literal | A_String_Literal =>
                  Put_Name ("Lit_Val", Tree.Lit_Val);

               when others =>
                  null;
            end case;

         when Flat_Pragma_Kinds =>
            Put_Name ("Pragma_Name", Tree.Pragma_Name);

         when A_Parameter_Specification | A_Formal_Object_Declaration =>
            Put_Attr ("Mode", Capitalize (Tree.Mode'Img));

         when A_Comment =>
            Put_Name ("Text", Tree.Text);

         when others =>
            null;
      end case;

      case Kind is
         when Opt_ASIS_Elems =>
            declare
               Qs : constant Query_List_Ptr := Appropriate_Queries (Kind);

            begin
               for Index in Tree.Subtrees'Range loop
                  if First_Time then
                     First_Time := False;

                  else
                     Put (",\n");
                  end if;

                  Put ("\1 =>\n", Q_Name (Qs (Index)));
                  Indent;
                  Put_Ada_Tree (Tree.Subtrees (Index));
                  Outdent;
               end loop;
            end;

         when Flat_List_Kinds =>
            Put ("(");

            for Index in Tree.Subtrees'Range loop
               if First_Time then
                  First_Time := False;

               else
                  Put (",\n");
               end if;

               Put ("\1 =>\n", Image (Index));
               Indent;
               Put_Ada_Tree (Tree.Subtrees (Index));
               Outdent;
            end loop;

            Put (")");

      end case;

      if not Asis.Text.Is_Nil (Tree.Sloc) then
         if First_Time then
            First_Time := False;

         else
            Put (",\n");
         end if;

         Put ("Sloc => ");
         Put
           ("(\1, \2, \3, \4)",
            Image (Tree.Sloc.First_Line),
            Image (Tree.Sloc.First_Column),
            Image (Tree.Sloc.Last_Line),
            Image (Tree.Sloc.Last_Column));
      end if;

      Put (")\o");
   end Put_Ada_Tree;

   pragma Style_Checks ("M200"); -- Allow long lines

   procedure Put_Head (Proc_Name : String; Tree : Ada_Tree) is
   begin
      Put ("pragma Ada_2012;\n\n");
      Put ("with Namet; use Namet;\n");
      Put ("with GNAT.OS_Lib; use GNAT.OS_Lib;\n");
      Put ("with Asis.Extensions.Flat_Kinds; ");
      Put ("use Asis.Extensions.Flat_Kinds;\n");
      Put ("with ASIS_UL.Formatted_Output; use ASIS_UL.Formatted_Output;\n");
      Put ("with ASIS_UL.String_Utilities; use ASIS_UL.String_Utilities;\n\n");
      Put ("with Ada_Trees; use Ada_Trees;\n");
      Put ("with Ada_Trees.Self_Rep; ");
      Put ("use Ada_Trees.Self_Rep;\n");
      Put ("with Ada_Trees.Factory; ");
      Put ("use Ada_Trees.Factory;\n\n");
      Put ("with Ada_Trees.Formatting; use Ada_Trees.Formatting;\n");
      Put ("with Pp.Formatting; use Pp.Formatting;\n");
      Put ("procedure \1 is\n", Proc_Name);
      Indent;
      Put
        ("\nuse all type Unit_Kinds, Unit_Classes, Unit_Origins, " &
         "Asis.Mode_Kinds;\n");
      Put
        ("\nfunction ""+"" (S : String) return Name_Id renames Name_Find;\n");
      Put ("\nThe_Tree : constant Ada_Tree :=\n");
      Indent;
      Put_Ada_Tree (Tree);
      Put (";\n\n");
      Outdent;
      Outdent;
   end Put_Head;

   procedure Put_Self_Rep (Tree : Ada_Tree) is
   begin
      Put_Head ("Self_Replicate", Tree);
      Put ("begin\n");
      Indent;
      Put ("Put_Self_Rep (The_Tree);\n");
      Outdent;
      Put ("end Self_Replicate;\n");
   end Put_Self_Rep;

   procedure Put_Regen_Ada (Tree : Ada_Tree) is
      Source_File_Name : constant String :=
        "stage/2/" & Get_Name_String (Tree.Source_File);

   begin
      Put_Head ("Regenerate_Ada", Tree);
      Indent;
      Put
        ("Source_File_Name : constant String :=\n\i""\1"";\o\n",
         Source_File_Name);
      Put ("Options : constant Formatting_Options :=\n");
      Put ("\i(Reformat_Comment_Block => True, others => <>);\o\n");
      Outdent;
      Put ("begin\n");
      Indent;
      if False then -- ???Messes up diff's
         Put ("Put (""--  Created by xml2gnat\\n\\n"");\n");
         --  It's not, but we want the 'diff' to work
      end if;
      Put ("Tree_To_Ada\n");
      Put ("\i(The_Tree, Source_File_Name, Options, Output_Name => """");\o\n");
      Outdent;
      Put ("end Regenerate_Ada;\n\n");
   end Put_Regen_Ada;

   procedure Stdo is
      use Text_IO;

   begin
      Set_Output (Standard_Output);
   end Stdo;

end Ada_Trees.Self_Rep;
