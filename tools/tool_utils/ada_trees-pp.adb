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

with System.WCh_Con;
with Opt;

with Ada_Trees;                use Ada_Trees;
with Ada_Trees.Self_Rep;
with Ada_Trees.Asis_To_Tree;   use Ada_Trees.Asis_To_Tree;
with Ada_Trees.Formatting; use Ada_Trees.Formatting;
with Pp.Buffers; use Pp.Buffers;
with Pp.Scanner;
with Pp.Formatting.Dictionaries;
with ASIS_UL.Formatted_Output; use ASIS_UL.Formatted_Output;
with Asis.Clauses;
with Asis.Compilation_Units;
with Asis.Elements;
with Asis.Set_Get;
with A4G.A_Types;

package body Ada_Trees.PP is
   use Asis;

   Pp_Off_On_Delimiters : constant Scanner.Pp_Off_On_Delimiters_Rec
     := (others => <>);

   Skip_Gen : Boolean renames ASIS_UL.Debug.Debug_Flag_3;

   procedure Walk_Direct_Dependencies
     (CU : Asis.Compilation_Unit;
      Action : not null access procedure (CU : Asis.Compilation_Unit));
   --  Calls Action for each compilation unit that CU directly depends upon. We
   --  use the RM definition of "semantic dependence", except that because of
   --  the way subunits work in GNAT, we also have a parent body depending on
   --  its subunits.

   procedure Walk_Direct_Dependencies
     (CU : Asis.Compilation_Unit;
      Action : not null access procedure (CU : Asis.Compilation_Unit)) is
      use Compilation_Units, Asis.Elements, Clauses, Extensions;
   begin
      case Unit_Class (CU) is
         when A_Public_Body | A_Private_Body =>
            Action (Corresponding_Declaration (CU));
            --  Body depends on spec
         when A_Separate_Body =>
            Action (Corresponding_Subunit_Parent_Body (CU));
            --  Subunit depends on parent body
         when A_Public_Declaration | A_Private_Declaration =>
            null;
         when A_Public_Declaration_And_Body =>
            null; -- no spec
         when Not_A_Class =>
            pragma Assert (False);
      end case;

      case Unit_Class (CU) is
         when A_Separate_Body =>
            null;
         when Not_A_Class =>
            pragma Assert (False);
         when others =>
            if not Set_Get.Is_Standard
              (Corresponding_Parent_Declaration (CU))
            then
               Action (Corresponding_Parent_Declaration (CU));
            end if;
            --  A library unit depends on its parent spec
      end case;

      case Unit_Class (CU) is
         when A_Public_Body | A_Private_Body | A_Separate_Body =>
            --  We probably don't actually need the subunits, so disable this.
            declare
               Subunit_List : constant Asis.Compilation_Unit_List :=
                 Subunits (CU);
            begin
               for J in Subunit_List'Range loop
                  if Set_Get.Kind (Subunit_List (J))
                    /= A_Nonexistent_Body
                  then
                     if False then
                        Action (Subunit_List (J));
                     end if;
                  end if;
               end loop;
            end;
         when others =>
            null;
      end case;

      --  A unit depends on units it with's

      for Clause of Context_Clause_Elements (CU) loop
         if Ekind (Clause) = A_With_Clause then
            for Name of Clause_Names (Clause) loop
               --  Hide errors in Corresponding_Name_Definition by calling
               --  Is_Uniquely_Defined.

               if Is_Uniquely_Defined (Name) then
                  Action (Enclosing_Compilation_Unit (Denotation (Name)));
               end if;
            end loop;
         end if;
      end loop;
   end Walk_Direct_Dependencies;

   use A4G.A_Types;

   --  We keep a cache of Ada_Trees. Valid elements of the cache are in the
   --  slice Cache (First_Unit_Id .. Cache_Last). Cache (Id) = null means the
   --  tree for that unit has not been generated; Pending means it is being
   --  generated, and anything else means it has been generated and that's the
   --  tree.

   The_Pending : aliased Ada_Tree_Rec := Nil.all;
   Pending : constant Ada_Tree := The_Pending'Access;
   Cache : array (Unit_Id range First_Unit_Id .. Unit_High_Bound)
     of Ada_Tree_Base;
   Cache_Last : Unit_Id := 0;

   procedure Flush_Cache;
   --  Free all the trees in the cache, and reset to Cache (1..0).

   procedure Flush_Cache is
   begin
      for Id in Cache'First .. Cache_Last loop
         if Cache (Id) /= null then
            pragma Assert (Cache (Id) /= Pending);
            Free_Tree (Cache (Id));
         end if;
      end loop;
      Cache_Last := 0;
   end Flush_Cache;

   procedure Maybe_To_Ada
     (CU : Asis.Compilation_Unit;
      Source_Name : String;
      Options : Formatting_Options;
      Output_Name : String;
      Form_String : String;
      Do_Diff : Boolean;
      Output_Written : out Boolean;
      To_Ada : Boolean);
   --  Helper for Asis_To_Ada. To_Ada is True for the first call, indicating
   --  we're going to generate Ada text; it is False for subsequent (recursive)
   --  calls, which merely generate trees for dependencies.

   procedure Maybe_To_Ada
     (CU      : Asis.Compilation_Unit;
      Source_Name : String;
      Options : Formatting_Options;
      Output_Name : String;
      Form_String : String;
      Do_Diff : Boolean;
      Output_Written : out Boolean;
      To_Ada : Boolean)
   is
      Src_Buf : Buffer;
      --  Buffer containing the text of the original source file

      Src_Tokens : Scanner.Token_Vector;
      Src_Gen_Regions : aliased Scanner.Token_Vector;
      Gen_Regions : Scanner.Token_Vector_Ptr := null;
      --  Set to point to Src_Gen_Regions if necessary.

      Write_BOM : Boolean;
      --  True if BOM should be written to the output

      procedure Scan_Dictionaries;

      procedure Walk_Dependencies (CU : Asis.Compilation_Unit);
      --  Recursively walk compilation units this one depends on.

      procedure Walk_Dependencies (CU : Asis.Compilation_Unit) is
         Ignore : Boolean;
      begin
         Maybe_To_Ada (CU, "no Source_Name", Options, "no Output_Name",
                       "no Form_String", False, Ignore, To_Ada => False);
      end Walk_Dependencies;

      procedure Scan_Dictionaries is
         use Standard.Pp.Formatting.Dictionaries;
      begin
         pragma Assert
           (String_Vectors.Is_Empty (Options.Dictionary_File_Names) =
            not Options.Use_Dictionary);
         Reset_Dictionary;
         for D_Name of Options.Dictionary_File_Names loop
            Scan_Dictionary (D_Name);
         end loop;
      end Scan_Dictionaries;

      Do_Dependencies : constant Boolean :=
        Options.PP_Type_Casing /= Options.PP_Name_Casing;
      --  Following all the dependencies is fairly expensive, so we only do it
      --  if necessary. It is necessary in order to get the casing right for
      --  the name of a task body, which should be PP_Type_Casing if it's the
      --  body of a task type, and PP_Name_Casing if it's the body of a
      --  singleton task. Same issue for protected bodies. See Do_Def_Name in
      --  ada_trees-formatting-tree_to_ada.adb.

      Id : constant Unit_Id := Set_Get.Get_Unit_Id (CU);
      use type System.WCh_Con.WC_Encoding_Method;
   begin
      Output_Written := False;
      while Cache_Last < Id loop
         Cache_Last := Cache_Last + 1;
         Cache (Cache_Last) := null;
      end loop;
      pragma Assert (Cache (Id) /= Pending);
      if Cache (Id) /= null then
         pragma Assert (not To_Ada);
         return;
      end if;

      Cache (Id) := Pending;

      if To_Ada or Skip_Gen then
         Read_Ada_File (Src_Buf, Set_Get.Source_File (CU),
                        Opt.Wide_Character_Encoding_Method, Write_BOM,
                        Expand_Tabs => True);
         --  Expand tabs unconditionally. This differs from the behavior of
         --  the old gnatpp, which has an option for that (but only for
         --  comments).
         pragma Assert
           (if Write_BOM then
              Opt.Wide_Character_Encoding_Method = System.WCh_Con.WCEM_UTF8);

         if Skip_Gen then
            Scanner.Get_Tokens
              (Src_Buf, Src_Tokens, Pp_Off_On_Delimiters,
               Gen_Regions => Src_Gen_Regions'Unchecked_Access);
            Gen_Regions := Src_Gen_Regions'Unchecked_Access;
         end if;
      end if;

      declare
         Tree : constant Ada_Tree :=
           Compilation_Unit_To_Tree (CU, Gen_Regions);
      begin
         Cache (Id) := Tree;
         Resolve_Symbols (Tree);

         if Ada_Trees.Debug_Mode or else ASIS_UL.Debug.Debug_Flag_2 then
            Ada_Trees.Self_Rep.Put_Ada_Tree (Tree);
            Put ("\n");
         end if;

         if Do_Dependencies then
            Walk_Direct_Dependencies (CU, Walk_Dependencies'Access);
         end if;

         if To_Ada then
            Scan_Dictionaries; -- ???Should do this earlier, and just once.
            Tree_To_Ada
              (Tree, Src_Buf, Write_BOM, Source_Name, Options, Output_Name,
               Form_String, Do_Diff, Output_Written);
         end if;
      end;
   end Maybe_To_Ada;

   procedure Asis_To_Ada
     (CU          : Asis.Compilation_Unit;
      Source_Name : String;
      Options     : Formatting_Options;
      Output_Name : String;
      Form_String : String;
      Do_Diff : Boolean;
      Output_Written : out Boolean)
   is
   begin
      pragma Assert (Is_Empty (Symtab));
      Maybe_To_Ada (CU, Source_Name, Options, Output_Name, Form_String,
                    Do_Diff, Output_Written, To_Ada => True);
      --  We have to flush the cache here, because Unit_Id's get reused between
      --  runs of this.
      Flush_Cache;
      Clear (Symtab);
   end Asis_To_Ada;

end Ada_Trees.PP;
