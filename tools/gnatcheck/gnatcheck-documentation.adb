------------------------------------------------------------------------------
--                                                                          --
--                          GNATCHECK COMPONENTS                            --
--                                                                          --
--              G N A T C H E C K . D O C U M E N T A T I O N               --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                     Copyright (C) 2009-2016, AdaCore                     --
--                                                                          --
-- GNATCHECK  is  free  software;  you can redistribute it and/or modify it --
-- under terms of the  GNU  General Public License as published by the Free --
-- Software Foundation;  either version 2, or ( at your option)  any  later --
-- version.  GNATCHECK  is  distributed in the hope that it will be useful, --
-- but  WITHOUT  ANY  WARRANTY;   without  even  the  implied  warranty  of --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General --
-- Public License for more details.  You should have received a copy of the --
-- GNU  General Public License distributed with GNAT; see file  COPYING. If --
-- not,  write to the  Free Software Foundation,  51 Franklin Street, Fifth --
-- Floor, Boston, MA 02110-1301, USA.                                       --
--                                                                          --
-- GNATCHECK is maintained by AdaCore (http://www.adacore.com).             --
--                                                                          --
------------------------------------------------------------------------------

pragma Ada_2012;

with Ada.Characters.Handling;    use Ada.Characters.Handling;
with Ada.Containers.Vectors;
with Ada.Text_IO;                use Ada.Text_IO;

with GNAT.Directory_Operations;  use GNAT.Directory_Operations;
with GNAT.OS_Lib;                use GNAT.OS_Lib;

with Gnatcheck.Categories;       use Gnatcheck.Categories;
with Gnatcheck.Ids;              use Gnatcheck.Ids;
with Gnatcheck.Options;          use Gnatcheck.Options;
with Gnatcheck.Rules;            use Gnatcheck.Rules;
with Gnatcheck.Rules.Rule_Table; use Gnatcheck.Rules.Rule_Table;

with ASIS_UL.Misc;               use ASIS_UL.Misc;
with ASIS_UL.Output;             use ASIS_UL.Output;

package body Gnatcheck.Documentation is

   Current_Dir : constant String_Access := new String'(Get_Current_Dir);

   Doc_File_Name : String_Access;
   Doc_File      : File_Type;

   -----------------------
   -- Local subprograms --
   -----------------------

   procedure Add_To_Doc (S : String);
   --  Adds S to the generated documentation file and appens a
   --  (platform-specific) line terminator.

   procedure New_Line_In_Doc;
   --  The same as Add_To_Doc ("");

   procedure Add_To_Doc_No_EOL (S : String);
   --  Adds S to the generated documentation file (with no line terminator
   --  appended).

   procedure Generate_Header;
   --  Create the heading part of the document.

   procedure Generate_Main_Menu;
   --  Generates the main menu of the document. This may differ from a regular
   --  menu generation for a category.

   procedure Generate_Category_Node (C : Category_Id);
   procedure Generate_Rule_Node (R : Rule_Id);
   --  Generates the texinfo node corresponding to the category or rule

   procedure Generate_Category_Menu (C : Category_Id);
   --  Generates the menu for category

   procedure Generate_Category_Section (C : Category_Id);
   --  Generates the documentation section for a category. Recursively calls
   --  itself for nested categories.

   procedure Generate_Rule_Sections (R : Rule_Id);
   --  Generate the documentation sections corresponding to the rules combined
   --  in a category. R should be the first fully implemented rule in the
   --  category. If No (R), does nothing

   procedure Generate_Rule_Doc_Section (R : Rule_Id);
   --  Tries to locate the documentation file for the rule and copies its
   --  content into the documentation file.

   procedure Finalize_Documentation;
   --  Generates the final part of the documentation file.

   function Get_Rule_Doc_File_Name (R : Rule_Id) return String;
   --  Gets the name that the file with the rule documentation should have.
   --  Assumes that Present (R)

   procedure Copy_To_Doc_File (Rule_Doc_File : String);
   --  Assumes that Rule_Doc_File is the name of some existing text file.
   --  Copies the needed parts of this file (depending on the documentation
   --  generation options specified) in the documentation file.

   -----------------------------------------------------
   -- Data structures and routines for copying rule   --
   -- documentation items into the documentation file --
   -----------------------------------------------------

   Max_Line_Len : constant Positive := 80;
   --  This constant is 1 more than maxial allowed line length in the rule
   --  documentation file.

   Line_Buf     :          String (1 .. Max_Line_Len);
   Line_Len     :          Natural range 0 .. Max_Line_Len;
   --  Buffer to place the next line from the rule documentation file. Line_Len
   --  is set to the next of this line. If Line_Len = Max_Line_Len , this means
   --  that the corresponding line in the rule documentation file is too long,
   --  in this  case the line is cut off to (Max_Line_Len - 1) character and
   --  the coresponding warning is generated.

   type Line_Item is record
      Len  : Natural range 0 .. Max_Line_Len;
      Line : String (1 .. Max_Line_Len);
   end record;

   package Doc_Buffers is new Ada.Containers.Vectors
     (Index_Type   => Positive,
      Element_Type => Line_Item);

   Doc_Buf : Doc_Buffers.Vector;
   --  Here we read a rule documentation item, and then we copy its content
   --  into the doc file

   Rule_Doc_File : File_Type;

   Next_Line : Natural;
   --  Line counter for rule documentation file (when reading the file into
   --  Doc_Fuf)

   procedure Init_Rule_Doc_Copyig;
   --  Resets and reinitializes all the data structures for rule documentation
   --  copying.

   procedure Read_Rule_Doc_In_Bufer (Rule_Doc_File_Name : String);
   --  Reads the content (or a part thereof) of Rule_Doc_File_Name (that is
   --  supposed to be the name of some existing file) into Doc_Buf. The part of
   --  the Rule_Doc_File that is read in depends on the settings of
   --  Add_Requirements and Add_Questions flags.
   --
   --  The only check that is made when reading the content of Rule_Doc_File is
   --  for the line lenght - is is checked that each line contains at most
   --  (Max_Line_Len - 1) characters, if it is not the case, the warning is
   --  generated and the part of the line starting from character number
   --  Max_Line_Len is cut off.
   --
   --  This procedure assumes that there is at most one 'Requirement' and
   --  'Questions' section in the rule documentation file and that if both
   --  sections present, 'Requirements' goes first.

   type Doc_Read_Statuses is (In_Doc, In_Quest, In_Req);
   Doc_Read_Status : Doc_Read_Statuses;
   --  Indicates where we are when reading the rule documentation item

   procedure Copy_From_Bufer_To_Doc;
   --  Appends the content of Doc_Buf to the documentation file.

   ----------------
   -- Add_To_Doc --
   ----------------

   procedure Add_To_Doc (S : String) is
   begin
      Put_Line (File => Doc_File,
                Item => S);
   end Add_To_Doc;

   -----------------------
   -- Add_To_Doc_No_EOL --
   -----------------------

   procedure Add_To_Doc_No_EOL (S : String) is
   begin
      Put (File => Doc_File,
           Item => S);
   end Add_To_Doc_No_EOL;

   --------------
   -- Clean_Up --
   --------------

   procedure Clean_Up is
   begin
      if Is_Open (Doc_File) then
         Close (Doc_File);
      end if;

      Change_Dir (Current_Dir.all);
   end Clean_Up;

   ---------------------------
   -- Compose_Documentation --
   ---------------------------

   procedure Compose_Documentation is
   begin
      Generate_Header;
      Generate_Main_Menu;
      Generate_Category_Section (Root_Category_Id);
      Finalize_Documentation;
   end Compose_Documentation;

   ----------------------------
   -- Copy_From_Bufer_To_Doc --
   ----------------------------

   procedure Copy_From_Bufer_To_Doc is
   begin

      for J in 1 .. Doc_Buffers.Length (Doc_Buf) loop

         if Doc_Buffers.Element (Doc_Buf, Positive (J)).Len = 0 then
            New_Line_In_Doc;
         else
            Add_To_Doc
              (Doc_Buffers.Element (Doc_Buf, Positive (J)).Line
                (1 .. Doc_Buffers.Element (Doc_Buf, Positive (J)).Len));
         end if;

      end loop;

   end Copy_From_Bufer_To_Doc;

   ----------------------
   -- Copy_To_Doc_File --
   ----------------------

   procedure Copy_To_Doc_File (Rule_Doc_File : String) is
   begin
      Init_Rule_Doc_Copyig;
      Read_Rule_Doc_In_Bufer (Rule_Doc_File);
      Copy_From_Bufer_To_Doc;

--      New_Line_In_Doc;
--      Add_To_Doc ("Rule documentaion should go here (not implemented yet)");
   end Copy_To_Doc_File;

   ----------------------------
   -- Finalize_Documentation --
   ----------------------------

   procedure Finalize_Documentation is
   begin
      New_Line_In_Doc;
      Add_To_Doc ("@node Index");
      Add_To_Doc ("@unnumbered Index");
      Add_To_Doc ("@printindex cp");
      New_Line_In_Doc;
      Add_To_Doc ("@bye");
   end Finalize_Documentation;

   ----------------------------
   -- Generate_Category_Menu --
   ----------------------------

   procedure Generate_Category_Menu (C : Category_Id) is
      Sub_Category : Category_Id;
      R            : Rule_Id;
   begin

      if C /= Root_Category_Id
        and then
           not (No (First_Child (C))
             and then
                No (First_Rule_In_Category (C, Rule_Doc_Status)))
      then
         New_Line_In_Doc;
         Add_To_Doc ("@menu");
         Sub_Category := First_Child (C);

         while Present (Sub_Category) loop
            Add_To_Doc ("* " & Category_Name (Sub_Category) & "::");
            Sub_Category := Next (Sub_Category);
         end loop;

         R := First_Rule_In_Category (C, Rule_Doc_Status);

         while Present (R) loop
            Add_To_Doc ("* " & Rule_Name (R) & "::");
            R := Get_Next_Rule (R, Rule_Doc_Status);
         end loop;

         Add_To_Doc ("@end menu");
      end if;

   end Generate_Category_Menu;

   ----------------------------
   -- Generate_Category_Node --
   ----------------------------

   procedure Generate_Category_Node (C : Category_Id) is
   begin
      pragma Assert (Present (C));

      if C /= Root_Category_Id then
         New_Line_In_Doc;
         Add_To_Doc ("@node " & Category_Name (C));

         if Parent_Category (C) = Root_Category_Id then
            Add_To_Doc ("@chapter " & Category_Name (C));
         else
            Add_To_Doc_No_EOL ("@");

            for J in 1 .. Nesting_Level (C) - 2 loop
               Add_To_Doc_No_EOL ("sub");
            end loop;

            Add_To_Doc ("section " & Category_Name (C));
         end if;

         Add_To_Doc ("@cindex " & Category_Name (C));

         if No (First_Child (C))
           and then
            No (First_Rule_In_Category (C, Rule_Doc_Status))
         then
            New_Line_In_Doc;
            Add_To_Doc ("@noindent");
            Add_To_Doc ("This rule category is empty at the moment");
         end if;

      end if;

   end Generate_Category_Node;

   -------------------------------
   -- Generate_Category_Section --
   -------------------------------

   procedure Generate_Category_Section (C : Category_Id) is
      Subcategory : Category_Id;
      R           : Rule_Id;
   begin
      Generate_Category_Node (C);
      Generate_Category_Menu (C);

      Subcategory := First_Child (C);

      while Present (Subcategory) loop
         Generate_Category_Section (Subcategory);
         Subcategory := Next (Subcategory);
      end loop;

      R := First_Rule_In_Category (C, Rule_Doc_Status);

      if C = Root_Category_Id and then Present (R) then
         Add_To_Doc ("@node General-purpose rules");
         Add_To_Doc ("@chapter General-purpose rules");
         Add_To_Doc ("@cindex General-purpose rules");

         New_Line_In_Doc;

         Add_To_Doc ("@menu");

            while Present (R) loop
               Add_To_Doc ("* " & Rule_Name (R) & "::");
               R := Get_Next_Rule (R, Rule_Doc_Status);
            end loop;

         Add_To_Doc ("@end menu");

         R := First_Rule_In_Category (C, Rule_Doc_Status);
      end if;

      Generate_Rule_Sections (R);

   end Generate_Category_Section;

   ---------------------
   -- Generate_Header --
   ---------------------

   procedure Generate_Header is
   begin
      Add_To_Doc ("\input texinfo   @c -*-texinfo-*-");
      Add_To_Doc ("@input texiplus");
      New_Line_In_Doc;

      Add_To_Doc ("@setfilename gnatcheck_rules.texi");
      Add_To_Doc ("@setchapternewpage odd");
      Add_To_Doc ("@syncodeindex fn cp");
      New_Line_In_Doc;

      Add_To_Doc ("@ifinfo");
      Add_To_Doc ("@node Top");
      Add_To_Doc ("@top GNATcheck rules");
      New_Line_In_Doc;

   end Generate_Header;

   ------------------------
   -- Generate_Main_Menu --
   ------------------------

   procedure Generate_Main_Menu is
      Next_Category : Category_Id;
   begin
      Add_To_Doc ("@menu");

      --  First-level subcategories:

      Next_Category := First_Child (Root_Category_Id);

      while Present (Next_Category) loop

         Add_To_Doc ("* " & Category_Name (Next_Category) & "::");

         Next_Category := Next (Next_Category);
      end loop;

      if Present
           (First_Rule_In_Category (Root_Category_Id, Rule_Doc_Status))
      then
         Add_To_Doc ("* General-purpose rules::");
      end if;

      Add_To_Doc ("* Index::");
      Add_To_Doc ("@end menu");
      Add_To_Doc ("@end ifinfo");
   end Generate_Main_Menu;

   -------------------------------
   -- Generate_Rule_Doc_Section --
   -------------------------------

   procedure Generate_Rule_Doc_Section (R : Rule_Id) is
      Rule_Doc_File_Name : constant String := Get_Rule_Doc_File_Name (R);
   begin
      New_Line_In_Doc;
      Add_To_Doc ("@noindent");

      case Rule_Status (R) is
         when Not_A_Rule_Status =>
            pragma Assert (False);
            null;
         when Under_Construction =>
            Add_To_Doc ("The rule is under construction for the moment");
            New_Line_In_Doc;
         when Non_Documented =>
            Add_To_Doc ("The rule is not fully documented for the moment");
            New_Line_In_Doc;
         when Fully_Implemented =>
            null;
      end case;

      if Is_Regular_File (Rule_Doc_File_Name) then
         Copy_To_Doc_File (Rule_Doc_File_Name);
      elsif Rule_Status (R) = Fully_Implemented then
         Add_To_Doc ("Rule documentation is missing");
         New_Line_In_Doc;
         Error ("missing documentation for rule " & Rule_Name (R));
      end if;

   end Generate_Rule_Doc_Section;

   ------------------------
   -- Generate_Rule_Node --
   ------------------------

   procedure Generate_Rule_Node (R : Rule_Id) is
   begin
      pragma Assert (Present (R));

      New_Line_In_Doc;
      Add_To_Doc ("@node " & Rule_Name (R));

      Add_To_Doc_No_EOL ("@");

      for J in 1 .. Nesting_Level (Parent_Category (R)) - 1 loop
         Add_To_Doc_No_EOL ("sub");
      end loop;

      Add_To_Doc ("section " & Rule_Name (R));
      Add_To_Doc ("@cindex " & Rule_Name (R));

   end Generate_Rule_Node;

   ----------------------------
   -- Generate_Rule_Sections --
   ----------------------------

   procedure Generate_Rule_Sections (R : Rule_Id) is
      Next_Rule : Rule_Id := R;
   begin
      while Present (Next_Rule) loop
         Generate_Rule_Node (Next_Rule);
         Generate_Rule_Doc_Section (Next_Rule);
         Next_Rule := Get_Next_Rule (Next_Rule, Rule_Doc_Status);
      end loop;
   end Generate_Rule_Sections;

   ----------------------------
   -- Get_Rule_Doc_File_Name --
   ----------------------------

   function Get_Rule_Doc_File_Name (R : Rule_Id) return String is
   begin
      pragma Assert (Present (R));

      return To_Lower (Rule_Name (R) & ".texi");
   end Get_Rule_Doc_File_Name;

   --------------------------
   -- Init_Rule_Doc_Copyig --
   --------------------------

   procedure Init_Rule_Doc_Copyig is
   begin
      Doc_Buffers.Clear (Doc_Buf);
      Line_Len           := 0;
      Next_Line          := 0;
      Doc_Read_Status    := In_Doc;
   end Init_Rule_Doc_Copyig;

   ---------------------
   -- New_Line_In_Doc --
   ---------------------

   procedure New_Line_In_Doc is
   begin
      New_Line (File => Doc_File);
   end New_Line_In_Doc;

   -----------------------------
   -- Process_Rule_Status_Par --
   -----------------------------

   procedure Process_Rule_Status_Par
     (Par     : String;
      Success : out Boolean)
   is
   begin

      if Par'Length /= 1 then
         Success := False;
      else
         Success := True;

         if Par = "1" then
            Rule_Doc_Status := Non_Documented;
         elsif Par = "2" then
            Rule_Doc_Status := Under_Construction;
         else
            Success := False;
         end if;
      end if;

   end Process_Rule_Status_Par;

   ----------------------------
   -- Read_Rule_Doc_In_Bufer --
   ----------------------------

   procedure Read_Rule_Doc_In_Bufer (Rule_Doc_File_Name : String) is
   begin
      Open (File => Rule_Doc_File,
            Mode => In_File,
            Name =>  Rule_Doc_File_Name);

      while not End_Of_File (Rule_Doc_File) loop
         Get_Line (File => Rule_Doc_File,
                   Item => Line_Buf,
                   Last => Line_Len);

         Next_Line := Next_Line + 1;

         if Line_Len = Max_Line_Len then
            Line_Len := Max_Line_Len - 1;
            Error ("Rule_Doc_File_Name:" & Image (Next_Line) &
                   " line too long, cut off");
         end if;

         case Doc_Read_Status is
            when In_Doc =>

               if Line_Len = 14
                 and then
                  Line_Buf (1 .. Line_Len) = "@c #Questions#"
               then
                  if not (Add_Questions or else Add_Requirements) then
                     exit;
                  else
                     Doc_Read_Status := In_Quest;
                  end if;
               elsif Line_Len = 17
                 and then
                  Line_Buf (1 .. Line_Len) = "@c #Requirements#"
               then
                  if not Add_Requirements then
                     exit;
                  else
                     Doc_Read_Status := In_Req;
                  end if;
               end if;

            when In_Quest =>

               if Line_Len = 17
                 and then
                  Line_Buf (1 .. Line_Len) = "@c #Requirements#"
               then

                  if not Add_Requirements then
                     exit;
                  else
                     Doc_Read_Status := In_Req;
                  end if;

               end if;
            when In_Req =>
               null;
         end case;

         Doc_Buffers.Append
           (Container => Doc_Buf,
            New_Item  => (Len  => Line_Len,
                          Line => Line_Buf));
      end loop;

      Close (Rule_Doc_File);
   exception
      when Status_Error =>
         Error ("file " & Rule_Doc_File_Name & "used by other program");
   end Read_Rule_Doc_In_Bufer;

   ------------------
   -- Set_Doc_File --
   ------------------

   procedure Set_Doc_File is
   begin
      --  At the moment we have a hard-coded name of the result documentation
      --  file - gnatcheck_rules.texi. It is supposed to be created in the
      --  docs directory of gnatcheck hierarchy

      Doc_File_Name :=
        new String'(Normalize_Pathname
          (Get_Current_Dir & Directory_Separator & "gnatcheck_rules.texi"));

      if Is_Regular_File (Doc_File_Name.all) then
         Open (File => Doc_File,
               Mode => Out_File,
               Name => Doc_File_Name.all);
      else
         Create (File => Doc_File,
                 Mode => Out_File,
                 Name => Doc_File_Name.all);
      end if;

   end Set_Doc_File;

   ---------------------
   -- Set_Max_Details --
   ---------------------

   procedure Set_Max_Details is
   begin
      Add_Requirements := True;
      Add_Questions    := True;
      Rule_Doc_Status  := Under_Construction;
   end Set_Max_Details;

end Gnatcheck.Documentation;
