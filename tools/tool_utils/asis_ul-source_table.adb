------------------------------------------------------------------------------
--                                                                          --
--                     ASIS UTILITY LIBRARY COMPONENTS                      --
--                                                                          --
--                 A S I S _ U L . S O U R C E _ T A B L E                  --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                     Copyright (C) 2004-2018, AdaCore                     --
--                                                                          --
-- Asis Utility Library (ASIS UL) is free software; you can redistribute it --
-- and/or  modify  it  under  terms  of  the  GNU General Public License as --
-- published by the Free Software Foundation; either version 3, or (at your --
-- option)  any later version.  ASIS UL  is distributed in the hope that it --
-- will  be  useful,  but  WITHOUT  ANY  WARRANTY; without even the implied --
-- warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the --
-- GNU  General Public License for more details. You should have received a --
-- copy of the  GNU General Public License  distributed with GNAT; see file --
-- COPYING3. If not,  go to http://www.gnu.org/licenses for a complete copy --
-- of the license.                                                          --
--                                                                          --
-- ASIS UL is maintained by AdaCore (http://www.adacore.com).               --
--                                                                          --
------------------------------------------------------------------------------

pragma Ada_2012;

with Ada.Characters.Handling;     use  Ada.Characters.Handling;
with Ada.Directories;
with Ada.Strings;                 use Ada.Strings;
with Ada.Strings.Fixed;           use Ada.Strings.Fixed;
with Ada.Text_IO;                 use  Ada.Text_IO;

with GNAT.Directory_Operations;   use GNAT.Directory_Operations;
with GNAT.Table;

with Asis.Compilation_Units;
with Asis.Elements;
with Asis.Extensions;
with Asis.Extensions.Strings;     use Asis.Extensions.Strings;

with GNATCOLL.VFS;                use GNATCOLL.VFS;

with ASIS_UL.Common;              use ASIS_UL.Common;
with ASIS_UL.Compiler_Options;    use ASIS_UL.Compiler_Options;
with ASIS_UL.Debug;               use ASIS_UL.Debug;
with ASIS_UL.Environment;         use ASIS_UL.Environment;
with ASIS_UL.Misc;                use ASIS_UL.Misc;
with ASIS_UL.Options;             use ASIS_UL.Options;
with ASIS_UL.Output;              use ASIS_UL.Output;
with ASIS_UL.Tree_Creation;       use ASIS_UL.Tree_Creation;

package body ASIS_UL.Source_Table is

   More_Then_One_Arg_File_Specified : Boolean := False;
   Arg_File_Name                    : String_Access;

   use Temporary_File_Storages;

   Temporary_File_Storage : Temporary_File_Storages.Set;

   -----------------------
   -- Source File table --
   -----------------------

   type SF_Record is record

      Source_Name  : String_Loc;
      --  If ASIS_UL.Common.Use_Project_File_Obsolete is set OFF, this field
      --  stores the source name with full directory information in absolute
      --  form, otherwise its value is the same as Short_Source_Name field.

      Short_Source_Name : String_Loc;
      --  The source name without directory information

      Suffixless_Name : String_Loc;
      --  The source name without directory information and suffix (if any)
      --  is used to create the names of the tree file and ALI files

      CU_Name : String_Loc;
      --  The (full expanded) Ada name of a compilation unit contained in the
      --  source, is set to Nil_String_Loc if the unit name is unknown at the
      --  moment or if the source file does not contain a legal unit.

      Could_Be_Body : Boolean;
      --  This flag indicates that the source file could be a body. For now,
      --  to decide that it could, we check that the suffix is '.adb'

      Status : SF_Status;
      --  Status of the given source. Initially is set to Waiting, then is
      --  changed according to the results of the metrics computation

      Hash_Link : SF_Id;
      --  Link to next entry in files table for same hash code

      Info : SF_Info;
      --  An integer value associated with each source. The usage is up to a
      --  client.

      Switches : String_List_Access;
      --  Used only if a project file is processed as a tool argument. Contains
      --  the list of options to be passed to the compiler to create the tree.

      Result_Dir : String_Access;
      --  Used only if a project file is processed as a tool argument. Contains
      --  the path to the directory the per-source results should be placed in.
   end record;

   package Source_File_Table is new GNAT.Table (
     Table_Component_Type => SF_Record,
     Table_Index_Type     => SF_Id,
     Table_Low_Bound      => First_SF_Id,
     Table_Initial        => 100,
     Table_Increment      => 100,
     Table_Name           => "Source file table");

   Source_Table : Source_File_Table.Table_Ptr renames Source_File_Table.Table;

   Last_Arg_Source : SF_Id := No_SF_Id;
   --  Used to store the Id of the last argument source

   Next_Source : SF_Id := First_SF_Id;
   --  Used in source file iterator

   Short_Source_Name_String : String_Access;
   Full_Source_Name_String  : String_Access;
   --  Two handlers for a file name (with no path information and with full
   --  absolute path) used for the file before we decide that the file should
   --  be stored into a file table. Also used in File_Find for storing the
   --  short file name to be passed into Hash function.

   New_SF_Record : constant SF_Record :=
     (Source_Name       => Nil_String_Loc,
      Short_Source_Name => Nil_String_Loc,
      Suffixless_Name   => Nil_String_Loc,
      CU_Name           => Nil_String_Loc,
      Status            => Waiting,
      Hash_Link         => No_SF_Id,
      Could_Be_Body     => False,
      Switches          => null,
      Result_Dir        => null,
      Info              => 0);
   --  Used to set the initial attributes for the new source file

   --  Hash function is the same as in Namet, the only difference is the way
   --  it takes the argument to compute the hash value:

   Hash_Num : constant Integer := 2**12;
   --  Number of headers in the hash table. Current hash algorithm is closely
   --  tailored to this choice, so it can only be changed if a corresponding
   --  change is made to the hash algorithm.

   Hash_Max : constant Integer := Hash_Num - 1;
   --  Indexes in the hash header table run from 0 to Hash_Num - 1

   subtype Hash_Index_Type is Integer range 0 .. Hash_Max;
   --  Range of hash index values

   Hash_Table : array (Hash_Index_Type) of SF_Id := (others => No_SF_Id);
   --  The hash table is used to locate existing entries in the files table.
   --  The entries point to the first names table entry whose hash value
   --  matches the hash code. Then subsequent names table entries with the
   --  same hash code value are linked through the Hash_Link fields.

   function Hash (File_Name : String) return Hash_Index_Type;
   --  Compute hash code for the file name. The argument should be a short
   --  file name with no directory information

   function Same_Name_File_Find (Short_SF_Name : String) return SF_Id;
   --  Similar to File_Find, but looks for the file with the same short name.

   procedure Source_Debug_Image (SF : SF_Id);
   --  Prints out the debug image of a single source stored in the source file
   --  table

   procedure Source_Table_Debug;
   --  Prints the source table

   function Non_Case_Sensitive_File_Find
     (SF_Name        : String;
      Use_Short_Name : Boolean := False)
      return           SF_Id;
   --  Used as a part of the implementation of File_Find. Tries to locate the
   --  argument in the source table when all the path/file names are converted
   --  to lower case.

   ----------------------------------------------------------------------
   -- Source file access/update routines not used outside this package --
   ----------------------------------------------------------------------

   procedure Set_Source_Name       (SF : SF_Id; N : String);
   procedure Set_Short_Source_Name (SF : SF_Id; N : String);
   procedure Set_Suffixless_Name   (SF : SF_Id; N : String);

   -----------------------
   -- Add_Needed_Source --
   -----------------------

   function Add_Needed_Source (Fname : String) return SF_Id is
      Old_SF     : SF_Id;
      Hash_Index : Hash_Index_Type;
      First_Idx  : Natural;
      Last_Idx   : Natural;

      Result : SF_Id;
   begin
      pragma Assert (Is_Regular_File (Fname));

      Source_File_Table.Append (New_SF_Record);
      Result := Source_File_Table.Last;

      if Debug_Flag_S then
         Info ("Adding needed source :>" & Fname & "<, ID=" & Result'Img);
      end if;

      Short_Source_Name_String := new String'(Base_Name (Fname));
      Hash_Index := Hash (To_Lower (Short_Source_Name_String.all));

      if Present (Hash_Table (Hash_Index)) then

         Old_SF := Hash_Table (Hash_Index);

         while Present (Source_Table (Old_SF).Hash_Link) loop
            Old_SF := Source_Table (Old_SF).Hash_Link;
         end loop;

         Source_Table (Old_SF).Hash_Link := Result;

      else
         Hash_Table (Hash_Index) := Result;
      end if;

      if Use_Project_File_Obsolete then
         Set_Source_Name (Result, Short_Source_Name_String.all);
      else
         Set_Source_Name
           (Result,
             Normalize_Pathname
              (Fname,
               Resolve_Links  => False,
               Case_Sensitive => True));
      end if;

      Set_Short_Source_Name (Result, Short_Source_Name_String.all);

      First_Idx := Short_Source_Name_String'First;
      Last_Idx  := Short_Source_Name_String'Last;

      for J in reverse  First_Idx + 1 .. Last_Idx loop

         if Short_Source_Name_String (J) = '.' then
            Last_Idx := J - 1;
            exit;
         end if;

      end loop;

      Set_Suffixless_Name
        (Result, Short_Source_Name_String (First_Idx .. Last_Idx));

      if To_Lower (Short_Source_Name_String
           (Last_Idx + 1 .. Short_Source_Name_String'Last)) = ".adb"
      then
         Source_Table (Result).Could_Be_Body := True;
      end if;

      Free (Short_Source_Name_String);

      return Result;

   end Add_Needed_Source;

   ---------------------------
   -- Add_Source_To_Process --
   ---------------------------

   procedure Add_Source_To_Process
     (Fname              : String;
      Arg_Project        : Arg_Project_Type'Class;
      Duplication_Report : Boolean := True;
      Status             : SF_Status := Waiting)
   is
      Old_SF : SF_Id;
      New_SF : SF_Id;

      Hash_Index : Hash_Index_Type;

      First_Idx : Natural;
      Last_Idx  : Natural;

      Res : Virtual_File;

   begin
      Free (Full_Source_Name_String);
      Free (Short_Source_Name_String);

      if Debug_Flag_S then
         Info ("Adding file to source table:>" & Fname & "<");
      end if;

      if not Use_Project_File_Obsolete then

         if Is_Regular_File (Fname) then
            Short_Source_Name_String := new String'(Fname);
         else
            if Is_Specified (Arg_Project) then
               Res := Create (Arg_Project, +Fname);

               if Res = No_File then
                  Free (Short_Source_Name_String);
               else
                  Short_Source_Name_String :=
                    new String'(Res.Display_Full_Name);
               end if;
            else
               if Source_Search_Path /= null then
                  Short_Source_Name_String :=
                    Locate_Regular_File (File_Name => Fname,
                                         Path      => Source_Search_Path.all);
               end if;
            end if;
         end if;

         if Short_Source_Name_String = null then
            Warning (Fname & " not found");
            return;
         else
            Full_Source_Name_String := new String'
              (Normalize_Pathname
                 (Short_Source_Name_String.all,
                  Resolve_Links  => False,
                  Case_Sensitive => True));

            Free (Short_Source_Name_String);
         end if;

      end if;

      Short_Source_Name_String := new String'(Base_Name (Fname));
      Hash_Index := Hash (To_Lower (Short_Source_Name_String.all));

      if Use_Project_File_Obsolete then
         Old_SF := File_Find (Short_Source_Name_String.all);

         if Present (Old_SF) then

            if Duplication_Report or else Debug_Flag_S then
               Error (Short_Source_Name_String.all & " duplicated");
            end if;

            return;
         end if;

      else

         --  Check if we already have a file with the same short name:

         if Present (Hash_Table (Hash_Index)) then
            Old_SF := File_Find (Full_Source_Name_String.all);

            if Present (Old_SF) then
               --  This means that we have already stored exactly the same
               --  file.
               if Duplication_Report or else Debug_Flag_S then
                  Error (Short_Source_Name_String.all & " duplicated");
               end if;

               return;
            else
               Old_SF := Same_Name_File_Find (Full_Source_Name_String.all);

               if Present (Old_SF) then
                  Error ("more than one version of "
                    & Short_Source_Name_String.all & " processed");
               end if;

            end if;

         end if;

      end if;

      --  If we are here, we have to store the file in the table

      Source_File_Table.Append (New_SF_Record);
      Last_Arg_Source := Source_File_Table.Last;
      New_SF          := Last_Arg_Source;

      if Debug_Flag_S then
         Info ("new source file index:" & New_SF'Img);
      end if;

      if Present (Hash_Table (Hash_Index)) then

         Old_SF := Hash_Table (Hash_Index);

         while Present (Source_Table (Old_SF).Hash_Link) loop
            Old_SF := Source_Table (Old_SF).Hash_Link;
         end loop;

         Source_Table (Old_SF).Hash_Link := New_SF;

      else
         Hash_Table (Hash_Index) := New_SF;
      end if;

      if Use_Project_File_Obsolete then
         Set_Source_Name (New_SF, Short_Source_Name_String.all);
      else
         Set_Source_Name (New_SF, Full_Source_Name_String.all);
      end if;

      Set_Short_Source_Name (New_SF, Short_Source_Name_String.all);
      Set_Source_Status     (New_SF, Status);

      First_Idx := Short_Source_Name_String'First;
      Last_Idx  := Short_Source_Name_String'Last;

      for J in reverse  First_Idx + 1 .. Last_Idx loop

         if Short_Source_Name_String (J) = '.' then
            Last_Idx := J - 1;
            exit;
         end if;

      end loop;

      Set_Suffixless_Name
        (New_SF, Short_Source_Name_String (First_Idx .. Last_Idx));

      if To_Lower (Short_Source_Name_String
           (Last_Idx + 1 .. Short_Source_Name_String'Last)) = ".adb"
      then
         Source_Table (New_SF).Could_Be_Body := True;
      elsif Last_Idx - 1 >= Short_Source_Name_String'First
         and then
            To_Lower (Short_Source_Name_String
              (Last_Idx - 1 .. Short_Source_Name_String'Last)) = ".2.ada"
      then
         Source_Table (New_SF).Could_Be_Body := True;
      end if;

      Free (Short_Source_Name_String);
      Free (Full_Source_Name_String);

   end Add_Source_To_Process;

   ------------------------------
   -- Add_Compilation_Switches --
   ------------------------------

   procedure Add_Compilation_Switches
     (SF       : SF_Id;
      Switches : String_List_Access)
   is
   begin
      Source_Table (SF).Switches := Switches;
   end Add_Compilation_Switches;

   --------------------------
   -- Arg_Source_File_Name --
   --------------------------

   function Arg_Source_File_Name return String is
   begin
      if Arg_File_Name = null then
         return "";
      else
         return Arg_File_Name.all;
      end if;
   end Arg_Source_File_Name;

   --------------------------
   -- Compilation_Switches --
   --------------------------

   function Compilation_Switches (SF : SF_Id) return String_List is
   begin
      if Source_Table (SF).Switches = null then
         return (1 .. 0 => <>);
      else
         return Source_Table (SF).Switches.all;
      end if;
   end Compilation_Switches;

   -----------------
   -- Create_Tree --
   -----------------

   procedure Create_Tree
     (SF               :     SF_Id;
      Success          : out Boolean;
      Compiler_Out     :     String  := "";
      All_Warnings_Off :     Boolean := True)
   is
      use Ada.Directories;
   begin
      --  If we have a mapping file created as a part of argument project file
      --  processing, we have to add it to the list of arguments here, because
      --  it is not stored in Arg_List.

      if Use_Parallel_Tree_Creation then
         Make_Dir (Image (Integer (SF)));

         Asis.Extensions.Compile
           (new String'(Source_Name (SF)),
            Arg_List.all & Compilation_Switches (SF) &
            new String'("-o") &
            new String'(Image (Integer (SF)) & Directory_Separator &
                              Suffixless_Name (SF) & ".o") &
            (if Get_Mapping_File_Name /= "" then
                (1 .. 1 => new String'("-gnatem=" & Get_Mapping_File_Name))
             else
                (1 .. 0 => null)),

            Success,
            GCC                   => Gcc_To_Call,
            Use_GPRBUILD          => Use_Gnatmake_To_Compile,
            Result_In_Current_Dir => Project_Support_Type =
              Use_Tmp_Project_File,
            Compiler_Out          => Compiler_Out,
            All_Warnings_Off      => All_Warnings_Off,
            Display_Call          => Debug_Flag_C);
      else
         Asis.Extensions.Compile
           (new String'(Source_Name (SF)),
            Arg_List.all & Compilation_Switches (SF) &
            (if Get_Mapping_File_Name /= "" then
                (1 .. 1 => new String'("-gnatem=" & Get_Mapping_File_Name))
             else
                (1 .. 0 => null)),

            Success,
            GCC                   => Gcc_To_Call,
            Use_GPRBUILD          => Use_Gnatmake_To_Compile,
            Result_In_Current_Dir => Project_Support_Type =
              Use_Tmp_Project_File,
            Compiler_Out          => Compiler_Out,
            All_Warnings_Off      => All_Warnings_Off,
            Display_Call          => Debug_Flag_C);
      end if;

      if not Success then
         Set_Source_Status (SF, Not_A_Legal_Source);
         Illegal_File_Detected := True;

         if not Fully_Quiet_Mode then
            Error ("cannot compile """ & Short_Source_Name (SF) & """");
         end if;
      else
         Set_Source_Status (SF, Tree_Is_Ready);

         --  Move the tree file into the right place for the ASIS tool to find
         --  it. See comments on Compiler_Output_Subdir for details.

         if Compiler_Output_Subdir /= null then
            declare
               Tree : constant String := Suffixless_Name (SF) & ".adt";
            begin
               Rename_File
                 (Old_Name => Compose (Compiler_Output_Subdir.all, Tree),
                  New_Name => Compose (Current_Directory, Tree),
                  Success => Success);
            end;
         end if;
      end if;

   end Create_Tree;

   -------------
   -- CU_Name --
   -------------

   function CU_Name (SF : SF_Id) return String is
   begin
      return Get_String (Source_Table (SF).CU_Name);
   end CU_Name;

   ----------------------
   -- Exempted_Sources --
   ----------------------

   function Exempted_Sources return Natural is
      Result : Natural := 0;
   begin
      for J in First_SF_Id .. Last_Source loop
         if Source_Info (J) = Ignore_Unit then
            Result := Result + 1;
         end if;
      end loop;

      return Result;
   end Exempted_Sources;

   ---------------
   -- File_Find --
   ---------------

   function File_Find (El : Asis.Element) return SF_Id is
      Result     : SF_Id := No_SF_Id;
   begin

      if not Asis.Elements.Is_Nil (El) then

         declare
            Full_Source_Name : constant String := Normalize_Pathname
              (To_String (Asis.Compilation_Units.Text_Name
                 (Asis.Elements.Enclosing_Compilation_Unit (El))),
               Case_Sensitive => True);

            Short_Source_Name : constant String :=
              Base_Name (Full_Source_Name);
         begin
            if Use_Project_File_Obsolete then
               Result := File_Find (Short_Source_Name);
            else
               Result := File_Find (Full_Source_Name);
            end if;
         end;
      end if;

      return Result;
   end File_Find;

   function File_Find
     (SF_Name        : String;
      Use_Short_Name : Boolean := False;
      Case_Sensitive : Boolean := File_Names_Case_Sensitive)
      return           SF_Id
   is
      Result       : SF_Id := No_SF_Id;
      Next_SF      : SF_Id;
      Base_SF_Name : constant String := Base_Name (SF_Name);
   begin
      Next_SF := Hash_Table (Hash (Base_Name (SF_Name)));

      while Present (Next_SF) loop

         if ((Use_Project_File_Obsolete or else Use_Short_Name)
            and then
             Base_SF_Name = Short_Source_Name (Next_SF))
           or else
            SF_Name = Source_Name (Next_SF)
         then
            Result := Next_SF;
            exit;
         end if;

         Next_SF := Source_Table (Next_SF).Hash_Link;
      end loop;

      if not Present (Result) and then not Case_Sensitive then
         Result := Non_Case_Sensitive_File_Find (SF_Name, Use_Short_Name);
      end if;

      return Result;
   end File_Find;

   ----------------------------
   -- File_Name_Is_Less_Than --
   ----------------------------

   function File_Name_Is_Less_Than (L, R : String) return Boolean is
      L_Last : constant Natural := L'Last;
      R_Last : constant Natural := R'Last;

      L_Dir_Separator : Natural :=
        Index (L, (1 => Directory_Separator), Backward);

      R_Dir_Separator : Natural :=
        Index (R, (1 => Directory_Separator), Backward);

   begin
      if L_Dir_Separator = 0 and then
         R_Dir_Separator = 0
      then
         return L < R;
      end if;

      if L_Dir_Separator = 0 then
         L_Dir_Separator := L'First;
      end if;

      if R_Dir_Separator = 0 then
         R_Dir_Separator := R'First;
      end if;

      if L (L_Dir_Separator .. L_Last) =
         R (R_Dir_Separator .. R_Last)
      then
         return L < R;
      else
         return L (L_Dir_Separator .. L_Last) < R (R_Dir_Separator .. R_Last);
      end if;

   end File_Name_Is_Less_Than;

   ---------------------------
   -- Files_In_Temp_Storage --
   ---------------------------

   function Files_In_Temp_Storage return Natural is
   begin
      return Natural (Length (Temporary_File_Storage));
   end Files_In_Temp_Storage;

   --------------------------------
   -- First_File_In_Temp_Storage --
   --------------------------------

   function First_File_In_Temp_Storage return String is
   begin
      return Ada.Directories.Simple_Name
        (Element (First (Temporary_File_Storage)));
   end First_File_In_Temp_Storage;

   --------------------------------
   -- Get_Compiler_Out_File_Name --
   --------------------------------

   function Get_Compiler_Out_File_Name (SF : SF_Id) return String is
   begin
      return "COMPILER_OUT_" & Image (Integer (SF));
   end Get_Compiler_Out_File_Name;

   --------------------
   -- Get_Result_Dir --
   --------------------

   function Get_Result_Dir (SF : SF_Id) return String is
   begin
      return
        (if Source_Table (SF).Result_Dir = null then
            ""
         else
            Source_Table (SF).Result_Dir.all & Directory_Separator);

   end Get_Result_Dir;

   ----------
   -- Hash --
   ----------

   --  The code is taken from Namet with small modifications

   function Hash (File_Name : String) return Hash_Index_Type is
      subtype Int_0_12 is Integer range 0 .. 12;
      --  Used to avoid when others on case jump below

      Name_Len    : constant Natural                := File_Name'Length;
      Name_Buffer : constant String (1 .. Name_Len) := To_Lower (File_Name);
      --  This allows us to use from Namet without any change at all

      Even_Name_Len : Integer;
      --  Last even numbered position (used for >12 case)

   begin

      --  Special test for 12 (rather than counting on a when others for the
      --  case statement below) avoids some Ada compilers converting the case
      --  statement into successive jumps.

      --  The case of a name longer than 12 characters is handled by taking
      --  the first 6 odd numbered characters and the last 6 even numbered
      --  characters

      if Name_Len > 12 then
         Even_Name_Len := (Name_Len) / 2 * 2;

         return ((((((((((((
           Character'Pos (Name_Buffer (01))) * 2 +
           Character'Pos (Name_Buffer (Even_Name_Len - 10))) * 2 +
           Character'Pos (Name_Buffer (03))) * 2 +
           Character'Pos (Name_Buffer (Even_Name_Len - 08))) * 2 +
           Character'Pos (Name_Buffer (05))) * 2 +
           Character'Pos (Name_Buffer (Even_Name_Len - 06))) * 2 +
           Character'Pos (Name_Buffer (07))) * 2 +
           Character'Pos (Name_Buffer (Even_Name_Len - 04))) * 2 +
           Character'Pos (Name_Buffer (09))) * 2 +
           Character'Pos (Name_Buffer (Even_Name_Len - 02))) * 2 +
           Character'Pos (Name_Buffer (11))) * 2 +
           Character'Pos (Name_Buffer (Even_Name_Len))) mod Hash_Num;
      end if;

      --  For the cases of 1-12 characters, all characters participate in the
      --  hash. The positioning is randomized, with the bias that characters
      --  later on participate fully (i.e. are added towards the right side).

      case Int_0_12 (Name_Len) is

         when 0 =>
            return 0;

         when 1 =>
            return
               Character'Pos (Name_Buffer (1));

         when 2 =>
            return ((
              Character'Pos (Name_Buffer (1))) * 64 +
              Character'Pos (Name_Buffer (2))) mod Hash_Num;

         when 3 =>
            return (((
              Character'Pos (Name_Buffer (1))) * 16 +
              Character'Pos (Name_Buffer (3))) * 16 +
              Character'Pos (Name_Buffer (2))) mod Hash_Num;

         when 4 =>
            return ((((
              Character'Pos (Name_Buffer (1))) * 8 +
              Character'Pos (Name_Buffer (2))) * 8 +
              Character'Pos (Name_Buffer (3))) * 8 +
              Character'Pos (Name_Buffer (4))) mod Hash_Num;

         when 5 =>
            return (((((
              Character'Pos (Name_Buffer (4))) * 8 +
              Character'Pos (Name_Buffer (1))) * 4 +
              Character'Pos (Name_Buffer (3))) * 4 +
              Character'Pos (Name_Buffer (5))) * 8 +
              Character'Pos (Name_Buffer (2))) mod Hash_Num;

         when 6 =>
            return ((((((
              Character'Pos (Name_Buffer (5))) * 4 +
              Character'Pos (Name_Buffer (1))) * 4 +
              Character'Pos (Name_Buffer (4))) * 4 +
              Character'Pos (Name_Buffer (2))) * 4 +
              Character'Pos (Name_Buffer (6))) * 4 +
              Character'Pos (Name_Buffer (3))) mod Hash_Num;

         when 7 =>
            return (((((((
              Character'Pos (Name_Buffer (4))) * 4 +
              Character'Pos (Name_Buffer (3))) * 4 +
              Character'Pos (Name_Buffer (1))) * 4 +
              Character'Pos (Name_Buffer (2))) * 2 +
              Character'Pos (Name_Buffer (5))) * 2 +
              Character'Pos (Name_Buffer (7))) * 2 +
              Character'Pos (Name_Buffer (6))) mod Hash_Num;

         when 8 =>
            return ((((((((
              Character'Pos (Name_Buffer (2))) * 4 +
              Character'Pos (Name_Buffer (1))) * 4 +
              Character'Pos (Name_Buffer (3))) * 2 +
              Character'Pos (Name_Buffer (5))) * 2 +
              Character'Pos (Name_Buffer (7))) * 2 +
              Character'Pos (Name_Buffer (6))) * 2 +
              Character'Pos (Name_Buffer (4))) * 2 +
              Character'Pos (Name_Buffer (8))) mod Hash_Num;

         when 9 =>
            return (((((((((
              Character'Pos (Name_Buffer (2))) * 4 +
              Character'Pos (Name_Buffer (1))) * 4 +
              Character'Pos (Name_Buffer (3))) * 4 +
              Character'Pos (Name_Buffer (4))) * 2 +
              Character'Pos (Name_Buffer (8))) * 2 +
              Character'Pos (Name_Buffer (7))) * 2 +
              Character'Pos (Name_Buffer (5))) * 2 +
              Character'Pos (Name_Buffer (6))) * 2 +
              Character'Pos (Name_Buffer (9))) mod Hash_Num;

         when 10 =>
            return ((((((((((
              Character'Pos (Name_Buffer (01))) * 2 +
              Character'Pos (Name_Buffer (02))) * 2 +
              Character'Pos (Name_Buffer (08))) * 2 +
              Character'Pos (Name_Buffer (03))) * 2 +
              Character'Pos (Name_Buffer (04))) * 2 +
              Character'Pos (Name_Buffer (09))) * 2 +
              Character'Pos (Name_Buffer (06))) * 2 +
              Character'Pos (Name_Buffer (05))) * 2 +
              Character'Pos (Name_Buffer (07))) * 2 +
              Character'Pos (Name_Buffer (10))) mod Hash_Num;

         when 11 =>
            return (((((((((((
              Character'Pos (Name_Buffer (05))) * 2 +
              Character'Pos (Name_Buffer (01))) * 2 +
              Character'Pos (Name_Buffer (06))) * 2 +
              Character'Pos (Name_Buffer (09))) * 2 +
              Character'Pos (Name_Buffer (07))) * 2 +
              Character'Pos (Name_Buffer (03))) * 2 +
              Character'Pos (Name_Buffer (08))) * 2 +
              Character'Pos (Name_Buffer (02))) * 2 +
              Character'Pos (Name_Buffer (10))) * 2 +
              Character'Pos (Name_Buffer (04))) * 2 +
              Character'Pos (Name_Buffer (11))) mod Hash_Num;

         when 12 =>
            return ((((((((((((
              Character'Pos (Name_Buffer (03))) * 2 +
              Character'Pos (Name_Buffer (02))) * 2 +
              Character'Pos (Name_Buffer (05))) * 2 +
              Character'Pos (Name_Buffer (01))) * 2 +
              Character'Pos (Name_Buffer (06))) * 2 +
              Character'Pos (Name_Buffer (04))) * 2 +
              Character'Pos (Name_Buffer (08))) * 2 +
              Character'Pos (Name_Buffer (11))) * 2 +
              Character'Pos (Name_Buffer (07))) * 2 +
              Character'Pos (Name_Buffer (09))) * 2 +
              Character'Pos (Name_Buffer (10))) * 2 +
              Character'Pos (Name_Buffer (12))) mod Hash_Num;

      end case;
   end Hash;

   ------------------------
   -- Is_Argument_Source --
   ------------------------

   function Is_Argument_Source (SF : SF_Id) return Boolean is
   begin
      return SF in First_SF_Id .. Last_Argument_Source;
   end Is_Argument_Source;

   ---------------
   -- Is_A_Body --
   ---------------

   function Is_A_Body (SF : SF_Id) return Boolean is
   begin
      return Source_Table (SF).Could_Be_Body;
   end Is_A_Body;

   ----------------------
   -- Is_Needed_Source --
   ----------------------

   function Is_Needed_Source (SF : SF_Id) return Boolean is
   begin
      return SF in Last_Argument_Source + 1 .. Source_File_Table.Last;
   end Is_Needed_Source;

   -----------------
   -- Last_Source --
   -----------------

   function Last_Source return SF_Id is
   begin
      return Source_File_Table.Last;
   end Last_Source;

   --------------------------
   -- Last_Argument_Source --
   --------------------------

   function Last_Argument_Source return SF_Id is
   begin
      return Last_Arg_Source;
   end Last_Argument_Source;

   -------------------------------
   -- Next_Non_Processed_Source --
   -------------------------------

   function Next_Non_Processed_Source
     (Only_Bodies            : Boolean := False;
      Include_Needed_Sources : Boolean := False)
      return                  SF_Id
   is
      Up_To            :          SF_Id   := Last_Argument_Source;
      New_Source_Found :          Boolean := False;
      Move_Next_Source :          Boolean := True;
      Result           :          SF_Id;
      In_Gnatcheck     : constant Boolean :=
        Index (Tool_Name.all, "gnatcheck") /= 0;
   begin

      if Include_Needed_Sources then
         Up_To := Last_Source;
      end if;

      for J in Next_Source .. Up_To loop

         if Source_Status (J) in
              Waiting       |
              Tree_Is_Ready |
              Not_A_Legal_Source_Needs_Listing_Processing
           and then
            (if Only_Bodies then Is_A_Body (J))
           and then
            ((Buld_Call_Graph and then not In_Gnatcheck)
            or else
             Source_Info (J) /= Ignore_Unit)
         then
            Result           := J;
            New_Source_Found := True;
            exit;
         end if;

      end loop;

      if not New_Source_Found then
         Result := No_SF_Id;
      else
         for J in Next_Source + 1 .. Result - 1 loop
            if Source_Status (J) in
                 Waiting         |
                 Waiting_Subunit |
                 Preparing_Tree
            then
               Move_Next_Source := False;
               exit;
            end if;
         end loop;

         if Move_Next_Source then
            Next_Source := Result;
         end if;
      end if;

      return Result;
   end Next_Non_Processed_Source;

   ----------------------------------
   -- Non_Case_Sensitive_File_Find --
   ----------------------------------

   function Non_Case_Sensitive_File_Find
     (SF_Name        : String;
      Use_Short_Name : Boolean := False)
      return           SF_Id
   is
      Result       : SF_Id := No_SF_Id;
      Next_SF      : SF_Id;
      Base_SF_Name : constant String := To_Lower (Base_Name (SF_Name));
      Arg_Name     : constant String := To_Lower (SF_Name);
   begin
      Next_SF := Hash_Table (Hash (Base_Name (SF_Name)));

      while Present (Next_SF) loop

         if ((Use_Project_File_Obsolete or else Use_Short_Name)
            and then
             Base_SF_Name = To_Lower (Short_Source_Name (Next_SF)))
           or else
            Arg_Name = To_Lower (Source_Name (Next_SF))
         then
            Result := Next_SF;
            exit;
         end if;

         Next_SF := Source_Table (Next_SF).Hash_Link;
      end loop;

      return Result;
   end Non_Case_Sensitive_File_Find;

   -------------------
   -- Output_Source --
   -------------------

   procedure Output_Source (SF : SF_Id) is
      N : constant String := Natural'Image (Sources_Left);
   begin
      if not (ASIS_UL.Common.Multiple_File_Mode or else Verbose_Mode)
        or else Is_Needed_Source (SF) or else Mimic_gcc
      then
         return;
      end if;

      if Progress_Indicator_Mode then
         declare
            Current : constant Integer := Total_Sources - Sources_Left + 1;
            Percent : String :=
              Integer'Image ((Current * 100) / Total_Sources);
         begin
            Percent (1) := '(';
            Info ("completed" & Integer'Image (Current) & " out of"
                  & Integer'Image (Total_Sources) & " "
                  & Percent & "%)...");
         end;
      end if;

      if Verbose_Mode or else Debug_Flag_S then
         Info_No_EOL ("[" & N (2 .. N'Last) & "] ");

         if Debug_Flag_S then
            Info (Source_Name (SF));
         else
            Info (Short_Source_Name (SF));
         end if;

      elsif not (Quiet_Mode or Progress_Indicator_Mode) then
         Info_No_EOL ("Units remaining:");
         Info_No_EOL (N);
         Info_No_EOL ("     ");
         Info_No_EOL ((1 => ASCII.CR));
      end if;

      Sources_Left := Sources_Left - 1;

   end Output_Source;

   -------------
   -- Present --
   -------------

   function Present (SF : SF_Id) return Boolean is
   begin
      return SF in  First_SF_Id .. Source_File_Table.Last;
   end Present;

   -------------------------
   -- Read_Args_From_File --
   -------------------------

   procedure Read_Args_From_File (Par_File_Name : String) is
      Arg_File         : File_Type;
      File_Name_Buffer : String (1 .. 16 * 1024);
      File_Name_Len    : Natural := 0;
      Next_Ch          : Character;
      End_Of_Line      : Boolean;

      function Get_File_Name return String;
      --  Reads from Par_File_Name the name of the next file (the file to read
      --  from should exist and be opened). Returns an empty string if there
      --  are no more file names in Par_File_Name.

      function Get_File_Name return String is
      begin
         File_Name_Len := 0;

         if not End_Of_File (Arg_File) then
            Get (Arg_File, Next_Ch);

            while Next_Ch in ASCII.LF | ASCII.CR loop
               exit when End_Of_File (Arg_File);
               Get (Arg_File, Next_Ch);
            end loop;

            --  If we are here. Next_Ch is neither a white space nor
            --  end-of-line character. Two cases are possible, they require
            --  different processing:
            --
            --  1. Next_Ch = '"', this means that the file name is surrounded
            --     by quotation marks and it can contain spaces inside.
            --
            --  2. Next_Ch /= '"', this means that the file name is bounded by
            --     a white space or end-of-line character

            if Next_Ch = '"' then

               --  We do not generate any warning for badly formatted content
               --  of the file such as
               --
               --    file_name_1
               --    "file name 2
               --    file_name_3
               --
               --  (We do not check that quotation marks correctly go by pairs)

               --  Skip leading '"'
               Get (Arg_File, Next_Ch);

               while not (Next_Ch = '"'
                  or else
                     Next_Ch = ASCII.LF
                  or else
                     Next_Ch = ASCII.CR)
               loop
                  File_Name_Len := File_Name_Len + 1;
                  File_Name_Buffer (File_Name_Len) := Next_Ch;

                  Look_Ahead (Arg_File, Next_Ch, End_Of_Line);

                  exit when End_Of_Line or else End_Of_File (Arg_File);

                  Get (Arg_File, Next_Ch);
               end loop;

               if Next_Ch = '"'
                 and then
                  not Ada.Text_IO.End_Of_Line (Arg_File)
               then
                  --  skip trailing '"'
                  Get (Arg_File, Next_Ch);
               end if;
            else
               while Next_Ch not in ASCII.LF | ASCII.CR loop
                  File_Name_Len := File_Name_Len + 1;
                  File_Name_Buffer (File_Name_Len) := Next_Ch;

                  Look_Ahead (Arg_File, Next_Ch, End_Of_Line);

                  exit when End_Of_Line or else End_Of_File (Arg_File);

                  Get (Arg_File, Next_Ch);
               end loop;
            end if;

         end if;

         return File_Name_Buffer (1 .. File_Name_Len);
      end Get_File_Name;

   --  Start of processing for Read_Args_From_File

   begin
      ASIS_UL.Options.No_Argument_File_Specified := False;

      if not Is_Regular_File (Par_File_Name) then
         Error (Par_File_Name & " does not exist");
         return;
      end if;

      Open (Arg_File, In_File, Par_File_Name);

      loop
         declare
            Tmp_Str : constant String := Get_File_Name;
         begin
            exit when Tmp_Str = "";

            Store_Sources_To_Process (Tmp_Str);
         end;

      end loop;

      if not More_Then_One_Arg_File_Specified then

         if Arg_File_Name /= null then
            --  We have already encountered one non-empty argument file
            Free (Arg_File_Name);
            More_Then_One_Arg_File_Specified := True;
         else
            Arg_File_Name := new String'(Par_File_Name);
         end if;

      end if;

      Close (Arg_File);
   exception
      when others =>
         Error ("cannot read arguments from " & Par_File_Name);
         --  Exception info will be generated in main driver
         raise;
   end Read_Args_From_File;

   --------------------------
   -- Temp_Storage_Iterate --
   --------------------------

   procedure Temp_Storage_Iterate
     (Action : not null access procedure (File_Name : String)) is
      C : Temporary_File_Storages.Cursor := First (Temporary_File_Storage);
   begin
      while C /= No_Element loop
         Action (Element (C));
         C := Next (C);
      end loop;
   end Temp_Storage_Iterate;

   ---------------------------------
   -- Read_Args_From_Temp_Storage --
   ---------------------------------

   procedure Read_Args_From_Temp_Storage
     (Duplication_Report : Boolean;
      Arg_Project        : Arg_Project_Type'Class;
      Status             : SF_Status := Waiting)
   is
      procedure Action (File_Name : String);
      procedure Action (File_Name : String) is
      begin
         Add_Source_To_Process
           (Fname              => File_Name,
            Arg_Project        => Arg_Project,
            Duplication_Report => Duplication_Report,
            Status             => Status);
      end Action;
   begin
      Temp_Storage_Iterate (Action'Access);
      Clear (Temporary_File_Storage);
   end Read_Args_From_Temp_Storage;

   ---------------------------
   -- Reset_Source_Iterator --
   ---------------------------

   procedure Reset_Source_Iterator is
   begin
      Next_Source := First_SF_Id;
   end Reset_Source_Iterator;

   -------------------------
   -- Same_Name_File_Find --
   -------------------------

   function Same_Name_File_Find (Short_SF_Name : String) return SF_Id is
      Result     : SF_Id := No_SF_Id;
      Next_SF    : SF_Id;
   begin
      Next_SF := Hash_Table (Hash (Short_SF_Name));

      while Present (Next_SF) loop

         if Short_SF_Name = Short_Source_Name (Next_SF) then
            Result := Next_SF;
            exit;
         end if;

         Next_SF := Source_Table (Next_SF).Hash_Link;
      end loop;

      return Result;
   end Same_Name_File_Find;

   -----------------
   -- Set_CU_Name --
   -----------------

   procedure Set_CU_Name (SF : SF_Id; N : String) is
   begin
      Source_Table (SF).CU_Name := Enter_String (N);
   end Set_CU_Name;

   --------------------
   -- Set_Result_Dir --
   --------------------

   procedure Set_Result_Dir
     (SF   : SF_Id;
      Path : String)
   is
   begin
      Source_Table (SF).Result_Dir := new String'(Path);
   end Set_Result_Dir;

   ---------------------
   -- Set_Source_Info --
   ---------------------

   procedure Set_Source_Info (SF : SF_Id; Info : SF_Info) is
   begin
      Source_Table (SF).Info := Info;
   end Set_Source_Info;

   ---------------------------
   -- Set_Short_Source_Name --
   ---------------------------

   procedure Set_Short_Source_Name (SF : SF_Id; N : String) is
   begin
      Source_Table (SF).Short_Source_Name := Enter_String (N);
   end Set_Short_Source_Name;

   ---------------------
   -- Set_Source_Name --
   ---------------------

   procedure Set_Source_Name (SF : SF_Id; N : String) is
   begin
      Source_Table (SF).Source_Name := Enter_String (N);
   end Set_Source_Name;

   -----------------
   -- Source_Info --
   -----------------

   function Source_Info (SF : SF_Id) return SF_Info is
   begin
      return Source_Table (SF).Info;
   end Source_Info;

   -------------------
   -- Set_Exemption --
   -------------------

   procedure Set_Exemption (Fname : String) is
      SF : constant SF_Id := File_Find (Fname, Use_Short_Name => True);
   begin
      if Present (SF) then
         Set_Source_Info (SF, ASIS_UL.Source_Table.Ignore_Unit);
      else
         ASIS_UL.Output.Warning ("exemption: source " & Fname & " not found");
      end if;
   end Set_Exemption;

   -----------------------
   -- Set_Source_Status --
   -----------------------

   procedure Set_Source_Status (SF : SF_Id; S : SF_Status) is
   begin
      Source_Table (SF).Status := S;

      case S is
         when Not_A_Legal_Source =>
            Illegal_Sources := Illegal_Sources + 1;
         when Error_Detected =>
            Tool_Failures := Tool_Failures + 1;
         when Out_File_Problem =>
            Out_File_Problems := Out_File_Problems + 1;
         when others =>
            null;
      end case;

   end Set_Source_Status;

   -------------------------
   -- Set_Suffixless_Name --
   -------------------------

   procedure Set_Suffixless_Name   (SF : SF_Id; N : String) is
   begin
      Source_Table (SF).Suffixless_Name := Enter_String (N);
   end Set_Suffixless_Name;

   -----------------------
   -- Short_Source_Name --
   -----------------------

   function Short_Source_Name (SF : SF_Id) return String is
   begin
      return Get_String (Source_Table (SF).Short_Source_Name);
   end Short_Source_Name;

   ---------------------
   -- Source_Clean_Up --
   ---------------------

   procedure Source_Clean_Up
     (SF             : SF_Id;
      Keep_ALI_Files : Boolean := False)
   is
      Success : Boolean;
   begin
      Context_Clean_Up;

      if Use_Parallel_Tree_Creation then
         Remove_Dir (Dir_Name => Image (Integer (SF)), Recursive => True);
      else
         Delete_File (Suffixless_Name (SF) & ".adt", Success);

         if not Keep_ALI_Files then
            Delete_File (Suffixless_Name (SF) & ".ali", Success);
         end if;
      end if;
   end Source_Clean_Up;

   -----------------
   -- Source_Name --
   -----------------

   function Source_Name (SF : SF_Id) return String is
   begin
      return Get_String (Source_Table (SF).Source_Name);
   end Source_Name;

   -------------------
   -- Source_Status --
   -------------------

   function Source_Status (SF : SF_Id) return SF_Status is
   begin
      return Source_Table (SF).Status;
   end Source_Status;

   ------------------------
   -- Source_Debug_Image --
   ------------------------

   procedure Source_Debug_Image (SF : SF_Id) is
      Ident_String : constant String      := "   ";
      Tmp          : constant String_List := Compilation_Switches (SF);
   begin
      Info_No_EOL ("SF =" & SF'Img);
      if SF > Last_Source then
         Info_No_EOL (" ( > Last_Source =" & Last_Source'Img & ")");
      end if;
      Info ("");

      Info_No_EOL (Ident_String);
      Info        ("Source_Name       = >" & Source_Name (SF) & "<");

      Info_No_EOL (Ident_String);
      Info        ("Short_Source_Name = >" & Short_Source_Name (SF) & "<");

      Info_No_EOL (Ident_String);
      Info        ("Source_Status     = " & Source_Status (SF)'Img);

      Info_No_EOL (Ident_String);
      Info        ("Contained Ada CU  = >" & CU_Name (SF) & "<");

      Info_No_EOL (Ident_String);
      Info        ("Hash_Link         =" &
                   Source_File_Table.Table (SF).Hash_Link'Img);

      Info_No_EOL (Ident_String);
      Info_No_EOL ("Switches          =");

      for J in Tmp'Range loop
         Info_No_EOL (Tmp (J).all & ' ');
      end loop;

      Info ("");

      if Source_File_Table.Table (SF).Info /= 0 then
         Info_No_EOL (Ident_String);
         Info        ("Info              =" &
                      Source_File_Table.Table (SF).Info'Img);
      end if;

   end Source_Debug_Image;

   ------------------------
   -- Source_Table_Debug --
   ------------------------

   procedure Source_Table_Debug is
   begin
      Info ("-= SOURCE TABLE DEBUG IMAGE =-");

      if Last_Argument_Source < First_SF_Id then
         Info ("  No source stored in source table");
         return;
      end if;

      Info ("");
      Info ("-= Argument sources =-");

      for J in First_SF_Id .. Last_Argument_Source loop
         Source_Debug_Image (J);
      end loop;

      Info ("");

      if Last_Source = Last_Argument_Source then
         Info ("  No needed source added in source table");
         return;
      end if;

      Info ("-= Needed sources =-");
      for J in Last_Argument_Source + 1 .. Last_Source loop
         Source_Debug_Image (J);
      end loop;
   end Source_Table_Debug;

   ------------------------------
   -- Source_Table_Debug_Image --
   ------------------------------

   procedure Source_Table_Debug_Image is
   begin
      if Debug_Flag_S then
         Source_Table_Debug;
      end if;
   end Source_Table_Debug_Image;

   ------------------------------
   -- Store_Sources_To_Process --
   ------------------------------

   procedure Store_Sources_To_Process
     (Fname : String;
      Store : Boolean := True)
   is
   begin
      ASIS_UL.Options.No_Argument_File_Specified := False;

      if Store then
         Include (Temporary_File_Storage, Fname);

         if Debug_Flag_S then
            Info ("Storing argument file:>" & Fname & "<");
         end if;
      end if;
   end Store_Sources_To_Process;

   ---------------------
   -- Suffixless_Name --
   ---------------------

   function Suffixless_Name   (SF : SF_Id) return String is
   begin
      return Get_String (Source_Table (SF).Suffixless_Name);
   end Suffixless_Name;

   ------------------------------
   -- Total_Sources_To_Process --
   ------------------------------

   function Total_Sources_To_Process return Natural is
      Result : Natural := 0;
   begin
      for J in First_SF_Id .. Last_Source loop
         if Source_Info (J) /= Ignore_Unit then
            Result := Result + 1;
         end if;
      end loop;

      return Result;
   end Total_Sources_To_Process;

end ASIS_UL.Source_Table;
