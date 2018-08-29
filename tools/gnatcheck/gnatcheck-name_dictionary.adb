------------------------------------------------------------------------------
--                                                                          --
--                          GNATCHECK COMPONENTS                            --
--                                                                          --
--            G N A T C H E C K . N A M E _ D I C T I O N A R Y             --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                    Copyright (C) 2005-2017, AdaCore                      --
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

with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Directories;
with Ada.Strings;             use Ada.Strings;
with Ada.Strings.Fixed;       use Ada.Strings.Fixed;
with Ada.Text_IO;             use Ada.Text_IO;

with Asis.Extensions.Strings; use Asis.Extensions.Strings;

with ASIS_UL.Misc;            use ASIS_UL.Misc;
with ASIS_UL.Output;          use ASIS_UL.Output;

with GNAT.OS_Lib;             use GNAT.OS_Lib;

with Gnatcheck.Rules.Rule_Table;

with GNAT.Table;

package body Gnatcheck.Name_Dictionary is

   ---------------------
   -- Name Dictionary --
   ---------------------

   type Dictionary_Id is new Integer range 0 .. Integer'Last;
   No_Dictionary_Entry : constant Dictionary_Id := Dictionary_Id'First;
   First_Dictionary_Id : constant Dictionary_Id := No_Dictionary_Entry + 1;

   function Present (Id : Dictionary_Id) return Boolean;
   --  Checks that Id points to an existing entry in the dictionary.

   type Dictionary_Record is record
      Word : String_Loc;
      --  We use ASIS_UL.Strings storage mechanism to keep words, because we
      --  are not expecting very big dictionary size here and, therefore, the
      --  corresponding performance problems. Otherwise we should use access to
      --  string

      Hash_Link : Dictionary_Id;
      --  Link to next entry in dictionary table for same hash code
   end record;

   package Dictionary_Entries is new GNAT.Table (
     Table_Component_Type => Dictionary_Record,
     Table_Index_Type     => Dictionary_Id,
     Table_Low_Bound      => First_Dictionary_Id,
     Table_Initial        => 100,
     Table_Increment      => 100,
     Table_Name           => "GNATCHECK Name Dictionary");

   package Dictionary_Hash_Table is new
     ASIS_UL.Misc.String_Hash_Table (Entry_Id => Dictionary_Id);
   use Dictionary_Hash_Table;

   -----------------------
   -- Local subprograms --
   -----------------------

   procedure Scan_Dictionary (Fname : String);
   --  Supposing that Fname is a name of a dictionary file, scans this file and
   --  fills in the dictionary table.

   -----------------------
   -- Add_To_Dictionary --
   -----------------------

   procedure Add_To_Dictionary (Name : String) is
      Name_To_Add : constant String (1 .. Name'Length) := To_Lower (Name);
      --  Convert casing to lower, and index range - to starting from 1

      Hash_Val            : constant Hash_Index_Type := Hash (Name_To_Add);
      Add_After           :          Dictionary_Id   := Hash_Table (Hash_Val);
      Name_In_Dictionary  :          Boolean         := False;
   begin

      if Present (Add_After) then
         --  Traversing the hash chain
         loop
            if Is_Equal
                 (Name_To_Add, Dictionary_Entries.Table (Add_After).Word)
            then
               Name_In_Dictionary := True;
               exit;
            elsif Present (Dictionary_Entries.Table (Add_After).Hash_Link) then
               Add_After := Dictionary_Entries.Table (Add_After).Hash_Link;
            else
               exit;
            end if;
         end loop;

      end if;

      if not Name_In_Dictionary then
         Dictionary_Entries.Append
           (New_Val => (Word      => Enter_String (Name_To_Add),
                        Hash_Link => No_Dictionary_Entry));

         if Present (Add_After) then
            Dictionary_Entries.Table (Add_After).Hash_Link :=
              Dictionary_Entries.Last;
         else
            Hash_Table (Hash_Val) := Dictionary_Entries.Last;
         end if;

      end if;

   end Add_To_Dictionary;

   ------------------------
   -- Name_In_Dictionary --
   ------------------------

   function Name_In_Dictionary (Name : Wide_String) return Boolean is
      Name_To_Search : constant String (1 .. Name'Length) :=
        To_Lower (To_String (Name));
      --  The name we actually will be looking for in the dictionary

      Result       : Boolean       := False;
      Search_Chain : Dictionary_Id := Hash_Table (Hash (Name_To_Search));
   begin

      while Present (Search_Chain) loop

         if Is_Equal (Name_To_Search,
                      Dictionary_Entries.Table (Search_Chain).Word)
         then
            Result := True;
            exit;
         else
            Search_Chain := Dictionary_Entries.Table (Search_Chain).Hash_Link;
         end if;

      end loop;

      return Result;
   end Name_In_Dictionary;

   -------------
   -- Present --
   -------------

   function Present (Id : Dictionary_Id) return Boolean is
   begin
      return Id in First_Dictionary_Id .. Dictionary_Entries.Last;
   end Present;

   ---------------------
   -- Scan_Dictionary --
   ---------------------

   procedure Scan_Dictionary (Fname : String) is
      Dictionary_File_Name : String_Access;
      Tmp                  : String_Access :=
        new String'(Gnatcheck.Rules.Rule_Table.Processed_Rule_File_Name);
      Idx : Natural;

      Dictionary_File : File_Type;

      Line_Num : Natural := 0;
      --  The number of the currently processed line, used to form diagnostic
      --  messages

      String_Buffer_Max_Len : constant Natural := 1024;
      --  Should be enough, I hope...

      String_Buffer : String (1 .. String_Buffer_Max_Len);
      --  Buffer to place the next dictionary file line in

      Len : Natural range 0 .. String_Buffer_Max_Len := 0;
      --  The length of the dictionary file line which is being processed

      procedure Process_Dictionary_File_Line;
      --  Reads the next line from the dictionary file into String_Buffer,
      --  parses String_Buffer, and adds the located names in the dictionary

      ----------------------------------
      -- Process_Dictionary_File_Line --
      ----------------------------------

      procedure Process_Dictionary_File_Line is
         Start_Word : Natural := 0;
         End_Word   : Natural := 0;

         procedure Get_Next_Word;
         --  Set Start_Word and End_Word to point to the next word in the
         --  String_Buffer. Set Start_Word to 0 if there is no word any more.

         function Is_Identifier return Boolean;
         --  Check if String_Buffer (Start_Word .. End_Word) satisfies the
         --  syntax of Ada identifier

         function Is_Comment return Boolean;
         --  Check if String_Buffer (Start_Word .. End_Word) is (the beginning
         --  of) the (Ada) comment line

         procedure Get_Next_Word is
         begin
            Start_Word := 0;

            for J in End_Word + 1 .. Len loop

               if not Is_White_Space (String_Buffer (J)) then
                  Start_Word := J;
                  exit;
               end if;

            end loop;

            if Start_Word /= 0 then
               End_Word := Len;

               for J in Start_Word + 1 .. Len loop

                  if Is_White_Space (String_Buffer (J)) then
                     End_Word := J - 1;
                     exit;
                  end if;

               end loop;

            end if;

         end Get_Next_Word;

         function Is_Comment return Boolean is
         begin
            return End_Word > Start_Word
                and then
                   String_Buffer (Start_Word .. End_Word) = "--";
         end Is_Comment;

         function Is_Identifier return Boolean is
            Got_Underscore : Boolean := False;
            Result         : Boolean := True;
         begin

            if not Is_Letter (String_Buffer (Start_Word)) then
               Result := False;
            else

               for J in Start_Word + 1 .. End_Word loop

                  if Is_Alphanumeric (String_Buffer (J)) then
                     Got_Underscore := False;
                  elsif String_Buffer (J) = '_' then

                     if Got_Underscore then
                        Result := False;
                        exit;
                     else
                        Got_Underscore := True;
                     end if;
                  else
                     Result := False;
                     exit;
                  end if;

               end loop;

            end if;

            return Result;

         end Is_Identifier;

      begin  --  Process_Dictionary_File_Line
         Get_Line (Dictionary_File, String_Buffer, Len);

         if Len = 0 then
            --  This is an empty line
            return;
         end if;

         Get_Next_Word;

         while Start_Word /= 0 loop

            if Is_Comment then
               --  nothing to do, the rest of the line is comment
               return;
            elsif Is_Identifier then
               Add_To_Dictionary (String_Buffer (Start_Word .. End_Word));
            else
               Error (Fname & ':' & Image (Line_Num) & ':' &
                      Image (Start_Word) &
                      " illegal word in dictionary, ignored");
            end if;

            Get_Next_Word;

         end loop;

      end Process_Dictionary_File_Line;

   begin
      --  First trying to open the dictionary file. If the option is specified
      --  in a rule file and Fname does not contain a path information, we
      --  firts try to locate the dictionaty file in the same directory where
      --  the rule file is located and if this attempt fails - in the current
      --  directory

      if Tmp.all = ""
        or else
         Fname /= Ada.Directories.Simple_Name (Fname)
      then
         Dictionary_File_Name := new String'(Fname);
      else
         Idx := Index (Tmp.all, (1 => Directory_Separator), Backward);

         if Idx = 0 then
            Dictionary_File_Name :=
              new String'(Tmp.all & Directory_Separator & Fname);
         else
            Dictionary_File_Name :=
              new String'(Tmp (Tmp'First .. Idx) & Fname);
         end if;

         if not Is_Regular_File (Dictionary_File_Name.all) then
            Free (Dictionary_File_Name);
            Dictionary_File_Name := new String'(Fname);
         end if;
      end if;

      Free (Tmp);

      begin
         Open (File => Dictionary_File,
               Mode => In_File,
               Name => Dictionary_File_Name.all);
      exception
         when Name_Error =>
            Error ("dictionary file " & Fname & " does not exit");
            Free (Dictionary_File_Name);
            return;
         when Status_Error =>
            Error ("can not open  dictionary file " & Fname);
            Free (Dictionary_File_Name);
            return;
      end;

      while not End_Of_File (Dictionary_File) loop
         Line_Num := Line_Num + 1;
         Process_Dictionary_File_Line;
      end loop;

      if Is_Open (Dictionary_File) then
         Close (Dictionary_File);
      end if;

      Free (Dictionary_File_Name);
   end Scan_Dictionary;

   --------------------------
   -- Scan_Dictionary_File --
   --------------------------

   procedure Scan_Dictionary_File
     (Parameter : String;
      Success   : out Boolean)
   is
   begin
      Success := False;

      Scan_Dictionary (Parameter);

      if Dictionary_Entries.Last >= First_Dictionary_Id then
         Success := True;
      end if;

   end Scan_Dictionary_File;

begin
   Dictionary_Hash_Table.Hash_Table := (others => No_Dictionary_Entry);
end Gnatcheck.Name_Dictionary;
