------------------------------------------------------------------------------
--                                                                          --
--                     ASIS UTILITY LIBRARY COMPONENTS                      --
--                                                                          --
--                         A S I S _ U L . M I S C                          --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                    Copyright (C) 2006-2016, AdaCore                      --
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

with Ada.Characters.Handling;          use Ada.Characters.Handling;
with Ada.Containers.Indefinite_Hashed_Sets;
with Ada.Strings.Hash;
with Ada.Strings.Fixed;
with Ada.Text_IO;                      use Ada.Text_IO;
with Ada.Wide_Characters.Unicode;

with GNAT.OS_Lib; use GNAT.OS_Lib;

with ASIS_UL.Common;                   use ASIS_UL.Common;
with ASIS_UL.Output;                   use ASIS_UL.Output;

package body ASIS_UL.Misc is

   -----------------------
   -- Local subprograms --
   -----------------------

   function Non_Case_Sensitive_Is_Equal (Left, Right : String) return Boolean;
   --  Non-case-sensitive string equality check

   --------------------------
   -- ASIS_Index_Non_Blank --
   --------------------------

   function ASIS_Index_Non_Blank
     (Source : String;
      Going  : Direction := Forward)
      return Natural
   is
   begin
      if Going = Forward then
         for J in Source'Range loop
            if not Is_White_Space (Source (J)) then
               return J;
            end if;
         end loop;

      else -- Going = Backward
         for J in reverse Source'Range loop
            if not Is_White_Space (Source (J)) then
               return J;
            end if;
         end loop;
      end if;

      --  Fall through if no match

      return 0;

   end ASIS_Index_Non_Blank;

   function ASIS_Index_Non_Blank
     (Source : Wide_String;
      Going  : Direction := Forward)
      return Natural
   is
   begin
      if Going = Forward then
         for J in Source'Range loop
            if not Is_White_Space (Source (J)) then
               return J;
            end if;
         end loop;

      else -- Going = Backward
         for J in reverse Source'Range loop
            if not Is_White_Space (Source (J)) then
               return J;
            end if;
         end loop;
      end if;

      --  Fall through if no match

      return 0;

   end ASIS_Index_Non_Blank;

   ---------------
   -- ASIS_Trim --
   ---------------

   function ASIS_Trim (Source : String) return String is
      Low, High : Integer := 0;
   begin

      for J in Source'Range loop

         if not Is_White_Space (Source (J)) then
            Low := J;
            exit;
         end if;

      end loop;

      if Low = 0 then
         --  Only white spaces, so
         return "";
      end if;

      for J in reverse Source'Range loop

         if not Is_White_Space (Source (J)) then
            High := J;
            exit;
         end if;

      end loop;

      return Source (Low .. High);
   end ASIS_Trim;

   function ASIS_Trim (Source : Wide_String) return Wide_String is
      Low, High : Integer := 0;
   begin

      for J in Source'Range loop

         if not Is_White_Space (Source (J)) then
            Low := J;
            exit;
         end if;

      end loop;

      if Low = 0 then
         --  Only white spaces, so
         return "";
      end if;

      for J in reverse Source'Range loop

         if not Is_White_Space (Source (J)) then
            High := J;
            exit;
         end if;

      end loop;

      return Source (Low .. High);
   end ASIS_Trim;

   ------------------------------
   -- Get_Nat_Switch_Parameter --
   ------------------------------

   function Get_Nat_Switch_Parameter (Val : String) return Natural is
      Result : Natural := 0;
   begin
      Result := Natural'Value (Val);

      return Result;
   exception
      when Constraint_Error =>
         Error ("wrong switch parameter " & Val);
         raise Parameter_Error;
   end Get_Nat_Switch_Parameter;

   -----------
   -- Image --
   -----------

   function Image (I : Integer) return String is
   begin
      return Ada.Strings.Fixed.Trim (Integer'Image (I), Ada.Strings.Both);
   end Image;

   -----------------
   -- Is_Ada_Name --
   -----------------

   function Is_Ada_Name (S : Wide_String) return Boolean is
      Result : Boolean := Is_Identifier (S);
   begin

      if not Result and then S /= "" then

         declare
            S_Last     : constant Natural := S'Last;
            Word_Start :          Natural := S'First;
            Next_Dot   :          Natural;

            procedure Set_Next_Dot;
            --  Sets Next_Dot pointing to the next dot character that follows
            --  Word_Start. Sets Next_Dot to Word_Start if Word_Start points to
            --  dot. Sets Next_Dot to S_Last + 1if there is no dot character
            --  after Word_Start.

            procedure Set_Next_Dot is
            begin
               Next_Dot := S_Last + 1;

               for J in Word_Start .. S_Last loop

                  if S (J) = '.' then
                     Next_Dot := J;
                     exit;
                  end if;

               end loop;

            end Set_Next_Dot;

         begin
            Result := True;
            Set_Next_Dot;

            while Word_Start <= S_Last loop

               if not Is_Identifier (S (Word_Start .. Next_Dot - 1)) then
                  Result := False;
                  exit;
               end if;

               Word_Start := Next_Dot + 1;
               Set_Next_Dot;
            end loop;

            if S (S_Last) = '.' then
               Result := False;
            end if;

         end;

      end if;

      return Result;
   end Is_Ada_Name;

   -------------------
   -- Is_Identifier --
   -------------------

   function Is_Identifier (S : Wide_String) return Boolean is
      Result : Boolean := False;
   begin

      if S'Length > 0
        and then
         Ada.Wide_Characters.Unicode.Is_Letter (S (S'First))
      then
         Result := Is_Identifier_Suffix (S (S'First + 1 .. S'Last));
      end if;

      return Result;
   end Is_Identifier;

   --------------------------
   -- Is_Identifier_Prefix --
   --------------------------

   function Is_Identifier_Prefix (Prefix : Wide_String) return Boolean is
      Result                  : Boolean := True;
      Last_Char_Was_Underline : Boolean := False;
   begin

      if Prefix'Length = 0 then
         return True;
      end if;

      if Prefix (Prefix'First) = '_' or else
         Ada.Wide_Characters.Unicode.Is_Digit (Prefix (Prefix'First))
      then
         return False;
      end if;

      for J in Prefix'Range loop

         if Ada.Wide_Characters.Unicode.Is_Letter (Prefix (J))
           or else
            Ada.Wide_Characters.Unicode.Is_Digit (Prefix (J))
         then
            Last_Char_Was_Underline := False;
         elsif Prefix (J) = '_' then

            if Last_Char_Was_Underline then
               Result := False;
               exit;
            else
               Last_Char_Was_Underline := True;
            end if;
         else
            Result := False;
            exit;
         end if;

      end loop;

      return Result;
   end Is_Identifier_Prefix;

   --------------------------
   -- Is_Identifier_Suffix --
   --------------------------

   function Is_Identifier_Suffix (Suffix : Wide_String) return Boolean is
      Result                  : Boolean := True;
      Last_Char_Was_Underline : Boolean := False;
   begin

      if Suffix'Length = 0 then
         return True;
      end if;

      for J in Suffix'Range loop

         if Ada.Wide_Characters.Unicode.Is_Letter (Suffix (J))
           or else
            Ada.Wide_Characters.Unicode.Is_Digit (Suffix (J))
         then
            Last_Char_Was_Underline := False;
         elsif Suffix (J) = '_' then

            if Last_Char_Was_Underline then
               Result := False;
               exit;
            else
               Last_Char_Was_Underline := True;
            end if;
         else
            Result := False;
            exit;
         end if;

      end loop;

      if Result then
         Result := Suffix (Suffix'Last) /= '_';
      end if;

      return Result;
   end Is_Identifier_Suffix;

   --------------------
   -- Is_White_Space --
   --------------------

   function Is_White_Space (Ch : Character) return Boolean is
   begin
      return Ch = ' ' or else Ch = ASCII.HT;
   end Is_White_Space;

   function Is_White_Space (WCh : Wide_Character) return Boolean is
   begin
      return WCh = ' ' or else To_Character (WCh) = ASCII.HT;
   end Is_White_Space;

   ---------------------------------
   -- Non_Case_Sensitive_Is_Equal --
   ---------------------------------

   function Non_Case_Sensitive_Is_Equal
     (Left, Right : String)
      return        Boolean
   is
      Result : Boolean := False;
   begin

      if Left'Length = Right'Length then
         Result := To_Lower (Left) = To_Lower (Right);
      end if;

      return Result;
   end Non_Case_Sensitive_Is_Equal;

   ---------------------
   -- Parse_File_List --
   ---------------------

   procedure Parse_File_List (File_List_Name : String) is
      Arg_File         : File_Type;
      File_Name_Buffer : String (1 .. 16 * 1024);
      File_Name_Len    : Natural := 0;
      Next_Ch          : Character;
      End_Of_Line      : Boolean;
      Tmp_Str          : String_Access;

      function Get_File_Name return String;
      --  Reads from Par_File_Name the name of the next file (the file to read
      --  from should exist and be opened). Returns an empty string if there is
      --  no file names in Par_File_Name any more

      function Get_File_Name return String is
      begin
         File_Name_Len := 0;

         if not End_Of_File (Arg_File) then
            Get (Arg_File, Next_Ch);

            while Is_White_Space (Next_Ch)
               or else
                  Next_Ch = ASCII.LF
               or else
                  Next_Ch = ASCII.CR
            loop
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
               while not (Is_White_Space (Next_Ch)
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
            end if;

         end if;

         return File_Name_Buffer (1 .. File_Name_Len);
      end Get_File_Name;

   begin
      Open (Arg_File, In_File, File_List_Name);
      Tmp_Str := new String'(Get_File_Name);

      while Tmp_Str.all /= "" loop
         Process_File (Tmp_Str.all);
         Free (Tmp_Str);
         Tmp_Str := new String'(Get_File_Name);
      end loop;

      Free (Tmp_Str);
   end Parse_File_List;

   -----------------
   -- Proper_Case --
   -----------------

   function Proper_Case (S : String) return String is
      Result           : String  := S;
      After_Underscore : Boolean := False;
   begin
      Result (Result'First) := To_Upper (Result (Result'First));

      for J in Result'First + 1 .. Result'Last loop

         if Result (J) = '_' then
            After_Underscore := True;
         else

            if After_Underscore then
               Result (J)       := To_Upper (Result (J));
               After_Underscore := False;
            else
               Result (J) := To_Lower (Result (J));
            end if;

         end if;

      end loop;

      return Result;
   end Proper_Case;

   -------------------
   -- Remove_Spaces --
   -------------------

   function Remove_Spaces (S : String) return String is
      Result   : String (1 .. S'Length);
      Res_Last : Natural := 0;
   begin
      for J in S'Range loop
         if not Is_White_Space (S (J)) then
            Res_Last := Res_Last + 1;
            Result (Res_Last) := S (J);
         end if;
      end loop;

      return Result (1 .. Res_Last);
   end Remove_Spaces;

   ------------------------------
   -- Simple_String_Dictionary --
   ------------------------------

   package body Simple_String_Dictionary is

      package Dictionaries is new Ada.Containers.Indefinite_Hashed_Sets
        (Element_Type        => String,
         Hash                => Ada.Strings.Hash,
         Equivalent_Elements => Non_Case_Sensitive_Is_Equal,
         "="                 => Non_Case_Sensitive_Is_Equal);

      Dictionary : Dictionaries.Set;

      Iterator : Dictionaries.Cursor := Dictionaries.No_Element;

      -----------------------
      -- Add_To_Dictionary --
      -----------------------

      procedure Add_To_Dictionary (S : String) is
      begin
         Dictionaries.Include (Dictionary, S);
      end Add_To_Dictionary;

      ------------
      --  Clear --
      ------------

      procedure Clear is
      begin
         Dictionaries.Clear (Dictionary);
      end Clear;

      ----------
      -- Done --
      ----------

      function Done return Boolean is
      begin
         return not Dictionaries.Has_Element (Iterator);
      end Done;

      --------------
      -- Is_Empty --
      --------------

      function Is_Empty return Boolean is
      begin
         return Dictionaries.Is_Empty  (Dictionary);
      end Is_Empty;

      ----------------------
      -- Is_In_Dictionary --
      ----------------------

      function Is_In_Dictionary (S : String) return Boolean is
      begin
         return Dictionaries.Contains (Dictionary, S);
      end Is_In_Dictionary;

      ----------------
      -- Next_Entry --
      ----------------

      function Next_Entry return String is
         Result : constant String := Dictionaries.Element (Iterator);
      begin
         Iterator := Dictionaries.Next (Iterator);
         return Result;
      end Next_Entry;

      ----------------------
      -- Print_Dictionary --
      ----------------------

      procedure Print_Dictionary is
         Next_Item : Dictionaries.Cursor;
      begin
         Put_Line
            (Standard_Error, "Content of dictionary " & Dictionary_Name);

         if Is_Empty then
            Put_Line (Standard_Error, "Empty");
         else
            Next_Item := Dictionaries.First (Dictionary);

            while Dictionaries.Has_Element (Next_Item) loop
               Put      (Standard_Error, ">>");
               Put      (Standard_Error, Dictionaries.Element (Next_Item));
               Put_Line (Standard_Error, "<<");
               Next_Item := Dictionaries.Next (Next_Item);
            end loop;

         end if;

      end Print_Dictionary;

      ----------------------------
      -- Remove_From_Dictionary --
      ----------------------------

      procedure Remove_From_Dictionary (S : String) is
      begin
         Dictionaries.Exclude (Dictionary, S);
      end Remove_From_Dictionary;

      --------------------
      -- Reset_Iterator --
      --------------------

      procedure Reset_Iterator is
      begin
         Iterator := Dictionaries.First (Dictionary);
      end Reset_Iterator;

   end Simple_String_Dictionary;

   ---------------
   -- Lock_File --
   ---------------

   procedure Lock_File (Lock_File_Name : Lock_Files.Path_Name) is
   begin
      Lock_Files.Lock_File
        (Lock_File_Name, Wait => 0.1, Retries => 2 * 60 * 10);
      --  Retry for 2 minutes, every 100 milliseconds.
   end Lock_File;

   procedure Unlock_File (Lock_File_Name : Lock_Files.Path_Name) is
   begin
      Lock_Files.Unlock_File (Lock_File_Name);
   end Unlock_File;

   -----------------------
   -- String_Hash_Table --
   -----------------------

   package body String_Hash_Table is

      ----------
      -- Hash --
      ----------

      function Hash (Name : String) return Hash_Index_Type is
         subtype Int_1_12 is Int range 1 .. 12;
         --  Used to avoid when others on case jump below

         Even_Name_Len : Integer;
         --  Last even numbered position (used for >12 case)

         --  We take one-to-one the code of the Hash function from Namet
         --  (namet.adb, rev. 1.90). Namet.Hash function works on the name
         --  buffer defined in Namet. We simulate this buffer by defining the
         --  following variables (note converting the argument string to lower
         --  case before computing the hash value):

         Name_Buffer : constant String  := To_Lower (Name);
         --  Note, that out Has function is not case-sensitive!

         Name_Len    : constant Natural := Name_Buffer'Last;
      begin
         --  Special test for 12 (rather than counting on a when others for the
         --  case statement below) avoids some Ada compilers converting the
         --  case statement into successive jumps.

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

         --  For the cases of 1-12 characters, all characters participate in
         --  the hash. The positioning is randomized, with the bias that
         --  characters later on participate fully (i.e. are added towards the
         --  right side).

         case Int_1_12 (Name_Len) is
            when 1 =>
               return Character'Pos (Name_Buffer (1));

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

   end String_Hash_Table;

   -------------------
   -- To_Lower_Case --
   -------------------

   function To_Lower_Case (S : Wide_String) return Wide_String is
      Result : Wide_String (S'Range);
   begin
      for J in Result'Range loop
         Result (J) := Ada.Wide_Characters.Unicode.To_Lower_Case (S (J));
      end loop;

      return Result;
   end To_Lower_Case;

   -------------------
   -- To_Upper_Case --
   -------------------

   function To_Upper_Case (S : Wide_String) return Wide_String is
      Result : Wide_String (S'Range);
   begin
      for J in Result'Range loop
         Result (J) := Ada.Wide_Characters.Unicode.To_Upper_Case (S (J));
      end loop;

      return Result;
   end To_Upper_Case;

end ASIS_UL.Misc;
