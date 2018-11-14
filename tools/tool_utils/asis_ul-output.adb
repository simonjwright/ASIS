------------------------------------------------------------------------------
--                                                                          --
--                  COMMON ASIS TOOLS COMPONENTS LIBRARY                    --
--                                                                          --
--                       A S I S _ U L . O U T P U T                        --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                    Copyright (C) 2004-2018, AdaCore                      --
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

with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Finalization;
with Ada.Strings;             use Ada.Strings;
with Ada.Strings.Fixed;       use Ada.Strings.Fixed;
with Ada.Text_IO;             use Ada.Text_IO;

with GNAT.OS_Lib;             use GNAT.OS_Lib;

with Asis.Extensions.Strings; use Asis.Extensions.Strings;
with Asis.Implementation;     use Asis.Implementation;

with Gnatvsn;

with ASIS_UL.Common;          use ASIS_UL.Common;
with ASIS_UL.Misc;            use ASIS_UL.Misc;
with ASIS_UL.Options;         use ASIS_UL.Options;

package body ASIS_UL.Output is

   Report_File_Name     : String_Access;
   XML_Report_File_Name : String_Access;
   Log_File_Name        : String_Access;
   Pipe_Mode            : Boolean         := False;
   Indent_String        : constant String := "   ";
   --  Variables that set the properties of the tool report and log files

   XML_Report_File : File_Type;
   Report_File     : File_Type;
   Log_File        : File_Type;

   Report_Locked     : Boolean := False;
   XML_Report_Locked : Boolean := False;
   --  True if Lock_File has been called, but not Unlock_File

   function Report_Lock_File_Name return String is
      (Report_File_Name.all & ".lockREP");

   function XML_Report_Lock_File_Name return String is
      (XML_Report_File_Name.all & ".lockXML");

   procedure Set_Report_File;
   procedure Set_XML_Report_File;
   --  Creates and/or opens the tool text/XML report file

   procedure Close_Report_File;
   procedure Close_XML_Report_File;
   --  Closes text/XML report file.

   --------------------
   -- Close_Log_File --
   --------------------

   procedure Close_Log_File is
   begin

      if Log_Mode then
         Close (Log_File);
         Log_Mode := False;
         Free (Log_File_Name);
      end if;

   end Close_Log_File;

   -----------------------
   -- Close_Report_File --
   -----------------------

   procedure Close_Report_File is
   begin
      --  This can be called on unhandled exceptions when we don't know the
      --  state of the Report_File and Report_Lock_File_Name, so we take care
      --  not to blow up.

      if not Pipe_Mode then
         if Is_Open (Report_File) then
            Close (Report_File);
         end if;

         if Report_Locked then
            pragma Assert (Mimic_gcc and Outer_Parallel);
            Report_Locked := False;
            Unlock_File (Report_Lock_File_Name);
         end if;
      end if;

   end Close_Report_File;

   ---------------------------
   -- Close_XML_Report_File --
   ---------------------------

   procedure Close_XML_Report_File is
   begin
      --  This can be called on unhandled exceptions when we don't know the
      --  state of the Report_File and Report_Lock_File_Name, so we take care
      --  not to blow up.

      if not Pipe_Mode then
         if Is_Open (XML_Report_File) then
            Close (XML_Report_File);
         end if;

         if XML_Report_Locked then
            pragma Assert (Mimic_gcc and Outer_Parallel);
            XML_Report_Locked := False;
            Unlock_File (XML_Report_Lock_File_Name);
         end if;
      end if;

   end Close_XML_Report_File;

   ------------------------
   -- Close_Report_Files --
   ------------------------

   procedure Close_Report_Files is
   begin
      pragma Assert (Text_Report_ON or else XML_Report_ON);

      if Text_Report_ON then
         Close_Report_File;
      end if;

      if XML_Report_ON then
         Close_XML_Report_File;
      end if;

   end Close_Report_Files;

   -----------
   -- Error --
   -----------

   procedure Error (Message : String) is
   begin
      Put (Standard_Error, Tool_Name.all & ": ");

      if Log_Mode and then Is_Open (Log_File) then
         Put (Log_File, Tool_Name.all & ": ");
      end if;

      Error_No_Tool_Name (Message);
   end Error;

   ------------------------
   -- Error_No_Tool_Name --
   ------------------------

   procedure Error_No_Tool_Name (Message : String) is
   begin
      Put_Line (Standard_Error, Message);

      if Log_Mode and then Is_Open (Log_File) then
         Put_Line (Log_File, Message);
      end if;

   end Error_No_Tool_Name;

   -----------------------
   -- Get_Indent_String --
   -----------------------

   function Get_Indent_String return String is
   begin
      return Indent_String;
   end Get_Indent_String;

   ----------------
   -- Get_Number --
   ----------------

   function Get_Number return String is
      Report_File_Name : constant String := (if Text_Report_ON then
                                                Get_Report_File_Name
                                             else
                                                Get_XML_Report_File_Name);

      Idx_1, Idx_2 : Natural;
   begin
      if not Aggregated_Project then
         return "";
      end if;

      Idx_1 := Index (Report_File_Name, "_", Backward);
      Idx_2 := Index (Report_File_Name, ".", Backward);

      pragma Assert (Idx_1 > 0);
      pragma Assert (Idx_2 > 0);
      pragma Assert (Idx_1 < Idx_2 - 1);

      return Report_File_Name (Idx_1 .. Idx_2 - 1);
   end Get_Number;

   -------------------------
   -- Get_Out_File_Format --
   -------------------------

   function Get_Out_File_Format (Val : String) return Out_File_Formats is
   begin

      if Val = "dos" or else Val = "crlf" then
         return CRLF;
      elsif Val = "unix" or else Val = "lf" then
         return LF;
      else
         Error ("Unrecognized output file format " & Val);
         raise Parameter_Error;
      end if;

   end Get_Out_File_Format;

   --------------------------
   -- Get_Report_File_Name --
   --------------------------

   function Get_Report_File_Name return String is
   begin
      if Report_File_Name = null then
         return "";
      end if;

      pragma Assert
        (Report_File_Name.all = Normalize_Pathname (Report_File_Name.all));
      return Report_File_Name.all;
   end Get_Report_File_Name;

   --------------------------
   -- Get_XML_Report_File_Name --
   --------------------------

   function Get_XML_Report_File_Name return String is
   begin
      if XML_Report_File_Name = null then
         return "";
      end if;

      pragma Assert
        (XML_Report_File_Name.all =
           Normalize_Pathname (XML_Report_File_Name.all));
      return XML_Report_File_Name.all;
   end Get_XML_Report_File_Name;

   ----------
   -- Info --
   ----------

   procedure Info
     (Message  : String;
      Line_Len : Natural := 0;
      Spacing  : Natural := 0)
   is
   begin
      Info_No_EOL (Message, Line_Len, Spacing);
      New_Line (Current_Error);

      if Log_Mode and then Is_Open (Log_File) then
         New_Line (Log_File);
      end if;

   end Info;

   -----------------
   -- Info_No_EOL --
   -----------------

   procedure Info_No_EOL
     (Message  : String;
      Line_Len : Natural := 0;
      Spacing  : Natural := 0)
   is
      Start_Idx   : constant Natural := Message'First;
      End_Idx     :          Natural := Message'Last;
      Start_From  :          Positive;
   begin

      if Line_Len = 0
        or else
         End_Idx - Start_Idx + 1 <= Line_Len
      then
         Put (Current_Error, Message);

         if Log_Mode and then Is_Open (Log_File) then
            Put (Log_File, Message);
         end if;

      else
         --  Define which part of the Message can be placed into one line:
         while End_Idx >= Start_Idx
             and then
               not (Message (End_Idx) = ' '
                  and then
                    End_Idx - Start_Idx + 1 <= Line_Len)
         loop
            End_Idx := End_Idx - 1;
         end loop;

         if End_Idx < Start_Idx then
            --  Cannot split Message, so:
            Put (Current_Error, Message);

            if Log_Mode and then Is_Open (Log_File) then
               Put (Log_File, Message);
            end if;

         else
            --  Index of the beginning of the remaining part of Message
            Start_From := End_Idx + 1;

            --  Now move End_Idx to the left to skip spaces:

            while End_Idx >= Start_Idx
                 and then
                  Message (End_Idx) = ' '
            loop
               End_Idx := End_Idx - 1;
            end loop;

            Put (Current_Error, Message (Start_Idx .. End_Idx));

            if Log_Mode and then Is_Open (Log_File) then
               Put (Log_File, Message (Start_Idx .. End_Idx));
            end if;

            --  Skip spaces in the remaining part of the message, if any:
            End_Idx := Message'Last;

            while Start_From <= End_Idx
                 and then
                  Message (Start_From) = ' '
            loop
               Start_From := Start_From + 1;
            end loop;

            if Start_From <= End_Idx then
               New_Line (Current_Error);

               if Log_Mode and then Is_Open (Log_File) then
                  New_Line (Log_File);
               end if;

               Info_No_EOL
                 (Message  => Spacing * ' ' & Message (Start_From .. End_Idx),
                  Line_Len => Line_Len,
                  Spacing  => Spacing);
            end if;

         end if;

      end if;

   end Info_No_EOL;

   ------------------------
   -- Print_Tool_Version --
   ------------------------

   procedure Print_Tool_Version (Released_At : Positive) is
   begin
      Put (To_Upper (Tool_Name.all));
      Put (' ');
      Put (Gnatvsn.Gnat_Version_String);
      New_Line;

      Put ("Copyright (C) ");
      Put (Image (Released_At));
      Put ('-');
      Put (Gnatvsn.Current_Year);
      Put (", ");
      Put (Gnatvsn.Copyright_Holder);
      New_Line;
      Put (Gnatvsn.Gnat_Free_Software);
      New_Line;
   end Print_Tool_Version;

   ------------------------
   -- Print_Version_Info --
   ------------------------

   procedure Print_Version_Info (Released_At : Positive) is
   begin
      Info (Tool_Name.all & ' ' & Gnatvsn.Gnat_Version_String);

      Info_No_EOL ("Copyright ");

      if Image (Released_At) /= Gnatvsn.Current_Year then
         Info_No_EOL (Image (Released_At));
         Info_No_EOL ("-");
      end if;

      Info_No_EOL (Gnatvsn.Current_Year);
      Info        (", AdaCore.");
   end Print_Version_Info;

   ------------
   -- Report --
   ------------

   procedure Report
     (Message      : String;
      Indent_Level : Natural := 0)
   is
   begin
      Report_No_EOL (Message, Indent_Level);
      Report_EOL;
   end Report;

   ----------------
   -- XML_Report --
   ----------------

   procedure XML_Report
     (Message      : String;
      Indent_Level : Natural := 0)
   is
   begin
      XML_Report_No_EOL (Message, Indent_Level);
      XML_Report_EOL;
   end XML_Report;

   ----------------
   -- Report_EOL --
   ----------------

   procedure Report_EOL is
   begin

      if Pipe_Mode then
         New_Line;
      else
         New_Line (Report_File);
      end if;

   end Report_EOL;

   --------------------
   -- XML_Report_EOL --
   --------------------

   procedure XML_Report_EOL is
   begin

      if Pipe_Mode then
         New_Line;
      else
         New_Line (XML_Report_File);
      end if;

   end XML_Report_EOL;

   -------------------
   -- Report_No_EOL --
   -------------------

   procedure Report_No_EOL
     (Message      : String;
      Indent_Level : Natural := 0)
   is
   begin

      if Pipe_Mode then

         for J in 1 .. Indent_Level loop
            Put (Indent_String);
         end loop;

         Put (Message);

      else

         for J in 1 .. Indent_Level loop
            Put (Report_File, Indent_String);
         end loop;

         Put (Report_File, Message);

      end if;

   end Report_No_EOL;

   -----------------------
   -- XML_Report_No_EOL --
   -----------------------

   procedure XML_Report_No_EOL
     (Message      : String;
      Indent_Level : Natural := 0)
   is
   begin

      if Pipe_Mode then

         for J in 1 .. Indent_Level loop
            Put (Indent_String);
         end loop;

         Put (Message);

      else

         for J in 1 .. Indent_Level loop
            Put (XML_Report_File, Indent_String);
         end loop;

         Put (XML_Report_File, Message);

      end if;

   end XML_Report_No_EOL;

   -------------------------------------
   -- Report_Unhandled_ASIS_Exception --
   -------------------------------------

   procedure Report_Unhandled_ASIS_Exception (Ex : Exception_Occurrence) is
   begin
      Error ("ASIS exception (" & Exception_Name (Ex) & ") is raised");
      Error ("ASIS Error Status is " & Asis.Implementation.Status'Img);
      Error ("ASIS Diagnosis is " & To_String (Diagnosis));

      Set_Status;
   end Report_Unhandled_ASIS_Exception;

   --------------------------------
   -- Report_Unhandled_Exception --
   --------------------------------

   procedure Report_Unhandled_Exception (Ex : Exception_Occurrence) is
   begin
      Error (Exception_Information (Ex));
   end Report_Unhandled_Exception;

   ------------------
   -- Set_Log_File --
   ------------------

   procedure Set_Log_File is
   begin

      if Log_Mode then

         if Log_File_Name = null then
            Log_File_Name :=
              new String'(Get_Global_Report_Dir & Tool_Name.all & ".log");
         end if;

         if Is_Regular_File (Log_File_Name.all) then
            Open (Log_File, Out_File, Log_File_Name.all);
         else
            Create (Log_File, Out_File, Log_File_Name.all);
         end if;

      end if;

   end Set_Log_File;

   -----------------------
   -- Set_Log_File_Name --
   -----------------------

   procedure Set_Log_File_Name (Fname : String) is
   begin
      Free (Log_File_Name);

      if Fname /= "" then
         Log_File_Name := new String'(Fname);
      end if;
   end Set_Log_File_Name;

   -------------------
   -- Set_Pipe_Mode --
   -------------------

   procedure Set_Pipe_Mode (On : Boolean := True) is
   begin
      if Text_Report_ON and then XML_Report_ON then
         raise Parameter_Error;
      end if;

      Pipe_Mode := On;
   end Set_Pipe_Mode;

   ---------------------
   -- Set_Report_File --
   ---------------------

   procedure Set_Report_File is
      Mode : constant File_Mode :=
        (if Mimic_gcc then Append_File else Out_File);
      Ignored : Boolean;
   begin

      if Pipe_Mode then
         if Report_File_Name /= null then
            Error ("pipe mode and output file cannot be set together");
            raise Fatal_Error;
         end if;
      else

         if Report_File_Name = null then
            Report_File_Name :=
              new String'(Get_Global_Report_Dir & Tool_Name.all & ".out");
         end if;

         Report_File_Name :=
           new String'(Normalize_Pathname (Report_File_Name.all));

         if not Mimic_gcc then
            --  Delete the lock file if it exists, in which case it is a stale
            --  lock file from a previous run. Failure is expected, and
            --  therefore ignored.

            GNAT.OS_Lib.Delete_File (Report_Lock_File_Name, Ignored);
         end if;

         if Is_Regular_File (Report_File_Name.all) then
            if Outer_Parallel then
               pragma Assert (Mimic_gcc);
               Lock_File (Report_Lock_File_Name);
               Report_Locked := True;
            end if;

            Open (Report_File, Mode, Report_File_Name.all);
         else
            pragma Assert (not Mimic_gcc);
            --  If Mimic_gcc is True, then the file was already created by the
            --  outer tool invocation.

            Create (Report_File, Out_File, Report_File_Name.all);
         end if;

      end if;

   exception
      when Lock_Error =>
         ASIS_UL.Output.Error ("cannot create " & Report_Lock_File_Name);
         ASIS_UL.Output.Error ("remove it by hand if stale");
         raise;
      when Status_Error =>
         Error ("can not open the report file, the file may be in use");
         raise Fatal_Error;
      when Fatal_Error =>
         null;
      when others =>
         Error ("can not open the report file: " & Report_File_Name.all);
         raise Fatal_Error;
   end Set_Report_File;

   -------------------------
   -- Set_XML_Report_File --
   -------------------------

   procedure Set_XML_Report_File is
      Mode : constant File_Mode :=
        (if Mimic_gcc then Append_File else Out_File);
      Ignored : Boolean;
   begin

      if Pipe_Mode then
         if XML_Report_File_Name /= null then
            Error ("pipe mode and output file cannot be set together");
            raise Fatal_Error;
         end if;
      else

         if XML_Report_File_Name = null then
            XML_Report_File_Name :=
              new String'(Get_Global_Report_Dir & Tool_Name.all & ".xml");
         end if;

         XML_Report_File_Name :=
           new String'(Normalize_Pathname (XML_Report_File_Name.all));

         if not Mimic_gcc then
            --  Delete the lock file if it exists, in which case it is a stale
            --  lock file from a previous run. Failure is expected, and
            --  therefore ignored.

            GNAT.OS_Lib.Delete_File (XML_Report_Lock_File_Name, Ignored);
         end if;

         if Is_Regular_File (XML_Report_File_Name.all) then
            if Outer_Parallel then
               pragma Assert (Mimic_gcc);
               Lock_File (XML_Report_Lock_File_Name);
               XML_Report_Locked := True;
            end if;

            Open (XML_Report_File, Mode, XML_Report_File_Name.all);
         else
            pragma Assert (not Mimic_gcc);
            --  If Mimic_gcc is True, then the file was already created by the
            --  outer tool invocation.

            Create (XML_Report_File, Out_File, XML_Report_File_Name.all);
         end if;

      end if;

   exception
      when Lock_Error =>
         ASIS_UL.Output.Error ("cannot create " & XML_Report_Lock_File_Name);
         ASIS_UL.Output.Error ("remove it by hand if stale");
         raise;
      when Status_Error =>
         Error ("can not open the report file, the file may be in use");
         raise Fatal_Error;
      when Fatal_Error =>
         null;
      when others =>
         Error ("can not open the report file: " & XML_Report_File_Name.all);
         raise Fatal_Error;
   end Set_XML_Report_File;

   ----------------------
   -- Set_Report_Files --
   ----------------------

   procedure Set_Report_Files is
   begin
      pragma Assert (Text_Report_ON or else XML_Report_ON);

      if Text_Report_ON then
         Set_Report_File;
      end if;

      if XML_Report_ON then
         Set_XML_Report_File;
      end if;

   end Set_Report_Files;

   --------------------------
   -- Set_Report_File_Name --
   --------------------------

   procedure Set_Report_File_Name (Fname : String) is
   begin
      Free (Report_File_Name);

      if Fname /= "" then
         Report_File_Name := new String'(Fname);
      end if;
   end Set_Report_File_Name;

   --------------------------
   -- Set_XML_Report_File_Name --
   --------------------------

   procedure Set_XML_Report_File_Name (Fname : String) is
   begin
      Free (XML_Report_File_Name);

      if Fname /= "" then
         XML_Report_File_Name := new String'(Fname);
      end if;
   end Set_XML_Report_File_Name;

   ----------------
   -- SLOC_Error --
   ----------------

   procedure SLOC_Error
     (Message : String;
      Elem    : Asis.Element)
   is
   begin
      SLOC_Error (Message, Build_GNAT_Location (Elem));
   end SLOC_Error;

   procedure SLOC_Error
     (Message : String;
      SLOC    : String)
   is
   begin
      Put (Standard_Error, SLOC & ": ");
      if Log_Mode and then Is_Open (Log_File) then
         Put (Log_File, SLOC & ": ");
      end if;

      Put (Standard_Error, Tool_Name.all & ": ");

      if Log_Mode and then Is_Open (Log_File) then
         Put (Log_File, Tool_Name.all & ": ");
      end if;

      Error_No_Tool_Name (Message);
   end SLOC_Error;

   -------------
   -- Warning --
   -------------

   procedure Warning (Message : String) is
   begin
      if Warning_Mode /= Quiet then
         Error (Message);
      end if;
   end Warning;

   --------------------------
   -- Warning_No_Tool_Name --
   --------------------------

   procedure Warning_No_Tool_Name (Message : String) is
   begin
      if Warning_Mode /= Quiet then
         Error_No_Tool_Name (Message);
      end if;
   end Warning_No_Tool_Name;

   ----------------

   --  We create a dummy object whose finalization calls Close_Report_File, so
   --  we don't leave stale lock files around even in case of unhandled
   --  exceptions.

   use Ada.Finalization;

   type Dummy_Type is new Limited_Controlled with null record;
   procedure Finalize (Ignore : in out Dummy_Type);
   procedure Finalize (Ignore : in out Dummy_Type) is
   begin
      Close_Report_File;
   end Finalize;

   Dummy : Dummy_Type;

end ASIS_UL.Output;
