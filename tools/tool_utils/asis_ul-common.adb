------------------------------------------------------------------------------
--                                                                          --
--                     ASIS UTILITY LIBRARY COMPONENTS                      --
--                                                                          --
--                       A S I S _ U L . C O M M O N                        --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                    Copyright (C) 2004-2017, AdaCore                      --
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
-- GNATCHECK is maintained by AdaCore (http://www.adacore.com).             --
--                                                                          --
------------------------------------------------------------------------------

pragma Ada_2012;

with Ada.Characters.Handling;  use Ada.Characters.Handling;
with Ada.Command_Line;         use Ada.Command_Line;
with Ada.Strings;              use Ada.Strings;
with Ada.Strings.Fixed;        use Ada.Strings.Fixed;
with Ada.Text_IO;              use Ada.Text_IO;

with GNAT.Directory_Operations;

with ASIS_UL.Output;           use ASIS_UL.Output;
with ASIS_UL.Compiler_Options; use ASIS_UL.Compiler_Options;

package body ASIS_UL.Common is

   -----------------------
   -- Local subprograms --
   -----------------------

   procedure Set_Tool_Name_And_Path;
   pragma Unreferenced (Set_Tool_Name_And_Path);
   --  Reads the tool name from the command line and sets Tool_Name. If the
   --  tool name contains directory information, adds the directory to the
   --  path.

   procedure Set_Tool_Name_And_Tool_Path;
   --  Reads the tool name from the command line and sets Tool_Name. If the
   --  tool name contains directory information, sets the directory where the
   --  tool is locates as the value or Tool_Dir

   Global_Report_Dir : String_Access := new String'("." & Directory_Separator);
   --  The name of the directory to place the global results into

   -------------------
   -- Detect_Target --
   -------------------

   function Detect_Target return String is
      Tgt_Last : constant Natural :=
        Index (Tool_Name.all, "-", Ada.Strings.Backward);
      AAMP_Idx : constant Natural := Index (Tool_Name.all, "gnaamp");
   begin

      if AAMP_Idx = Tool_Name'First then
         return "AAMP";
      elsif Tgt_Last > 0 then
         return Tool_Name (Tool_Name'First .. Tgt_Last - 1);
      else
         return "";
      end if;

   exception
      when others =>
         return "";

   end Detect_Target;

   ---------------------------
   -- Get_Global_Report_Dir --
   ---------------------------

   function Get_Global_Report_Dir return String is
   begin
      return Global_Report_Dir.all;
   end Get_Global_Report_Dir;

   --------------------------
   -- Process_Project_File --
   --------------------------

   procedure Process_Project_File (Project_File_Name : String) is
   begin
      --  pragma Assert (False, "???Process_Project_File is not used");
      --  Actually, it is still used by Ada2java. This should be replaced with
      --  proper project support as has been done for other ASIS tools.

      if Is_Regular_File (Project_File_Name) then
         Project_File_Obsolete :=
           new String'(Normalize_Pathname (Project_File_Name));

         if Project_Support_Type = No_Tmp_Project_File then
            Store_Option ("-P" & Project_File_Obsolete.all);
         end if;

      else
         Error ("the project file " & Project_File_Name & " not found");
         raise Parameter_Error;
      end if;

      Gcc_To_Call := Gnatmake_To_Call;

      if Gcc_To_Call /= null then
         Use_Gnatmake_To_Compile := True;
      else
         Error ("can not locate gnatmake to compile with a project file");
         raise Parameter_Error;
      end if;

      Use_Project_File_Obsolete := True;

   end Process_Project_File;

   ---------------------------
   -- Set_Global_Report_Dir --
   ---------------------------

   procedure Set_Global_Report_Dir (Dir : String) is
   begin
      Free (Global_Report_Dir);
      pragma Assert (Dir /= "");
      Global_Report_Dir := new String'(Dir & Directory_Separator);
   end Set_Global_Report_Dir;

   ----------------------------
   -- Set_Tool_Name_And_Path --
   ----------------------------

   procedure Set_Tool_Name_And_Path is
      Full_Tool_Name : constant String := Ada.Command_Line.Command_Name;
      Exe_Suffix     : String_Access   := Get_Executable_Suffix;
   begin
      Tool_Name :=
        new String'(To_Lower
                      (GNAT.Directory_Operations.Base_Name
                         (Full_Tool_Name, Suffix => Exe_Suffix.all)));

      for Index in reverse Full_Tool_Name'Range loop
         if Full_Tool_Name (Index) = Directory_Separator then
            declare
               Absolute_Dir : constant String :=
                                 Normalize_Pathname
                                  (Full_Tool_Name
                                     (Full_Tool_Name'First .. Index));

               PATH : constant String :=
                 Absolute_Dir & Path_Separator & Getenv ("PATH").all;

            begin
               Setenv ("PATH", PATH);
            end;

            exit;
         end if;
      end loop;

      Free (Exe_Suffix);
   end Set_Tool_Name_And_Path;

   ---------------------------------
   -- Set_Tool_Name_And_Tool_Path --
   ---------------------------------

   procedure Set_Tool_Name_And_Tool_Path is
      Full_Tool_Name : constant String := Ada.Command_Line.Command_Name;
      Exe_Suffix     : String_Access   := Get_Executable_Suffix;
      Idx            : Natural;
      Tmp            : String_Access   := new String'(Full_Tool_Name);
   begin
      Tool_Name :=
        new String'(To_Lower
                      (GNAT.Directory_Operations.Base_Name
                         (Full_Tool_Name, Suffix => Exe_Suffix.all)));

      Idx := Index (Tmp.all, (1 => Directory_Separator), Backward);

      if Idx = 0 then
         --  UNIX environment, no path is returned by Ada.Comand_Line
         Free (Tmp);

         Tmp := Locate_Exec_On_Path (Tool_Name.all);
         Idx := Index (Tmp.all, (1 => Directory_Separator), Backward);
      end if;

      if Idx /= 0 then
         Tool_Dir := new String'
           (Normalize_Pathname
             (Tmp (Tmp'First .. Idx - 1)));
      else
         Put_Line ("cannot compute the tool directory for " & Full_Tool_Name);
         raise Fatal_Error;
      end if;

      Free (Exe_Suffix);
      Free (Tmp);
   exception
      when others =>
         Put_Line ("Full_Tool_Name is " & Full_Tool_Name);
         Put_Line ("Idx is" & Idx'Img);
         Put_Line ("Tmp is " & Tmp.all);
         raise;
   end Set_Tool_Name_And_Tool_Path;

begin
   --  We have to set the tool name at the elaboration stage, before we start
   --  any processing. Error messages use tool name, so it shoul be in place
   --  before the first error message can be generated.
   Set_Tool_Name_And_Tool_Path;
end ASIS_UL.Common;
