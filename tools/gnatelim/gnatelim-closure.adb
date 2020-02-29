------------------------------------------------------------------------------
--                                                                          --
--                           GNATELIM COMPONENTS                            --
--                                                                          --
--                     G N A T E L I M .C L O S U R E                       --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                    Copyright (C) 2009-2018, AdaCore                      --
--                                                                          --
-- GNATELIM  is  free software;  you can  redistribute it and/or  modify it --
-- under the terms of the  GNU  General Public License  as published by the --
-- Free Software Foundation; either version 3 or (at your option) any later --
-- version. GNATELIM is distributed in the hope that it will be useful, but --
-- WITHOUT ANY WARRANTY; without even the implied warranty of  MERCHANTABI- --
-- LITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public Li- --
-- cense for  more details.  You should  have  received  a copy of the  GNU --
-- General Public License distributed with GNAT; see file COPYING3. If not, --
-- go to http://www.gnu.org/licenses for a complete copy of the license.    --
--                                                                          --
-- The original version  of  Gnatelim  was developed by  Alain  Le  Guennec --
-- It is now maintained by AdaCore (http://www.adacore.com)                 --
--                                                                          --
------------------------------------------------------------------------------

--  This package contains routines that compute (and process, if needed)
--  closures of the main unit.

with Ada.Strings;                     use Ada.Strings;
with Ada.Strings.Fixed;               use Ada.Strings.Fixed;
with Ada.Text_IO;                     use Ada.Text_IO;

with GNAT.Directory_Operations;       use GNAT.Directory_Operations;
with GNAT.OS_Lib;                     use GNAT.OS_Lib;
with GNAT.Table;

--  with Asis.Implementation;

with ASIS_UL.Common;                  use ASIS_UL.Common;
with ASIS_UL.Compiler_Options;        use ASIS_UL.Compiler_Options;
with ASIS_UL.Debug;                   use ASIS_UL.Debug;
with ASIS_UL.Environment;             use ASIS_UL.Environment;
with ASIS_UL.Options;                 use ASIS_UL.Options;
with ASIS_UL.Output;                  use ASIS_UL.Output;
with ASIS_UL.Projects;                use ASIS_UL.Projects;
with ASIS_UL.Source_Table;            use ASIS_UL.Source_Table;
with ASIS_UL.Source_Table.Processing; use ASIS_UL.Source_Table.Processing;
--  with ASIS_UL.Source_Table.Processing; use ASIS_UL.Source_Table.Processing;

with Gnatelim.Options;
with Gnatelim.Projects;

package body Gnatelim.Closure is

   Program_Output_File : File_Type;
   --  File used to redirect the program output into.

   Max_Str_Len : constant Positive := 1024;
   Str_Buff    : String (1 .. Max_Str_Len);
   Str_Len     : Natural;

   package Tool_Switches is new GNAT.Table (
      Table_Component_Type => String_Access,
      Table_Index_Type     => Natural,
      Table_Low_Bound      => 1,
      Table_Initial        => 20,
      Table_Increment      => 100,
      Table_Name           => "Tool options");
   --  Used to compose a list of switches to call a tool

   -----------------------
   -- Local subprograms --
   -----------------------

   procedure Get_Next_Source (Source : in out String_Access);
   pragma Unreferenced (Get_Next_Source);
   --  Computes the name of the next source to compile using the call to
   --  'gnatmake -n ...' with the name of the main unit as the argument. If
   --  there is no source to compile any more, returns null;

   procedure Read_Sources_From_Binder_Output (Success : in out Boolean);
   --  Calls the binder for the main unit with '-R' option to get the list of
   --  source making up the main unit closure. If this attempt is successful,
   --  tries to read these sources into the source file table. Set the Success
   --  parameter ON if all these actions are successful, otherwise success
   --  is set to False.

   ---------------------
   -- Get_Next_Source --
   ---------------------

   procedure Get_Next_Source (Source : in out String_Access) is
   begin
      Free (Source);

      ASIS_UL.Output.Error
        ("gnatelim can only be called after a full successful build");
      raise ASIS_UL.Common.Fatal_Error;
   end Get_Next_Source;

   ---------------------
   -- Process_Closure --
   ---------------------

   procedure Process_Closure is
   begin
      if not Gnatelim.Options.Gnatelim_Prj.Is_Specified then
         Error ("can only be called after a full successful build");
         Error_No_Tool_Name ("cannot compute closure if no project set");
         raise ASIS_UL.Common.Fatal_Error;
      end if;

      Gnatelim.Projects.Closure_Setup (Gnatelim.Options.Gnatelim_Prj);

      Change_Dir (Tool_Temp_Dir.all & Directory_Separator &
                  Closure_Object_Subdir.all);

      Gnatelim.Projects.Add_Main
        (Gnatelim.Options.Gnatelim_Prj,
         ASIS_UL.Options.Main_Subprogram_Name.all);

      if Debug_Flag_U then
         Closure_Debug_Image_Mains;
      end if;

      loop
         Process_Sources (Keep_ALI => True);

         exit when Gnatelim.Projects.Closure_Complete
                      (Gnatelim.Options.Gnatelim_Prj);
      end loop;

      Change_Dir (Tool_Temp_Dir.all);

      Gnatelim.Projects.Closure_Clean_Up (Gnatelim.Options.Gnatelim_Prj);

   exception
      when others =>
         Change_Dir (Tool_Temp_Dir.all);
         Gnatelim.Projects.Closure_Clean_Up (Gnatelim.Options.Gnatelim_Prj);
         raise;
   end Process_Closure;

   -------------------------------------
   -- Read_Sources_From_Binder_Output --
   -------------------------------------

   procedure Read_Sources_From_Binder_Output (Success : in out Boolean) is
      Command : String_Access;
      Arg     : String_Access;
      Tmp     : String_Access;

      Dot_Idx : Natural := 0;

      Return_Code : Integer;

      Bind_Out_File_Name : constant String := Tool_Temp_Dir.all & ".bind_out";
      Tmp_Success        : Boolean;
      pragma Unreferenced (Tmp_Success);

   begin
      if Gnatelim.Options.Gnatelim_Prj.Is_Specified then

         ASIS_UL.Projects.Store_Main_Unit
           (Unit_Name => Main_Subprogram_Name.all);

         Gnatelim.Projects.Store_Files_From_Closure
           (Gnatelim.Options.Gnatelim_Prj,
            Success);

         if Success then
            Read_Args_From_Temp_Storage
              (Duplication_Report => False,
               Arg_Project        => Gnatelim.Options.Gnatelim_Prj);
         end if;
      else

         --  Define the name of the command to call:
         Arg     := Locate_Exec_On_Path (Gcc_To_Call.all);
         Dot_Idx := Index (Arg.all, "gcc", Backward) - 1;
         Command := new String'(Arg.all (Arg'First .. Dot_Idx));

         Free (Arg);
         Arg := new String'(Command.all & "gnatbind");
         Free (Command);
         Command := new String'(Arg.all);
         Free (Arg);

         --  Compose binder parameters
         Tool_Switches.Init;

         if Get_RTS_Path /= "" then
            Tool_Switches.Append (new String'("--RTS=" & Get_RTS_Path));
         end if;

         Tool_Switches.Append (new String'("-R"));

         Tool_Switches.Append (new String'("-ws"));

         --  If we are not using a project file, we have to provide the
         --  compiler-I options, if any. When doing this, we also create a
         --  search path for locating sources that we expect to get from the
         --  binder:

         --  Set binder argument:
         Arg := new String'(Main_Subprogram_Name.all);

         for J in reverse Arg'Range loop

            if Arg (J) = '.' then
               Dot_Idx := J - 1;
               exit;
            elsif Arg (J) = Directory_Separator then
               exit;
            end if;

         end loop;

         if Dot_Idx > 0 then
            Tmp := new String'(Arg.all (Arg'First .. Dot_Idx));
            Free (Arg);
            Arg := new String'(Tmp.all);
            Free (Tmp);
         end if;

         --  Call binder

         if Debug_Flag_C then
            Info_No_EOL (Command.all);
            Info_No_EOL (" ");

            for J in 1 .. Tool_Switches.Last loop
               Info_No_EOL (Tool_Switches.Table (J).all);
               Info_No_EOL (" ");
            end loop;

            Info (Arg.all);

         end if;

         Spawn
           (Program_Name => Command.all,
            Args         => String_List
              (Tool_Switches.Table (1 .. Tool_Switches.Last)) &
               Arg,
            Output_File  => Bind_Out_File_Name,
            Success      => Success,
            Return_Code  => Return_Code,
            Err_To_Out   => True);

         --  Read source files

         if Success then
            Open (Program_Output_File, In_File, Bind_Out_File_Name);

            --  OK

            if Return_Code = 0 then
               Skip_Line (Program_Output_File);    --  ???
               Skip_Line (Program_Output_File);    --  ???

               while not End_Of_File (Program_Output_File) loop
                  Get_Line (Program_Output_File, Str_Buff, Str_Len);

                  Add_Source_To_Process
                    (Fname => Trim (Str_Buff (1 .. Str_Len), Both),
                     Duplication_Report => False,
                     Arg_Project => Gnatelim.Options.Gnatelim_Prj);

               end loop;

            --  Not OK. Print out whatever error messages were produced by the
            --  binder. This is the case where Get_Next_Source is called, which
            --  will print an error message and raise Fatal_Error.

            else
               ASIS_UL.Output.Error ("gnatbind result =" & Return_Code'Img);

               while not End_Of_File (Program_Output_File) loop
                  Get_Line (Program_Output_File, Str_Buff, Str_Len);
                  Put_Line (Str_Buff (1 .. Str_Len));
               end loop;

               Success := False;
            end if;

            Close (Program_Output_File);
         end if;

         --  Clean-up
         Free (Command);
         Free (Arg);

         if not Debug_Flag_N then
            Delete_File (Bind_Out_File_Name, Tmp_Success);
         end if;

      end if;

      Total_Sources := Natural (Last_Source);
      Sources_Left  := Total_Sources;

   exception
      when others =>
         if Is_Open (Program_Output_File) then
            Close (Program_Output_File);
         end if;

      if not Debug_Flag_N then
         Delete_File (Bind_Out_File_Name, Tmp_Success);
      end if;

         raise;
   end Read_Sources_From_Binder_Output;

   --------------------------------
   -- Try_Get_Sources_From_Build --
   --------------------------------

   procedure Try_Get_Sources_From_Build (Success : in out Boolean) is
   begin
      Change_Dir (Tool_Current_Dir.all);

      Read_Sources_From_Binder_Output (Success);

      Change_Dir (Tool_Temp_Dir.all);
   end Try_Get_Sources_From_Build;

end Gnatelim.Closure;
