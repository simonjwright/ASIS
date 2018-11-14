------------------------------------------------------------------------------
--                                                                          --
--                     ASIS UTILITY LIBRARY COMPONENTS                      --
--                                                                          --
--                  A S I S _ U L . E N V I R O N M E N T                   --
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

with Ada.Command_Line;
with Ada.Directories;             use Ada.Directories;
with Ada.Text_IO;                 use Ada.Text_IO;
with System.Multiprocessors;

with GNATCOLL.Projects;           use GNATCOLL.Projects;

with Asis.Ada_Environments;       use Asis.Ada_Environments;

with A4G.A_Debug;
with A4G.GNAT_Int;
with A4G.GNSA_Switch;

with ASIS_UL.Common;              use ASIS_UL.Common;
with ASIS_UL.Compiler_Options;    use ASIS_UL.Compiler_Options;
with ASIS_UL.Debug;               use ASIS_UL.Debug;
with ASIS_UL.Formatted_Output;
with ASIS_UL.Options;             use ASIS_UL.Options;
with ASIS_UL.Output;              use ASIS_UL.Output;
with ASIS_UL.Projects;            use ASIS_UL.Projects;
with ASIS_UL.Source_Table;        use ASIS_UL.Source_Table;

package body ASIS_UL.Environment is

   -----------------------
   -- Local subprograms --
   -----------------------

   pragma Warnings (Off, More_Arguments);
   --  At least gnatstub does not use this procedure, so we have to avoid
   --  warnings about unreferenced procedure

   procedure Check_Parameters;
   --  Checks that the tool settings are compatible with each other. All
   --  possible check are tool-specific, the corresponding subunit
   --  should be provided for each tool

   --  The two procedures Scan_Parameters (see below) and Check_Parameters (see
   --  above) - are a part of the Initialize procedure.  The important thing is
   --  that after finishing Initialize either the source table should contain
   --  at least one name of an existing file (in case the tool is based on
   --  ASIS_UL.Several_Files_Driver driver), or ASIS_UL.Common.Arg_File should
   --  point to an existing file (in case the tool is based on
   --  ASIS_UL.One_Arg_Driver driver procedure). File names in both cases
   --  should be full normalized names.
   --
   --  ASIS_UL.Compiler_Options.Arg_List should collect all the needed options
   --  to call gcc for tree creation

   procedure Tool_Specific_Initialization_1;
   procedure Tool_Specific_Initialization_2;
   --  Do the initialization actions that are specific for a tool. The first
   --  subprogram is called before reading the tool command-line parameters,
   --  the second - when the command-line parameters have just been read in and
   --  analyzed. If the tool needs any specific initialization actions, the
   --  corresponding subunits should be provided for these subprograms.

   function Builder_Command_Line return Argument_List;
   --  Called by the outer invocation of an ASIS tool when in Incremental_Mode.
   --  Returns a sequence of command-line arguments suitable for invoking the
   --  builder. See also the comments on ASIS_UL.Options.Incremental_Mode.

   function Get_Temp_Dir_Parent return String;
   --  If non-empty, points to the name of the directory in which to create the
   --  tool's temporary directory. This comes from the values of environment
   --  variables TMPDIR, TEMP or TMP (in this order of priority). If empty, the
   --  temporary directory is created in the current directory.

   procedure GNSA_GNATLS_Workaround;
   --  A temporary and very simple-minded workaround for GNSA mode. Currently
   --  for project file processing routines from GNATCOLL.Projects need
   --  calls to gnatls. To be sure that we have gnatls on the path we
   --  just appends asis-gnsa/bin to the value of the PATH environment
   --  variable.

   -------------------------
   -- Get_Temp_Dir_Parent --
   -------------------------

   function Get_Temp_Dir_Parent return String is
      Tmpdirs : String_List_Access :=
        new String_List'(1 => new String'("TMPDIR"),
                         2 => new String'("TEMP"),
                         3 => new String'("TMP"));
      Dir      : String_Access;
      Detected : Boolean := False;
   begin
      for J in Tmpdirs'Range loop
         Free (Dir);
         Dir := Getenv (Tmpdirs (J).all);

         if Dir.all /= ""
           and then Is_Absolute_Path (Dir.all)
           and then Is_Directory (Dir.all)
         then
            Detected := True;
            exit;
         end if;

      end loop;

      declare
         Result : constant String :=
           (if Detected then
               Normalize_Pathname (Dir.all)
            else
               "");
      begin
         Free (Dir);
         Free (Tmpdirs);
         return Result;
      end;

   end Get_Temp_Dir_Parent;

   Temp_Dir_Parent : constant String := Get_Temp_Dir_Parent;

   function Temp_Dir_Exists return Boolean is
      (Tool_Temp_Dir /= null);
   --  True if the Tool_Temp_Dir has been created, but not yet deleted by
   --  Clean_Up. Note that we create and delete the Tool_Temp_Dir multiple
   --  times per run.

   Tmpdir_Displayed : Boolean := False;
   --  True if the value of the TMPDIR environment variable has been displayed;
   --  we don't want to display it more than once per run.

   --------------------------
   -- Builder_Command_Line --
   --------------------------

   function Builder_Command_Line return Argument_List is
      pragma Assert (Incremental_Mode);

      Builder_Args, Inner_Args : String_Vector;
      --  Builder_Args are passed to the builder. Inner_Args are passed to the
      --  inner invocations of the ASIS tool by passing them to the builder
      --  after "-cargs".
      Cur : Positive := 1;
      In_Gnatcheck_Rules : Boolean := False;
      --  True if the loop below is in the "-rules" section

      This_Is_Gnatcheck : constant Boolean :=
        Has_Suffix (Tool_Name.all, Suffix => "gnatcheck");
      --  Flag for special-casing gnatcheck. "Suffix" instead of "=" in case
      --  it's a cross compiler.

      use Ada.Command_Line;

   begin
      --  Tell the builder to keep quiet

      if Debug_Flag_C then
         Append (Builder_Args, "-v");
      else
         Append (Builder_Args, "-q");
      end if;

      --  Tell the builder to pretend that the ASIS tool is the compiler, and
      --  which project-file package to use.

      Append (Builder_Args,
              String'("--compiler-subst=ada," &
                        Ada.Command_Line.Command_Name));

      declare
         Pkg_Name : constant String :=
           --  Package name (in project file) corresponding to the
           --  tool. Empty string if none.
           (if Has_Suffix (Tool_Name.all, Suffix => "gnat2xml")
              then "gnat2xml"
            elsif Has_Suffix (Tool_Name.all, Suffix => "gnatpp")
              then "pretty_printer"
            elsif Has_Suffix (Tool_Name.all, Suffix => "gnatcheck")
              then "check"
            elsif Has_Suffix (Tool_Name.all, Suffix => "gnatmetric")
              then "metrics"
            else raise Program_Error); -- other tools don't have --incremental
      begin
         Append (Builder_Args,
                 String'("--compiler-pkg-subst=" & Pkg_Name));
      end;

      --  Tell the builder to create necessary directories if they don't exist

      Append (Builder_Args, "-p");

      --  Tell the builder not to complain about missing object files. We are
      --  pretending that the ASIS tool is the compiler, but of course ASIS
      --  tools don't generate object files.

      Append (Builder_Args, "--no-object-check");

      --  Compile only

      Append (Builder_Args, "-c");

      --  Unique compilation, only compile the given files if any files are
      --  given.

      Append (Builder_Args, "-U");

      --  Tell the builder to place ALI files in a subdirectory of the object
      --  directory -- a different subdirectory for each tool. This is
      --  necessary because otherwise the ALI files would conflict with each
      --  other. For one thing, ASIS tools run the compiler (the real one) with
      --  different switches than normal builds, so we don't want to overwrite
      --  one kind of ALI file with the other. For another thing, just because
      --  the files generated by gnat2xml are up to date doesn't mean that the
      --  files generated by gnatcheck are up to date. So if the object
      --  directory is 'obj', normal builds put ALI files in obj, "gnat2xml
      --  --incremental" puts ALI files in obj/ALI-FILES-gnat2xml, and so on.

      Append (Builder_Args,
              String'("--subdirs=" & "ALI-FILES-" & Tool_Name.all));

      --  Don't bother with code in other languages.

      Append (Builder_Args, "--restricted-to-languages=ada");

      --  Inform the inner invocation where to find input files.

      Append (Inner_Args, String'("--outer-dir=" & Tool_Current_Dir.all));

      --  Modify the --output-dir= switch so it uses a full pathname.
      --  Otherwise, the output files would end up in something like
      --  obj/ALI-FILES-gnat2xml.

      if Out_Dir /= null then
         Append (Inner_Args,
                 String'("--output-dir=" & Out_Dir.all));
      end if;

      --  Set the report file name for the inner invocation using a full path
      --  name, because sometimes the inner invocation has a different current
      --  directory.

      if This_Is_Gnatcheck then
         if Text_Report_ON then
            Append (Inner_Args, "-o");
            Append (Inner_Args, Get_Report_File_Name);
         end if;

         if XML_Report_ON then
            Append (Inner_Args, "-ox");
            Append (Inner_Args, Get_XML_Report_File_Name);
         end if;
      end if;

      --  Now deal with command-line arguments from this invocation of an ASIS
      --  tool (the outer one). Most are copied to Builder_Args or Inner_Args.

      while Cur <= Argument_Count loop
         declare
            Arg : constant String := Argument (Cur);

            function Match
              (Check : String;
               Kind : Character := ' ';
               Builder_Arg, Inner_Arg : Boolean := False;
               Response_File : Boolean := False) return Boolean
              with Pre => Kind in ' ' | ':' | '=' | '!'
                          and then (if Response_File then Kind = '=');
            --  Checks if Arg matches Check. Kind = ' ' means Arg does not have
            --  a parameter. The other possibilities for Kind mean the same
            --  thing as in GNAT.Command_Line.Getopt. If there is a match, then
            --  we move past the arg and its parameter, if any, and append them
            --  onto Builder_Args and/or Inner_Args, as indicated by the
            --  Builder_Arg and Inner_Arg flags. Return True iff there is a
            --  match. To ignore an argument, leave Builder_Arg and Inner_Arg
            --  False. Response_File is True for the "-file=" switch.

            function Match
              (Check : String;
               Kind : Character := ' ';
               Builder_Arg, Inner_Arg : Boolean := False;
               Response_File : Boolean := False) return Boolean is

               procedure App;
               procedure App is
               begin
                  if Builder_Arg then
                     Append (Builder_Args, Argument (Cur));
                  end if;
                  if Inner_Arg then
                     Append (Inner_Args, Argument (Cur));
                  end if;
               end App;

               procedure App_Response_File (Response_File_Name : String);
               --  We turn "-file=files.txt" into "@files.txt"
               procedure App_Response_File (Response_File_Name : String) is
                  Arg : constant String := "@" & Response_File_Name;
               begin
                  Append (Builder_Args, Arg);
               end App_Response_File;

               Old_Cur : constant Positive := Cur;
            begin

               case Kind is
                  when ' ' =>
                     if Arg = Check then
                        App;
                        Cur := Cur + 1;
                     end if;

                  when ':' =>
                     if Arg = Check then
                        App;
                        Cur := Cur + 1;
                        App;
                        Cur := Cur + 1;
                     elsif Has_Prefix (Arg, Prefix => Check) then
                        App;
                        Cur := Cur + 1;
                     end if;

                  when '=' =>
                     if Response_File then
                        if Arg = Check then
                           Cur := Cur + 1;
                           App_Response_File (Argument (Cur));
                           Cur := Cur + 1;
                        elsif Has_Prefix (Arg, Prefix => Check & "=") then
                           App_Response_File
                             (Strip_Prefix (Argument (Cur),
                                            Prefix => Check & "="));
                           Cur := Cur + 1;
                        end if;
                     else
                        if Arg = Check then
                           App;
                           Cur := Cur + 1;
                           App;
                           Cur := Cur + 1;
                        elsif Has_Prefix (Arg, Prefix => Check & "=") then
                           App;
                           Cur := Cur + 1;
                        end if;
                     end if;

                  when '!' =>
                     if Has_Prefix (Arg, Prefix => Check) then
                        App;
                        Cur := Cur + 1;
                     end if;

                  when others => raise Program_Error;
               end case;

               return Cur /= Old_Cur;
            end Match;

         begin
            --  We shouldn't be seeing -c, -gnatc, -gnatec, -gnatem, or -gnateO
            --  here; those are passed by gnatmake or gprbuild to the inner
            --  invocations, whereas we're in the outer one. Might as well
            --  ignore them. "--incremental" needs to be ignored; the inner
            --  invocations shouldn't get here.  "--output-dir" is handled
            --  specially above. Project-related arguments go in
            --  Builder_Args. Non-switches (i.e. file names) go in
            --  Builder_Args. Anything else goes in Inner_Args.

            if Match ("-c", ' ')
              or else Match ("-xml", ' ', Inner_Arg => True)
              or else Match ("-gnatc", '!')
              or else Match ("-gnatec", '!')
              or else Match ("-gnatem", '!')
              or else Match ("-gnateO", '!')
              or else Match ("--incremental", ' ')
              or else Match ("--output-dir", '=')

              or else Match ("-P", ':', Builder_Arg => True)
              or else Match ("-U", ' ', Builder_Arg => True)
              or else Match ("-X", '!', Builder_Arg => True)
              or else Match ("--subdirs", '=', Builder_Arg => True)

              or else Match ("--no_objects_dir", ' ', Inner_Arg => True)
              or else Match ("-o", '=', Inner_Arg => True)
            then
               null; -- One of those Matches already Appended if appropriate
            elsif Match ("-j", '!', Builder_Arg => True) then
               --  The Match already Appended to Builder_Args, but we also want
               --  to pass --outer-parallel to the inner invocation.
               Append (Inner_Args, "--outer-parallel");
            elsif not Has_Prefix (Arg, Prefix => "-") then
               --  If it doesn't look like a switch (e.g. a source file name),
               --  we want to pass it to the builder, except that gnatcheck
               --  rules like "+Blah", should go to the inner tool invocation.
               if In_Gnatcheck_Rules then
                  Append (Inner_Args, Arg);
               else
                  Append (Builder_Args, Arg);
               end if;
               Cur := Cur + 1;
            elsif Match ("-files", '=', Response_File => True) then
               null;
            elsif Arg = "-cargs" then
               --  We can't just pass -cargs, because it would be hi-jacked by
               --  the builder. So we pass -inner-cargs instead. The inner
               --  invocation will then use this different arg when it
               --  processes Process_cargs_Section. So for example if our
               --  (outer) args are:
               --
               --      some_file.adb -cargs -gnat2012
               --
               --  we will pass:
               --
               --      ... -cargs some_file.adb -inner-cargs -gnat2012 ...
               --
               --  to the builder, which will then pass:
               --
               --      ... some_file.adb -inner-cargs -gnat2012 ...
               --
               --  to the inner invocation.

               Append (Inner_Args, "-inner-cargs");
               Cur := Cur + 1;
            else
               pragma Assert (Has_Prefix (Arg, Prefix => "-"));
               --  Here for all other switches, including -d (debug switches).
               --  Pass the switch along to the inner invocation. In addition,
               --  pass -dn (keep temp files) along to the builder.
               Append (Inner_Args, Arg);
               if Arg in "-dn" | "-debugn" then
                  Append (Builder_Args, "-dn");
               end if;
               Cur := Cur + 1;

               if This_Is_Gnatcheck and then Arg = "-rules" then
                  In_Gnatcheck_Rules := True;
               end if;
            end if;
         end;
      end loop;

      --  Include extra args specific to the ASIS tool

      Append (Inner_Args, Extra_Inner_Post_Args);
      Prepend (Inner_Args, Extra_Inner_Pre_Args);

      --  -cargs means to pass the following arguments along to the ASIS tool

      if Last_Index (Inner_Args) > 0 then
         Prepend (Inner_Args, "-cargs:Ada");
      end if;

      --  Finally, construct the result, which is basically
      --  "Builder_Args & Inner_Args".

      return Result : Argument_List
        (1 .. Last_Index (Builder_Args) + Last_Index (Inner_Args))
      do
         declare
            X : Natural := 0;
         begin
            for Arg of Builder_Args loop
               X := X + 1;
               Result (X) := new String'(Arg);
            end loop;

            for Arg of Inner_Args loop
               X := X + 1;
               Result (X) := new String'(Arg);
            end loop;

            pragma Assert (X = Result'Last);
         end;
      end return;
   end Builder_Command_Line;

   ------------------
   -- Call_Builder --
   ------------------

   procedure Call_Builder is
   begin
      if not A4G.GNAT_Int.Execute
        (ASIS_UL.Common.Gprbuild_To_Call,
         Builder_Command_Line,
         Display_Call => ASIS_UL.Debug.Debug_Flag_C)
      then
         raise ASIS_UL.Common.Fatal_Error;
         --  Presumably the builder or one of the inner invocations printed an
         --  error message.
      end if;
   end Call_Builder;

   ----------------------
   -- Check_Parameters --
   ----------------------

   procedure Check_Parameters is separate;

   --------------
   -- Clean_Up --
   --------------

   procedure Clean_Up is
      Success : Boolean := False;
   begin
      Source_Table_Debug_Image;

      Context_Clean_Up;

      --  Clean up temporary dir

      if not Debug_Flag_N and then Temp_Dir_Exists then

         pragma Assert
           (Get_Current_Dir = Tool_Temp_Dir.all & Directory_Separator
            or else Get_Current_Dir = Initial_Dir & Directory_Separator);
         --  It can be Initial_Dir if we fail early
         Change_Dir (Initial_Dir);
         --  to avoid being in the Tool_Temp_Dir when we delete it

         for J in 1 .. 10 loop
            --  On windows, there might be a slight delay between the return of
            --  the close function on a file descriptor and the actual closing
            --  done by the system. Since it's not possible to remove a
            --  directory as long as there are handles on it, this Remove_Dir
            --  may fail. So, if a call to Remove_Dir raises Directory_Error,
            --  we try several times after some delay, and only if all the
            --  attempts fail, we generate an error message and raise an
            --  exception.

            begin
               Remove_Dir (Tool_Temp_Dir.all, Recursive => True);
               Success := True;
               exit;
            exception
               when Directory_Error =>
                  delay 0.05;
            end;
         end loop;

         if not Success then
            --  Because of some unknown reason the temporary directory cannot
            --  be removed:
            Free (Tool_Temp_Dir);  -- to avoid cycling
            Error ("cannot remove temporary directory");
            raise Fatal_Error;
         end if;

         Free (Tool_Temp_Dir);
      end if;
   end Clean_Up;

   ----------------------
   -- Context_Clean_Up --
   ----------------------

   procedure Context_Clean_Up is
   begin
      if Is_Open (The_Context) then
         Close (The_Context);
      end if;

      if Has_Associations (The_Context) then
         Dissociate (The_Context);
      end if;
   end Context_Clean_Up;

   ---------------------
   -- Create_Temp_Dir --
   ---------------------

   procedure Create_Temp_Dir (Success : out Boolean) is
      FD        : File_Descriptor;
      Temp_Name : Temp_File_Name;
   begin
      Success := False;

      pragma Assert (Get_Current_Dir = Initial_Dir & Directory_Separator);
      pragma Assert (not Temp_Dir_Exists);

      if Temp_Dir_Parent /= "" then
         Change_Dir (Temp_Dir_Parent);
         --  So when we create the Tool_Temp_Dir below, it will be a
         --  subdirectory of Temp_Dir_Parent.
      end if;

      --  ??? We create the temp dir by first creating the temp file, then
      --  closing and deleting it, then creating a dir with the same name.
      --  This is not atomic as another program can sneak in between file
      --  deletion and dir creation and snatch this name for itself. This is
      --  quite unlikely and anyway we don't have any other system-independent
      --  way at the moment
      Create_Temp_File (FD, Temp_Name);

      if FD = Invalid_FD then
         --  We do not generate any diagnostic here - this procedure is always
         --  called in tools even if a tool is called with '--version' or
         --  '--help' option.
         return;
      end if;

      Close (FD);
      Delete_File (Temp_Name, Success);

      if not Success then
         Error ("can not delete the temporary file that was "
              & "just created");

         raise Fatal_Error;
      end if;

      Tool_Temp_Dir := new String' -- Remove NUL
        (Normalize_Pathname
           (Temp_Name (Temp_Name'First .. Temp_Name'Last - 1)));

      Parallel_Make_Dir (Tool_Temp_Dir.all);

      Change_Dir (Initial_Dir);

      Success := True;

   exception
      when Directory_Error =>
         Error ("cannot create the temporary directory");
         raise Fatal_Error;
   end Create_Temp_Dir;

   -------------------
   -- Copy_Gnat_Adc --
   -------------------

   procedure Copy_Gnat_Adc is
      Success : Boolean;
   begin
      if Is_Regular_File ("gnat.adc") then
         Copy_File
           (Name     => Tool_Current_Dir.all & Directory_Separator &
                        "gnat.adc",
            Pathname => Tool_Temp_Dir.all & Directory_Separator &
                        "gnat.adc",
            Success  => Success,
            Mode     => Copy);
      end if;

--    pragma Assert (Project_File_Obsolete = null,
--                   "???The following code is not used");
      if Project_File_Obsolete /= null
        and then
         Project_Support_Type = Use_Tmp_Project_File
      then
--         pragma Assert (False);
         Change_Dir (Tool_Temp_Dir.all);

         declare
            Temp_Project_File : File_Type;
         begin
            --  Creating the temporary project file
            Create (Temp_Project_File, Out_File, "tmp.gpr");

            Put (Temp_Project_File, "project Tmp extends """);
            Put (Temp_Project_File, Project_File_Obsolete.all);
            Put (Temp_Project_File, """ is");
            New_Line (Temp_Project_File);

            Put (Temp_Project_File, "   for Object_Dir use ""."";");
            New_Line (Temp_Project_File);

            Put (Temp_Project_File, "end Tmp;");
            New_Line (Temp_Project_File);

            Close (Temp_Project_File);

            --  Storing the temporary project file as an option:

            Store_Option ("-Ptmp.gpr");
            Set_Arg_List;

         exception
            when others =>
               Error ("can not create the temporary project file");
               raise Fatal_Error;
         end;

         Change_Dir (Tool_Current_Dir.all);
      end if;
   exception
      when Directory_Error =>
         Error ("cannot create the temporary directory");
         raise Fatal_Error;
   end Copy_Gnat_Adc;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Prj : in out ASIS_UL.Projects.Arg_Project_Type'Class)
   is
      Temp_Dir_Created : Boolean;
   begin
      Create_Temp_Dir (Temp_Dir_Created);

      Tool_Specific_Initialization_1;
      --  A tool-specific version should be provided!

      Scan_Parameters (Prj);

      if Incremental_Mode then
         if not Prj.Is_Specified then
            Error ("--incremental mode requires a project file, " &
                     "and cannot be used with the gnat driver");
            raise Fatal_Error;
         end if;
      end if;

      Set_Log_File;

      Set_Tree_Creator (Prj);

      Check_Parameters; --  A tool-specific version should be provided!

      Create_Mapping_File_Copies;

      if not Temp_Dir_Created
       and then
         not ASIS_UL.Options.Nothing_To_Do
      then
         Error ("cannot create temporary directory, check write permission " &
                "for " & Initial_Dir);
         raise Fatal_Error;
      end if;

      Tool_Specific_Initialization_2;
      --  A tool-specific version should be provided!

      if Temp_Dir_Parent /= "" then
         --  In debug mode that keeps temporary files and directories, display
         --  once the value of TMPDIR, so that if temp files cannot be created,
         --  it is easier to understand where temp files are supposed to be
         --  created.

         if ASIS_UL.Debug.Debug_Flag_N and then not Tmpdir_Displayed then
            Info_No_EOL ("TMPDIR = """);
            Info_No_EOL (Temp_Dir_Parent);
            Info        ("""");
            Tmpdir_Displayed := True;
         end if;
      end if;

      if ASIS_UL.Options.Nothing_To_Do then
         return;
      end if;

      Copy_Gnat_Adc;
      pragma Assert (Get_Current_Dir = Tool_Current_Dir.all &
                          Directory_Separator);

      if not Incremental_Mode
        and then
         not In_Aggregate_Project
      then
         Change_Dir (Tool_Temp_Dir.all);
         Store_I_Options;
      end if;

      --  Create output directory if necessary

      if Out_Dir /= null  then
         Parallel_Make_Dir
           (Out_Dir.all, Give_Message => Verbose_Mode);
      end if;
   exception
      when Parameter_Error =>
         --  The diagnosis is already generated
         Try_Help;
         raise Fatal_Error;
      when Fatal_Error =>
         --  The diagnosis is already generated
         raise;
      when others =>
         Error ("initialization failed");
         --  Exception info will be generated in main driver
         raise;
   end Initialize;

   --------------------
   -- More_Arguments --
   --------------------

   function More_Arguments
     (Store_Arguments : Boolean := True;
      In_Switches     : Boolean := False;
      Parser          : Opt_Parser := Command_Line_Parser)
      return            Boolean
   is
      Result : Boolean := False;
   begin
      loop
         declare
            Arg : constant String := Get_Argument
              (Do_Expansion => True,
               Parser       => Parser);
         begin
            exit when Arg = "";
            Result := True;

            if In_Switches then
               Error ("Switches attribute cannot contain argument sources");
               raise Parameter_Error;
            end if;

            if ASIS_UL.Projects.U_Option_Set then
               ASIS_UL.Projects.Store_Main_Unit (Arg, Store_Arguments);
            else
               Store_Sources_To_Process (Arg, Store_Arguments);
            end if;
         end;
      end loop;

      return Result;
   end More_Arguments;

   ------------------------
   -- Print_Command_Line --
   ------------------------

   procedure Print_Command_Line is
      use Ada.Command_Line;
   begin
      if ASIS_UL.Options.Incremental_Mode then
         Formatted_Output.Put ("(outer)\n  ");
      end if;
      if ASIS_UL.Options.Mimic_gcc then
         Formatted_Output.Put ("(inner)\n  ");
      end if;

      Formatted_Output.Put ("current directory = \1\n", Current_Directory);
      if False then -- disable for now
         A4G.A_Debug.Print_Env;
      end if;

      Formatted_Output.Put ("  \1", Command_Name);

      for X in 1 .. Argument_Count loop
         Formatted_Output.Put (" \1", Argument (X));
      end loop;
      Formatted_Output.Put ("\n");
   end Print_Command_Line;

   ---------------------
   -- Scan_Parameters --
   ---------------------

   procedure Scan_Parameters
     (Prj : in out ASIS_UL.Projects.Arg_Project_Type'Class)
   is
      procedure Pre_Scan_Arguments;
      --  Set various flags, such as the Debug_Flag_X's, based on the
      --  command-line options. This is done before the normal 2-pass argument
      --  scan, so we can use these flags during the argument scan.

      -------------------------
      --  Pre_Scan_Arguments --
      -------------------------

      procedure Pre_Scan_Arguments is
         use Ada.Command_Line;
      begin
         for X in 1 .. Argument_Count loop
            declare
               Arg : constant String := Argument (X);
               pragma Assert (Arg'First = 1);
            begin
               --  gnatmetric uses "-debug" instead of "-d"
               if Has_Suffix (Tool_Name.all, Suffix => "gnatmetric") then
                  if Arg'Length = 7 and then Arg (1 .. 6) = "-debug" then
                     Set_Debug_Options (Arg (7 .. 7));
                  end if;
               else
                  if Arg'Length = 3 and then Arg (1 .. 2) = "-d" then
                     Set_Debug_Options (Arg (3 .. 3));
                  end if;
               end if;

               if Arg = "--incremental" then
                  Incremental_Mode := True;
               elsif Has_Prefix (Arg, Prefix => "--outer-dir=") then
                  --  We use --outer-dir to detect that we were called from
                  --  gprbuild.
                  Mimic_gcc := True;
               end if;
            end;
         end loop;

         --  We need to ignore --incremental in the inner invocation, because
         --  --incremental could be specified in package Pretty_Printer of the
         --  project file, which will cause the builder to pass it to the inner
         --  invocation.

         if Mimic_gcc then
            Incremental_Mode := False;
         end if;
      end Pre_Scan_Arguments;

   --  Start of processing for Scan_Parameters

   begin
      GNSA_GNATLS_Workaround; --  ???

      Pre_Scan_Arguments;

      if Debug_Flag_C then
         Print_Command_Line;
      end if;

      Initialize_Option_Scan
        (Stop_At_First_Non_Switch => False,
         Section_Delimiters       => Prj.Get_Section_Delimiters);

      Prj.Scan_Arguments (First_Pass => True);
      pragma Assert (not (Incremental_Mode and Mimic_gcc));

      if Print_Version then
         Print_Tool_Version (2004);
         Clean_Up;
         OS_Exit (0);
      end if;

      if Print_Usage then
         Prj.Print_Tool_Usage;
         Clean_Up;
         OS_Exit (0);
      end if;

      --  Tool_Current_Dir and Tool_Inner_Dir have been set if and only if this
      --  is an inner invocation. If it's NOT an inner invocation, we set
      --  Tool_Current_Dir, and leave Tool_Inner_Dir null.

      pragma Assert ((Tool_Current_Dir /= null) = Mimic_gcc);
      pragma Assert ((Tool_Inner_Dir /= null) = Mimic_gcc);

      if not Mimic_gcc then
         Tool_Current_Dir := new String'(Initial_Dir);
      end if;

      --  If we have the project file specified as a tool parameter, analyze
      --  it. ???This is the obsolete version.

      ASIS_UL.Projects.Process_Project_File (Prj);

      --  And finally - analyze the command-line parameters. We do this even in
      --  Incremental_Mode, so we get any errors on the outer call to the ASIS
      --  tool, rather than on the inner calls.

      Initialize_Option_Scan
        (Stop_At_First_Non_Switch => False,
         Section_Delimiters       => Prj.Get_Section_Delimiters);

      Prj.Scan_Arguments;
      pragma Assert (not (Incremental_Mode and Mimic_gcc));
   end Scan_Parameters;

   ---------------------
   -- Scan_Common_Arg --
   ---------------------

   function Scan_Common_Arg
     (First_Pass      : Boolean;
      Parser          : Opt_Parser;
      In_Switches     : Boolean;
      In_Project_File : Boolean;
      Initial_Char    : Character) return Common_Arg_Status
   is

      Result : Common_Arg_Status := Arg_Not_Processed;
      --  Every switch recognized below MUST set Result!

      --  Note also that if you add processing for a switch here, you must add
      --  it to the string passed to Getopt in each ASIS tool you want to
      --  support that switch.
   begin
      case Initial_Char is
         when 'c' =>
            if Full_Switch (Parser => Parser) = "c" then
               Result := Arg_Processed;
            end if;

         when 'd' =>
            --  gnatmetric uses "-debug" instead of "-d".  Either way,
            --  Set_Debug_Options was already called by Pre_Scan_Arguments.

            if Has_Suffix (Tool_Name.all, Suffix => "gnatmetric") then
               if Full_Switch (Parser => Parser) = "debug" then -- "-debugX"
                  Result := Arg_Processed;
               end if;
            else
               if Full_Switch (Parser => Parser) = "d" then -- "-dX"
                  Result := Arg_Processed;
               end if;
            end if;

         when 'f' =>
            if Full_Switch (Parser => Parser) = "files" then
               Result              := Arg_Processed;
               File_List_Specified := True;

               if First_Pass then
                  Files_Switch_Used := True;
                  Read_Args_From_File (Parameter (Parser => Parser));
               elsif In_Project_File then
                  if In_Switches then
                     Error ("-files option is not allowed " &
                              "for Switches attribute");
                     raise Parameter_Error;
                  else
                     Read_Args_From_File (Parameter (Parser => Parser));
                  end if;
               end if;
            end if;

         when 'g' =>
            --  Various options that are passed to the inner tool invocations
            --  for incremental mode. These should mostly be passed along to
            --  the real compiler.

            --  -gnatec=configuration_pragmas_file
            --  -gnatem=mapping_file
            --  -gnateO=object_path_file_name

            if Full_Switch (Parser => Parser)
              in "gnatec" | "gnatem" | "gnateO"
            then
               Result := Arg_Processed;
               if not First_Pass then
                  Store_GNAT_Option_With_Path
                    (Full_Switch (Parser => Parser),
                     Parameter (Parser => Parser));
               end if;

            elsif Full_Switch (Parser => Parser) = "gnatA" then
               Result := Arg_Processed;
               if not First_Pass then
                  Store_Option
                    ("-" & Full_Switch (Parser => Parser));
               end if;

            elsif Full_Switch (Parser => Parser) = "gnatc" then
               Result := Arg_Processed;
               --  We can ignore -gnatc, because ASIS always calls the compiler
               --  with -gnatc.
            end if;

         when 'I' =>
            if Full_Switch (Parser => Parser) = "I" then
               Result := Arg_Processed;
               if not First_Pass then
                  Store_I_Option (Parameter (Parser => Parser));
               end if;
            end if;

         when 'j' =>
            if Full_Switch (Parser => Parser) = "j" then
               Result := Arg_Processed;

               --  We need to ignore -j in the inner invocation; otherwise we
               --  will complain about mixing -j with -rnb when not in
               --  --incremental mode.

               if not First_Pass and then not Mimic_gcc then
                  begin
                     J_Specified := True;
                     ASIS_UL.Options.Process_Num :=
                       Natural'Value (Parameter (Parser => Parser));

                     if ASIS_UL.Options.Process_Num = 0 then
                        ASIS_UL.Options.Process_Num :=
                          Positive (System.Multiprocessors.Number_Of_CPUs);
                     end if;
                  exception
                     when Constraint_Error =>
                        Error ("Wrong Parameter of '-j' option: " &
                               Parameter (Parser => Parser));
                        raise Parameter_Error;
                  end;
               end if;
            end if;

         when 'v' =>
            if Full_Switch (Parser => Parser) = "v" then
               Result := Arg_Processed;
               if First_Pass then -- Handle verbose switch early
                  Verbose_Mode := True;
               end if;
            end if;

         when 'x' =>
            --  Ignore "-x ada". We only do this in Mimic_gcc, because
            --  gnatmetric uses the -x switch for something else.
            --  ???gnatmetric does not currently support --incremental;
            --  if it ever does, we will need to resolve this conflict.

            if Mimic_gcc and then Full_Switch (Parser => Parser) = "x" then
               Result := Arg_Processed;
            end if;

         when '-' =>
            if Full_Switch (Parser => Parser) = "-help" then
               Result := Quit;
               if In_Project_File then
                  Error ("project file should not contain '--help' option");
                  raise Parameter_Error;
               end if;

               Print_Usage := True;

            elsif Full_Switch (Parser => Parser) = "-version" then
               Result := Quit;
               if In_Project_File then
                  Error
                    ("project file should not contain '--version' option");
                  raise Parameter_Error;
               end if;

               Print_Version := True;

            elsif Full_Switch (Parser => Parser) = "-ignore" then
               Result := Arg_Processed;

               if Is_Regular_File (Parameter (Parser => Parser)) then
                  ASIS_UL.Options.Exempted_Units :=
                    new String'(Normalize_Pathname
                                  (Parameter (Parser => Parser)));
               else
                  Error (Parameter (Parser => Parser) & " not found");
                  raise Parameter_Error;
               end if;

            elsif Full_Switch (Parser => Parser) = "-incremental" then
               Result := Arg_Processed;
               --  Incremental_Mode was already set by Pre_Scan_Arguments

            elsif Full_Switch (Parser => Parser) = "-outer-parallel" then
               Result := Arg_Processed;
               if not First_Pass then
                  Outer_Parallel := True;
                  pragma Assert (Mimic_gcc);
               end if;

            elsif Full_Switch (Parser => Parser) = "-outer-dir" then
               Result := Arg_Processed;
               if First_Pass then -- process --outer-dir early
                  Tool_Current_Dir := new String'
                    (Full_Name (Parameter (Parser => Parser)));
                  Tool_Inner_Dir := new String'(Initial_Dir);
                  pragma Assert
                    (Full_Name (Parameter (Parser => Parser)) =
                       Parameter (Parser => Parser));
                  pragma Assert (Mimic_gcc);
                  pragma Assert (Get_Current_Dir = Initial_Dir &
                                   Directory_Separator);
                  Change_Dir (Tool_Current_Dir.all);
               end if;

            elsif Full_Switch (Parser => Parser) = "-output-dir" then
               Result := Arg_Processed;
               if not First_Pass then
                  Out_Dir := new String'
                    (Full_Name (Parameter (Parser => Parser)));
               end if;

            elsif Full_Switch (Parser => Parser) = "-target" then
               Result := Arg_Processed;

               if First_Pass then  --  we need to know this before parsing
                                    --  the argument project file
                  Target := new String'(Parameter (Parser => Parser));
               end if;
            end if;

         when others => null;
      end case;

      return Result;
   end Scan_Common_Arg;

   ----------------------------------
   -- Tool_Specific_Initialization --
   ----------------------------------

   procedure Tool_Specific_Initialization_1 is separate;
   procedure Tool_Specific_Initialization_2 is separate;

   ----------------------------
   -- GNSA_GNATLS_Workaround --
   ----------------------------

   procedure GNSA_GNATLS_Workaround is
   begin
      if A4G.GNSA_Switch.Use_GNSA then
         declare
            Idx : Natural;
            Old_Path : constant String := Getenv ("PATH").all;
         begin
            Idx := Tool_Dir'Last - 3;
            Setenv ("PATH",
                  Tool_Dir (Tool_Dir'First .. Idx) &
                            A4G.GNSA_Switch.GNSA_Dir &
                            Directory_Separator & "bin" &
                            Path_Separator & Old_Path);
         exception
            when others =>
               if Tool_Dir = null then
                  Info ("no Tool_Dir is known in GNSA_GNATLS_Workaround!");
               else
                  Info ("GNSA_GNATLS_Workaround: Tool_Dir is:");
                  Info (">>" & Tool_Dir.all & "<<");
               end if;

               raise;
         end;
      end if;
   end GNSA_GNATLS_Workaround;

end ASIS_UL.Environment;
