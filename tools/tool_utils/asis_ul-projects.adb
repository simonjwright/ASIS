------------------------------------------------------------------------------
--                                                                          --
--                     ASIS UTILITY LIBRARY COMPONENTS                      --
--                                                                          --
--                     A S I S _ U L . P R O J E C T S                      --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                     Copyright (C) 2013-2019, AdaCore                     --
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

with Ada.Characters.Handling;    use Ada.Characters.Handling;
with Ada.Containers.Ordered_Sets;
with Ada.Environment_Variables;
with Ada.Strings;                use Ada.Strings;
with Ada.Strings.Fixed;          use Ada.Strings.Fixed;
with Ada.Text_IO;                use Ada.Text_IO;

with GNAT.Directory_Operations;  use GNAT.Directory_Operations;

with GNATCOLL.Projects.Aux;
with GNATCOLL.Traces;

with A4G.GNSA_Switch;

with ASIS_UL.Common;             use ASIS_UL.Common;
with ASIS_UL.Compiler_Options;   use ASIS_UL.Compiler_Options;
with ASIS_UL.Environment;        use ASIS_UL.Environment;
with ASIS_UL.Options;            use ASIS_UL.Options;
with ASIS_UL.Output;             use ASIS_UL.Output;
with ASIS_UL.Projects.Aggregate; use ASIS_UL.Projects.Aggregate;
with ASIS_UL.Source_Table;       use ASIS_UL.Source_Table;
with ASIS_UL.String_Utilities;   use ASIS_UL.String_Utilities;
with ASIS_UL.Tree_Creation;
--  with Ada.Text_IO;

--  with GPR;

package body ASIS_UL.Projects is

   Project_Env      : Project_Environment_Access;
   Project_File_Set : Boolean := False;

   Config_File_Name  : String_Access;
   Mapping_File_Name : String_Access;

   Mapping_File_Copies : String_List_Access;
   --  Here the names of the copies of the mapping file are stored

   type Mapping_File_Occupations is array (Natural range <>) of Boolean;
   type Mapping_File_Occupations_Access is access Mapping_File_Occupations;
   Mapping_File_Occupation : Mapping_File_Occupations_Access;
   --  This array indicates which copies of the mapping file are used in
   --  compilations

   ------------------------
   --  Closure computing --
   ------------------------

   Main_Files : GNATCOLL.VFS.File_Array_Access;
   --  List of units to compute the closure for.

   Closure : Temporary_File_Storages.Set := Temporary_File_Storages.Empty_Set;

   Closure_Subdirs_To_Clean : Temporary_File_Storages.Set :=
     Temporary_File_Storages.Empty_Set;
   --  Names of object subdirs created in the argument project - we have to
   --  remove them when the work is done.

   -------------------------------
   --  External variables table --
   -------------------------------

   type X_Var_Record is record
      Var_Name          : String_Access;
      Var_Value         : String_Access;
   end record;

   function "<" (Left, Right : X_Var_Record) return Boolean is
     (To_Lower (Left.Var_Name.all) < To_Lower (Right.Var_Name.all));

   function "=" (Left, Right : X_Var_Record)  return Boolean is
     (To_Lower (Left.Var_Name.all) = To_Lower (Right.Var_Name.all));

   package X_Vars_Sets is new Ada.Containers.Ordered_Sets
     (Element_Type => X_Var_Record);

   X_Vars : X_Vars_Sets.Set;

   ------------------------
   --  Local subprograms --
   ------------------------

   procedure Load_Aggregated_Project
     (My_Project : in out Arg_Project_Type'Class) with
        Pre => ASIS_UL.Options.Aggregated_Project;
   --  Loads My_Project (that is supposed to be an aggregate project), then
   --  unloads it and loads in the same environment the project passes as a
   --  parameter of '-A option' (which is supposesd to be a (non-aggregate)
   --  project aggregated by My_Project

   function Needed_For_Tree_Creation (Option : String) return Boolean;
   --  Checks if the argument is the compilation option that is needed for tree
   --  creation. Also gives an error message if there is a preprocessor switch,
   --  and Preprocessing_Allowed is False.

   function Is_Ada_File
     (File :       Virtual_File;
      My_Project : Arg_Project_Type)
      return Boolean;
   --  Checks if the given source file is an Ada file.

   function Is_Externally_Built
     (File :       Virtual_File;
      My_Project : Arg_Project_Type)
      return Boolean;
   --  Checks if the given source file belongs to an externally build library.

   ------------
   --  Debug --
   ------------

   procedure Print_Debug_Info (I : File_Info);
   --  Prints out the debug info from the argument

   --------------
   -- Add_Main --
   --------------

   procedure Add_Main
     (My_Project : Arg_Project_Type;
      Main_Name  : String)
   is
      pragma Unreferenced (My_Project);
      SF : constant SF_Id := File_Find (Main_Name);
   begin
      pragma Assert (Present (SF));

      Append (Main_Files, Create (+Main_Name));
      Set_Source_Status (SF, Waiting);

      if not Closure.Contains (Main_Name) then
         Closure.Include (Main_Name);
      end if;

   end Add_Main;

   -------------------------------------
   -- Aggregate_Project_Report_Header --
   -------------------------------------

   procedure Aggregate_Project_Report_Header (My_Project : Arg_Project_Type) is
      pragma Unreferenced (My_Project);
   begin
      if Text_Report_ON then
         Report ("Argument project is an aggregate project");
         Report ("Aggregated projects are processed separately");
      end if;

      if XML_Report_ON then
         XML_Report ("<aggregated-project-reports>",
                     Indent_Level => 1);
      end if;
   end Aggregate_Project_Report_Header;

   --------------
   -- Clean_Up --
   --------------

   procedure Clean_Up (My_Project : Arg_Project_Type) is
      Root_Prj : Project_Type;
      Success  : Boolean;
   begin
      if not Debug_Flag_N
        and then
         Is_Specified (My_Project)
      then
         Root_Prj := Root_Project (My_Project);

         if Root_Prj /= No_Project then
            GNATCOLL.Projects.Aux.Delete_All_Temp_Files (Root_Prj);

            if Mapping_File_Copies /= null then
               for J in Mapping_File_Copies'Range loop
                  Delete_File (Get_Mapping_File_Copy_Name (J), Success);

                  if not Success then
                     Error ("cannot delete copy of mapping file " &
                            Get_Mapping_File_Copy_Name (J));
                  end if;
               end loop;
            end if;
         end if;
      end if;
   end Clean_Up;

   ------------------------------------
   -- Close_Aggregate_Project_Report --
   ------------------------------------

   procedure Close_Aggregate_Project_Report (My_Project : Arg_Project_Type) is
      pragma Unreferenced (My_Project);
   begin
      if XML_Report_ON then
         XML_Report ("</aggregated-project-reports>",
                     Indent_Level => 1);
      end if;
   end Close_Aggregate_Project_Report;

   ----------------------
   -- Closure_Clean_Up --
   ----------------------

   procedure Closure_Clean_Up (My_Project : in out Arg_Project_Type) is
      pragma Unreferenced (My_Project);
   begin

      if not Closure_Subdirs_To_Clean.Is_Empty then
         declare
            Cur : Temporary_File_Storages.Cursor :=
              Closure_Subdirs_To_Clean.First;
         begin
            while Temporary_File_Storages.Has_Element (Cur) loop
               if Is_Directory (Temporary_File_Storages.Element (Cur)) then
                  begin
                     Remove_Dir (Temporary_File_Storages.Element (Cur),
                                 Recursive => True);
                  exception
                     when Directory_Error =>
                        Warning ("cannot delete temp closure dir " &
                                 Temporary_File_Storages.Element (Cur));
                  end;
               end if;
               Temporary_File_Storages.Next (Cur);
            end loop;
         end;
      end if;

   end Closure_Clean_Up;

   ----------------------
   -- Closure_Complete --
   ----------------------

   function Closure_Complete (My_Project : Arg_Project_Type) return Boolean is
      Result        : Boolean := True;
      Closure_Files : GNATCOLL.VFS.File_Array_Access;
      Status        : Status_Type;
      SF            : SF_Id;
   begin
      if Debug_Flag_U then
         Closure_Debug_Image_Closure;
      end if;

      Get_Closures
        (My_Project.Root_Project,
         Main_Files,
         Status => Status,
         Result => Closure_Files);

      if Status = Error then
         Error ("cannot complete closure");
         raise Fatal_Error;
      end if;

      if Debug_Flag_U then
         Info ("...checking closure completeness");
      end if;

      if Closure_Files /= null then
         for I in Closure_Files'Range loop
            declare
               F_Name : constant String := Closure_Files (I).Display_Full_Name;
            begin

               if not Closure.Contains (F_Name) then

                  if Debug_Flag_U then
                     Info (Ident_String & "adding " & F_Name & " to closure");
                  end if;

                  Closure.Include (F_Name);

                  SF := File_Find (F_Name);

                  if Present (SF) then
                     if Source_Status (SF) = Waiting_For_Closure then
                        Set_Source_Status (SF, Waiting);
                     end if;
                  else
                     Error ("cannot locate " & F_Name &
                             ", closure incomplete");
                  end if;

                  Result := False;
               else
                  if Debug_Flag_U then
                     Info (Ident_String & F_Name & " already in closure");
                  end if;
               end if;
            end;
         end loop;
      end if;

      return Result;
   end Closure_Complete;

   -------------------------------
   -- Closure_Debug_Image_Mains --
   -------------------------------

   procedure Closure_Debug_Image_Mains is
   begin
      Info ("Main Units to compute the closure for:");

      if Main_Files = null
        or else
         Main_Files'Length = 0
      then
         Info (Ident_String & "no main unit set");
      else
         for J in Main_Files'Range loop
            Info (Ident_String &
                  String (Filesystem_String'(Full_Name (Main_Files (J)))));
         end loop;
      end if;
   end Closure_Debug_Image_Mains;

   ---------------------------------
   -- Closure_Debug_Image_Closure --
   ---------------------------------

   procedure Closure_Debug_Image_Closure is
      use Temporary_File_Storages;

      procedure Print_Out (C : Cursor);

      procedure Print_Out (C : Cursor) is
      begin
         Info (Ident_String & Element (C));
      end Print_Out;
   begin
      Info ("Current state of computed closure:");

      if Closure.Is_Empty then
         Info (Ident_String & "empty");
      else
         Closure.Iterate (Print_Out'Access);
      end if;
   end Closure_Debug_Image_Closure;

   -------------------
   -- Closure_Setup --
   -------------------

   procedure Closure_Setup (My_Project : in out Arg_Project_Type) is
      Dummy_Proj_File_Name : constant String :=
        Tool_Temp_Dir.all & Directory_Separator & "closure.gpr";

      Old_Config_Option : constant String :=
        "-gnatec=" & Get_Config_File_Name;

      F : File_Type;

      Success : Boolean;
   begin
      Tool_Computes_Closure := True;

      Closure_Object_Subdir :=
        new String'("TMP_" & Tool_Name.all & "_closure");

      --  * creates a temporary wrapper project file used to compute closure;
      pragma Assert (Normalize_Pathname (Get_Current_Dir) =
                      Normalize_Pathname (Tool_Temp_Dir.all));

      Ada.Text_IO.Create (F, Out_File, Dummy_Proj_File_Name);

      Put_Line
        (F,
         "project closure extends all """
         & Source_Prj (My_Project)
         & """ is");
      Put_Line (F, "end closure;");
      Close (F);

      --  * unloads the tool argument project and loads this temporary project
      --    file;
      --  We have to delete the mapping and configuration files created for
      --  the originally loaded argument project
      GNATCOLL.Projects.Aux.Delete_All_Temp_Files
        (My_Project.Root_Project);
      My_Project.Unload;

      Project_Env.Set_Object_Subdir (+Closure_Object_Subdir.all);
      My_Project.Load (Create (+Dummy_Proj_File_Name), Project_Env);

      --  Storing objects subdirs for closure computation created on the fly
      --  to delete them at clean-up

      declare
         Iter : Project_Iterator :=
           Start (My_Project.Root_Project,
                  Recursive        => True,
                  Direct_Only      => False,
                  Include_Extended => True);
      begin
         while Current (Iter) /= No_Project loop
            Closure_Subdirs_To_Clean.Include
              (Current (Iter).Object_Dir.Display_Full_Name);
            Next (Iter);
         end loop;
      end;

      Create_Mapping_File (My_Project);
      Create_Configuration_File (My_Project);
      --  and we have to correct configuration file option in the argument list
      --  used to create the trees

      for J in Arg_List'Range loop
         if Arg_List (J).all = Old_Config_Option then
            Free (Arg_List (J));
            Arg_List (J) := new String'("-gnatec=" & Get_Config_File_Name);
            exit;
         end if;
      end loop;

      --  * stores all the sources from this project into the source table, for
      --    each sourceexcept the main unit the status is set to
      --    Waiting_For_Closure

      Get_Sources_From_Project (My_Project, Unconditionally => True);

      Read_Args_From_Temp_Storage
        (Duplication_Report => False,
         Arg_Project        => My_Project,
         Status             => Waiting_For_Closure);

      Set_Individual_Source_Options (My_Project);
      Total_Sources := Natural (Last_Source);
      Sources_Left  := Total_Sources;

      --  * parallel tree creation should be disabled if set!
      if Process_Num /= 1 then

         for J in Mapping_File_Copies'Range loop
            GNAT.OS_Lib.Delete_File (Get_Mapping_File_Copy_Name (J), Success);

            if not Success then
               Error ("cannot delete copy of mapping file " &
                      Get_Mapping_File_Copy_Name (J));
            end if;
         end loop;

         GNAT.OS_Lib.Free (Mapping_File_Copies);

         Process_Num := 1;
         ASIS_UL.Tree_Creation.Set_Max_Processes;

         if not Quiet_Mode then
            Info ("parallel tree creation disabled because of " &
                  "closure computing");
         end if;
      end if;

   end Closure_Setup;

   -------------------------------
   -- Create_Configuration_File --
   -------------------------------

   procedure Create_Configuration_File (My_Project : Arg_Project_Type) is
      Config_Name : constant String :=
        GNATCOLL.Projects.Aux.Create_Config_Pragmas_File
          (My_Project.Root_Project);
   begin
      Store_Config_File_Name (Config_Name);
      Store_Option ("-gnatec=" & Config_Name);
   end Create_Configuration_File;

   -------------------------
   -- Create_Mapping_File --
   -------------------------

   procedure Create_Mapping_File (My_Project : Arg_Project_Type) is
      Mapping_Name : constant String :=
        GNATCOLL.Projects.Aux.Create_Ada_Mapping_File
          (My_Project.Root_Project);
   begin
      Store_Mapping_File_Name (Mapping_Name);
      --  We do not store the corresponding '=gnatem=...' option here - if we
      --  are in parallel tree creation mode, we need a separate mapping file
      --  for each thread
   end Create_Mapping_File;

   --------------------------------
   -- Create_Mapping_File_Copies --
   --------------------------------

   procedure Create_Mapping_File_Copies is
      Success : Boolean;
   begin
      if Get_Mapping_File_Name = ""
        or else
         Process_Num = 1
      then
         return;
      end if;

      Mapping_File_Copies := new String_List (1 .. Process_Num - 1);
      Mapping_File_Occupation :=
        new Mapping_File_Occupations'(1 .. Process_Num - 1 => True);
      --  All copies of the mapping file are free for use

      for J in Mapping_File_Copies'Range loop
         Copy_File
            (Name     => Get_Mapping_File_Name,
             Pathname => Get_Mapping_File_Name & Trim (J'Img, Both),
             Success  => Success);

         Mapping_File_Copies (J) :=
            new String'(Get_Mapping_File_Name & Trim (J'Img, Both));
      end loop;

   exception
      when others =>
         Error ("Cannot create copies of mapping file for " &
                "parallel tree creation");
         raise Fatal_Error;
   end Create_Mapping_File_Copies;

   ------------------------------------
   -- Extract_Compilation_Attributes --
   ------------------------------------

   procedure Extract_Compilation_Attributes
     (My_Project : in out Arg_Project_Type)
   is
      Proj      : Project_Type := My_Project.Root_Project;
      Attr_Proj : Project_Type;

      --  Attributes to check:
      Builder_Global_Configuration_Pragmas : constant Attribute_Pkg_String :=
        Build (Builder_Package, "Global_Configuration_Pragmas");
      Builder_Global_Config_File : constant Attribute_Pkg_String :=
        Build (Builder_Package, "Global_Config_File");

      Needs_RTS : Boolean := False;
   begin

      if Has_Attribute (Proj, Builder_Global_Configuration_Pragmas) then
         Attr_Proj := Attribute_Project
                        (Project   => Proj,
                         Attribute => Builder_Global_Configuration_Pragmas);

         declare
            Attr_Val : constant String :=
              Attribute_Value (Proj, Builder_Global_Configuration_Pragmas);
         begin
            Store_Option
              ("-gnatec=" &
               Normalize_Pathname
                 (Name      => Attr_Val,
                  Directory =>
                    GNAT.Directory_Operations.Dir_Name
                      (Display_Full_Name (Project_Path (Attr_Proj)))));
         end;
      end if;

      if Has_Attribute (Proj, Builder_Global_Config_File, "ada") then
         Attr_Proj := Attribute_Project
                        (Project   => Proj,
                         Index     => "ada",
                         Attribute => Builder_Global_Config_File);

         declare
            Attr_Val : constant String :=
              Attribute_Value (Proj, Builder_Global_Config_File, "ada");
         begin

            Store_Option
              ("-gnatec=" &
               Normalize_Pathname
                 (Name      => Attr_Val,
                  Directory =>
                    GNAT.Directory_Operations.Dir_Name
                      (Display_Full_Name (Project_Path (Attr_Proj)))));
         end;
      end if;

      if Get_RTS_Path /= "" then
         --  We have --RTS specified as a command line parameter, so no
         --  need to get it from a project file
         goto No_Need_To_Get_RTS;
      end if;

      Needs_RTS := Has_Attribute (Proj, Runtime_Attribute, Index => "Ada");

      while not Needs_RTS
          and then
            Proj /= No_Project
      loop
         Proj := Extended_Project (Proj);
         Needs_RTS := Has_Attribute (Proj, Runtime_Attribute, Index => "Ada");
      end loop;

      if Needs_RTS then
         --???
         --  There is some code duplication with
         --  ASIS_UL.Compiler_Options.Get_Full_Path_To_RTS, needs refactoring
         declare
            Dirs : constant File_Array := Project_Env.Predefined_Object_Path;
            Idx  : Natural;
         begin
            for J in Dirs'Range loop
               Idx := Index (Dirs (J).Display_Full_Name, "adalib");

               if Idx /= 0 then
                  declare
                     Result : constant String   := Dirs (J).Display_Full_Name;
                     F_Idx  : constant Positive := Result'First;
                  begin
                     Store_Option
                       ("--RTS=" & Trim (Result (F_Idx .. Idx - 2), Both));

                     Custom_RTS := new String'(Get_Runtime (Proj));

                     goto Done;
                  end;
               end if;
            end loop;

            Error ("cannot detect the full path to runtime " &
                   "from Runtime attribute");
            raise Fatal_Error;
            <<Done>> null;
         end;
      end if;

      <<No_Need_To_Get_RTS>> null;

   end Extract_Compilation_Attributes;

   --------------------------
   -- Extract_Tool_Options --
   --------------------------

   procedure Extract_Tool_Options (My_Project : in out Arg_Project_Type) is
      Arg_File_Name         : String_Access;

      Proj          : constant Project_Type := Root_Project (My_Project);

      Attr_Switches : constant Attribute_Pkg_List :=
        Build (Tool_Package_Name (Arg_Project_Type'Class (My_Project)),
              "Switches");
      Attr_Def_Switches : constant Attribute_Pkg_List
        := Build (Tool_Package_Name (Arg_Project_Type'Class (My_Project)),
                  "Default_Switches");

      Attr_Indexes  : String_List_Access;
      Tool_Switches : String_List_Access;
      Index_Found   : Boolean := False;

      Options_Defined : Boolean := False;

      Proj_Args_Parser : Opt_Parser;

   begin
      if Files_In_Temp_Storage = 1 then
         Arg_File_Name := new String'(First_File_In_Temp_Storage);

         Attr_Indexes :=
           new String_List'(Attribute_Indexes (Proj, Attr_Switches));

         for J in  Attr_Indexes'Range loop
            if Arg_File_Name.all = Attr_Indexes (J).all then
               --  What about non-case-sensitive system?
               Index_Found := True;
               exit;
            end if;
         end loop;
      end if;

      if not Index_Found then
         --  We have to get tool options from Default_Sources

         if Has_Attribute (Proj, Attr_Def_Switches, "ada") then
            Tool_Switches := Attribute_Value (Proj, Attr_Def_Switches, "ada");
            Options_Defined := True;
         end if;
      else
         if Has_Attribute (Proj, Attr_Switches) then
            Tool_Switches :=
              Attribute_Value (Proj, Attr_Switches, Arg_File_Name.all);
            Options_Defined := True;
         end if;
      end if;

      if Options_Defined then
         Initialize_Option_Scan
           (Parser                   => Proj_Args_Parser,
            Command_Line             => Tool_Switches,
            Switch_Char              => '-',
            Stop_At_First_Non_Switch => False,
            Section_Delimiters       => My_Project.Get_Section_Delimiters);

         Scan_Arguments
           (My_Project  => Arg_Project_Type'Class (My_Project),
            Parser      => Proj_Args_Parser,
            In_Switches => Index_Found);

      end if;

   end Extract_Tool_Options;

   --------------------------
   -- Get_Config_File_Name --
   --------------------------

   function Get_Config_File_Name return String is
   begin
      if Config_File_Name = null then
         return "";
      else
         return Config_File_Name.all;
      end if;
   end Get_Config_File_Name;

   ---------------------------
   -- Get_Free_Mapping_File --
   ---------------------------

   function Get_Free_Mapping_File return Natural is
   begin
      for J in Mapping_File_Occupation'Range loop
         if Mapping_File_Occupation (J) then
            return J;
         end if;
      end loop;

      Error ("no free mapping file for tree creation process");
      raise Fatal_Error;

   end Get_Free_Mapping_File;

   ---------------------------
   -- Get_Mapping_File_Name --
   ---------------------------

   function Get_Mapping_File_Name return String is
   begin
      if Mapping_File_Name = null then
         return "";
      else
         return Mapping_File_Name.all;
      end if;
   end Get_Mapping_File_Name;

   --------------------------------
   -- Get_Mapping_File_Copy_Name --
   --------------------------------

   function Get_Mapping_File_Copy_Name (J : Natural) return String is
   begin
      pragma Assert (J in 1 .. Process_Num - 1);
      pragma Assert (Mapping_File_Copies /= null
            and then Mapping_File_Copies'Length >= J);

      return Mapping_File_Copies (J).all;
   end Get_Mapping_File_Copy_Name;

   ------------------------------
   -- Get_Sources_From_Project --
   ------------------------------

   procedure Get_Sources_From_Project
     (My_Project : Arg_Project_Type;
      Unconditionally : Boolean := False)
   is
      Prj      : Project_Type;
      Files    : File_Array_Access;
      Success  : Boolean := False;
   begin
      if Unconditionally
        or else
         (Compute_Project_Closure (Arg_Project_Type'Class (My_Project))
         and then
            (ASIS_UL.Options.No_Argument_File_Specified
           or else
             (U_Option_Set
             and then
              (not File_List_Specified
              or else
               Index (Tool_Name.all, "gnatelim") /= 0))))
      then
         if Unconditionally
          or else
            Main_Unit = null
         then
            Prj := My_Project.Root_Project;

            Files := Prj.Source_Files
              (Recursive => U_Option_Set or else Unconditionally);

            for F in Files'Range loop
               if not Is_Externally_Built (Files (F), My_Project)
                 and then
                  Is_Ada_File (Files (F), My_Project)
               then
                  ASIS_UL.Source_Table.Store_Sources_To_Process
                    (Files (F).Display_Base_Name);
               end if;
            end loop;

            if Unconditionally
              or else
               U_Option_Set
            then
               if Files'Length = 0 then
                  Error (My_Project.Source_Prj.all &
                         " does not contain source files");
                  return;
               end if;
            else
               Prj := Extended_Project (Prj);

               while Prj /= No_Project loop
                  Unchecked_Free (Files);
                  Files := Prj.Source_Files (Recursive => False);

                  for F in Files'Range loop
                     if not Is_Externally_Built (Files (F), My_Project)
                       and then
                        Is_Ada_File (Files (F), My_Project)
                     then
                        ASIS_UL.Source_Table.Store_Sources_To_Process
                          (Files (F).Display_Base_Name);
                     end if;
                  end loop;

                  Prj := Extended_Project (Prj);
               end loop;

            end if;

         else
            Store_Files_From_Closure (My_Project, Success);
         end if;
      end if;
   end Get_Sources_From_Project;

   ----------------------------
   -- Initialize_Environment --
   ----------------------------

   procedure Initialize_Environment is
      Firts_Idx : constant Natural := Tool_Name'First;
      Last_Idx  : constant Natural :=
        Index (Tool_Name.all, "-", Ada.Strings.Backward);

   begin
      GNATCOLL.Traces.Parse_Config_File;
      Initialize (Project_Env);

      Project_Env.Set_Target_And_Runtime
        ((if Target = null then
             Tool_Name (Firts_Idx .. Last_Idx - 1)
          else
             Target.all),
         Get_RTS_Path);

      if Follow_Symbolic_Links then
         Project_Env.Set_Trusted_Mode (True);
      end if;

      Set_Automatic_Config_File (Project_Env.all);

   end Initialize_Environment;

   -----------------
   -- Is_Ada_File --
   -----------------

   function Is_Ada_File
     (File :       Virtual_File;
      My_Project : Arg_Project_Type)
      return Boolean
   is
   begin
      return To_Lower (Language (Info (My_Project, File))) = "ada";
   end Is_Ada_File;

   -------------------------
   -- Is_Externally_Built --
   -------------------------

   function Is_Externally_Built
     (File :       Virtual_File;
      My_Project : Arg_Project_Type)
      return Boolean
   is
      F_Info : constant File_Info    := Info (My_Project, File);
      Proj   : constant Project_Type := Project (F_Info);
      Attr   : constant Attribute_Pkg_String := Build ("", "externally_built");
   begin
      if Has_Attribute (Proj, Attr) then
         if Attribute_Value (Proj, Attr) = "true" then
            return True;
         end if;
      end if;
      return False;
   end Is_Externally_Built;

   ------------------
   -- Is_Specified --
   ------------------

   function Is_Specified (My_Project : Arg_Project_Type) return Boolean is
   begin
      return My_Project.Source_Prj /= null;
   end Is_Specified;

   -----------------------------
   -- Load_Aggregated_Project --
   -----------------------------

   procedure Load_Aggregated_Project
     (My_Project : in out Arg_Project_Type'Class)
   is
      procedure Errors (S : String);
      --  ??? needs improvement!
      procedure Errors (S : String) is
      begin
         if Index (S, " not a regular file") /= 0 then
            Error ("project file " & My_Project.Source_Prj.all & " not found");
         elsif Index (S, "is illegal for typed string") /= 0 then
            Error (S);
            raise Parameter_Error;
         elsif Index (S, "warning") /= 0
              and then Index (S, "directory") /= 0
              and then Index (S, "not found") /= 0
         then
            return;
         else
            Error (S);
         end if;
      end Errors;
   begin
      My_Project.Load
        (GNATCOLL.VFS.Create (+My_Project.Source_Prj.all),
         Project_Env,
         Errors              => Errors'Unrestricted_Access,
         Report_Missing_Dirs => False);

      if My_Project.Root_Project = No_Project then
         Error ("project not loaded");
      end if;

      pragma Assert (Is_Aggregate_Project (My_Project.Root_Project));

      My_Project.Unload;

      if Subdir_Name /= null then
         Set_Object_Subdir (Project_Env.all, +Subdir_Name.all);
      end if;

      Load
        (Self                => My_Project,
         Root_Project_Path   => Create (Filesystem_String
                                          (Get_Aggregated_Project)),
         Env                 => Project_Env,
         Errors              => Errors'Unrestricted_Access,
         Report_Missing_Dirs => False);

   end Load_Aggregated_Project;

   -----------------------
   -- Load_Tool_Project --
   -----------------------

   procedure Load_Tool_Project (My_Project : in out Arg_Project_Type) is
      Aggregated_Prj_Name : Filesystem_String_Access;

      procedure Errors (S : String);
      procedure Errors (S : String) is
      begin
         if Index (S, " not a regular file") /= 0 then
            Error ("project file " & My_Project.Source_Prj.all & " not found");
         elsif Index (S, "is illegal for typed string") /= 0 then
            Error (S);
            raise Parameter_Error;
         elsif Index (S, "warning") /= 0
              and then Index (S, "directory") /= 0
              and then Index (S, "not found") /= 0
         then
            return;
         else
            Error (S);
         end if;
      end Errors;
   begin
      if Subdir_Name /= null then
         Set_Object_Subdir (Project_Env.all, +Subdir_Name.all);
      end if;

      My_Project.Load
        (GNATCOLL.VFS.Create (+My_Project.Source_Prj.all),
         Project_Env,
         Errors              => Errors'Unrestricted_Access,
         Report_Missing_Dirs => False);

      if Is_Aggregate_Project (My_Project.Root_Project) then

         if My_Project.Root_Project = No_Project then
            Error ("project not loaded");
         end if;

         Collect_Aggregated_Projects (My_Project.Root_Project);

         if Debug_Flag_A then
            Aggregated_Projects_Debug_Image;
         end if;

         N_Of_Aggregated_Projects := Num_Of_Aggregated_Projects;

         case N_Of_Aggregated_Projects is
            when 0 =>
               --  Pathological case, but we need to generate a reasonable
               --  message
               Error
                 ("aggregate project does not contain anything to process");
               raise Parameter_Error;

            when 1 =>
               --  Important and useful particular case - exactly one project
               --  is aggregated, so we load it in the environment that already
               --  has all the settings from the argument aggregate project:

               Aggregated_Prj_Name := new Filesystem_String'
                 (Full_Name (Get_Aggregated_Prj_Src));

               My_Project.Unload;

               Load
                 (Self                => My_Project,
                  Root_Project_Path   => Create (Aggregated_Prj_Name.all),
                  Env                 => Project_Env,
                  Errors              => Errors'Unrestricted_Access,
                  Report_Missing_Dirs => False);
               --  Can we reuse My_Project here or should we use another
               --  Project_Tree variable???

               Free (Aggregated_Prj_Name);

            when others =>
               --  General case - more than one project is aggregated. We have
               --  process them one by one spawning gnatcheck for each project.

               if not Has_Suffix (Tool_Name.all, Suffix => "gnatcheck") then
                  --  Currently full support for aggregate projects is
                  --  implemented for gnatcheck only
                  Error ("aggregate projects are not supported");
                  raise Parameter_Error;
               end if;
         end case;
      end if;

   exception
      when Invalid_Project =>
         raise Parameter_Error;
   end Load_Tool_Project;

   ------------------------------
   -- Needed_For_Tree_Creation --
   ------------------------------

   function Needed_For_Tree_Creation (Option : String) return Boolean is
      Result : Boolean := False;
   begin
      if Has_Prefix (Option, Prefix => "-gnateD")
        or else Has_Prefix (Option, Prefix => "-gnatep")
      then
         if Preprocessing_Allowed then
            Result := True;
         else
            Error ("cannot preprocess argument file, " &
                     "do preprocessing as a separate step");
            raise Parameter_Error;
         end if;
      elsif Option = "-gnat83"
        or else Option = "-gnat95"
        or else Option = "-gnat05"
        or else Option = "-gnat2005"
        or else Option = "-gnat12"
        or else Option = "-gnat2012"
        or else Option = "-gnatdm"
        or else Option = "-gnatd.V"
        or else Option = "-gnatd.M"
        or else Option = "-gnatI"
        or else
         Has_Prefix (Option, Prefix => "--RTS=")
      then
         Result := True;
      end if;

      return Result;
   end Needed_For_Tree_Creation;

   ----------------------
   -- Print_Debug_Info --
   ----------------------

   procedure Print_Debug_Info (I : File_Info) is
   begin
      Info ("  Unit_Part " & Unit_Part (I)'Img);
      Info ("  Unit_Name " & Unit_Name (I));
      Info ("  File      " & Display_Base_Name (File (I)));
   end Print_Debug_Info;

   --------------------------
   -- Process_Project_File --
   --------------------------

   procedure Process_Project_File
     (My_Project : in out Arg_Project_Type'Class)
   is
   begin

      if not My_Project.Is_Specified then
         return;
      end if;

      Register_Tool_Attributes (My_Project);
      Initialize_Environment;
      Set_External_Values (My_Project);

      if Aggregated_Project then
         Load_Aggregated_Project (My_Project);
      else
         Load_Tool_Project (My_Project);
      end if;

      if N_Of_Aggregated_Projects > 1 then

         if not No_Argument_File_Specified then
            Error ("no argument file should be specified if aggregate " &
                   "project");
            Error_No_Tool_Name
              ("aggregates more than one non-aggregate project");

            raise Parameter_Error;
         end if;

         if Main_Unit /= null then
            Error ("'-U main' cannot be used if aggregate project");
            Error_No_Tool_Name
              ("aggregates more than one non-aggregate project");

            raise Parameter_Error;
         end if;

         --  No information is extracted from the aggregate project
         --  itself
         In_Aggregate_Project := True;
         return;
      else
         Extract_Compilation_Attributes (My_Project);
         Extract_Tool_Options           (My_Project);
         Get_Sources_From_Project       (My_Project);
         Create_Mapping_File            (My_Project);
         Create_Configuration_File      (My_Project);
      end if;

--      GPR.Current_Verbosity := GPR.High;

   end Process_Project_File;

   -------------------------------
   -- Report_Aggregated_Project --
   -------------------------------

   procedure Report_Aggregated_Project
     (Aggregate_Prj          : Arg_Project_Type;
      Arrgegated_Prj_Name    : String;
      Expected_Text_Out_File : String;
      Expected_XML_Out_File  : String)
   is
      pragma Unreferenced (Aggregate_Prj);
   begin

      if Text_Report_ON then
         Report ("");
         Report ("Processing aggregated project " & Arrgegated_Prj_Name);
         Report ("Expected report file: " & Expected_Text_Out_File);
      end if;

      if XML_Report_ON then
         XML_Report ("<aggregated-project>",
                     Indent_Level => 2);

         XML_Report ("<project-file>" & Arrgegated_Prj_Name &
                     "</project-file>",
                     Indent_Level => 3);

         XML_Report ("<report-file>" & Expected_XML_Out_File &
                     "</report-file>",
                     Indent_Level => 3);
      end if;
   end Report_Aggregated_Project;

   -----------------------------------------
   -- Report_Aggregated_Project_Exit_Code --
   -----------------------------------------

   procedure Report_Aggregated_Project_Exit_Code
     (Aggregate_Prj : Arg_Project_Type;
      Exit_Code     : Integer)
   is
      pragma Unreferenced (Aggregate_Prj);
   begin
      if Text_Report_ON then
         Report ("Exit code is" & Exit_Code'Img);
      end if;

      if XML_Report_ON then
         XML_Report ("<exit-code>" & Image (Exit_Code) & "</exit-code>",
                     Indent_Level => 3);

         XML_Report ("</aggregated-project>",
                     Indent_Level => 2);
      end if;
   end Report_Aggregated_Project_Exit_Code;

   --------------------
   -- Scan_Arguments --
   --------------------

   procedure Scan_Arguments
     (My_Project  : in out Arg_Project_Type;
      First_Pass  :        Boolean    := False;
      Parser      :        Opt_Parser := Command_Line_Parser;
      In_Switches :        Boolean    := False)
   is
      pragma Unreferenced (In_Switches, Parser, First_Pass, My_Project);
   begin
      Error ("Scan_Arguments procedure should be defined for the tool!");
      raise Fatal_Error;
   end Scan_Arguments;

   ------------------------
   -- Section_Delimiters --
   ------------------------

   function Section_Delimiters (My_Project : Arg_Project_Type) return String is
      pragma Unreferenced (My_Project);
   begin
      return "cargs asis-tool-args";
      --  The undocumented -asis-tool-args section is used in incremental mode
      --  to pass extra args *after* the other section(s), such as -cargs down
      --  to the inner invocations of the tool.
   end Section_Delimiters;

   function Get_Section_Delimiters
     (My_Project : Arg_Project_Type'Class) return String is
      Delim : constant String := My_Project.Section_Delimiters;
   begin
      return
         (if Mimic_gcc
            then Replace_String
              (Delim, From => "cargs",
               To => "inner-cargs") -- See doc in asis_ul-environment.adb
            else Delim);
   end Get_Section_Delimiters;

   --------------
   -- Set_Busy --
   --------------

   procedure Set_Busy (J : Natural) is
   begin
      if Mapping_File_Occupation = null then
         return;
      end if;

      pragma Assert (J in 1 .. Process_Num - 1);
      pragma Assert (Mapping_File_Occupation (J));
      Mapping_File_Occupation (J) := False;
   end Set_Busy;

   -------------------------
   -- Set_External_Values --
   -------------------------

   procedure Set_External_Values (My_Project : Arg_Project_Type) is
      use X_Vars_Sets;
      C            : Cursor;
      Next_Var     : X_Var_Record;
      GPR_TOOL_Set : Boolean := False;
   begin
      C := First (X_Vars);

      while Has_Element (C) loop

         Next_Var := Element (C);

         Project_Env.Change_Environment
           (Next_Var.Var_Name.all, Next_Var.Var_Value.all);

         if Next_Var.Var_Name.all = "GPR_TOOL" then
            GPR_TOOL_Set := True;
         end if;

         C := Next (C);

      end loop;

      --  Set GPR_TOOL, if needed
      if not Ada.Environment_Variables.Exists ("GPR_TOOL")
        and then
         not GPR_TOOL_Set
      then
         Project_Env.Change_Environment
           ("GPR_TOOL",
            To_Lower (Tool_Package_Name (My_Project)));
      end if;

   end Set_External_Values;

   --------------
   -- Set_Free --
   --------------

   procedure Set_Free (J : Natural) is
   begin
      if Mapping_File_Occupation = null then
         return;
      end if;

      pragma Assert (J in 1 .. Process_Num - 1);
      pragma Assert (Mapping_File_Occupation (J) = False);
      Mapping_File_Occupation (J) := True;
   end Set_Free;

   ----------------------------
   -- Set_Global_Result_Dirs --
   ----------------------------

   procedure Set_Global_Result_Dirs (My_Project : in out Arg_Project_Type) is
      Global_Report_Dir : Virtual_File;
   begin

      if not No_Object_Dir then
         Global_Report_Dir := My_Project.Root_Project.Object_Dir;

         if Global_Report_Dir = No_File then
            Global_Report_Dir := My_Project.Root_Project.Project_Path;
         end if;

         Set_Global_Report_Dir (Display_Dir_Name (Global_Report_Dir));
      end if;

   end Set_Global_Result_Dirs;

   -----------------------------------
   -- Set_Individual_Source_Options --
   -----------------------------------

   procedure Set_Individual_Source_Options (My_Project : Arg_Project_Type) is
      Sources : constant File_Array_Access :=
        My_Project.Root_Project.Source_Files (Recursive => True);

      Per_File_Output_Needed : constant Boolean :=
        Needs_Per_File_Output (Arg_Project_Type'Class (My_Project));

      Project_U   : Project_Type;
      Attr_Proj   : Project_Type;
      Source_Info : File_Info;

      Sws        : String_List_Access;
      Is_Default : Boolean := False;
      SF         : SF_Id;

      File_Switches : String_Access;
      Tmp           : String_Access;
      Gnatec_Opts   : String_List_Access;
      Tmp_Str_List  : String_List_Access;

      procedure Scan_Switches;
      --  Works on Sws as on a global object. Scans the argument, checks if
      --  the element being visited is needed for tree creation, and if it is,
      --  stores it in File_Switches

      procedure Add_Switch (S : String);
      --  Adds S to File_Switches;

      function Copy_Of (L : String_List_Access) return String_List_Access;
      --  Create a copy of the list to be stored in persistent table, may be
      --  needed for a list that can be freed.

      Compiler_Local_Configuration_Pragmas : constant Attribute_Pkg_String :=
        Build (Compiler_Package, "Local_Configuration_Pragmas");
      Compiler_Local_Config_File : constant Attribute_Pkg_String :=
        Build (Compiler_Package, "Local_Config_File");

      function Copy_Of (L : String_List_Access) return String_List_Access is
         Result : constant String_List_Access := new String_List (L'Range);
      begin
         for J in L'Range loop
            Result (J) := new String'(L (J).all);
         end loop;

         return Result;
      end Copy_Of;

      function Normalize_Switch (S : String) return String;
      --  If the switch contains a path, normalizes this path. This is needed
      --  because the switch will be used from the temporary directory created
      --  by a tool

      procedure Add_Switch (S : String) is
      begin
         if File_Switches = null then
            File_Switches := new String'(S);
         else
            Tmp := new String'(File_Switches.all & ' ' & S);
            Free (File_Switches);
            File_Switches := new String'(Tmp.all);
            Free (Tmp);
         end if;
      end Add_Switch;

      procedure Scan_Switches is
      begin
         for J in Sws'Range loop
            if ASIS_UL.Debug.Debug_Flag_C then
               Info_No_EOL (Sws (J).all & ' ');
            end if;

            if Needed_For_Tree_Creation (Sws (J).all) then
               Add_Switch (Normalize_Switch (Sws (J).all));
            end if;
         end loop;

         if ASIS_UL.Debug.Debug_Flag_C then
            if Is_Default then
               Info_No_EOL ("(default)");
            end if;

            Info ("");
         end if;

         Free (Sws);
      end Scan_Switches;

      function Normalize_Switch (S : String) return String is
         Res : constant String := Trim (S, Both);
         Opt_Start  : constant Natural := S'First;
         Opt_End    :          Natural;
         Path_Start :          Natural;
         Path_End   : constant Natural := S'Last;
      begin
         if Res'Length >= 9
           and then
            Res (Opt_Start .. Opt_Start + 5) = "-gnate"
           and then
            Res (Opt_Start + 6) in 'e' | 'p'
         then
            Opt_End    := Opt_Start + 6;
            Path_Start := Opt_End + 1;

            while Path_Start < Path_End and then
                  Res (Path_Start) in ' ' | '='
            loop
               Path_Start := Path_Start + 1;
            end loop;

            return Res (Opt_Start .. Opt_End) &
                        Normalize_Pathname (Res (Path_Start .. Path_End));
         else
            return Res;
         end if;
      end Normalize_Switch;

   --  Start of processing for Set_Individual_Source_Options

   begin
      for S in Sources'Range loop
         Source_Info  := My_Project.Info (Sources (S));
         Project_U    := Project (Source_Info);

         SF :=
           File_Find (Display_Base_Name (Sources (S)), Use_Short_Name => True);

         if Present (SF) then

            if ASIS_UL.Debug.Debug_Flag_C then
               Info ("Switches defined for " & Short_Source_Name (SF));

               if ASIS_UL.Debug.Debug_Flag_P then
                  Print_Debug_Info (Source_Info);
               end if;
            end if;

            Switches
              (Project          => Project_U,
               In_Pkg           => Compiler_Package,
               File             => Sources (S),
               Language         => "ada",
               Value            => Sws,
               Is_Default_Value => Is_Default);

            Scan_Switches;

            Switches
              (Project          => Project_U,
               In_Pkg           => Builder_Package,
               File             => Sources (S),
               Language         => "ada",
               Value            => Sws,
               Is_Default_Value => Is_Default);

            Scan_Switches;

            if not U_Option_Set
              and then
               Has_Attribute
                 (Project_U, Compiler_Local_Configuration_Pragmas)
            then
               Attr_Proj :=
                 Attribute_Project
                   (Project   => Project_U,
                    Attribute => Compiler_Local_Configuration_Pragmas);
               declare
                  Attr_Val : constant String :=
                    Attribute_Value
                      (Project_U, Compiler_Local_Configuration_Pragmas);
               begin
                  --  We cannot use Add_Switch for '-gnatec=<path>' options
                  --  because path may contain spaces

                  Gnatec_Opts := new String_List'
                    (1 => new String'
                      ("-gnatec=" & Normalize_Pathname
                       (Name      => Attr_Val,
                        Directory =>
                          GNAT.Directory_Operations.Dir_Name
                           (Display_Full_Name (Project_Path (Attr_Proj))))));

               end;
            end if;

            if not U_Option_Set
              and then
               Has_Attribute
                 (Project_U, Compiler_Local_Config_File, "ada")
            then
               Attr_Proj :=
                 Attribute_Project
                   (Project   => Project_U,
                    Attribute => Compiler_Local_Config_File,
                    Index     => "ada");

               declare
                  Attr_Val : constant String :=
                    Attribute_Value
                      (Project_U, Compiler_Local_Config_File, "ada");
               begin
                  if Gnatec_Opts = null then
                     Gnatec_Opts := new String_List'
                       (1 => new String'
                         ("-gnatec=" & Normalize_Pathname
                          (Name      => Attr_Val,
                           Directory =>
                             GNAT.Directory_Operations.Dir_Name
                              (Display_Full_Name
                                (Project_Path (Attr_Proj))))));
                  else
                     Tmp_Str_List := new String_List'(Gnatec_Opts.all);

                     Free (Gnatec_Opts);

                     Gnatec_Opts := new String_List'
                       (Tmp_Str_List.all &
                        new String'
                         ("-gnatec=" &
                          Normalize_Pathname
                            (Name      => Attr_Val,
                             Directory =>
                                GNAT.Directory_Operations.Dir_Name
                                  (Display_Full_Name
                                    (Project_Path (Attr_Proj))))));

                     Free (Tmp_Str_List);
                  end if;

               end;
            end if;

            if File_Switches /= null or else Gnatec_Opts /= null then
               declare
                  Empty_Arg_List : Argument_List (2 .. 1);

                  Switches_To_Add : constant Argument_List :=
                    (if File_Switches /= null then
                        Argument_String_To_List (File_Switches.all).all
                     else
                        Empty_Arg_List) &
                    (if Gnatec_Opts /= null then
                        Copy_Of (Gnatec_Opts).all
                     else
                        Empty_Arg_List);
               begin

                  Add_Compilation_Switches
                    (SF, new String_List'(Switches_To_Add));

                  if ASIS_UL.Debug.Debug_Flag_C then
                     Info ("Stored switches :");

                     for J of Switches_To_Add loop
                        Info (" >>>" & J.all & "<<<");
                     end loop;
                  end if;

                  Free (File_Switches);
                  Free (Gnatec_Opts);
               end;
            elsif ASIS_UL.Debug.Debug_Flag_C then
               Info ("No stored switches");
            end if;

            --  Defining the directory to place the file-specific results into:

            if not No_Object_Dir
              and then
               Per_File_Output_Needed
            then
               Set_Result_Dir
                 (SF,
                  Source_Result_Dir
                    (Arg_Project_Type'Class (My_Project),
                     Project_U,
                     Sources (S)));
            end if;
         end if;
      end loop;
   end Set_Individual_Source_Options;

   ---------------------
   -- Set_Subdir_Name --
   ---------------------

   procedure Set_Subdir_Name (S : String) is
   begin
      Free (Subdir_Name);
      Subdir_Name := new String'(S);

   end Set_Subdir_Name;

   procedure Set_Tree_Creator (My_Project  : Arg_Project_Type) is
--      Full_Tool_Name : constant String := Ada.Command_Line.Command_Name;
      Exe_Suffix     : String_Access   := Get_Executable_Suffix;
      Idx            : Natural;

      Gprbuild_Name  : constant String := "gprbuild";
      GNSA_Path      : String_Access;
   begin

      if Target = null
        --  No --target= option
        and then
          My_Project.Is_Specified
      then
         Target :=
           new String'(My_Project.Root_Project.Get_Target
                       (Default_To_Host => False));
      end if;

      if A4G.GNSA_Switch.Use_GNSA then
         --  In this case we do not care about target, we use gcc and gprbuild
         --  from GNSA, and we rely on the fact that the location of these
         --  gcc and gprbuild can be computed starting from Tool_Dir
         --  We assume the following structure:
         --
         --  ...
         --   |
         --   -- bin
         --   |   |
         --   |   -- gnatcheck
         --   |   |
         --   |   -- gnatstub
         --   |   |
         --   |   |
         --   |   |
         --   |   -- ...
         --   libexec
         --       |
         --       asis-gnsa
         --            |
         --            -- bin
         --                |
         --                --- gcc
         --                --- gprbuild

         Idx       := Tool_Dir'Last - 3;
         GNSA_Path := new String'(Tool_Dir (Tool_Dir'First .. Idx) &
                                  A4G.GNSA_Switch.GNSA_Dir         &
                                  Directory_Separator & "bin");

         if ASIS_UL.Debug.Debug_Flag_C then
            Info ("*** GNSA mode");
            Info ("GNSA_Path is " & GNSA_Path.all);
         end if;

         Gcc_To_Call :=
           new String'(GNSA_Path.all & Directory_Separator &
                       "gcc" & Exe_Suffix.all);

         if not Is_Executable_File (Gcc_To_Call.all) then
            Error ("Cannot locate gcc to create trees (GNSA mode)");
            raise Fatal_Error;
         end if;

--         pragma Assert (Is_Executable_File (Gcc_To_Call.all));

         if ASIS_UL.Debug.Debug_Flag_C then
            Info ("GCC is " & Gcc_To_Call.all);
         end if;

         --  ??? !!! Temporary solution! Should be revised to make it possible
         --  just to locate gprbuild on the path

         Gprbuild_To_Call := Locate_Exec_On_Path (Gprbuild_Name);

         if not Is_Executable_File (Gprbuild_To_Call.all) then
            Error ("Cannot locate gprbuild (GNSA mode)");
            raise Fatal_Error;
         end if;

--         pragma Assert (Is_Executable_File (Gprbuild_To_Call.all));
      else

         if ASIS_UL.Debug.Debug_Flag_C then
            Info ("*** Standard GNAT mode");
         end if;

         if Target = null
           or else
            Target.all = ""
         then
            Free (Target);
            Target := new String'(Detect_Target);
         end if;

         declare
            Gcc_Name : constant String :=
              (if Target.all = "AAMP" then "gnaamp"
               elsif Target.all = "" then "gcc"
               elsif Target (Target'Last) = '-' then Target.all & "gcc"
               else Target.all & "-gcc");
         begin

            if Tool_Dir /= null then
               Setenv ("PATH",
                        Tool_Dir.all & Path_Separator & Getenv ("PATH").all);
            end if;

            Gcc_To_Call      := Locate_Exec_On_Path (Gcc_Name);
            Gprbuild_To_Call := Locate_Exec_On_Path (Gprbuild_Name);

            if Gcc_To_Call = null
              or else
               not Is_Executable_File (Gcc_To_Call.all)
            then
               Error ("Cannot locate " &
                      (if Gcc_To_Call = null then
                         "compiler"
                      else
                         Gcc_To_Call.all)
                      & " to create trees");
               raise Fatal_Error;
            end if;

            if not Is_Executable_File (Gprbuild_To_Call.all) then
               Error ("Cannot locate gprbuild");
               raise Fatal_Error;
            end if;

--          pragma Assert (Gcc_To_Call /= null, "could not find " & Gcc_Name);
--          pragma Assert (Gprbuild_To_Call /= null);
--          pragma Assert (Is_Executable_File (Gcc_To_Call.all));
--          pragma Assert (Is_Executable_File (Gprbuild_To_Call.all));

            Gnatmake_To_Call := Gprbuild_To_Call;
         end;

         if ASIS_UL.Debug.Debug_Flag_C then
            Info ("GCC is " & Gcc_To_Call.all);
         end if;
      end if;

      Gnatmake_To_Call := Gprbuild_To_Call;
      Free (Exe_Suffix);

      Store_Full_Path_To_RTS;
   end Set_Tree_Creator;

   ----------------
   -- Source_Prj --
   ----------------

   function Source_Prj (My_Project : Arg_Project_Type) return String is
   begin
      if Is_Specified (My_Project) then
         return My_Project.Source_Prj.all;
      else
         return "";
      end if;
   end Source_Prj;

   -----------------------
   -- Source_Result_Dir --
   -----------------------

   function Source_Result_Dir
     (My_Project  : Arg_Project_Type;
      Source_Prj  : Project_Type;
      Source_File : Virtual_File)
   return           String
   is
      pragma Unreferenced (Source_File);
      Report_Dir : Virtual_File;
   begin
      Report_Dir := Source_Prj.Object_Dir;

      if Report_Dir = No_File then
         Report_Dir := My_Project.Root_Project.Project_Path;
      else
         if not Is_Directory (Display_Dir_Name (Report_Dir)) then
            begin
               Warning ("object directory " &
                        Display_Dir_Name (Report_Dir) &
                        " not found, creating it");
               Make_Dir (Report_Dir, Recursive => True);
            exception
               when others =>
                  Error ("Cannot create missing object directory");
                  raise Fatal_Error;
            end;
         end if;
      end if;

      return Display_Dir_Name (Report_Dir);
   end Source_Result_Dir;

   ------------------------------
   -- Store_Files_From_Closure --
   ------------------------------

   procedure Store_Files_From_Closure
   (My_Project       :     Arg_Project_Type;
    Complete_Closure : out Boolean)
   is
      Closure_Files : GNATCOLL.VFS.File_Array_Access;
      Main_Files    : GNATCOLL.VFS.File_Array_Access;
      Status        : Status_Type;
   begin
      Append (Main_Files, Create (+Main_Unit.all));
      Get_Closures
        (My_Project.Root_Project,
         Main_Files,
         All_Projects => True,
         Include_Externally_Built => False,
         Status => Status,
         Result => Closure_Files);

      case Status is
         when Error =>
            Error_No_Tool_Name ("could not get closure of " & Main_Unit.all);
            raise Fatal_Error;
         when Incomplete_Closure =>
            Complete_Closure := False;

            if Index (Tool_Name.all, "gnatelim") = 0
              or else
               Verbose_Mode
            then
               Error_No_Tool_Name
                 ("could not get complete closure from build of " &
                  Main_Unit.all);
            end if;
         when Success =>
            Complete_Closure := True;
      end case;

      if Debug_Flag_U then
         Info ("Closure:");
      end if;
      for I in Closure_Files'Range loop
            ASIS_UL.Source_Table.Store_Sources_To_Process
           (Fname => Closure_Files (I).Display_Full_Name);
         if Debug_Flag_U then
            Info (Closure_Files (I).Display_Full_Name);
         end if;
      end loop;

      --  Clean-up
      Unchecked_Free (Main_Files);
      Unchecked_Free (Closure_Files);

   end Store_Files_From_Closure;

   -----------------------------
   -- Store_External_Variable --
   -----------------------------

   procedure Store_External_Variable (Var : String) is
      Var_Name_Start  : constant Natural := Var'First;
      Var_Name_End    :          Natural := Index (Var, "=");
      Var_Value_Start :          Natural;
      Var_Value_End   : constant Natural := Var'Last;

      New_Var_Rec : X_Var_Record;

      use X_Vars_Sets;
      C : Cursor;
   begin
      if Var_Name_End <= Var_Name_Start then
         Error ("wrong parameter of -X option: " & Var);
         raise Parameter_Error;
      else
         Var_Name_End    := Var_Name_End - 1;
         Var_Value_Start := Var_Name_End + 2;
         New_Var_Rec    :=
           (Var_Name  => new String'(Var (Var_Name_Start .. Var_Name_End)),
            Var_Value => new String'(Var (Var_Value_Start .. Var_Value_End)));
      end if;

      C := Find (X_Vars, New_Var_Rec);

      if Has_Element (C) then
         Replace_Element (Container => X_Vars,
                          Position  => C,
                          New_Item  => New_Var_Rec);

      else
         Insert (X_Vars, New_Var_Rec);
      end if;
   end Store_External_Variable;

   ---------------------
   -- Store_Main_Unit --
   ---------------------

   procedure Store_Main_Unit
     (Unit_Name   : String;
      Store       : Boolean := True)
   is
   begin
      if ASIS_UL.Projects.Main_Unit = null then

         if Store then
            ASIS_UL.Projects.Main_Unit := new String'(Unit_Name);
         end if;
      else
         if Store then
            Error ("cannot specify more than one main after -U");
            raise Parameter_Error;
         end if;
      end if;
   end Store_Main_Unit;

   -----------------------------
   -- Store_Mapping_File_Name --
   -----------------------------

   procedure Store_Mapping_File_Name (S : String) is
   begin
      Free (Mapping_File_Name);
      Mapping_File_Name := new String'(S);
   end Store_Mapping_File_Name;

   ----------------------------
   -- Store_Config_File_Name --
   ----------------------------

   procedure Store_Config_File_Name (S : String) is
   begin
      Free (Config_File_Name);
      Config_File_Name := new String'(S);
   end Store_Config_File_Name;

   --------------------------
   -- Store_Project_Source --
   --------------------------

   procedure Store_Project_Source
     (My_Project         : in out Arg_Project_Type;
      Project_File_Name  : String) is

      Ext : constant String :=
        (if Has_Suffix (Project_File_Name, Suffix => ".gpr")
           then ""
           else ".gpr");
   begin
      if Project_File_Set then
         Error ("cannot have several project files specified");
         raise Parameter_Error;
      else
         Project_File_Set := True;
      end if;

      My_Project.Source_Prj := new String'(Project_File_Name & Ext);

   end Store_Project_Source;

   -----------------------
   -- Tool_Package_Name --
   -----------------------

   function Tool_Package_Name (My_Project : Arg_Project_Type) return String is
      pragma Unreferenced (My_Project);

      Result    : constant String  := Tool_Name.all;
      First_Idx : Natural := Index (Result, "-", Ada.Strings.Backward);
      Last_Idx  : constant Natural := Result'Last;

   begin
      if First_Idx = 0 then
         First_Idx := Tool_Name'First;
      else
         First_Idx := First_Idx + 1;
      end if;

      return Result (First_Idx .. Last_Idx);
   end Tool_Package_Name;

end ASIS_UL.Projects;
