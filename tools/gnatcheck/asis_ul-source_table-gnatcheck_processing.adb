------------------------------------------------------------------------------
--                                                                          --
--                          GNATCHECK COMPONENTS                            --
--                                                                          --
--                ASIS_UL.SOURCE_TABLE.GNATCHECK_PROCESSING                 --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                     Copyright (C) 2004-2016, AdaCore                     --
--                                                                          --
-- GNATCHECK  is  free  software;  you can redistribute it and/or modify it --
-- under terms of the  GNU  General Public License as published by the Free --
-- Software Foundation;  either version 3, or ( at your option)  any  later --
-- version.  GNATCHECK  is  distributed in the hope that it will be useful, --
-- but  WITHOUT  ANY  WARRANTY;   without  even  the  implied  warranty  of --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General --
-- Public License for more details.  You should have received a copy of the --
-- GNU General Public License distributed with GNAT; see file  COPYING3. If --
-- not,  go  to  http://www.gnu.org/licenses  for  a  complete  copy of the --
-- license.                                                                 --
--                                                                          --
-- GNATCHECK is maintained by AdaCore (http://www.adacore.com).             --
--                                                                          --
------------------------------------------------------------------------------
pragma Ada_2012;

with Text_IO;

with Ada.Characters.Handling;    use Ada.Characters.Handling;
with Ada.Finalization;
with Ada.Directories;            use Ada;

with GNAT.Directory_Operations;  use GNAT.Directory_Operations;
with GNAT.OS_Lib;                use GNAT.OS_Lib;

with Asis;                       use Asis;
with Asis.Ada_Environments;
with Asis.Compilation_Units;     use Asis.Compilation_Units;
with Asis.Elements;              use Asis.Elements;
with Asis.Errors;
with Asis.Exceptions;
with Asis.Extensions;            use Asis.Extensions;
with Asis.Extensions.Strings;    use Asis.Extensions.Strings;
with Asis.Implementation;
with Asis.Set_Get;
with Asis.Text;                  use Asis.Text;

with ASIS_UL.Common;             use ASIS_UL.Common;
with Gnatcheck.Source_Checks;
with ASIS_UL.Debug;              use ASIS_UL.Debug;
with ASIS_UL.Environment;
with ASIS_UL.Global_State;
with ASIS_UL.Global_State.CG;
with ASIS_UL.Misc;               use ASIS_UL.Misc;
with ASIS_UL.Options;            use ASIS_UL.Options;
with ASIS_UL.Output;             use ASIS_UL.Output;
with ASIS_UL.Source_Table;
with ASIS_UL.String_Utilities;
with ASIS_UL.Tree_Creation;      use ASIS_UL.Tree_Creation;
with ASIS_UL.Utilities;          use ASIS_UL.Utilities;

with Gnatcheck.Compiler;         use Gnatcheck.Compiler;
with Gnatcheck.Diagnoses;
with Gnatcheck.Diagnoses_Old;
with Gnatcheck.Options;
with Gnatcheck.Rules;            use Gnatcheck.Rules;
with Gnatcheck.Rules.Traversing; use Gnatcheck.Rules.Traversing;
with Gnatcheck.Traversal_Stack;

package body ASIS_UL.Source_Table.Gnatcheck_Processing is

   ------------------------
   --  Local subprograms --
   ------------------------

   procedure Process_Sources_From_Table (Only_Bodies : Boolean);
   --  Processes sources stores in the sources table trying to minimize
   --  compilations needed to create the tree files. If Only_Bodies is set ON,
   --  only files with .adb suffixes are compiled for the trees.

   procedure Process_Source (SF : SF_Id; Only_Bodies : Boolean);
   --  Processes the source file stored under SF index into source file table.
   --  The caller is responsible to keep the actual parameter inside the
   --  range of the existing table entries. The processing consists of
   --  creating the tree file for this source, and if the tree is successfully
   --  created, then the ASIS Compilation Unit corresponding to this source
   --  is processed. Then this routine tries to locate in the set of ASIS
   --  Compilation Units representing by this tree units corresponding to some
   --  other sources stored in the source table, and to process all these
   --  units. When the processing is complete, the tree file and the
   --  corresponding ALI file are deleted from the temporary directory.
   --  The Only_Bodies flag is used to optimize the search for the sources for
   --  parallel tree creations.

   procedure Traverse_Source
     (CU : Asis.Compilation_Unit;
      SF : SF_Id);
   --  Implements the general ASIS Compilation Unit traversal algorithm:
   --  traverses the ASIS Compilation Unit CU contained in the source file SF,
   --  checks all the active rules, collects the information needed to create
   --  global structure. Sets SF status to Processed.

   function Deletion_Lock_File_Name return String is
     (Gnatcheck.Options.Deletion_File_Name.all & ".lockDEL");

   procedure Write_Deletion_File;
   --  Called only by inner invocations. Appends the name of the .ali file to
   --  the deletion file.  See also comments on Deletion_File_Name in
   --  Gnatcheck.Options.

   ----------------------
   -- Define_Exit_Code --
   ----------------------

   procedure Define_Exit_Code is
   begin

      if Gnatcheck.Diagnoses.Detected_Non_Exempted_Violations > 0 then
         Exit_Code := E_Violation;
      elsif Tool_Failures = 0 then
         Exit_Code := E_Success;
      else
         Exit_Code := E_Non_Trusted;
      end if;

   end Define_Exit_Code;

   ----------------------
   -- Delete_ALI_Files --
   ----------------------

   procedure Delete_ALI_Files is
      --  If this is the outer process of an incremental build, we delete all
      --  the .ali files corresponding to failed checks. We don't need any file
      --  locking here, because all the inner processes that were writing to
      --  the Deletion_File have finished.

      use Text_IO;
      Deletion_File : File_Type;
      Deletion_File_Name : String renames
        Gnatcheck.Options.Deletion_File_Name.all;
      Ignored : Boolean;
   begin
      if Incremental_Mode then
         Open (Deletion_File, In_File, Name => Deletion_File_Name);

         --  The Deletion_File contains an initial blank line, due to Text_IO
         --  weirdness, so we need to discard it.

         declare
            Discard : constant String := Get_Line (Deletion_File);
            pragma Unreferenced (Discard);
         begin
            null;
         end;

         --  Read lines from the deletion file, and do the deletions.

         while not End_Of_File (Deletion_File) loop
            declare
               To_Delete : constant String := Get_Line (Deletion_File);
               use String_Utilities;
            begin
               if not Has_Suffix (To_Delete, Suffix => ".ali") then
                  raise Program_Error;
               end if;
               GNAT.OS_Lib.Delete_File (To_Delete, Ignored);
               --  No point in complaining on failure
            end;
         end loop;

         Close (Deletion_File);

         --  Finally, delete the deletion file itself

         if not Debug_Flag_N then
            GNAT.OS_Lib.Delete_File (Deletion_File_Name, Ignored);
            --  No point in complaining on failure
         end if;
      end if;
   end Delete_ALI_Files;

   --------------------
   -- Exit_Gnatcheck --
   --------------------

   procedure Exit_Gnatcheck (Exit_Code : Exit_Code_Type) is
   begin
      if Debug_Flag_C then
         if ASIS_UL.Options.Incremental_Mode then
            Text_IO.Put ("(outer)");
         end if;
         if ASIS_UL.Options.Mimic_gcc then
            Text_IO.Put ("(inner)");
         end if;
         Text_IO.Put (" exiting");
         if Exit_Code /= E_Success then
            Text_IO.Put (" with exit code " & Exit_Code'Img);
         end if;
         Text_IO.Put_Line (".");
      end if;

      if ASIS_UL.Options.Test_Mode then
         if Tool_Failures = 0 then
            OS_Exit (0);
         else
            OS_Exit (1);
         end if;
      else
         if Mimic_gcc and then Exit_Code /= E_Success then
            Write_Deletion_File;
         end if;

         case Exit_Code is
            when E_Success     => OS_Exit (0);
            when E_Violation   => OS_Exit (if Mimic_gcc then 0 else 1);
            --  For the inner invocation in an incremental build, we don't want
            --  rule violations to stop the builder from continuing to process
            --  more files.
            when E_Fatal       => OS_Exit (2);
            when E_Non_Trusted => OS_Exit (2);
            when No_Check      => OS_Exit (3);
         end case;
      end if;
   end Exit_Gnatcheck;

   --------------
   -- Finalize --
   --------------

   procedure Finalize is
   begin

      if not ASIS_UL.Options.Nothing_To_Do then
         if ASIS_UL.Options.Buld_Call_Graph then

            --  When creating a global data structure, we may have added new
            --  files (as needed files) in the sources table, and nodes in
            --  the global structure may refer to these added source as
            --  their enclosing sources. So we have to add the corresponding
            --  nodes to the diagnosis mapping table.

            for J in Last_Argument_Source + 1 .. Last_Source loop
               Gnatcheck.Diagnoses_Old.Add_Line_To_Mapping_Table;
            end loop;

            if not ASIS_UL.Global_State.CG.Traverse_Renamings_Done then
               ASIS_UL.Global_State.CG.Traverse_Renamings;
            end if;

            if Do_Transitive_Closure then

               if Debug_Flag_1 then
                  Info ("Call graph closure ... ");
               end if;

               ASIS_UL.Global_State.CG.Transitive_Closure;
            end if;

            if Debug_Flag_1 then
               Info ("...Done");
            end if;

         end if;

         Gnatcheck.Rules.Traversing.Check_Global_Rules;

         if Debug_Flag_1 then
            ASIS_UL.Global_State.Print_Global_Structure;
         end if;

         if Debug_Flag_1 then
            Info ("Generate report ... ");
         end if;

         if Gnatcheck.Options.Qualification_Report then
            Gnatcheck.Diagnoses.Generate_Qualification_Report;
         else
            Gnatcheck.Diagnoses_Old.Generate_Regular_Report;
         end if;

         ASIS_UL.Output.Close_Report_Files;

         if Debug_Flag_1 then
            Info ("...Done");
         end if;

         if Tool_Failures > 0 then
            Info ("Total gnatcheck failures :" & Tool_Failures'Img);
         end if;

      end if;

   end Finalize;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
      use String_Utilities.String_Vectors, Gnatcheck.Options;
      Deletion_File : Text_IO.File_Type;
      Ignored : Boolean;
   begin
      if not ASIS_UL.Options.Nothing_To_Do then
         if Mimic_gcc then
            pragma Assert (Directories.Exists (Deletion_File_Name.all),
                           Deletion_File_Name.all & " not found");
         else
            Deletion_File_Name := new String'
              (Directories.Compose
                 (ASIS_UL.Environment.Tool_Temp_Dir.all, "to-delete"));

            --  Delete the lock file if it exists, in which case it is a stale
            --  lock file from a previous run. Failure is expected, and
            --  therefore ignored.

            GNAT.OS_Lib.Delete_File (Deletion_Lock_File_Name, Ignored);

            --  Create an empty deletion file, so ASIS_Processing can append
            --  to it. (Small annoyance: the file is not actually empty; it
            --  contains a single blank line, and Finalize has to work around
            --  that.) See also comments on Deletion_File_Name in
            --  Gnatcheck.Options.

            Text_IO.Create (Deletion_File,
                            Name => Deletion_File_Name.all);
            Text_IO.Close (Deletion_File);

            if Incremental_Mode then
               Change_Dir (ASIS_UL.Environment.Tool_Current_Dir.all);
               Append (ASIS_UL.Environment.Extra_Inner_Pre_Args,
                       String'("-asis-tool-args"));
               Append (ASIS_UL.Environment.Extra_Inner_Post_Args,
                       String'("-asis-tool-args"));
               Append (ASIS_UL.Environment.Extra_Inner_Pre_Args,
                       String'("--deletion-file=" & Deletion_File_Name.all));
               return;
            end if;
         end if;
      end if;

      Gnatcheck.Diagnoses.Init_Exemptions;
      Gnatcheck.Diagnoses_Old.Create_Mapping_Table;

      if ASIS_UL.Options.Buld_Call_Graph then
         ASIS_UL.Global_State.Initialize;
      end if;

      if Check_Restrictions or else Use_gnatw_Option then
         Create_Restriction_Pragmas_File;
      end if;

      Gnatcheck.Traversal_Stack.Initialize;
   end Initialize;

   --------------------
   -- Process_Source --
   --------------------

   procedure Process_Source (SF : SF_Id; Only_Bodies : Boolean) is

      --  Compare the following to Process_Source in
      --  ASIS_UL.Source_Table.Processing. The two should be kept in synch;
      --  they cannot easily be merged.

      Add_Needed_Sources : constant Boolean := Mimic_gcc;
      Keep_ALI_Files     : constant Boolean := Mimic_gcc;

      procedure Process_Subunits (CU : Asis.Compilation_Unit);
      --  Assuming that CU is of Compilation_Unit kind that may have subunits,
      --  recursively process all the subunits that are the arguments of the
      --  tool (or, if Add_Needed_Sources is True, all subunits whether or not
      --  they are arguments).

      procedure Process_Needed_Source (Needed_CU : Asis.Compilation_Unit);
      --  If Add_Needed_Sources is True, add the needed Source_File to the
      --  Source_Table if it's not already there. Then do ASIS_Processing on it
      --  if it is now there (i.e. was already there or is newly added).
      --
      --  When Process_Source is called for a library unit body, it calls
      --  Process_Needed_Source for the corresponding spec and for all
      --  subunits. In Mimic_gcc mode, Process_Source is called ONLY for bodies
      --  (and specs without bodies). For library unit bodies, it is important
      --  that the spec (if any) be processed as part of that, using the tree
      --  for the body, because otherwise the tree file for the spec would
      --  overwrite the one for the body, which would confuse
      --  gnatmake. Subunits are also processed as part of the library unit
      --  body.

      procedure Process_Needed_Source (Needed_CU : Asis.Compilation_Unit) is
         File_Name : constant String :=
           Normalize_Pathname
             (To_String (Text_Name (Needed_CU)),
              Resolve_Links  => False,
              Case_Sensitive => True);
         Needed_SF : SF_Id := File_Find (File_Name);
      begin
         if Add_Needed_Sources and then not Present (Needed_SF) then
            Needed_SF := Add_Needed_Source (File_Name);
            Gnatcheck.Diagnoses_Old.Add_Line_To_Mapping_Table;
            Total_Sources := Natural (Last_Source);
            Sources_Left  := Sources_Left + 1;
         end if;
         pragma Assert (if Add_Needed_Sources then Present (Needed_SF));

         if Present (Needed_SF)
           and then Source_Status (Needed_SF) = Waiting
         then
            Output_Source (Needed_SF);
            Traverse_Source (Needed_CU, Needed_SF);

            if Source_Status (Needed_SF) in Waiting | Tree_Is_Ready then
               Set_Source_Status (Needed_SF, Processed);
            end if;
         end if;

      exception
         when Ex :
           Asis.Exceptions.ASIS_Inappropriate_Context          |
           Asis.Exceptions.ASIS_Inappropriate_Container        |
           Asis.Exceptions.ASIS_Inappropriate_Compilation_Unit |
           Asis.Exceptions.ASIS_Inappropriate_Element          |
           Asis.Exceptions.ASIS_Inappropriate_Line             |
           Asis.Exceptions.ASIS_Inappropriate_Line_Number      |
           Asis.Exceptions.ASIS_Failed                         =>
            ASIS_UL.Output.Report_Unhandled_ASIS_Exception (Ex);
            Source_Clean_Up (Needed_SF, Keep_ALI_Files);
            raise Fatal_Error;

         when Ex : others =>
            Error
              ("unknown bug detected when processing " &
                Source_Name (Needed_SF));
            Error_No_Tool_Name
              ("Please submit bug report to report@adacore.com");
            Report_Unhandled_Exception (Ex);
            Source_Clean_Up (Needed_SF, Keep_ALI_Files);
            raise Fatal_Error;
      end Process_Needed_Source;

      procedure Process_Subunits (CU : Asis.Compilation_Unit) is
         Subunit_List : constant Asis.Compilation_Unit_List := Subunits (CU);
      begin
         for J in Subunit_List'Range loop
            if Asis.Set_Get.Kind (Subunit_List (J)) /= A_Nonexistent_Body then
               Process_Needed_Source (Subunit_List (J));
               Process_Subunits (Subunit_List (J));
            end if;
         end loop;
      end Process_Subunits;

      CU_Tmp  : Asis.Compilation_Unit;
      Success : Boolean;
      pragma Warnings (Off, Success);
      use type Asis.Errors.Error_Kinds;
      Compiler_Out_File_Name : constant String :=
        Get_Compiler_Out_File_Name (SF);

      --  Start of processing for Process_Source
   begin
      --  The current directory is Tool_Temp_Dir. In Mimic_gcc mode, we need to
      --  switch to Tool_Inner_Dir so the ALI file ends up in the right
      --  directory. We switch back at the end (see <<Done>> below).

      if Mimic_gcc then
         pragma Assert (Is_Argument_Source (SF));
         Change_Dir (ASIS_UL.Environment.Tool_Inner_Dir.all);
      end if;

      Output_Source (SF);

      --  We always send compiler output to temporary file to be able to
      --  store compiler error messages and to include them in final gnatcheck
      --  report

      if Source_Status (SF) = Waiting then
         Create_Tree
           (SF,
            Success,
            Compiler_Out      => Compiler_Out_File_Name,
            All_Warnings_Off  => Suppess_Compiler_Check);
      end if;

      Start_Tree_Creations
        (SF,
         Only_Bodies,
         Need_Compiler_Output => True,
         All_Warnings_Off     => Suppess_Compiler_Check);

      if Source_Status (SF) in
           Not_A_Legal_Source |
           Not_A_Legal_Source_Needs_Listing_Processing
      then

         if Source_Status (SF) =
              Not_A_Legal_Source_Needs_Listing_Processing
         then
            Set_Source_Status (SF, Not_A_Legal_Source);
         end if;

         if Analyze_Compiler_Output then

            Analyze_Error_Messages
              (Compiler_Out_File_Name,
               Wrong_Option => Success);

            if Success then
               --  This means that wrong (style) parameters have been specified
               --  as compiler-related rule parameter, so the same error will
               --  be detected when compiling another source, so:
               raise Fatal_Error;
            end if;
         end if;

         Gnatcheck.Diagnoses.Store_Error_Messages (Compiler_Out_File_Name, SF);

         goto Done;
      end if;

      if Use_Parallel_Tree_Creation then
         Asis.Ada_Environments.Associate
          (The_Context => The_Context,
           Name        => "",
           Parameters  => "-C1 "
                         & To_Wide_String
                             (Image (Integer (SF)) & Directory_Separator &
                              Suffixless_Name (SF) & ".adt"));
      else
         Asis.Ada_Environments.Associate
          (The_Context => The_Context,
           Name        => "",
           Parameters  => "-C1 "
                         & To_Wide_String (Suffixless_Name (SF) & ".adt"));
      end if;

      begin
         Asis.Ada_Environments.Open (The_Context);
         Success := True;

         if Debug_Flag_T then
            Print_Tree_Sources;
         end if;
      exception
         when Asis.Exceptions.ASIS_Failed =>
            --  The only known situation when we can not open a C1 context for
            --  newly created tree is recompilation of System (see D617-017)

            if Asis.Implementation.Status = Asis.Errors.Use_Error
              and then
               Asis.Implementation.Diagnosis = "Internal implementation error:"
               & " Asis.Ada_Environments.Open - System is recompiled"
            then
               Error ("can not process redefinition of System in " &
                       Source_Name (SF));

               Set_Source_Status (SF, Not_A_Legal_Source);
               Success := False;
            else
               raise;
            end if;

      end;

      if Success then

         The_CU := Main_Unit_In_Current_Tree (The_Context);

         if Unit_Origin (The_CU) /= An_Application_Unit
           and then
            not Process_RTL_Units
         then
            Error ("cannot process RTL unit " & Source_Name (SF) &
                   " Use '-a' option for processing RTL components");
            Set_Source_Status (SF, Processed);
         else
            if Mimic_gcc then
               if CU_Requires_Body (The_CU)
                 or else Unit_Class (The_CU) = A_Separate_Body
               then
                  if Debug_Flag_V then
                     Text_IO.Put_Line ("ignoring  " & Short_Source_Name (SF));
                  end if;
                  Set_Source_Status (SF, Processed);
                  goto Done;
               else
                  if Debug_Flag_V then
                     Text_IO.Put_Line ("doing     " & Short_Source_Name (SF));
                  end if;
               end if;
            end if;

            Traverse_Source (The_CU, SF);

            if Mimic_gcc then
               --  If it's a library unit body, process the spec

               if Unit_Class (The_CU) in A_Public_Body | A_Private_Body then
                  CU_Tmp := Corresponding_Declaration (The_CU);

                  if not Is_Nil (CU_Tmp) then
                     Process_Needed_Source (CU_Tmp);
                  end if;
               end if;

               --  If it's a body, process any subunits

               if Unit_Class (The_CU) in
                 A_Public_Body | A_Private_Body | A_Separate_Body
               then
                  Process_Subunits (The_CU);
               end if;
            end if;
         end if;
      end if;

      if Analyze_Compiler_Output then
         Analyze_Compiler_Warnings (Compiler_Out_File_Name, SF, Success);

         if not Success then
            Set_Source_Status (SF, Error_Detected);
         end if;
      end if;

      if not Debug_Flag_N
        and then
          Is_Regular_File (Compiler_Out_File_Name)
      then
         Delete_File (Compiler_Out_File_Name, Success);
      end if;

      <<Done>>
      Source_Clean_Up (SF, Keep_ALI_Files);
      if Mimic_gcc then
         Change_Dir (ASIS_UL.Environment.Tool_Temp_Dir.all);
      end if;

   exception

      when Fatal_Error =>
         raise;

      when Ex : Asis.Exceptions.ASIS_Inappropriate_Context          |
                Asis.Exceptions.ASIS_Inappropriate_Container        |
                Asis.Exceptions.ASIS_Inappropriate_Compilation_Unit |
                Asis.Exceptions.ASIS_Inappropriate_Element          |
                Asis.Exceptions.ASIS_Inappropriate_Line             |
                Asis.Exceptions.ASIS_Inappropriate_Line_Number      |
                Asis.Exceptions.ASIS_Failed                         =>

         ASIS_UL.Output.Report_Unhandled_ASIS_Exception (Ex);

         raise Fatal_Error;

      when Ex : others =>

         Error ("unknown bug detected when processing " & Source_Name (SF));
         Error_No_Tool_Name
           ("Please submit bug report to report@adacore.com");
         Report_Unhandled_Exception (Ex);

         Source_Clean_Up (SF, Keep_ALI_Files);
         raise Fatal_Error;

   end Process_Source;

   ---------------------
   -- Process_Sources --
   ---------------------

   procedure Process_Sources is
   begin
      pragma Assert (not Incremental_Mode);

      if not Nothing_To_Do then
         Asis.Implementation.Initialize ("-k -asis05 -ws -sv");

         Process_Sources_From_Table (Only_Bodies => True);
         Process_Sources_From_Table (Only_Bodies => False);

         Asis.Implementation.Finalize;
      end if;
   end Process_Sources;

   --------------------------------
   -- Process_Sources_From_Table --
   --------------------------------

   procedure Process_Sources_From_Table (Only_Bodies : Boolean) is
      Next_SF : SF_Id;
   begin
      Reset_Source_Iterator;

      loop
         Next_SF := Next_Non_Processed_Source (Only_Bodies);
         exit when not Present (Next_SF);
         Process_Source (Next_SF, Only_Bodies);
         Check_Tree_Creations;
      end loop;

   end Process_Sources_From_Table;

   ---------------------
   -- Traverse_Source --
   ---------------------

   procedure Traverse_Source
     (CU : Asis.Compilation_Unit;
      SF : SF_Id)
   is
      Program_Unit  : constant Asis.Element      := Unit_Declaration (CU);
      Contex_Clause : constant Asis.Element_List :=
         Context_Clause_Elements (CU, True);

      Comp_Pragmas : constant Asis.Element_List :=
        Compilation_Pragmas (CU);
      First_Pragma_After : List_Index              := Comp_Pragmas'Last + 1;
      Unit_Span          : constant Asis.Text.Span :=
        Element_Span (Program_Unit);
      --  We also may have to check pragmas after the unit, that's why we need
      --  these objects.

      Check_Control : Traverse_Control     := Continue;
      Check_State   : Rule_Traversal_State :=
        (Initial_State, False, SF, 0, Nil_String_Loc, 0, 0, 0);
   begin
      if Mimic_gcc and then (Verbose_Mode or else Debug_Flag_V) then
         Text_IO.Put_Line (" checking " & Short_Source_Name (SF));
      end if;

--      if Gnatcheck.Options.Active_Rule_Present then
      --  ??? we need to process exemptions sections for compiler checks anyway
      Gnatcheck.Diagnoses.Init_Postponed_Check_Exemptions;
      --  This is for non-parametrized checks only!

      if Gnatcheck.Options.Analyse_Source_Text then
         Gnatcheck.Source_Checks.Init_Source_Text_Checks (Program_Unit);
      end if;

      for J in Comp_Pragmas'Range loop

         if Is_Equal (Enclosing_Compilation_Unit (Comp_Pragmas (J)), CU)
            --  condition needed to filter out pragmas from configuration file
           and then
            Unit_Span.Last_Line <=
            Element_Span (Comp_Pragmas (J)).First_Line
         then
            First_Pragma_After := J;
            exit;
         end if;

      end loop;

      for J in Contex_Clause'Range loop
         Check_Rules (Contex_Clause (J), Check_Control, Check_State);
      end loop;

      Check_Rules (Program_Unit, Check_Control, Check_State);

      for J in First_Pragma_After .. Comp_Pragmas'Last loop

         if Is_Equal (Enclosing_Compilation_Unit (Comp_Pragmas (J)), CU) then
            --  We may have configuration pragmas in the list
            Check_Rules (Comp_Pragmas (J), Check_Control, Check_State);
         end if;

      end loop;

      if Gnatcheck.Options.Analyse_Source_Text then
         Gnatcheck.Source_Checks.Check_Text_Rules_For_Remaining_Lines
           (Unit  => Program_Unit,
            State => Check_State);
      end if;

      Gnatcheck.Diagnoses.Check_Unclosed_Rule_Exemptions (SF, Program_Unit);
--      end if;

      if Source_Status (SF) in Waiting | Tree_Is_Ready then
         Set_Source_Status (SF, Processed);
      end if;

   exception
      when others =>
         Set_Source_Status (SF, Error_Detected);

         Gnatcheck.Diagnoses.Store_Diagnosis
           (Text           => Short_Source_Name (SF) & ":1:1: " &
                              Tool_Name.all & " internal error",
            Diagnosis_Kind => Gnatcheck.Diagnoses.Compiler_Error,
            SF             => SF);

   end Traverse_Source;

   -------------------------
   -- Write_Deletion_File --
   -------------------------

   procedure Write_Deletion_File is
      pragma Assert (Mimic_gcc);
      use Text_IO;
      Deletion_File_Name : String renames
        Gnatcheck.Options.Deletion_File_Name.all;
      Lock_File_Name : constant String := Deletion_Lock_File_Name;

      SF : constant SF_Id := First_SF_Id;
      --  Since we're in Mimic_gcc mode, we only have one main source (plus its
      --  spec and subunits, if any).

      Base : constant String := Directories.Base_Name (Short_Source_Name (SF));
      ALI_File_Name : constant String :=
        Directories.Compose (ASIS_UL.Environment.Initial_Dir, Base, "ali");

      procedure Do_Write;
      --  Write the .ali file name to the file name file. This is split out
      --  into a procedure so we can call it with and without file locking, as
      --  appropriate.

      procedure Do_Write is
         Deletion_File : File_Type;
      begin
         Open (Deletion_File,
               Mode => Append_File,
               Name => Deletion_File_Name);
         Put_Line (Deletion_File, ALI_File_Name);
         Close (Deletion_File);
      end Do_Write;

   --  Start of processing for Write_Deletion_File

   begin
      if Outer_Parallel then
         Lock_File (Lock_File_Name);
         declare
            --  We create a dummy object whose finalization calls
            --  Unlock_File, so we don't leave stale lock files around even
            --  in case of unhandled exceptions.

            type Dummy_Type is new Ada.Finalization.Limited_Controlled with
              null record;
            procedure Finalize (Ignore : in out Dummy_Type);
            procedure Finalize (Ignore : in out Dummy_Type) is
            begin
               Unlock_File (Lock_File_Name);
            end Finalize;

            Dummy : Dummy_Type;

         begin
            Do_Write;
         end;

      --  Otherwise, it's safe to do the writes without any locking. We want
      --  to avoid locking when possible, because it reduces the likelihood
      --  of stale locks left lying around. It's a little more efficient,
      --  too.

      else
         Do_Write;
      end if;
   exception
      when Lock_Error =>
         ASIS_UL.Output.Error ("cannot create " & Lock_File_Name);
         ASIS_UL.Output.Error ("delete it by hand if stale");
         raise;
   end Write_Deletion_File;

end ASIS_UL.Source_Table.Gnatcheck_Processing;
