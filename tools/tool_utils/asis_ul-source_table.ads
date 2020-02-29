------------------------------------------------------------------------------
--                                                                          --
--                     ASIS UTILITY LIBRARY COMPONENTS                      --
--                                                                          --
--                 A S I S _ U L . S O U R C E _ T A B L E                  --
--                                                                          --
--                                 S p e c                                  --
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

--  This package defines the source file table - the table containing the
--  information about the source files to be processed and the state of their
--  processing. Used by Several_Files_Driver.

with Ada.Containers.Indefinite_Ordered_Sets;

with GNAT.OS_Lib; use GNAT.OS_Lib;

with Asis;

with ASIS_UL.Misc;
with ASIS_UL.Projects; use ASIS_UL.Projects;

package ASIS_UL.Source_Table is

   Low_SF_Bound  : constant := 0;
   High_SF_Bound : constant := 999_999;
   --  Almost 1_000_000 source files for one run of the tool

   type SF_Id is range Low_SF_Bound .. High_SF_Bound;

   No_SF_Id    : constant SF_Id := Low_SF_Bound;
   First_SF_Id : constant SF_Id := No_SF_Id + 1;

   Total_Sources : Natural;
   Sources_Left  : Natural;
   --  Counters used to form and output progress information.

   type SF_Status is (
      Waiting,
      --  Waiting for processing

      Waiting_Subunit,
      --  We have visited the source, detected that it is a subunit and
      --  postponed its processing.

      Not_A_Legal_Source,
      --  The file does not contain compilable source

      Not_A_Legal_Source_Needs_Listing_Processing,
      --  The file does not contain compilable source. The difference with the
      --  previous state is that this state tells the gnatcheck tool that the
      --  compiler error messages should be collected for gnatcheck report.

      Error_Detected,
      --  Some tool problem has been detected when processing this source
      --  so the results of processing may not be safe

      Out_File_Problem,
      --  The out file was not successfully created or opened

      ASIS_Processed,

      Non_ASIS_Processed,

      Processed,
      --  The source file has been successfully processed

      Preparing_Tree,
      --  The compilation that creates the tree for the source has been started
      --  but not sure to be finished

      Tree_Is_Ready,
      --  The tree for the source has been successfully created.

      Waiting_For_Closure
      --  The source is waiting for the decision if it should be added to the
      --  closure being computed
      );

   type SF_Info is new Integer;
   --  The type to be used for the integer values associate with each source in
   --  the source file table. The use of this value is client-specific

   function Present (SF : SF_Id) return Boolean;
   --  Checks that SF is not is equal to No_SF_Id

   function Get_File_Names_Case_Sensitive return Integer;
   pragma Import (C, Get_File_Names_Case_Sensitive,
                  "__gnat_get_file_names_case_sensitive");
   File_Names_Case_Sensitive : constant Boolean :=
                                 Get_File_Names_Case_Sensitive /= 0;
   --  Set to indicate whether the operating system convention is for file
   --  names to be case sensitive (e.g., in Unix, set True), or non case
   --  sensitive (e.g., in Windows, set False). This code is taken as is from
   --  the GNAT Osint package to avoid dependency on Osint

   function File_Find
     (SF_Name        : String;
      Use_Short_Name : Boolean := False;
      Case_Sensitive : Boolean := File_Names_Case_Sensitive)
      return           SF_Id;
   --  Returns the Id of the file with name SF_Name stored in the files
   --  table. Returns No_SF_Id if the table does not contain such a file.
   --  In case if ASIS_UL.Common.Use_Project_File_Obsolete is set ON, or
   --  Use_Short_Name parameter is set ON, the short file name is used to
   --  locate the file; if the argument contains a directory information it is
   --  stripped out.  Otherwise this function tries to locate the name with the
   --  full normalized name equal to SF_Name.
   --  If Case_Sensitive is OFF, then this function first looks for the SF_Name
   --  using the original casing of SF_Name and files stored in the Source
   --  Table, and if it cannot locate the file, it repeats the search with all
   --  the path/file names converted to lower case.

   function File_Find (El : Asis.Element) return SF_Id;
   --  Returns the Id of the file containing the argument Element enclosing
   --  compilation unit. Returns No_SF_Id if the argument is Nil_Element or
   --  if the source file table does not contain the given file.

   procedure Add_Source_To_Process
     (Fname              : String;
      Arg_Project        : Arg_Project_Type'Class;
      Duplication_Report : Boolean   := True;
      Status             : SF_Status := Waiting);
   --  Fname is treated as the name of the file to process by the tool. We try
   --  to add this file to the table of files to process. The following checks
   --  are performed:
   --
   --  If ASIS_UL.Common.Use_Project_File_Obsolete is set ON:
   --
   --  - This routine checks if we have already stored a file with the same
   --    short(!) name. If we already have such a file, it generates a
   --    warning message (if Duplication_Report is set ON) and does nothing
   --    else.
   --
   --    Note that ASIS_UL.Common.Use_Project_File_Obsolete corresponds to the
   --    very old partial implementation of the project support for ASIS tools,
   --    we keep it for compatibility reasons only (AJIS???). It should not be
   --    used by any new tool.
   --
   --  If ASIS_UL.Common.Use_Project_File_Obsolete is set OFF:
   --
   --  - First, this routine checks if Fname is the name of some existing file,
   --    and if it is not, generates the corresponding diagnosis and does
   --    nothing more. If Arg_Project represents a project file being a tool
   --    parameter, it uses the corresponding project to check the file
   --    existence (no source search path should be used in this case!).
   --    Otherwise this procedure takes into account the search path for the
   --    Ada sources only if ASIS_UL.Compiler_Options.Source_Search_Path is
   --    set, otherwise it just calls GNAT.OS_Lib.Is_Readable_File (Fname)
   --    and checks the result.
   --
   --  - Then, it checks if we already have stored a file with the same name.
   --    If we have the file with the same name, but from a different
   --    directory, a warning is generated, but the file is added to the file
   --    table (the situation when the metric tool is called to process files
   --    with the same name but from different directories looks strange, but
   --    this may be quite legal and reasonable). But if we have already stored
   --    in the list the name of exactly the same file, we generate the error
   --    message and do not change anything in the list of files.
   --
   --  At this stage we do not know if Fname denotes a compilable Ada source
   --  file.
   --
   --  This procedure tries to detect if this source is the source of a
   --  body unit, and if so, stores this in the corresponding record of the
   --  source table. To define this, it checks the suffix of the file name.
   --  It treats suffixes '.adb' and '.2.ada' as suffixes of body files.
   --
   --  It is supposed to be used as a part of the tool parameter processing
   --  in the following way:
   --
   --      loop
   --         declare
   --            Arg : constant String := Get_Argument (...);
   --         begin
   --            exit when Arg = "";
   --            Add_Source_To_Process (Arg);
   --         end;
   --      end loop;

   procedure Store_Sources_To_Process
     (Fname : String;
      Store : Boolean := True);
   --  Fname is stored in an internal database as the name of the file to be
   --  processed by the tool. No check is made if Fname denotes an existing
   --  file. Similar to the previous Add_Source_To_Process routine, this
   --  procedure is supposed to be used when processing tool parameters. The
   --  reason to use this procedure instead of Add_Source_To_Process is to do
   --  the checks if a file exists only when we already have the full search
   --  path for sources.
   --
   --  If Store is OFF then the procedure does not store anything.
   --
   --  If Fname is not an empty string, sets the
   --  ASIS_UL.Options.No_Argument_File_Specified flag OFF

   procedure Read_Args_From_Temp_Storage
     (Duplication_Report : Boolean;
      Arg_Project        : Arg_Project_Type'Class;
      Status             : SF_Status := Waiting);
   --  Reads argument files from temporary storage (where they are placed by
   --  Store_Sources_To_Process/Store_Args_From_File routine(s)). Uses
   --  Add_Source_To_Process to read each file, so the check if a file exists
   --  is performed on the base of the source search path
   --  (ASIS_UL.Compiler_Options.Source_Search_Path) or the project file that
   --  is a tool argument. This procedure calls Add_Source_To_Process for each
   --  file to do the existence test and to store source in the source table.
   --  The temporary storage is cleaned up.
   --
   --  The Duplication_Report parameter has the same meaning as for
   --  Add_Source_To_Process.

   procedure Read_Args_From_File (Par_File_Name : String);
   --  Reads argument files from the file. Stores the file names in the
   --  temporary storage as Store_Sources_To_Process does. This procedure
   --  assumes that the file named by Par_File_Name contains argument file
   --  names, one per line.
   --
   --  This procedure sets the ASIS_UL.Options.No_Argument_File_Specified flag
   --  OFF.

   function Files_In_Temp_Storage return Natural;
   --  Returns the number of files stored in temporary storage.

   function First_File_In_Temp_Storage return String;
   --  Returns the simple name of the first file in the temporary file storage
   --  (according to the way how the storage is ordered). A caller should make
   --  sure that the temporary storage is not empty.
   --
   --  What actually is needed on a tool side is the name of the file if this
   --  file is the only file that is kept in the temporary storage. So it may
   --  make sense to revise this function.

   procedure Temp_Storage_Iterate
     (Action : not null access procedure (File_Name : String));
   --  Call Action for each File_Name in the temporary file storage

   function Arg_Source_File_Name return String;
   --  If the tool is called with all the argument sources specified in a
   --  single text file (using '-files=arg_files' option), returns the name of
   --  this file. Otherwise returns null string.
   --  If the tool is called from the GNAT driver, the result of this function
   --  corresponds to the call generated by the GNAT driver, but not to the
   --  call to the GNAT driver.

   Individual_Files_Specified : Boolean := False;
   --  Flag indicating if for the tool call there is at least one source file
   --  explicitly specified in the command line. Note that the tool has itself
   --  to take care of proper setting of this flag.

   -------------------------------------------------------
   --  Recommended way of reading tool argument sources --
   -------------------------------------------------------

   --  If you would like to make sure that the search path specified by the
   --  tool '-I' options or by a project file (in case when the tool is called
   --  from the GNAT driver) is used when locating the argument sources, use
   --  the following procedure:
   --
   --  - When processing tool parameters, use Store_Sources_To_Process instead
   --    of Add_Source_To_Process;
   --
   --  - after reading all the tool arguments, use the following sequence of
   --    calls:
   --
   --      ASIS_UL.Compiler_Options.Process_ADA_PRJ_INCLUDE_FILE;
   --      ASIS_UL.Compiler_Options.Set_Source_Search_Path;
   --      ASIS_UL.Source_Table.Read_Args_From_Temp_Storage (...);'

   function Add_Needed_Source (Fname : String) return SF_Id;
   --  This function unconditionally stores the file with the name Fname in the
   --  source file table and returns the corresponding Id. Fname should be a
   --  full (???) name of existing file. This procedure is supposed to be
   --  called to store the name of the file that is not the argument of the
   --  tool, but that is detected as a file that is needed because of any
   --  reason for some argument file. For example, it may be a file containing
   --  a unit needed to be analyzed to build a full call graph. The caller is
   --  responsible for the fact that this source has not been already stored in
   --  the file table.

   function Last_Source return SF_Id;
   --  Returns the Id of the last source stored in the source table. Returns
   --  No_SF_Id if there is no source file stored

   function Total_Sources_To_Process return Natural;
   --  Returns the number of the arument sources to be processed. This may be
   --  different from Last_Source if '--ignore=...' option specifies the list
   --  of files to be ignored.

   function Exempted_Sources return Natural;
   --  Returns the number of (existing) sources marked as ignored/exempted as
   --  the result of '--ignore=...' option.

   function Last_Argument_Source return SF_Id;
   --  Returns the Id of the last argument source stored in the source table.
   --  An argument source is the source set as the argument of the tool call.

   function Is_Argument_Source (SF : SF_Id) return Boolean;
   --  Checks if SF is from tool argument sources

   function Is_Needed_Source (SF : SF_Id) return Boolean;
   --  Checks if SF is a source that has been added as a needed source for some
   --  argument source (i.e. Is_Argument_Source is False).

   procedure Reset_Source_Iterator;

   function Next_Non_Processed_Source
     (Only_Bodies            : Boolean := False;
      Include_Needed_Sources : Boolean := False)
      return                   SF_Id;

   procedure Create_Tree
     (SF               :     SF_Id;
      Success          : out Boolean;
      Compiler_Out     :     String  := "";
      All_Warnings_Off :     Boolean := True);
   --  Tries to create a tree file for SF. Sets Success ON if this attempt is
   --  successful. Otherwise sets Success OFF, generates the diagnostic
   --  information, sets the file status for SF to Not_A_Legal_Source and
   --  decreases the counter of the sources which have to be processed
   --  (Sources_Left)
   --  If Compiler_Out is a name of an existing file, this file is used
   --  to redirect the compiler output into. Otherwise the compiler output is
   --  sent to Stderr
   --  If All_Warnings_Off is set ON, the call to the compiler contains an
   --  option that suppresses all the compiler warnings.
   --  The tree and the corresponding ALI file are placed in the subdirectory
   --  of the tool temporary directory that has the name equal to SF string
   --  image. The file the compiler output is redirected into is placed into
   --  the tool temporary directory.

   procedure Output_Source (SF : SF_Id);
   --  Depending on the options set generates the trace of the units/sources
   --  processing. If Verbose_Mode is ON, outputs into Stderr the number of the
   --  units left and the name of the source being processed. Otherwise, if
   --  Quiet_Mode is OFF, outputs only the number of units left. If
   --  Progress_Indicator_Mode is ON, generates the output to be used for GPS
   --  progress indicator. (Unconditionally) decreases the counter of the
   --  sources which have to be processed (Sources_Left)

   procedure Source_Clean_Up
     (SF             : SF_Id;
      Keep_ALI_Files : Boolean := False);
   --  Minimal clean-up needed for one source (closing and dissociating the
   --  Context, removing the tree and ALI files created for this source in
   --  the temporary directory, if Keep_ALI_Files is set ON, ALI file(s) is
   --  (are) not deleted).

   function Is_A_Body (SF : SF_Id) return Boolean;
   --  Checks if SF could be an Ada body file.

   ----------------------------------------
   -- Source file access/update routines --
   ----------------------------------------

   function Source_Name (SF : SF_Id) return String;
   --  If ASIS_UL.Common.Use_Project_File_Obsolete is set OFF, this function
   --  returns the full source file name in absolute normalized form, otherwise
   --  it result is the same as returned by Short_Source_Name for the same
   --  argument.

   function Short_Source_Name (SF : SF_Id) return String;
   --  Short file name with no directory information

   function Suffixless_Name (SF : SF_Id) return String;
   --  Returns the file name with no directory information and with
   --  no suffix (if any). Can be used to create the name of the tree and
   --  ALI file that correspond to SF.

   function CU_Name (SF : SF_Id) return String;
   procedure Set_CU_Name (SF : SF_Id; N : String);
   --  Returns (sets) the full expanded Ada name of the compilation unit that
   --  is contained in the source.
   --  ??? Should this function return Wide_String???

   function  Source_Status     (SF : SF_Id) return SF_Status;
   procedure Set_Source_Status (SF : SF_Id; S : SF_Status);
   --  Queries and updates the source status.

   function  Source_Info     (SF : SF_Id) return SF_Info;
   procedure Set_Source_Info (SF : SF_Id; Info : SF_Info);
   --  Queries and updates the source Info value. The use of this value is up
   --  to the client of the source file table. You can store some integer-coded
   --  information or you can use this value as an index value in some other
   --  structure.

   Ignore_Unit : constant ASIS_UL.Source_Table.SF_Info := 1;
   --  Used to mark units to be ignored in the source table. The exact meaning
   --  of "to be ignored" depends on a tool.

   procedure Set_Exemption (Fname : String);
   --  Marks the argument file in the source table as exempted (depending on
   --  the tool, either the file is not processed or no result is generated
   --  for the tool). Generates a warning if Fname does not point to argument
   --  file).

   procedure Process_Exemptions is new
     ASIS_UL.Misc.Parse_File_List (Set_Exemption);
   --  Reads the content of the text file that contains a list of the units to
   --  be exempts/ignored and marks the corresponding units in the source
   --  table.

   function Compilation_Switches (SF : SF_Id) return String_List;
   --  Returns the list of switches to be used for this particular file to
   --  call the compiler to create the tree.

   procedure Add_Compilation_Switches
     (SF       : SF_Id;
      Switches : String_List_Access);
   --  Stores a switch list to be used to create the tree for SF. Once stored,
   --  the list cannot be freed, so if you use this procedure more then once
   --  for the same file this results in memory leaks.

   function Get_Result_Dir (SF : SF_Id) return String;
   procedure Set_Result_Dir
     (SF   : SF_Id;
      Path : String);
   --  Gets/stores a path to the directory where per-file results should be
   --  placed in.

   function Get_Compiler_Out_File_Name (SF : SF_Id) return String;
   --  Gets the name of a temporary file used to redirect the compiler output
   --  into. In case of gnatcheck we have to analyze the messages generated by
   --  each compilation issued for tree creation because the compiler error
   --  messages are included into gnatcheck report. The returned file names are
   --  different for different argument sources,

   procedure Source_Table_Debug_Image;
   --  Prints into Stdout the debug image of the current state of source table
   --  if the corresponding debug flag is ON (or if ASIS_UL.Options.Debug_Mode
   --  is ON, but we have to get rid of this flag), otherwise does nothing.

   -----------------------------
   --  Temporary file storage --
   -----------------------------

   --  We use an ordered set for temporary file storage to ensure as much
   --  determinism in the tool output as possible (in case if a tool prints out
   --  the results and/or diagnoses on per-file basis).

   function File_Name_Is_Less_Than (L, R : String) return Boolean;
   --  Assuming that L and R are file names compares them as follows:
   --
   --  * if L and/or R contains a directory separator, compares
   --    lexicographicaly parts that follow the rightmost directory separator.
   --    If these parts are equal, compares L and R lexicographicaly
   --
   --  * otherwise compares L and R lexicographicaly
   --
   --  Comparisons are case-sensitive.

   package Temporary_File_Storages is new
     Ada.Containers.Indefinite_Ordered_Sets
       (Element_Type => String,
        "<"          => File_Name_Is_Less_Than);
   ----------------------
   -- Problem counters --
   ----------------------

   Illegal_Sources   : Natural := 0;
   Out_File_Problems : Natural := 0;

end ASIS_UL.Source_Table;
