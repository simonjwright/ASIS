------------------------------------------------------------------------------
--                                                                          --
--                     ASIS UTILITY LIBRARY COMPONENTS                      --
--                                                                          --
--                     A S I S _ U L . P R O J E C T S                      --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                     Copyright (C) 2013-2018, AdaCore                     --
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

--  This package contains high-level interfaces to project files. These
--  interfaces are supposed to be enough to extract all the information an
--  [ASIS-based] tool may need from the argument project file.
--
--  A part of project file processing is tool-specific: there are tool-specific
--  packages and parameters in the project file, so this package defines only
--  the common steps needed for extracting the information from the project
--  file by providing the Arg_Project_Type type. A tool has to extend this
--  type by providing operations for dealing with tool-specific attributes.
--
--  The interface provided here assumes that a tool is based on the Source
--  Table defined in ASIS_UL.Source_Table.
--
--  The recommended way of project file processing is:
--
--  Two main steps of processing the project file that is the tool argument
--  should be done:
--
--  (a) analyzing the project file as a part of the analysis of the tool
--      command line arguments. At this stage the following information is
--      supposed to be extracted and stored in the tool data structures:
--
--      * options to be used to call gcc to create tree files;
--      * tool-specific options;
--      * a list of argument sources (in case when a tool is called without
--        argument sources, that means that all the sources of the project
--        should be processed);
--
--  (b) creating the mapping file for the project sources and storing the
--      compilation options that are specific for the argument sources.
--
--  The important thing is that when a project file is provided as a tool
--  parameter, the tool cannot use any source search path to locate sources and
--  to form the set of options (-I options or placing the source search path
--  into an environment variable) for gcc call.
--
--  The general tool architecture implemented in the ASIS Utility Library
--  defines the following sequence of tool initialization:
--
--  (1) tool parameters are analyzed and stored, at this stage the tool
--      argument files are stored in some temporary storage, but not in the
--      source table;
--
--  (2) a check is made that the set of tool options is consistent, and if
--      it is, the argument sources are moved from the temporary storage into
--      the source table, and at this stage it is checked that a source to be
--      placed in the source table exists, and that there is no duplication
--      in the set of argument sources. As a result, the source table
--      contains only existing sources with no duplication.
--
--  ???
--  The step (a) above is a part of the step (1) of the general tool
--  initialization, but the step (b) can be done only when we have a list of
--  the existing argument files with no duplications, so it is performed as a
--  part of step (2) *after* moving the argument files into the source table.
--  This is why there are basically two places in the tool code that is
--  responsible for the project file processing.
--  ???
--
--  To represent the project file provided as a tool parameter, we define the
--  Arg_Project_Type (that is derived from GNATCOLL.Projects.Project_Tree. In
--  the specification below the primitives of this type are separated into
--  three groups:
--  * operations that hardly need redefinition for a tool;
--  * operations that may need redefinition for a tool;
--  * operations that for sure need redefinition for a tool;
--
--  It is supposed that a tool may have at most one project file as its
--  argument, so it can be only one object of a tool-specific descendant of the
--  Arg_Project_Type declared in the tool code.
--
--  *** Processing tool parameters ***
--
--  If a tool can accept a project file as an argument, we have to process tool
--  command line parameters twice and in between we have to process the project
--  file. The reason is that tool options may be specified both on the command
--  line and in the tool-specific package of the project file, and the
--  resulting list of options to be applied to the tool should be the
--  superposition of the options from the project file and from the command
--  line in the following order:
--
--   <options from project file><options from command line>
--
--  So, the processing of the tool parameters should be a sequence of three
--  steps:
--
--  1. Scan the command-line parameters, but store only the following
--     information:
--     * project-specific options (-P, -U, -X, -vP)
--     * '--help' and '--version'
--     * argument sources (specified either directly or by any other means,
--       for example as '--files=list_of_arg_sources.txt'
--       where list_of_arg_sources.txt is a text file containing the names of
--       the argument sources
--
--  2. If at the first step either '--help' or '--version' option is detected,
--     print out the corresponding information and exit from the tool. Else if
--     at the first step '-P prj' option is detected, analyze the project file
--     prj (taking into account all the other project-specific options). This
--     includes extracting the tool options, compilation parameters for
--     creating tree files and argument sources (if needed).
--
--  3. Scan again the command-line parameters, but this time ignore all the
--     options listed in paragraph 1 above, but store all the other options.
--     This gives the required superposition of the options specified in the
--     project file and in the command line.
--
--  The proposed implementation of the project support relies on the following:
--
--  * All the tool-specific options in a project file are given in a
--    tool-specific package;
--  * This tool-specific package can contain two attributes - Default_Switches
--    indexed by "ada" and Switches indexed by the names of Ada sources;
--  * The list of values of these attributes has exactly the same structure
--    as the list of options and arguments in the command line tool call with
--    the following exceptions:
--    - no project-specific option is allowed;
--    - '--help' and '--version' options are not allowed
--    - Switches attribute cannot have argument sources as its values
--      (specified either directly or by a list of sources)
--
--  As a result, the same scanner is used to process the command line and the
--  values of the attributes Default_Switches and Switches - this is the
--  operation Scan_Arguments of Arg_Project_Type that has to be defined for
--  each tool. This procedure with the appropriate command line parser and
--  boolean parameters that indicate the case of ins usage) implements steps 1
--  and 3 above. For step 2, see the class-wide procedure Process_Project_File
--  that combines all the steps of loading and analyzing the project file.
--
--  Procedure Set_Individual_Source_Options stores compilation attributes
--  specific for individual sources. It should be called (immediately) after
--  moving all the argument sources into the source table.

pragma Ada_2012;

with GNAT.Command_Line; use GNAT.Command_Line;
with GNAT.OS_Lib;       use GNAT.OS_Lib;

with GNATCOLL.Projects; use GNATCOLL.Projects;
with GNATCOLL.VFS;      use GNATCOLL.VFS;

with ASIS_UL.Debug;     use ASIS_UL.Debug;

package ASIS_UL.Projects is

   ------------------------------
   -- Project-specific options --
   ------------------------------

   Try_Project_File : Boolean renames Debug_Flag_9;
   --  If this flag is ON, Process_Project_File does not do any processing of
   --  the argument project file, it only tries to load it and to print out
   --  project sources. And then an exit to OS is issued. This may be useful to
   --  do a general check for GNATCOLL.Projects routines on cross and bb
   --  platforms where we might not have GNATCOLL installed.

   ---------------------------------------------------------------------
   -- -eL  : Follow all symbolic links when processing project files. --
   ---------------------------------------------------------------------

   Follow_Symbolic_Links : Boolean := False;

   ------------------------------------------------------
   -- -vPn  : verbosity level on project file analysis --
   ------------------------------------------------------

   subtype Verbosity_Levels is Natural range 0 .. 2;

   Verbosity_Level : Verbosity_Levels := 0;

   --------------------------------------------------------------------------
   -- -U [main_unit]  : get the source or main_unit closure of the project --
   --------------------------------------------------------------------------

   File_List_Specified : Boolean := False;
   --  Indicates if '-files=...' option is specified. We need to know this when
   --  getting the list of files from the project - '-U' should be  ignored if
   --  '-files=...' is specified.

   U_Option_Set : Boolean := False;
   --  Indicates if -U option is specified for the project (to process all the
   --  units of the closure of the argument project or to process the closure
   --  of the main unit if the main unit is set)

   Main_Unit : String_Access;
   --  If the tool is called with "... Pproj -U main_unit", main_unit is stored
   --  here

   procedure Store_Main_Unit
     (Unit_Name   : String;
      Store       : Boolean := True);
   --  Processes the result returned by GNAT.Comand_Line.Get_Argument provided
   --  that it is supposed to be the main unit name for '-U' project file
   --  option.
   --
   --  If Store is ON, stores the name of the unit and generates an error
   --  message if the name is already stored (-U allows only one name of the
   --  main unit specified). If Store is OFF, does not do any checks and does
   --  not store anything.

   -----------------------------------------------------------
   --  -Xvariable=value  : set values of external variables --
   -----------------------------------------------------------

   --  An external variable is the parameter of a '-X<par>' tool option, it
   --  is guaranteed that it has a structure <variable_name>=<value>

   procedure Store_External_Variable (Var : String);
   --  Checks if the value for the given external variable is already stored
   --  (the check is case-sensitive), and if it is not, stores the value
   --  setting for the given variable. If it is, overrides the stored value.

   --------------------------------------------------------------------------
   -- --subdirs=<d>  : <d> is subdirectories to place the tool output into --
   --------------------------------------------------------------------------

   Subdir_Name : String_Access;
   --  If Subdir_Name is null, no special subdirectory is used for tool
   --  results.

   procedure Set_Subdir_Name (S : String);
   --  Sets Subdir_Name to S (if Subdir_Name is not null, frees the old value).
   --  We may have '--subdirs=<d>' option both in command line and in the list
   --  of values of Default_Switches and Switches attributes in the tool
   --  package in the project file.

   ----------------------------------------------------------------
   -- --no_object_dir : do not place the results into object dir --
   ----------------------------------------------------------------

   No_Object_Dir : Boolean := False;
   --  If this flag is ON, the tool should emulate the old behavior it has had
   --  when the tool has been called as 'gnat <tool> -P prj. Most of the ASIS
   --  tools in this case place their results into current dir, but not in the
   --  object directory of the project file.

   ---------------------------------------------------------
   -- Type to represent a project passed as a tool option --
   ---------------------------------------------------------

   type Arg_Project_Type is abstract new Project_Tree with private;
   --  This type is the base for each tool-specific project type. Most of its
   --  primitives does not need any redefinition for a specific tool.

   ------------------------------------------------------------------
   -- Primitives that hardly need redefinition for a specific tool --
   ------------------------------------------------------------------

   procedure Store_Project_Source
     (My_Project         : in out Arg_Project_Type;
      Project_File_Name  : String);
   --  If Project_File_Name ends with ".gpr", it is taken to be the name of
   --  the project file; otherwise Project_File_Name & ".gpr" is used.
   --  Checks that:
   --    - this is the first -P option provided as a tool parameter;
   --    - the project file exists.
   --  Raises ASIS_UL.Common.Parameter_Error if any of these check fails,
   --  stores the name of the project file My_Project otherwise.

   function Is_Specified (My_Project : Arg_Project_Type) return Boolean;
   --  Checks if the argument represents a project that corresponds to some
   --  project file specified as a tool parameter.

   procedure Clean_Up (My_Project : Arg_Project_Type);
   --  Removes all the temporary files created when loading a project. Does
   --  nothing of Debug_Flag_N is ON.

   function Source_Prj (My_Project : Arg_Project_Type) return String;
   --  If My_Project.Is_Specified then returns the full normalized name of the
   --  project file, otherwise returns a null string.

   procedure Load_Tool_Project (My_Project : in out Arg_Project_Type);
   --  Does the same as GNATCOLL.Projects.Load, the only difference is that
   --  all the parameters except the project are hidden. This procedure never
   --  generates any error or warning message, because here we do not know
   --  values of external variables.

   procedure Set_External_Values (My_Project : Arg_Project_Type);
   --  For each value of an external variable that has been stored as a result
   --  of the initial parameter processing, changes environment accordingly.
   --  Any inconsistencies coming from improper values of scenario variables
   --  etc. will be reported during project loading.

   procedure Extract_Compilation_Attributes
     (My_Project : in out Arg_Project_Type);
   --  Extracts and stores as compiler options the attributes that may be
   --  needed to call the compiler from the tool to create the tree.
   --
   --  The default version extracts and stores options corresponding to the
   --  following attributes:
   --     Builder.Global_Configuration_Pragmas
   --     Builder.Global_Config_File
   --     Compiler.Local_Configuration_Pragmas
   --     Compiler.Local_Config_File
   --
   --  This procedure is not supposed to be used by the tool directly, but it
   --  is used by Extract_Tool_Attributes, and you may want to redefine it for
   --  your tool.
   --
   --  ??? Should we extract anything more then global configuration file
   --  here???

   procedure Get_Sources_From_Project
     (My_Project      : Arg_Project_Type;
      Unconditionally : Boolean := False);
   --  Extracts and stores the list of sources of the project to process as
   --  tool arguments.
   --
   --  If Unconditionally is ON, this procedure extracts all the project
   --  sources regradless on all the other options.
   --
   --  More documentation is needed:
   --
   --  * when we extract the sources from the project
   --  * what happens when
   --    o there is no -U option
   --    o -U option is specified, but without the main unit
   --    o -U option is specified with the main unit name
   --
   --  ??? Extended projects???

   procedure Set_Global_Result_Dirs (My_Project : in out Arg_Project_Type);
   --  Sets the directory to place the global tool results into.

   procedure Create_Configuration_File (My_Project : Arg_Project_Type);
   procedure Create_Mapping_File (My_Project : Arg_Project_Type);
   --  Create the configuration/mapping file for the project and adds the
   --  corresponding option to the list of options used to create the tree.
   --
   --  These procedures form the important parameters for the compiler call to
   --  create the tree, so the caller should call to
   --  ASIS_US.Compiler_Options.Set_Arg_List after the calls to these
   --  subprograms.

   procedure Set_Individual_Source_Options (My_Project : Arg_Project_Type);
   --  Also for each file from the project that is a tool argument computes and
   --  stores in the source table the list of compiler options needed to create
   --  the tree for this particular file. It also defines the directory the
   --  file-specific results of the tool should be placed into. This procedure
   --  should be called *after* storing the argument files in the source
   --  table. It is NOT a part of the actions combined in Process_Project_File
   --  procedure.

   procedure Store_Files_From_Closure
     (My_Project       :     Arg_Project_Type;
      Complete_Closure : out Boolean);
   --  Provided that the tool arguments contain '-U main_unit' parameter,
   --  tries to get the full closure of main_unit and to store it as tool
   --  argument files.
   --  When the computed closure is complete, Complete_Closure is set to True,
   --  otherwise to False.

   ---------------------------------------------------------------
   -- Primitives that may need redefinition for a specific tool --
   ---------------------------------------------------------------

   function Tool_Package_Name (My_Project : Arg_Project_Type) return String;
   --  Returns the name of the tool-specific package in a project file.
   --  Sometimes the name is the same as the tool name in a native GNAT
   --  installation (this is the result of this function), sometimes -
   --  something else (such as "Check" for "gnatcheck"), in this case the
   --  function has to be redefined in the tool.

   function Section_Delimiters (My_Project : Arg_Project_Type) return String;
   --  Returns the string defining section delimiters in the command line/
   --  list of values for Default_Switches/Switches attributes, The default
   --  version returns "cargs'. You may need to redefine this function for your
   --  tool. For example gnatcheck needs "cargs rules"

   function Get_Section_Delimiters
     (My_Project : Arg_Project_Type'Class) return String;
   --  Dispatches to the project-type-specific Section_Delimiters function, and
   --  returns the result, except that "cargs" is replaced with "inner-cargs"
   --  when appropriate, as explained in asis_ul-environment.adb.

   procedure Register_Tool_Attributes (My_Project : Arg_Project_Type) is null;
   --  For the tools that for a long time are a part of the GNAT toolset and
   --  that have got the GNAT Driver support we do not need to register any
   --  attribute because they are already known by the project parser. But for
   --  a new tool you may need to register attributes that are of interest for
   --  extracting tool options or source-specific switches

   procedure Extract_Tool_Options (My_Project : in out Arg_Project_Type);
   --  Extracts tool attributes from the project file. The default does the
   --  following:
   --  * if there is exactly one source file specified, tries to get the tool
   --    options from the Switches attribute with the corresponding index. If
   --    there is no such Switches attribute, tries to get tool attributes from
   --    the Default_Switches attribute.
   --  * otherwise tries to get the tool attributes from the Default_Switches
   --    attribute.

   function Needs_Per_File_Output
     (My_Project : Arg_Project_Type)
      return       Boolean is (False);
   --  Tells if the tool needs to generate some output for each argument file.

   function Source_Result_Dir
     (My_Project  : Arg_Project_Type;
      Source_Prj  : Project_Type;
      Source_File : Virtual_File)
   return           String;
   --  Detects the path to the directory where the source-specific results
   --  for Source_File should be placed. The default version returns the object
   --  directory for the project the source belongs to (Source_Prj), it takes
   --  into account the effect of  '--subdir=<dir>' option.

   function Compute_Project_Closure
     (My_Project  : Arg_Project_Type)
      return        Boolean is (True);
   --  Tells if source files should be extracted from the project if no
   --  argument file is specified explicitly. There are at least two tools
   --  that has to redefine this function to False - gnatelim and gnatstub.

   procedure Set_Tree_Creator (My_Project  : Arg_Project_Type);
   --  This procedure should be called *after* completing analysis of the
   --  argument project file, it may be called even if no project file is
   --  specified as a tool parameter.
   --
   --  Currently this procedure assumes that it is called from the GNAT tool
   --  (that is, it relies on a specific location of the executable).
   --
   --  This procedure tries to define and to locate the programs to be used
   --  for tree creation. It uses the settting from target (from the command
   --  line, from the argument project file, from the tool name - in decreasing
   --  priority order, and the value of A4G.GNSA_Switch.Use_GNSA flag.

   -----------------------------------------
   --  Computing and processing clousures --
   -----------------------------------------

   procedure Closure_Setup (My_Project : in out Arg_Project_Type);
   --  Does the general preparations for closure processing (this procedure
   --  should be called from the tool temporary directory!):
   --
   --  * creates a temporary wrapper project file used to compute closure;
   --
   --  * unloads the tool argument project and loads this temporary project
   --    file (mapping and configuration files used for tree creation should be
   --    updated);
   --
   --  * stores all the sources from this project into the source table, for
   --    each source the status is set to Waiting_For_Closure
   --
   --  * for each source stores its specific compilation options;
   --
   --  * if parallel tree creation is set it is disabled

   procedure Closure_Clean_Up (My_Project : in out Arg_Project_Type);
   --  Removes all the temporary stuff created for closure computation. Most
   --  probably there is no need to redefine this procedure for tools. The
   --  default version just removes object subdirs created for closure
   --  computation.

   procedure Add_Main
     (My_Project : Arg_Project_Type;
      Main_Name  : String);
   --  Adds Main_Name to the list of mains to compute the closure for. Also
   --  sets the status for this source to Waiting in the source table. This
   --  procedure assumes that the file with main_Name is already stored in
   --  Source Table.
   --  This procedure also adds Main_Name to the closure being computed.

   function Closure_Complete (My_Project : Arg_Project_Type) return Boolean;
   --  Is supposed to be called after creation of at least one new tree file.
   --  Checks if new sources should be added to the closure being processed.
   --  if there are such sources, sets their status in the Source Table to
   --  Waiting and returns False. Otherwise returns True.

   procedure Closure_Debug_Image_Mains;
   procedure Closure_Debug_Image_Closure;

   -------------------------------------------------------------------------
   -- Primitives that will for sure need redefinition for a specific tool --
   -------------------------------------------------------------------------

   procedure Print_Tool_Usage (My_Project : Arg_Project_Type) is abstract;
   --  Prints out the tool usage info for '--help' option.

   procedure Scan_Arguments
     (My_Project  : in out Arg_Project_Type;
      First_Pass  :        Boolean    := False;
      Parser      :        Opt_Parser := Command_Line_Parser;
      In_Switches :        Boolean    := False);
   --  This procedure should be redefined for each tool project type. It
   --  should be called immediately after the call to Initialize_Option_Scan
   --  that should create the Parser for it. The procedure defines the loop
   --  through the parameters - either command-line parameters or tool
   --  parameters defined in a tool-specific package of the tool argument
   --  project file.
   --  If the actual for Parser is different from Command_Line_Parser, the
   --  procedure assumes the options from the project file. If In_Switches is
   --  ON, it assumes that the options are the values of the Switches
   --  attribute, otherwise - of the Default_Switches attribute.
   --  This procedure is supposed to be used for processing command-line
   --  parameters twice - first, it detects and stores only project-specific
   --  attributes, argument sources and '--help' and "--version' options, when
   --  called for this, it should have First_Pass parameter ON. Then, after
   --  processing the project file (if it is provided as a tool parameter and
   --  detected during the first pass through the command-line parameters),
   --  all the other command-line parameters should be processed and stored
   --  (this allows the parameters specified in the command line to override
   --  the parameters given in the project file), and when called for this,
   --  the procedure should have First_Pass set OFF.

   procedure Aggregate_Project_Report_Header (My_Project : Arg_Project_Type);
   --  Prints header in the summary report file created if the argument project
   --  is an aggregate project. In this case a tool is spawned to run separatly
   --  for each project being aggregated, and each such run creates its own
   --  report separate file.

   procedure Close_Aggregate_Project_Report (My_Project : Arg_Project_Type);
   --  Finalizes in the summary report file created if the argument project
   --  is an aggregate project. In this case a tool is spawned to run separatly
   --  for each project being aggregated, and each such run creates its own
   --  report separate file.
   --
   --  The default version of this procedure does nothing except closing the
   --  aggregated-project-reports tag in summary XML report file (if XML report
   --  mode is ON).

   procedure Report_Aggregated_Project
     (Aggregate_Prj          : Arg_Project_Type;
      Arrgegated_Prj_Name    : String;
      Expected_Text_Out_File : String;
      Expected_XML_Out_File  : String);
   --  Starts a record about processing of an aggregated project in a summary
   --  report file if the tool argument project is an aggregate project. By
   --  default, prints out the name of the aggregated project to process and
   --  the name(s) of the report file(s) that is (are) expected to be created.

   procedure Report_Aggregated_Project_Exit_Code
     (Aggregate_Prj : Arg_Project_Type;
      Exit_Code     : Integer);
   --  Starts a record about processing of an aggregated project in a summary
   --  report file if the tool argument project is an aggregate project. By
   --  default prints out the (text image of the) exit code.

   ------------------------------
   -- Non-primitive operations --
   ------------------------------

   procedure Store_Mapping_File_Name (S : String);
   procedure Store_Config_File_Name  (S : String);
   function Get_Mapping_File_Name return String;
   function Get_Config_File_Name  return String;
   --  A tool has to store the names of the Ada mapping file and configuration
   --  flles created as a result of project file analysis to have a possibility
   --  to delete them (GNATCOLL.Projects.Aux.Delete_All_Temp_Files cannot be
   --  used for this because reading tree files destroys the data structures
   --  it uses). Procedure Store_Mapping_File_Name (Store_Config_File_Name)
   --  stores its argument as the name of the mapping (configuration) file.
   --  Function Get_Mapping_File_Name (Get_Config_File_Name) returns the (last)
   --  name stored as the name of the mapping (configuration) file or an empty
   --  string of the mapping file name has not been stored.

   --  The following routines are used to resolve the problem with a mapping
   --  file in case of an asynchronous tree creation. In the text below N is
   --  the number of asynchronous processes defined by '-jn' option.

   procedure Create_Mapping_File_Copies;
   --  Should be called after all phases of tool argument processing. If there
   --  is a mapping file created as a result of argument project file, and if
   --  asynchronous tree creation is specified, this procedure creates N-1 copy
   --  of the mapping file and stores their names. These copies are to be used
   --  by the tree creation processed being spawned by the main process (the
   --  main process uses the original mapping file created when analyzing the
   --  project file), the idea is that two asynchronous compilations should
   --  never use the same mapping file. This procedure also creates a boolean
   --  array of the length N-1 that indicates if a mapping file copy is used by
   --  a compiler run.

   subtype Mapping_File_Copy_Num is Natural;
   No_Mapping_File_Copy : constant Mapping_File_Copy_Num := 0;

   function Get_Mapping_File_Copy_Name (J : Natural) return String;
   --  Returns the name of the mapping file copy. J is ne number of the copy,
   --  copies are numbered from 1 to N-1.

   function Get_Free_Mapping_File return Natural;
   --  Returns the number of a mapping file copy that is not used in any
   --  compilation at the moment. The result is in 1 .. N-1

   procedure Set_Busy (J : Natural);
   procedure Set_Free (J : Natural);
   --  Set that a mapping file copy number J is/is not used in the compilation.
   --  J should be in 1 .. N-1.

   --------------------------------------
   --  General project file processing --
   --------------------------------------

   procedure Initialize_Environment;
   --  Initializes the environment for extracting the information from the
   --  project file. This includes setting the parameters specific for the
   --  given tool version assuming that the tools for cross environment are
   --  named in a standard way (that is, <cross-prefix>-<tool_name>.

   procedure Process_Project_File (My_Project : in out Arg_Project_Type'Class);
   --  Combines all the actions needed to process the argument project file
   --  except storing individual compilation options for argument files.

private
   type Arg_Project_Type is abstract new Project_Tree with record
      Source_Prj : String_Access;
      --  Stores the name of the project file that is a tool argument
   end record;

end ASIS_UL.Projects;
