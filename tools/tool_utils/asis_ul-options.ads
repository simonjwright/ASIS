------------------------------------------------------------------------------
--                                                                          --
--                     ASIS UTILITY LIBRARY COMPONENTS                      --
--                                                                          --
--                      A S I S _ U L . O P T I O N S                       --
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

--  This package defines options that are supposed to be of a common interest
--  for all the tools.

with GNAT.OS_Lib; use GNAT.OS_Lib;

with ASIS_UL.Debug;

package ASIS_UL.Options is

   Print_Version : Boolean := False;
   --  '--version'
   --  Print version info and exit

   Print_Usage : Boolean := False;
   --  '--help'
   --  Print usage info and exit

   Verbose_Mode : Boolean := False;
   --  The verbose mode.
   --  '-v'

   Quiet_Mode       : Boolean := False;
   Fully_Quiet_Mode : Boolean := False;
   --  The quiet mode
   --  '-q'
   --  !!! At some point we have to get rid of this flag and to use only
   --  fine tuned debug options defined by the flags in ASIS_UL.Debug. Most of
   --  the tools are supposed to use Quiet_Mode flag. The Fully_Quiet_Mode flag
   --  is "more quiet than just Quiet_Mode (for example it suppresses the error
   --  message when the tool cannot compile the source for the tree).
   --  Fully_Quiet_Mode is used in Q4A test driver.

   Progress_Indicator_Mode : Boolean renames ASIS_UL.Debug.Debug_Flag_D;
   --  Generate the output to be used for GPS progress indicator.
   --  '-dd'

   Generate_XML_Help : Boolean := False;
   --  If this file is ON, the tool generates the XML description of the tool
   --  parameters to be used for creating the GUI in GPS.
   --  '-hx'.

   Compute_Timing : Boolean := False;
   --  If this flag is ON, the total execution time (wall clock) of the tool
   --  is computed and printed out.
   --  '-t'

   type Warning_Modes is
     (Quiet,  --  all warnings are suppressed
      Short,
      Normal,
      Full);

   Warning_Mode : Warning_Modes := Normal;
   --  Specifies the warning message level
   --  '-w(q|s|n|f)

   Log_Mode : Boolean := False;
   --  Create the log file and duplicate in this file all the messages
   --  generated by a tool.
   --  '-log'

   Test_Mode : Boolean := False;
   --  Run the tool in a test mode that is suitable for Q4A massive test
   --  driver. The details of the test mode are tool-specific and should be
   --  set in the tool.
   --  '--test'

   Exempted_Units : GNAT.OS_Lib.String_Access := null;
   --  '--ignore=<filename>
   --  File containing a list of units to be exempted. (Depending on a tool,
   --  such units either are not processed or the tool does not generate
   --  results for them).

   Aggregated_Project : Boolean := False;
   --  '-A <project_file>
   --  True if this is a tool call spawned from an original tool call with
   --  aggregated project as a parameter. In this mode the tool processes only
   --  one (non-aggregate) project from the projects being aggregated.

   In_Aggregate_Project : Boolean := False;
   --  True if the tool is called for an aggregate project that aggregates more
   --  than one (non-aggregate) project/

   Incremental_Mode : Boolean := False;
   --  True if --incremental was given on the command line. In this mode, the
   --  ASIS tool is incremental on a file-by-file basis (e.g. don't run
   --  gnat2xml if the xml file is already up to date).
   --
   --  Incremental_Mode works like this: gnat2xml (or whatever other ASIS tool
   --  that supports this mode) invokes a builder (currently gprbuild, but
   --  could be gnatmake), telling the builder to pretend that gnat2xml is the
   --  "compiler". So the builder invokes gnat2xml once for each relevant
   --  file. So we have an "outer" invocation of gnat2xml, and many "inner"
   --  invocations.
   --
   --  The command-line arguments passed to the outer gnat2xml are modified
   --  before passing them along to the builder. "--incremental" is not passed
   --  to the builder. Project-related arguments are passed to the
   --  builder. Most arguments need to be seen by the inner gnat2xmls, so they
   --  are passed to the builder after "-cargs".
   --  See ASIS_UL.Environment.Builder_Command_Line for details.

   --  In incremental mode, Incremental_Mode is True for the outer invocation,
   --  and Mimic_gcc is True for the inner invocations. In nonincremental mode,
   --  both are False. They are never both True.

   Incremental_Mode_By_Default : constant Boolean := False;
   --  Set this to True to force --incremental mode ON in cases where it is
   --  legal.

   Mimic_gcc : Boolean := False;
   --  True if this is an inner invocation of the tool for incremental mode, so
   --  that the ASIS tool should mimic the gcc compiler in certain ways.
   --
   --  The tool sets Mimic_gcc to True when it is invoked by the builder (see
   --  Pre_Scan_Arguments in ASIS_UL.Environment for how this is detected).
   --
   --  When Mimic_gcc is True, the tool behavior is modified as follows:
   --
   --     - When a library unit body is processed, also process the spec and
   --       all subunits. This is necessary because the builder does not invoke
   --       the "compiler" on specs with bodies, nor on subunits. This involves
   --       setting Add_Needed_Sources ON.
   --
   --     - When the ASIS tool invokes the real compiler on a library unit body
   --       or bodiless spec, it does so in the Tool_Inner_Dir, rather than
   --       the usual Tool_Temp_Dir, so that the ALI file will be in the right
   --       place for subsequent runs of the builder.
   --
   --     - When doing cleanup, we set Keep_ALI_Files to True so the .ali files
   --       are kept around for subsequent runs of the builder.

   Outer_Parallel : Boolean := False;
   --  True if Mimic_gcc is True, and the outer invocation had -j for
   --  parallelism. This indicates that we need to do file locking.

   Incremental_Switches : constant String :=
   --  Switches accepted by ASIS tools that can operate in incremental
   --  mode. Some are used in Incremental_Mode, some in Mimic_gcc.
     "-incremental " &
     "-outer-parallel " &
     "-outer-dir= " &
     "c gnatc gnatA gnatem! x: " & -- switches passed by gprbuild
     "gnatec! gnateO! ";

   Compiler_Output_Subdir : String_Access := null;
   --  When using incremental mode without a project file (which is not
   --  currently supported!), this is the directory in which the compiler puts
   --  its output (ALI and tree files).
   --
   --  In all other cases (nonincremental mode, and incremental mode with a
   --  project file), this horsing around is unnecessary, and
   --  Compiler_Output_Subdir is null.

   Files_Switch_Used : Boolean := False;
   --  True if the files= switch was used

   Out_Dir : String_Access := null;
   --  If the command-line switch "--output-dir=xxx" was given, this is
   --  "xxx". If no --output-dir switch was given, this is null.

   Process_Num : Natural := 1;
   --  The maximal number of parallel tree creations
   --  -jN

   J_Specified : Boolean := False;
   --  True if the -jN option was given. This is used to distinguish -j0 on a
   --  uniprocessor from no -j switch.

   Generate_Representation_Clauses : Boolean := False;
   --  True if gnat2xml should generate representation clauses. This is
   --  controlled by the --rep-clauses switch.

   ----------------------------------------
   -- Flags computed from other settings --
   ----------------------------------------

   --  The flags listed below are not set by some options, but they are
   --  computed from gnatcheck command line and rule options

   No_Argument_File_Specified : Boolean := True;
   --  Flag indicating if no argument file is specified for the tool call.
   --  Usually the tool generated the brief help info in this case.

   Nothing_To_Do : Boolean := False;
   --  Flag indicating if a tool does not have any real work to do - that is,
   --  that there are some argument files specified, and there is at least one
   --  argument file that exists. A tool should not rely on the default setting
   --  of this file.

   ------------------------------------------------------
   -- options related to program global state analysis --
   ------------------------------------------------------

   Process_RTL_Units : Boolean := False;
   --  If this flag is set ON, a tool tries to look into RTL units when
   --  analyzing global properties of the argument sources, even if these RTL
   --  units are not specified as tool arguments
   --  '-a'

   Generate_Global_Structure_Warnings : Boolean := False;
   --  Generate warning messages in case if a problem that prevents the
   --  complete analysis of the program global structure is detected

   Buld_Call_Graph : Boolean := False;
   --  Flag indicating if we have to collect the call graph information.

   Do_Transitive_Closure : Boolean := False;
   --  Flag indicating if the transitive closure of the call graph is needed.

   Collect_Data_Info : Boolean := False;
   --  Flag indicating if the data information should be included into the
   --  program global structure representation

   Main_Subprogram_Name : String_Access;
   --  -main=<name of the main subprogram>
   --  The name of the source file containing the main subprogram. The name
   --  may or may not contain the suffix. This subprogram is called by the
   --  environment task.

   --  More flags? Flags to represent type?

   Represent_Dispatching_Calls : Boolean := True;
   --  If this flag is ON, the call graph represents the dispatching calls.
   --  This flag should be removed after completing the implementation of
   --  dispatching calls representation in the call graph.

   Skip_Dispatching_Calls : Boolean := False;
   --  If this flag is ON, and Represent_Dispatching_Calls is also ON,
   --  dispatching calls are stored separately and a tool may deside if it
   --  needs transitive closure with or without dispatching calls. Originally
   --  this flag has been introduced for Skip_Dispatching_Calls parameter of
   --  the gnatcheck Recursive_Subprograms rule.

end ASIS_UL.Options;
