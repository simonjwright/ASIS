------------------------------------------------------------------------------
--                                                                          --
--                  COMMON ASIS TOOLS COMPONENTS LIBRARY                    --
--                                                                          --
--                  A S I S _ U L . E N V I R O N M E N T                   --
--                                                                          --
--                                 S p e c                                  --
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
-- ASIS UL is maintained by AdaCore (http://www.adacore.com).               --
--                                                                          --
------------------------------------------------------------------------------

pragma Ada_2012;

--  This package contains routines for creating, maintaining and cleaning up
--  the working environment for an ASIS tool

with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with GNAT.OS_Lib;               use GNAT.OS_Lib;
with GNAT.Command_Line;         use GNAT.Command_Line;

with ASIS_UL.Projects;
with ASIS_UL.String_Utilities; use ASIS_UL.String_Utilities;
use ASIS_UL.String_Utilities.String_Vectors;

package ASIS_UL.Environment is

   procedure Initialize
     (Prj : in out ASIS_UL.Projects.Arg_Project_Type'Class);
   --  This procedure is supposed to perform the common initialization
   --  actions, such as scanning the tool parameters, checking that the
   --  parameter values are compatible, creating the temporary directory and
   --  moving into it. Parameter scanning and checking are tool-specific,
   --  so the corresponding bodies should be provided (see the body for the
   --  details).
   --
   --  If a project file is used, this processes it.
   --
   --  When this procedure is completed, ASIS_UL.Compiler_Options.Arg_List
   --  should contain the full list of options needed to call the compiler to
   --  create the tree, and all the source files that will have to be processed
   --  by the tool. That is, if the tool is based on ASIS_UL.One_Arg_Driver
   --  driver, then ASIS_UL.Common.Arg_File should be a full normalized name of
   --  some existing file (given in absolute form). If the tool is based on
   --  ASIS_UL.Several_Files_Driver??? driver, the source file table should
   --  contain full normalized names of some existing files (also given in
   --  absolute form).
   --
   --  This procedure should raise Parameter_Error in case anything is wrong
   --  with tool parameters or/and options. In particular, Parameter_Error
   --  should be raised if no existing source file is set.

   procedure Scan_Parameters
    (Prj : in out ASIS_UL.Projects.Arg_Project_Type'Class);
   --  Scans the tools parameters and initializes the corresponding data
   --  structures representing the tool options, and also stores in the
   --  temporary database the argument sources. This procedure first scans the
   --  tool command line and extracts and stores only the following
   --  information
   --
   --  * project-specific options (-Pprj, -Xref=value, -vPn, -U [main]);
   --
   --  * argument sources (both specified directly and passed through
   --    '-files=list_of_arg_files' option;

   --  * '--version' and '--help' options;
   --
   --  If either '--help' or '--version' option is detected, this procedure
   --  prints out the corresponding information and exits to OS.
   --
   --  If a project file is specified as a tool argument, the corresponding
   --  project file is loaded and processed: the tool-specific options are
   --  extracted and stored, and if no argument source is specified in a
   --  command line parameters, the project sources are extracted from the
   --  project and stored as argument sources (a set of sources extracted from
   --  the project depends on the fact if '-U' option is given in the command
   --  line, and if it contains the name of the main unit.
   --
   --  Then, the command line parameters are scanned again, but this time
   --  neither the project-specific options nor the argument sources are
   --  extracted and stored in the tool environment.
   --
   --  The procedure that actually scans the parameters (either given in the
   --  command-line or specified in the project file) and the procedure that
   --  prints out the usage information for '--help' option should be defined
   --  as primitive operations of the tool project type.

   procedure Create_Temp_Dir (Success : out Boolean);
   --  Creates the temporary directory for all the compilations performed by
   --  the tool. Sets Success to False if creating of the temporary directory
   --  failed because of any reason. Otherwise sets Success to True and sets
   --  ASIS_UL.Environment.Tool_Temp_Dir to full name (as an absolute path
   --  name) of the the temporory directory created.

   procedure Copy_Gnat_Adc;
   --  Copies the "gnat.adc" file from the Tool_Current_Dir to the
   --  Tool_Temp_Dir.
   --
   --  ???Currently also does some obsolete project-file stuff.

   procedure Context_Clean_Up;
   --  Closes and dissociates the context, if needed

   procedure Clean_Up;
   --  Performs the general final clean-up actions, including closing and
   --  deleting of all files in the temporary directory and deleting this
   --  temporary directory itself.

   function More_Arguments
     (Store_Arguments : Boolean := True;
      In_Switches     : Boolean := False;
      Parser          : Opt_Parser := Command_Line_Parser)
      return            Boolean;
   --  Is supposed to be used as a common part of Scan_Parameters in the
   --  loop that iterates through the command line. Is supposed to be called
   --  when Getop returns ASCII.NUL. Tries to read arguments and store them (if
   --  any). Returns True if at least one argument is found and False
   --  otherwise.
   --  In case if '-U' option is specified, this procedure stores the first
   --  argument file into ASIS_UL.Projects.Main_Unit, but if
   --  ASIS_UL.Projects.Main_Unit is already set or if it processes more then
   --  one argument, it generates an error message that says that only one main
   --  unit can be specified for '-U' and raises Parameter_Error.

   type Common_Arg_Status is (Arg_Processed, Arg_Not_Processed, Quit);

   function Scan_Common_Arg
     (First_Pass : Boolean;
      Parser : Opt_Parser;
      In_Switches : Boolean;
      In_Project_File : Boolean;
      Initial_Char : Character)
     return Common_Arg_Status;
   --  This is used to scan a command-line argument that is common to more than
   --  one ASIS tool, or might be in the future. If the current argument is a
   --  common one, this processes it. Otherwise, it does nothing. Initial_Char
   --  is the character returned by Getopt, and First_Pass indicates which pass
   --  we are in.
   --
   --  If the argument is processed by Scan_Common_Arg, it returns
   --  Arg_Processed, unless the switch indicates that we should quit
   --  (e.g. "--help" or "--version"), in which case it returns Quit.  If the
   --  argument is not processed by Scan_Common_Arg, it returns
   --  Arg_Not_Processed, in which case the caller is expected to deal with
   --  the argument.
   --
   --  If this function can handle a particular argument (say, "--my-switch"),
   --  that doesn't mean --my-switch is meaningful for every ASIS tool that
   --  calls Scan_Common_Arg. It will be meaningful only if "-my-switch" is
   --  included in the string passed to Getopt.

   procedure Call_Builder;
   --  Used by the outer invocation in incremental mode to call the
   --  builder. Raises Fatal_Error on failure.

   Extra_Inner_Pre_Args, Extra_Inner_Post_Args : String_Vector;
   --  In Incremental_Mode, these may be used by the outer invocation of the
   --  tool to pass information to the inner invocations. The Pre ones go
   --  first; the Post ones go last.

   Initial_Dir : constant String := Normalize_Pathname (Get_Current_Dir);
   --  This is the current directory at the time the current process started.

   Tool_Current_Dir : String_Access;
   --  This is the full path name of the current directory when the user
   --  invoked the tool. This is the same as Initial_Dir, except in the case of
   --  an inner invocation.
   --
   --  If --outer-dir=/some/path was passed on the command line, then this is
   --  an inner invocation, and this is set to "/some/path". In incremental
   --  mode (with a project file), the builder sets the current directory for
   --  the inner invocations to a subdirectory of the object directory
   --  (Tool_Inner_Dir below). So the outer invocation passes --outer-dir to
   --  allow the inner one to find the original directory in which the tool was
   --  run. We switch to this directory during command-line processing, so we
   --  can find files based on what the user expects. For example, for
   --
   --     gnatcheck -rules -from-file=rules.txt
   --
   --  we want to look for rules.txt in the directory where gnatcheck was
   --  originally run from.
   --
   --  Thus, Tool_Current_Dir in the inner invocation is the same as
   --  Tool_Current_Dir in the outer invocation.

   Tool_Temp_Dir : String_Access;
   --  Contains the full path name of the temporary directory created by the
   --  ASIS tools for the tree files, etc.

   Tool_Inner_Dir : String_Access;
   --  For an inner invocation, this is the subdirectory of the object
   --  directory in which gprbuild invoked this process; it's the same as
   --  Initial_Dir. If this is not an inner invocation, this is null,
   --  and not used.

   procedure Print_Command_Line;
   --  Prints the command line to standard output for debugging

end ASIS_UL.Environment;
