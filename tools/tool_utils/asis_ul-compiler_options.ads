------------------------------------------------------------------------------
--                                                                          --
--                     ASIS UTILITY LIBRARY COMPONENTS                      --
--                                                                          --
--             A S I S _ U L . C O M P I L E R _ O P T I O N S              --
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

--  This package provides simple utilities and data structures for processing
--  and storing the ASIS tool arguments that have to be passed to the compiler
--  when the compiler is called to create the tree file for ASIS

with GNAT.Command_Line; use GNAT.Command_Line;
with GNAT.OS_Lib;       use GNAT.OS_Lib;

package ASIS_UL.Compiler_Options is

   --  To use the resources provided by this package, the tool is supposed to
   --  use the following way of parameter processing and creating the trees
   --  for ASIS:
   --
   --  ..._Environment.Initialize.Scan_Parameters should contain the following
   --  sequence of statements (provided that GNAT.Command_Line is used):
   --
   --       Initialize_Option_Scan
   --        (Stop_At_First_Non_Switch => False,
   --         Section_Delimiters       => "cargs");
   --      loop
   --         case
   --            GNAT.Command_Line.Getopt
   --              (<tool-specific options, may include compiler options>)
   --         is
   --            ...
   --            when 'I' | 'g' =>
   --               if Full_Switch = "gnatec" then
   --                  Store_gnatec_Option (Parameter);
   --               elsif Full_Switch = "I"
   --                  Store_I_Option (Parameter);
   --               ...
   --               end if;
   --            ...
   --         end case;
   --      end loop;
   --
   --      ...
   --
   --      Read_Tool_Arguments; -- Any procedure or operator sequence that
   --                           --  reads tool argument(s)
   --
   --      ...
   --
   --      Process_cargs_Section;
   --
   --  Calls to Asis.Extensions.Compile should use
   --  ASIS_UL.Compiler_Options.Arg_List as an actual for Args parameter.
   --
   --  The call to Initialize_Option_Scan may define other option sections,
   --  if so, the processing for these sections should be provided along with
   --  the call to Process_cargs_Section.

   Arg_List : Argument_List_Access;
   --  This variable should contain a full list of compilation options to be
   --  passed to gcc when gcc is called from the tool to create the tree for
   --  ASIS. This list does not have to contain the following options
   --  '-c -gnatc -gnatt -gnatws -x ada', they are always set by the
   --  Asis.Extensions.Compile routine. The options that are supposed to be
   --  represented by this variable include Ada version option, reference
   --  to configuration file etc. Note, that this list should NOT contain -Idir
   --  options, all these options are supposed to be converted to the
   --  corresponding directory names included in the file pointed by the
   --  ADA_PRJ_INCLUDE_FILE environment variable. But '-I-' option can be in
   --  this list.
   --
   --  Note that you should NOT call the Free procedure for this variable,
   --  because if this variable is set, Free will erase all the compiler
   --  options being stored in its value.

   Source_Search_Path : String_Access;
   --  This variable should contain a path (in the format suitable for passing
   --  to Locate_Regular_File procedure) to look for the source files specified
   --  as tool arguments or searched as sources that are needed by the tool to
   --  complete the processing.

   procedure Store_RTS_Path (S : String);
   --  Stores its parameter assuming that it is the parameter of --RTS= option.

   function Get_RTS_Path return String;
   --  Returns strig saved with the call to Store_RTS_Path. Returns empty
   --  string nothing has been stored with Store_RTS_Path.

   Custom_RTS : String_Access;
   --  If --RTS option is specified, contains the parameter of this option
   --  ??? Duplicates Get_RTS_Path/Store_RTS_Path from ASIS_UL.Projects???

   ADA_PRJ_INCLUDE_FILE_Full_Name : String_Access;
   --  Is supposed to contain the full normalized name of the file pointed by
   --  the ADA_PRJ_INCLUDE_FILE environment variable

   Need_Tmp_ADA_PRJ_INCLUDE_FILE : Boolean := False;
   --  This flag is set ON by Process_ADA_PRJ_INCLUDE_FILE procedure if it
   --  detects that there is no existing file pointed by the value of
   --  ADA_PRJ_INCLUDE_FILE environment variable. If it is ON, this means that
   --  a new file should be created if I_Options_Specified is ON and we have to
   --  put the corresponding directories in the file.

   I_Options_Specified : Boolean := False;
   --  If this flag is ON this means that '-Idir' options have been specified
   --  as tool arguments, so the corresponding directories should be added
   --  in the file pointed by the ADA_PRJ_INCLUDE_FILE environment variable

   procedure Store_I_Option (Path : String);
   --  Similar to Store_Path_Option, but takes into account -I- option as well.
   --  In case if -I- it is stored as is, because if "-" is passed as a
   --  parameter of path normalization, the result is an empty string.

   procedure Store_GNAT_Option_With_Path (Option : String; Path : String);
   --  Similar to Store_Path_Option, but takes into account that for the
   --  Option the path parameter may or may not be separated by the space
   --  character or '='

   procedure Store_Option (Switch : String);
   --  Stores the option as is.

   procedure Set_Arg_List;
   --  Assigns the value to Arg_List variable.
   --
   --  Note, that '-I' options are NOT placed to Arg_List, because it may
   --  result in a very long list that may exceed command line length
   --  limitation on some platforms, in particular on Windows. Instead, each
   --  directory that is an argument of '-Idir' option is added to the file
   --  pointed by ADA_PRJ_INCLUDE_FILE environment variable.

   procedure PAL;
   --  Print_Arg_List. Print out the content of the Arg_List variable, to be
   --  used for debugging purposes.

   Add_I_Options_To_Source_Search_Path : Boolean := False;
   --  If this flag is ON, -I options that are explicitly specified for the
   --  tool are added to the value of Source_Search_Path. This is up to the
   --  tool to set this flag ON

   procedure Store_I_Options;
   --  This is supposed to be the last procedure called to process and to store
   --  compiler options. If '-Idir' options are specified, it either adds them
   --  in the beginning of the file pointed by the ADA_PRJ_INCLUDE_FILE
   --  environment variable or, if this variable does not point to any
   --  existing file, creates a temporary file, stores the (normalized)
   --  directory names provided as parameters of '-Idir' in this file and sets
   --  it as the value of ADA_PRJ_INCLUDE_FILE environment variable. This
   --  procedure also generates the debug info containing the source search
   --  path and the content of ADA_PRJ_INCLUDE_FILE file.

   procedure Store_Full_Path_To_RTS;
   --  Stores --RTS=<full path to runtime> if an --RTS option was passed as
   --  a command line parameter. Otherwise does nothing.
   --
   --  Raises Fatal_Error if the parameter specified for command-line --RTS=...
   --  option cannot be resolved to a full path to runtime.

   procedure Process_cargs_Section_Old
     (Parser : Opt_Parser := Command_Line_Parser;
      Preprocessing_Allowed : Boolean := True);
   --  Implements the processing of all the options in -cargs section. Note
   --  that this procedure does NOT call Initialize_Option_Scan to define
   --  'cargs' section. The actual for Parser parameter should be the option
   --  parser created by the appropriate call to Initialize_Option_Scan.
   --  If Preprocessing_Allowed is set OFF, then this procedure raises
   --  Parameter_Error with the corresponding error message for the
   --  preprocessor options ('-gnatep', '-gnateD'). This prevents a tool
   --  from calling the preprocessor for an argument source before processing
   --  the source.
   --  If -cargs section contains '-gnata' option, this option is just skipped.
   --  '-gnata' should not be passed to the compiler call that creates the tree
   --  because it may cause tree transformations that are not good for ASIS.
   --
   --  This routine implements the old version of processing of the -cargs
   --  section, when we try to filter out options that are not needed or even
   --  could be dangerous for tree creation.

   procedure Process_cargs_Section
     (Parser : Opt_Parser := Command_Line_Parser);
   --  Stores the compilation option passed in -cargs section. All the options
   --  are stored "as is", no analysis of filtering os performed except
   --  normalizing parameters of -I, -gnatec, -gnatem and -gnated options.

   procedure Process_ADA_PRJ_INCLUDE_FILE;
   --  If ADA_PRJ_INCLUDE_FILE points to an existing file, scans this file
   --  and stores the directories listed in it as source search path for the
   --  tool argument files.

end ASIS_UL.Compiler_Options;
