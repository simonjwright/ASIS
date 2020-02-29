------------------------------------------------------------------------------
--                                                                          --
--                            GNATPP COMPONENTS                             --
--                                                                          --
--                       G N A T P P . O P T I O N S                        --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                    Copyright (C) 2001-2019, AdaCore                      --
--                                                                          --
-- GNATPP  is free software; you can redistribute it and/or modify it under --
-- terms  of  the  GNU  General  Public  License  as  published by the Free --
-- Software Foundation;  either version 3, or ( at your option)  any  later --
-- version.  GNATCHECK  is  distributed in the hope that it will be useful, --
-- but  WITHOUT  ANY  WARRANTY;   without  even  the  implied  warranty  of --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General --
-- Public License for more details.  You should have received a copy of the --
-- GNU General Public License distributed with GNAT; see file  COPYING3. If --
-- not,  go  to  http://www.gnu.org/licenses  for  a  complete  copy of the --
-- license.                                                                 --
--                                                                          --
-- GNATPP is maintained by AdaCore (http://www.adacore.com)                 --
--                                                                          --
------------------------------------------------------------------------------

pragma Ada_2012;

--  This package contains all the GNATPP options and control parameters, and
--  some auxiliary flags and objects needed to control pretty-printing

with GNAT.OS_Lib; use GNAT.OS_Lib;
with System.WCh_Con; use System.WCh_Con;

with Pp.Formatting; use Pp.Formatting;
with ASIS_UL.String_Utilities; use ASIS_UL.String_Utilities;

with GNATPP.Projects;

package GNATPP.Options is

   The_Formatting_Options : Formatting_Options :=
     (Is_PP => True, others => <>);
   Opt : Formatting_Options renames The_Formatting_Options;

   PP_Indentation : Positive renames Opt.PP_Indentation;

   PP_Cont_Line_Indentation_Set : Boolean := False;

   PP_Cont_Line_Indentation : Positive renames Opt.PP_Cont_Line_Indentation;

   PP_Attribute_Casing : PP_Casing renames Opt.PP_Attribute_Casing;

   PP_Keyword_Casing : PP_Casing renames Opt.PP_Keyword_Casing;

   PP_Pragma_Casing : PP_Casing renames Opt.PP_Pragma_Casing;

   PP_Name_Casing : PP_Casing renames Opt.PP_Name_Casing;

   PP_Enum_Literal_Casing : PP_Casing renames Opt.PP_Enum_Literal_Casing;
   PP_Enum_Literal_Casing_Specified : Boolean   := False;

   PP_Type_Casing           : PP_Casing renames Opt.PP_Type_Casing;
   PP_Type_Casing_Specified : Boolean   := False;

   PP_Nnumbers_Casing           : PP_Casing renames Opt.PP_Nnumbers_Casing;
   PP_Nnumbers_Casing_Specified : Boolean   := False;

   Use_Predefined_Casing : Boolean renames Opt.Use_Predefined_Casing;

   Use_Dictionary : Boolean renames Opt.Use_Dictionary;
   Dictionary_File_Names : String_Vector renames Opt.Dictionary_File_Names;

   Format_Comments : Boolean renames Opt.Format_Comments;

   GNAT_Comment_Inden : Boolean renames Opt.GNAT_Comment_Inden;

   Standard_Comment_Indent : Boolean renames Opt.Standard_Comment_Indent;

   GNAT_Comment_Start : Boolean renames Opt.GNAT_Comment_Start;

   Reformat_Comment_Block : Boolean renames Opt.Reformat_Comment_Block;

   Preserve_Special_Comments : Boolean renames Opt.Preserve_Special_Comments;

   No_Tab_In_Comments : Boolean renames Opt.No_Tab_In_Comments;

   Comments_Only : Boolean renames Opt.Comments_Only;

   End_Labels : Boolean renames Opt.End_Labels;

   Add_FF : Boolean renames Opt.Add_FF;

   Compact_Layout : Boolean renames Opt.Compact_Layout;

   End_Id : Boolean renames Opt.End_Id;

   Separate_Line_For_IS : Boolean renames Opt.Separate_Line_For_IS;

   Separate_Line_For_THEN_and_LOOP : Boolean renames
     Opt.Separate_Line_For_THEN_and_LOOP;

   No_Separate_Line_For_THEN_and_LOOP : Boolean renames
     Opt.No_Separate_Line_For_THEN_and_LOOP;

   Separate_Line_For_Label : Boolean renames Opt.Separate_Line_For_Label;

   Separate_Line_For_USE : Boolean renames Opt.Separate_Line_For_USE;

   Separate_Line_For_Stmt_Name : Boolean renames
     Opt.Separate_Line_For_Stmt_Name;

   Split_Line_Before_Op : Boolean renames Opt.Split_Line_Before_Op;

   RM_Style_Spacing : Boolean renames Opt.RM_Style_Spacing;

   Add_Empty_Lines : Boolean renames Opt.Add_Empty_Lines;

   Insert_Blank_Lines : Boolean renames Opt.Insert_Blank_Lines;
   Preserve_Blank_Lines : Boolean renames Opt.Preserve_Blank_Lines;

   Max_Line_Length : Natural renames Opt.Max_Line_Length;

   Align_Colons_In_Decl : Boolean renames Opt.Align_Colons_In_Decl;
   Align_Asign_In_Decl  : Boolean renames Opt.Align_Asign_In_Decl;
   Align_Asign_In_Stmts : Boolean renames Opt.Align_Asign_In_Stmts;
   Align_Arrows         : Boolean renames Opt.Align_Arrows;
   Align_Ats            : Boolean renames Opt.Align_Ats;

   Case_Threshold : Natural renames Opt.Case_Threshold;

   Par_Specs_Threshold : Natural renames Opt.Par_Specs_Threshold;

   Par_Associations_Threshold : Natural renames Opt.Par_Associations_Threshold;

   Decimal_Grouping : Natural renames Opt.Decimal_Grouping;
   Based_Grouping : Natural renames Opt.Based_Grouping;

   Pp_Off_String : String_Access renames Opt.Pp_Off_String;
   Pp_On_String : String_Access renames Opt.Pp_On_String;

   type Output_Modes is
   --  Defines the where and how gnatpp places the result source.
     (Pipe,
      --  Sends the output into Stderr.
      Create_File,
      --  Creates the file with the name specified in 'o' option. If the
      --  file with the given name already exists, does not erase it and gives
      --  up.
      Force_Create_File,
      --  Creates the file with the name specified in 'o' option. If the
      --  file with the given name already exists, erases the old file and
      --  replaces it with the pretty-printed source.
      Replace,
      --  Replaces the argument source with the pretty-printed source. The
      --  original source is stored in the file <arg_source>.npp. If the file
      --  with such a name already exists, gnatpp gives up.
      Force_Replace,
      --  Replaces the argument source with the pretty-printed source. The
      --  original source is stored in the file <arg_source>.npp. If the file
      --  with such a name already exists, gnatpp overrides it.
      Replace_No_Backup,
      --  Replaces the argument source with the pretty-printed source. The
      --  original source is not stored in any back-up file.
      Default,
      --  Put the result source into <arg_source>.pp, overriding the existing
      --  file if any.
      Out_Directory);
      --  Put the result into <arg_source_simple_name> in directory Out_Dir.

   PP_Suffix : constant String := ".pp";
   NPP_Suffix : constant String := ".npp";
   --  The suffixes for the file names for default result and backup copy
   --  files.

   subtype Create_Modes is Output_Modes with
     Predicate => Create_Modes in Create_File | Force_Create_File;
   subtype Replace_Modes is Output_Modes with
     Predicate => Replace_Modes in Replace | Force_Replace | Replace_No_Backup;

   Output_Mode : Output_Modes := Default;

   Res_File_Name   : String_Access;
   --  ???This is used only for the -o and -of switches.

   --  Add an "enumeration literal" WCEM_Default to type WC_Encoding_Method

   subtype Optional_WC_Encoding_Method is WC_Encoding_Method'Base
     range WC_Encoding_Method'First .. WC_Encoding_Method'Last + 1;

   WCEM_Default : constant Optional_WC_Encoding_Method :=
     Optional_WC_Encoding_Method'Last;

   Output_Encoding : Optional_WC_Encoding_Method := WCEM_Default;
   --  Defines the encoding used for the result file(s).

   Form_String : String_Access := new String'("");
   --  Used as the value for the Form parameter of Open and Create procedures,
   --  defines the encoding of the result file

   File_Name_File_Name : String_Access;
   --  There is a "file name file"; this is its name. ASIS_Processing writes
   --  the output to a temp file, and Finalize moves the temp file to the
   --  actual output file. The file name file is used to pass the names of the
   --  temp and output files from ASIS_Processing to Finalize (both subunits of
   --  ASIS_UL.Source_Table.Processing).
   --
   --  ASIS_Processing is called once for each file, and it writes two lines to
   --  the file name file: the name of the temp file, and then the name of the
   --  output file. Finalize reads pairs of lines from the file name file, and
   --  moves temp --> output.
   --
   --  The reason for passing information via a file is that in
   --  Incremental_Mode, ASIS_Processing and Finalize are running in two
   --  different processes; the inner processes do ASIS_Processing, and need
   --  to pass those file names back to the outer process. The builder is in
   --  between inner and outer, and doesn't know how to cooperate in this
   --  dance.
   --
   --  The reason for doing all the renames at the end (after all
   --  ASIS_Processing is done) is again Incremental_Mode, specifically
   --  Replace_Modes. We don't want to replace the original input with the
   --  output during ASIS_Processing, because that would change timestamps and
   --  confuse the builder.
   --
   --  In Incremental_Mode, the File_Name_File_Name is constructed in the outer
   --  invocation (in Initialize), and passed down to the inner invocations via
   --  the command-line switch --file-name-file=. --file-name-file is not
   --  documented for users; it is for internal use only. In other modes, it is
   --  constructed in Initialize.
   --
   --  We use the file name file even in non-Incremental_Mode, even though it's
   --  not really necessary, just for uniformity/simplicity.
   --
   --  In Replace_Modes, we optimize by not overwriting the output (i.e. the
   --  input) if it didn't change. This is especially important in
   --  Incremental_Mode, because of the way the builder works: it will invoke
   --  gnatpp (in Mimic_gcc mode) on something.adb, which will pretty-print
   --  something.ads. If something.ads didn't need pretty-printing, we don't
   --  want to change its timestamp, causing real (code-generating) builds to
   --  do unnecessary recompiles.

   Gnatpp_Prj : GNATPP.Projects.Gnatpp_Project_Type;

end GNATPP.Options;
