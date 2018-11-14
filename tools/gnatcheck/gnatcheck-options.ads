------------------------------------------------------------------------------
--                                                                          --
--                          GNATCHECK COMPONENTS                            --
--                                                                          --
--                    G N A T C H E C K. O P T I O N S                      --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                     Copyright (C) 2004-2017, AdaCore                     --
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

--  This package defines options that are supposed to be of a common interest
--  for all the tools.

with GNAT.OS_Lib;

with ASIS_UL.Debug;

with Gnatcheck.Projects;

package Gnatcheck.Options is

   type Rule_Statuses is
     (Not_A_Rule_Status,
      Under_Construction,
      --  rule is not fully implemented, so it cannot be used
      Non_Documented,
      --  the rule is implemented, but it is not described in GNAT UGN, so it
      --  should not be shown when gnatcheck prints out a list of implemented
      --  rules
      Fully_Implemented);
   --  Describes the current rule implementation status.

   pragma Ordered (Rule_Statuses);

   Generate_Rules_Help : Boolean := False;
   --  '-h'
   --  Generate the rules help information (note, that we can do it only after
   --  registering the rules)

   Generate_Category_Help : Boolean := False;
   --  ''-hcr'
   --  Generate the rule categories help information (note, that we can do it
   --  only after registering the rules). Depending on the flag Recursive_Help,
   --  help information may or may not contain full details for each category.

   Check_Param_Redefinition : Boolean := False;
   --  '--check-redefinition'
   --  Check if for parametrized rule the rule parameter is defined more than
   --  once (may happen if gnatcheck has several rule files as parameters, or
   --  when a rule is activated both in the command line and in the rule file.
   --  Currently the debug option '-dw' also sets this flag ON.

   Recursive_Help : Boolean := False;
   --  '-hcr'

   Generate_Coding_Standard : Boolean := False;
   --  '--write-rules=file' or else '--dump-code-standard'

   type Coding_Standard_Kinds is
     (Not_A_Coding_Standard,
      Template_All_ON,
      Template_All_OFF,
      GNAT,
      HIE);
   --  Possible kind of the coding standard file gnatcheck can generate

   subtype Template_Coding_Standard_Kinds is Coding_Standard_Kinds
     range Template_All_ON .. Template_All_OFF;

   Coding_Standard_Kind : Coding_Standard_Kinds := Not_A_Coding_Standard;
   --  '--dump-code-standard=<kind>'

   Default_Coding_Standard_Kind : Coding_Standard_Kinds := Template_All_OFF;
   --  Used in case if '--write-rules option is set, but '--dump-code-standard'
   --  is not

   Rule_Report_Status : Rule_Statuses := Fully_Implemented;
   --  '-hcr1', '-hcr2'
   --  When generating recursive category help, report rules with the status
   --  equal to or greater than Rule_Report_Status

   Generate_Global_Structure_Warnings : Boolean := False;
   --  ???
   --  Generate warning messages in case if a problem that prevents the
   --  complete analysis of the program global structure is detected

   Active_Rule_Present : Boolean := False;
   --  Flag indicating if the tool has an activated rule to check. It does not
   --  take into account compiler check, use
   --  Gnatcheck.Compiler.Analyze_Compiler_Output to see if any of the compiler
   --  check is active.

   Analyse_Expanded_Code : Boolean := False;
   --  If this flag is ON, gnatcheck analyses expanded spec and expanded body
   --  for generic instantiations

   Analyse_Source_Text : Boolean := False;
   --  If this flag is ON, gnatcheck applies enabled rules from the text rules
   --  hierarchy to the source code of the analyzed compilation unit.

   --------------------------------------
   -- Controlling the gnatcheck report --
   --------------------------------------

   Qualification_Report : Boolean := True;
   --  '--old-report-format'  (turns it OFF)
   --  If this flag is ON, the qualification report file is created instead of
   --  regular one

   Short_Report : Boolean := False;
   --  '-s'
   --  Print the short version of the report file. For new format of the report
   --  file - only diagnoses are included in the report file. For old format -
   --  no header, no lists of enabled and disabled rules, no list of checked
   --  sources, no introductory paragraph for the sections

   Max_Diagnoses : Natural := 500;
   --  '-m'
   --  Maximum number of diagnoses to print out into Stdout. Zero means that
   --  there is no limitation on the number of diagnoses to be printed out into
   --  Stderr.

   Mapping_Mode : Boolean := False;
   --  If this flag is ON, a rule name is added to the text of each diagnosis.

   User_Info_File           : Standard.GNAT.OS_Lib.String_Access;
   User_Info_File_Full_Path : Standard.GNAT.OS_Lib.String_Access;
   --  --include-file=<filename>
   --  Name of the user-provided text file to be added as the last (???)
   --  section of the report file. If this option is not set, this section is
   --  not created in the report file.

   Individual_Rules_Set        : Boolean := False;
   More_Then_One_Rule_File_Set : Boolean := False;
   --  Flags used to detect if all the rules specified for a given gnatcheck
   --  call, should be set when parsing rule options

   Rule_File_Name : Standard.GNAT.OS_Lib.String_Access;
   --  If More_Then_One_Rule_File_Set is OFF and if a rule file has been
   --  processed, keeps the name of this file, otherwise is null.

   ---------------------
   -- Project support --
   ---------------------

   Gnatcheck_Prj : Gnatcheck.Projects.Gnatcheck_Project_Type;

   -----------------------------------------------------------
   --  Options related to the old format of the report file --
   -----------------------------------------------------------

   Print_Exemption_Section : Boolean := False;
   --  '--exemption'
   --  !!! We should get rid of it!!!
   --  If this flag is OFF, the generated report file does not contain the
   --  section for exempted rules diagnosis.

   Output_Section_1 : Boolean := True;
   Output_Section_2 : Boolean := True;
   Output_Section_3 : Boolean := True;
   --  '-sn, n = 1, 2 or 3
   --  Print only specified sections, if at least one '-sn' option is set. Does
   --  not silently impose '-s'.

   Full_Source_Locations : Boolean := False;
   --  '-l'
   --  If this flag is set ON, gnatcheck adds full source locations in the
   --  report file. In case of an entity declared in the expanded generic
   --  code the full location indicates the location of a construct in the
   --  template and then - the location of the corresponding instantiation of
   --  the template (long location chains are used in case of nested
   --  instantiations). Short location shows only the location of the
   --  corresponding construct in the instantiation.

   No_Column_Num_In_Diagnoses : Boolean := False;
   --  '--no-column'
   --  If this flag is ON, the column number is cut off from the diagnostic
   --  messages. Is needed for qualification tests.

   -----------------------
   -- ALI file deletion --
   -----------------------

   Deletion_File_Name : Standard.GNAT.OS_Lib.String_Access;
   --  In --incremental mode, if gnatcheck detects rule violations on a certain
   --  file, we don't want to consider that one "done". That is, next time we
   --  run gnatcheck, we want to reprocess that file, so we see the error
   --  messages again. Therefore, when that happens, we delete the
   --  corresponding .ali file.
   --
   --  The inner invocation detects the rule violation, but it can't delete the
   --  .ali file, because that would confuse the builder. So instead, it
   --  appends the name of the .ali file to the "deletion file". At the end,
   --  the outer invocation deletes all the files listed in the deletion file.
   --
   --  Deletion_File_Name is the name of the deletion file. This is initially
   --  determined by the outer invocation, which creates the file. The name is
   --  then passed to the inner invocations via the --deletion-file=... switch.
   --
   --  Note the similarity to the File_Name_File used by gnatpp.

   ------------------------------------
   -- gnatcheck-specific debug flags --
   ------------------------------------

   Debug_Diagnoses_Storage : Boolean renames ASIS_UL.Debug.Debug_Flag_1;

end Gnatcheck.Options;
