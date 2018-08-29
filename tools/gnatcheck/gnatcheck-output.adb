------------------------------------------------------------------------------
--                                                                          --
--                          GNATCHECK COMPONENTS                            --
--                                                                          --
--                     G N A T C H E C K . O U T P U T                      --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                     Copyright (C) 2004-2017, AdaCore                     --
--                                                                          --
-- GNATCHECK  is  free  software;  you can redistribute it and/or modify it --
-- under terms of the  GNU  General Public License as published by the Free --
-- Software Foundation;  either version 2, or ( at your option)  any  later --
-- version.  GNATCHECK  is  distributed in the hope that it will be useful, --
-- but  WITHOUT  ANY  WARRANTY;   without  even  the  implied  warranty  of --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General --
-- Public License for more details.  You should have received a copy of the --
-- GNU  General Public License distributed with GNAT; see file  COPYING. If --
-- not,  write to the  Free Software Foundation,  51 Franklin Street, Fifth --
-- Floor, Boston, MA 02110-1301, USA.                                       --
--                                                                          --
-- GNATCHECK is maintained by AdaCore (http://www.adacore.com).             --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Text_IO;                use Ada.Text_IO;

with GNAT.OS_Lib;                use GNAT.OS_Lib;

with ASIS_UL.Common;             use ASIS_UL.Common;
with ASIS_UL.Output;             use ASIS_UL.Output;

with Gnatcheck.Ids;              use Gnatcheck.Ids;
with Gnatcheck.Options;          use Gnatcheck.Options;
with Gnatcheck.Rules;            use Gnatcheck.Rules;
with Gnatcheck.Rules.Rule_Table; use Gnatcheck.Rules.Rule_Table;

package body Gnatcheck.Output is

   Coding_Standard_File_Name : String_Access := new String'("ada.rules");

   Coding_Standard_File : File_Type;
   --  The file to place the sample coding standard into

   procedure Write_GNAT_Coding_Standard;
   procedure Write_HIE_Coding_Standard;
   --  Generates the corresponding coding standards.

   ----------------
   -- Brief_Help --
   ----------------

   procedure Brief_Help is
   begin
      pragma Style_Checks ("M200"); -- Allow long lines

      Info ("gnatcheck: the GNAT rule checking tool");
      Info ("usage: gnatcheck [options] {filename} {-files=filename} [-cargs gcc_switches] -rules rule_switches");
      Info ("options:");
      Info (" --version - Display version and exit");
      Info (" --help    - Display usage and exit");
      Info ("");
      Info (" -Pproject        - Use project file project. Only one such switch can be used");
      Info (" -U               - check all sources of the argument project");
      Info (" -U main          - check the closure of units rooted at unit main");
      Info (" -Xname=value     - specify an external reference for argument project file");
      Info (" --subdirs=dir    - specify subdirectory to place the result files into");
      Info (" --no_objects_dir - place results into current dir instead of project dir");
      Info (" -eL              - follow all symbolic links when processing project files");
      Info ("");
      Info (" -a   - process RTL units");
      Info (" -h   - print out the list of the currently implemented rules");
      Info (" -mn  - n is the maximal number of diagnoses in Stdout");

      Info ("        (n in 0 .. 1000, 0 means no limit)");
      Info (" --incremental -- incremental processing on a per-file basis");
      Info (" -jn  - n is the maximal number of processes");
      Info (" -q   - quiet mode (do not report detections in Stdout)");
      Info (" -t   - report execution time in Stderr");
      Info (" -v   - verbose mode");
      Info (" -log - duplicate all the messages sent to Stderr in log file");
      Info ("        " & Tool_Name.all & ".log");
      Info (" -s   - short form of the report file");
      Info (" -xml - generate report in XML format");
      Info (" -nt  - do not generate text report (enforces '-xml')");
      Info ("");

      Info (" --show-rule - apend rule names to diagnoses generated");
      Info ("");

      Info (" --check-redefinition - issue warning if a rule parameter is defined");
      Info ("                        more than once");
      Info ("");

      Info (" --include-file=filename - add the content of filename into generated report");

      Info ("");

      Info (" -o filename   - specify the name of the next report file");
      Info (" -ox filename  - specify the name of the XML report file (enforces '-xml')");
      Info ("");

      Info ("filename                 - the name of the Ada source file to be analyzed.");
      Info ("                           Wildcards are allowed");
      Info ("-files=filename          - the name of the text file containing a list of Ada");
      Info ("                           source files to analyze");
      Info ("--ignore=filename        - do not process sources listed in filename");
      Info ("--write-rules=filename   - the name of a template rule file");
      Info ("");

      Info ("gcc_switches             - switches to be passed to gcc called by " & ASIS_UL.Common.Tool_Name.all);
      Info ("");

      Info ("rule_switches          - a list of the following switches");
      Info ("   -from=filename      - read rule options from filename");
      Info ("   +R<rule_id>[:param] - turn ON a given rule [with given parameter]");
      Info ("   -R<rule_id>         - turn OFF a given rule");
      Info ("   -R<rule_id>:param   - turn OFF some of the checks for a given  rule,");
      Info ("                         depending on the specified parameter");
      Info ("where <rule_id> - ID of one of the currently implemented");
      Info ("                  rules, use '-h' for the full list");
      Info ("      param     - string representing parameter(s) of a given rule, more than ");
      Info ("                  one parameter can be set separated by ','");

      pragma Style_Checks ("M79");
   end Brief_Help;

   ---------------------------
   -- Print_Gnatcheck_Usage --
   ---------------------------

   procedure Print_Gnatcheck_Usage is
   begin
      Set_Error (Standard_Output);
      Brief_Help;

      New_Line;
      New_Line;
      Put_Line ("Report bugs to report@adacore.com");
   end Print_Gnatcheck_Usage;

   -----------------------------------
   -- Set_Coding_Standard_File_Name --
   -----------------------------------

   procedure Set_Coding_Standard_File_Name (Fname : String) is
   begin
      Free (Coding_Standard_File_Name);
      Coding_Standard_File_Name := new String'(Fname);
   end Set_Coding_Standard_File_Name;

   ---------------------------
   -- Write_Coding_Standard --
   ---------------------------

   procedure Write_Coding_Standard is
      Max_Len : Positive := 1;
      Tmp_Max : Positive;
      --  Maximal length of the rule option

   begin
      if Is_Regular_File (Coding_Standard_File_Name.all) then
         Open (Coding_Standard_File, Out_File, Coding_Standard_File_Name.all);
      else
         Create
           (Coding_Standard_File, Out_File, Coding_Standard_File_Name.all);
      end if;

      if Coding_Standard_Kind = Not_A_Coding_Standard then
         Coding_Standard_Kind := Default_Coding_Standard_Kind;
      end if;

      case Coding_Standard_Kind is
         when Template_Coding_Standard_Kinds =>
            --  Compute the maximal length of the rule option:
            for J in First_Rule .. All_Rules.Last loop
               if All_Rules.Table (J).Rule_Status = Fully_Implemented then
                  Tmp_Max :=
                    Rule_Option (All_Rules.Table (J).all,
                                 Coding_Standard_Kind)'Length;

                  if Tmp_Max > Max_Len then
                     Max_Len := Tmp_Max;
                  end if;

               end if;
            end loop;

            Max_Len := Max_Len + 2;

            Put
              (Coding_Standard_File,
               "--  Sample coding standard file, with all available rules ");

            if Coding_Standard_Kind = Template_All_ON then
               Put_Line
                 (Coding_Standard_File,
                  "enabled");
            else
               Put_Line
                 (Coding_Standard_File,
                  "disabled");
            end if;

            Put_Line
              (Coding_Standard_File,
               "--  Should be reviewed and corrected manually");
            New_Line (Coding_Standard_File);

            for J in First_Rule .. All_Rules.Last loop
               if All_Rules.Table (J).Rule_Status = Fully_Implemented then
                  Sample_Image
                    (All_Rules.Table (J).all,
                     Coding_Standard_Kind,
                     Coding_Standard_File,
                     Max_Len);
               end if;
            end loop;

            New_Line (Coding_Standard_File);

            Put_Line
              (Coding_Standard_File,
               "----------------------------");
            Put_Line
              (Coding_Standard_File,
               "--  Compiler-based checks --");
            Put_Line
              (Coding_Standard_File,
               "----------------------------");

            if Coding_Standard_Kind = Template_All_ON then
               Put_Line
                 (Coding_Standard_File,
                  "+R Warnings     : a  --  activate all optional warnings");

               Put_Line
                 (Coding_Standard_File,
                  "+R Style_Checks : y  --  activate all the style checks");

               Put_Line
                 (Coding_Standard_File,
                  "+R Restrictions : No_Obsolescent_Features ");
            else
               Put_Line
                 (Coding_Standard_File,
                  "+R Warnings:s                              --  disable " &
                  "all warnings");

               Put_Line
                 (Coding_Standard_File,
                  "+R Style_Checks:N                          --  disable " &
                  "all style checks");

               Put_Line
                 (Coding_Standard_File,
                  "-R Restrictions : No_Obsolescent_Features  --  disable " &
                  "specific restriction");
            end if;
         when Not_A_Coding_Standard =>
            pragma Assert (False);
            null;
         when Gnatcheck.Options.GNAT =>
            Write_GNAT_Coding_Standard;
         when HIE =>
            Put_Line
              (Coding_Standard_File,
               "--  Generation of this coding standard " &
               "is not implemented yet");

            Write_HIE_Coding_Standard;
      end case;

      Close (Coding_Standard_File);
   exception
      when Status_Error =>
         Error ("can not open the coding standard file, " &
                "the file may be in use");
         raise Fatal_Error;
      when others =>
         Error ("can not open the coding standard file, check the file name");
         raise Fatal_Error;
   end Write_Coding_Standard;

   --------------------------------
   -- Write_GNAT_Coding_Standard --
   --------------------------------

   procedure Write_GNAT_Coding_Standard is
   begin
      Put_Line
        (Coding_Standard_File,
         "--  Generally recommended coding standard --");
      New_Line (Coding_Standard_File);

      Put_Line
        (Coding_Standard_File,
         "+R Anonymous_Arrays");
      Put_Line
        (Coding_Standard_File,
         "+R Boolean_Relational_Operators");
      Put_Line
        (Coding_Standard_File,
         "+R Enumeration_Ranges_In_CASE_Statements");
      Put_Line
        (Coding_Standard_File,
         "+R Exceptions_As_Control_Flow");
      Put_Line
        (Coding_Standard_File,
         "+R Expanded_Loop_Exit_Names");
      Put_Line
        (Coding_Standard_File,
         "+R Explicit_Full_Discrete_Ranges");
      Put_Line
        (Coding_Standard_File,
         "+R Float_Equality_Checks");
      Put_Line
        (Coding_Standard_File,
         "+R Function_Style_Procedures");
      Put_Line
        (Coding_Standard_File,
         "+R Generics_In_Subprograms");
      Put_Line
        (Coding_Standard_File,
         "+R Non_Short_Circuit_Operators");
      Put_Line
        (Coding_Standard_File,
         "+R Non_Visible_Exceptions");
      Put_Line
        (Coding_Standard_File,
         "+R Separate_Numeric_Error_Handlers");
      Put_Line
        (Coding_Standard_File,
         "+R Unassigned_OUT_Parameters");
   end Write_GNAT_Coding_Standard;

   -------------------------------
   -- Write_HIE_Coding_Standard --
   -------------------------------

   procedure Write_HIE_Coding_Standard is
   begin
      null;
   end Write_HIE_Coding_Standard;

end Gnatcheck.Output;
