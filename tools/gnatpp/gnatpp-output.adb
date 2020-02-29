------------------------------------------------------------------------------
--                                                                          --
--                            GNATPP COMPONENTS                             --
--                                                                          --
--                        G N A T P P . O U T P U T                         --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                    Copyright (C) 2001-2017, AdaCore                      --
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

with System.WCh_Con; use System.WCh_Con;

with Ada.Text_IO;
with Ada.Wide_Text_IO;          use Ada.Wide_Text_IO;

with GNAT.OS_Lib;               use GNAT.OS_Lib;

with ASIS_UL.Common;
with ASIS_UL.Output;            use ASIS_UL.Output;

with GNATPP.Options;            use GNATPP.Options;

package body GNATPP.Output is

   ----------------
   -- Brief_Help --
   ----------------

   procedure Brief_Help is
   begin
      pragma Style_Checks ("M200"); -- Allow long lines

      Info ("usage: gnatpp [options] {filename} {-files filename} " &
            "[-cargs gcc_switches]");
      Info (" options:");
      Info (" --version - Display version and exit");
      Info (" --help    - Display usage and exit");
      Info ("");
      Info (" -Pproject     - Use project file project. Only one such switch can be used.");
      Info (" -U            - process all sources of the argument project");
      Info (" -U main       - process the closure of units rooted at unit main");
      Info (" -Xname=value  - specify an external reference for argument project file");
      Info (" -eL           - follow all symbolic links when processing project files");

      Info (" other options (in alphabetic order):");

      Info (" -A(0|1) - set alignment");
      Info ("   0 - set alignment OFF");
      Info ("   1 - set alignment ON (set as default)");

      Info (" -a(L|U|M) - set attribute casing");
      Info ("   L - lower case");
      Info ("   U - upper case");
      Info ("   M - mixed case (set as default)");

      Info (" --based-grouping=n  - underscores in based literals every n characters");

      Info (" -c(0|1|3|4|5) - comments layout");
      Info ("   0 - do not format comments");
      Info ("   1 - GNAT style comment line indentation (set as default)");
      Info ("   3 - GNAT style comment beginning");
      Info ("   4 - fill comment blocks");
      Info ("   5 - do not change comments with a special character " &
            "just after --");
      Info (" --comments-only - format just the comments");

      Info (" -clnnn - indentation level for continuation lines, " &
            "nnn from 1 .. 9");

      Info (" -D<file> - set <file> as the dictionary file defining casing " &
            "exceptions");
      Info (" -D-      - do not use RM-defined casing for predefined " &
            "names, use casing ");
      Info ("            defined by -n parameter and dictionary file(s) " &
            "instead");

      Info (" --decimal-grouping=n  - underscores in decimal literals every n characters");

      Info (" -ff - put Form Feed after a pragma Page");
      Info (" -gnatec<path> - the same as GNAT -gnatec option");
      Info (" -innn - indentation level, nnn from 1 .. 9, " &
            "the default value is 3");

      Info (" -I<dir> - the same as gcc -I option");

      Info (" -I-     - the same as gcc -I- option");

      Info (" -k(L|U) - set keyword casing");
      Info ("   L - lower case (set as default)");
      Info ("   U - upper case");

      Info (" -Mnnn - set maximum line length, nnn from 32 .. 256, " &
            "the default value is 79");

      Info (" -n(D|U|L|M) - set name casing (for both defining and usage " &
            "occurrences)");
      Info ("   D - as declared (set as default)");
      Info ("   U - all in upper case");
      Info ("   L - all in lower case");
      Info ("   M - mixed");

      Info (" -ne(D|U|L|M) - set enumeration literal casing (for both defining and usage");
      Info ("                occurrences), parameters have the same meaning as for -n option");
      Info ("                if not set, -n is used to define enumeration literal casing");

      Info (" -nt(D|U|L|M) - set casing for names introduced by type and subtype");
      Info ("                declarations (both defining and usage occurrences), parameters");
      Info ("                have the same meaning as for -n option. If not set, -n is used");

      Info (" -nn(D|U|L|M) - set casing for names introduced by number declarations (both");
      Info ("                (defining and usage occurrences), parameters have the same");
      Info ("                meaning as for -n option. If not set, -n is used");

      Info (" -N - no tabulation in comments");

      Info (" -p(L|U|M) - set pragma casing");
      Info ("   L - lower case");
      Info ("   U - upper case");
      Info ("   M - mixed case (set as default)");

      Info (" --pp-off=xxx - Use ""--xxx"" as the comment string to disable");
      Info ("                pretty printing instead of the default " &
              """--!pp off""");
      Info (" --pp-on=xxx - Use ""--xxx"" as the comment string to reenable");
      Info ("                pretty printing instead of the default " &
              """--!pp on""");

      Info (" --RTS=<dir> - the same as gcc --RTS option");

      Info (" -q  - quiet mode");

      Info (" --no-separate-is        - try not to place 'IS' on a separate " &
            " line in");
      Info ("                           a subprogram body");
      Info (" --separate-loop-then    - use a separate line for LOOP and " &
            "THEN keywords");

      Info (" --no-separate-loop-then - do not use a separate line for LOOP " &
            "and THEN");
      Info ("                           keywords, uncompatible with " &
            "--separate-loop-then");

      Info (" --use-on-new-line       - use separate lines for USE clauses ");
      Info ("                           in a context clause");

      Info (" --insert-blank-lines    - insert blank lines where appropriate");

      Info (" --preserve-blank-lines  - preserve blank lines in the input");

      Info (" --split-line-before-op  - operator on next line");

      Info (" --RM-style-spacing      - no extra space before " &
            "'(' and ':'");

      Info (" --par_threshold=nnn     - if the number of parameter specifications is greater");
      Info ("                           than nnn, each specification starts from a new line");

      Info (" --call_threshold=nnn    - if the number of parameter associations in a call is");
      Info ("                           greater than nnn and there is at least one named");
      Info ("                           association, each association starts from a new line");

      Info (" --incremental -- incremental processing on a per-file basis");
      Info (" -jn - n is the maximal number of processes to carry out");
      Info (" -t  - display execution time");

      Info (" -v  - verbose mode");

      Info (" -dd - progress indicator verbose mode");
      Info ("");

      Info ("Output file control:");
      Info (" -pipe - send the output into Stdout");
      Info (" -o output_file - write the output into output_file. Give up " &
            "if output_file");
      Info ("                  already exists");
      Info (" -of output_file - write the output into output_file, " &
            "overriding the existing ");
      Info ("                   file");
      Info (" --output-dir=dir -- create output files in dir");
      Info (" -r   - replace the argument source with the pretty-printed" &
            " source and copy the");
      Info ("        argument source into filename" & NPP_Suffix &
            ". Give up if filename" &  NPP_Suffix);
      Info ("        already exists");
      Info (" -rf  - replace the argument source with the pretty-printed " &
            "source and copy the");
      Info ("        argument source into filename" & NPP_Suffix &
            ", overriding the existing file");

      Info (" -rnb - replace the argument source with the pretty-printed " &
            "source and do not");
      Info ("        create the back-up copy of the argument source");
      Info ("");

      Info (" filename - the name of the Ada source file to be reformatted. ");
      Info ("            Wildcards are allowed");
      Info (" -files=filename - the name of a text file containing a list");
      Info ("                   of Ada source files to reformat");
      Info (" --ignore=filename - do not process sources listed in filename");
      Info (" --eol=text_format - sets the format of the gnatpp output " &
        "file(s),");
      Info ("                    can not be used together with -pipe option");
      Info ("       text_format can be - 'unix' or 'lf'   - lines end with " &
        "LF character");
      Info ("                          - 'dos'  or 'crlf' - lines end with " &
        "CRLF characters");

      Info (" -W(h|u|s|e|8|b) - sets the wide character encoding of the " &
        "result file");
      Info ("    h - Hex ESC encoding");
      Info ("    u - Upper half encoding");
      Info ("    s - Shift-JIS encoding");
      Info ("    e - EUC Encoding");
      Info ("    8 - UTF-8 encoding");
      Info ("    b - Brackets encoding (this is the default)");
      Info ("");

      Info (" gcc_switches - switches to be passed to gcc called by " &
            ASIS_UL.Common.Tool_Name.all);

      pragma Style_Checks ("M79");
   end Brief_Help;

   ------------------------
   -- Print_Gnatpp_Usage --
   ------------------------

   procedure Print_Gnatpp_Usage is
   begin
      Ada.Text_IO.Set_Error (Ada.Text_IO.Standard_Output);
      Brief_Help;

      New_Line;
      New_Line;
      Put_Line ("Report bugs to report@adacore.com");
   end Print_Gnatpp_Usage;

   ---------------------
   -- Set_Form_String --
   ---------------------

   procedure Set_Form_String is
   begin

      case Output_Encoding is
         when WCEM_Hex =>
            Free (Form_String);
            Form_String := new String'("WCEM=h");
         when WCEM_Upper =>
            Free (Form_String);
            Form_String := new String'("WCEM=u");
         when WCEM_Shift_JIS =>
            Free (Form_String);
            Form_String := new String'("WCEM=s");
         when WCEM_EUC =>
            Free (Form_String);
            Form_String := new String'("WCEM=e");
         when WCEM_UTF8 =>
            Free (Form_String);
            Form_String := new String'("WCEM=8");
         when WCEM_Brackets =>
            Free (Form_String);
            Form_String := new String'("WCEM=b");
         when WCEM_Default =>
            null;
      end case;

   end Set_Form_String;

   --------------
   -- XML_Help --
   --------------

   procedure XML_Help is
   begin
      pragma Style_Checks (Off);

      Info ("<switches lines=""4"">");
      Info ("");
      Info ("   <title line=""1"" ></title>");
      Info ("   <title line=""2"" >Layout</title>");
      Info ("   <title line=""3"" >Output file format</title>");
      Info ("   <title line=""4"" >General</title>");
      Info ("");
      Info ("   <popup label=""Spacing"" line =""1"" lines=""3"">");
      Info ("");
      Info ("      <spin label=""Indentation"" switch=""-i"" min=""1"" max=""9"" default=""3"">");
      Info ("         <tip> number of spaces for each indentation level </tip>");
      Info ("      </spin>");
      Info ("");
      Info ("      <spin label=""Continuation lines"" switch=""-cl"" min=""1"" max=""9"" default=""2""");
      Info ("            tip=""indentation level for a continuation line"" />");
      Info ("      <spin label=""Maximum line length"" switch=""-M"" min=""20"" max=""256""");
      Info ("            default=""79"" />");
      Info ("   </popup>");
      Info ("");
      Info ("   <popup label=""Casing"" line=""1"" lines=""6"">");
      Info ("      <combo label=""Name"" switch=""-n"" line=""1"" noswitch=""D""");
      Info ("             tip=""identifier casing"" >");
      Info ("         <combo-entry label=""As declared"" value=""D"" />");
      Info ("         <combo-entry label=""Lower case"" value=""L"" />");
      Info ("         <combo-entry label=""Upper case"" value=""U"" />");
      Info ("         <combo-entry label=""Mixed case"" value=""M"" />");
      Info ("      </combo>");
      Info ("");
      Info ("      <combo label=""Enumeration literal"" switch=""-ne"" line=""1"" noswitch=""D"">");
      Info ("         <combo-entry label=""As declared"" value=""D"" />");
      Info ("         <combo-entry label=""Lower case"" value=""L"" />");
      Info ("         <combo-entry label=""Upper case"" value=""U"" />");
      Info ("         <combo-entry label=""Mixed case"" value=""M"" />");
      Info ("      </combo>");
      Info ("");
      Info ("      <field label=""Keep casing"" line=""2"" switch=""-D"" as-file=""true""");
      Info ("             tip=""specifies the dictionary with the names to keep casing unchanged"" />");
      Info ("");
      Info ("      <check label=""Do not use default dictionaries"" switch=""-D-"" line=""3""");
      Info ("             tip=""Do not keep casing defined for Ada predefined names and names from GNAT library"" />");
      Info ("");
      Info ("      <combo label=""Keyword"" switch=""-k"" line=""4"" noswitch=""L"" nodigit=""L"" >");
      Info ("         <combo-entry label=""Lower case"" value=""L"" />");
      Info ("         <combo-entry label=""Upper case"" value=""U"" />");
      Info ("      </combo>");
      Info ("");
      Info ("      <combo label=""Attribute"" switch=""-a"" line=""5"" noswitch=""M"" nodigit=""M"" >");
      Info ("         <combo-entry label=""Mixed case"" value=""M"" />");
      Info ("         <combo-entry label=""Lower case"" value=""L"" />");
      Info ("         <combo-entry label=""Upper case"" value=""U"" />");
      Info ("      </combo>");
      Info ("      <combo label=""Pragma"" switch=""-p"" line=""6"" noswitch=""M"" nodigit=""M"" >");
      Info ("         <combo-entry label=""Mixed case"" value=""M"" />");
      Info ("         <combo-entry label=""Lower case"" value=""L"" />");
      Info ("         <combo-entry label=""Upper case"" value=""U"" />");
      Info ("      </combo>");
      Info ("");
      Info ("   </popup>");
      Info ("");
      Info ("   <popup label=""Alignment"" line =""1"" lines=""6"">");
      Info ("");
      Info ("          <check label=""Default for all alignments OFF""");
      Info ("                 tip=""all the alignments are off except of those that are selected explicitly""");
      Info ("                 switch=""-A0"" line=""1"" />");
      Info ("          <check label=""Colons in declarations"" switch=""-A1"" line=""2"" />");
      Info ("          <check label=""Assignments in declarations"" switch=""-A2"" line=""3"" />");
      Info ("          <check label=""Assignments in statements"" switch=""-A3"" line=""4"" />");
      Info ("          <check label=""Arrow delimiters in associations"" switch=""-A4"" line=""5"" />");
      Info ("          <check label=""AT keywords in component clauses"" switch=""-A5"" line=""6"" />");
      Info ("   </popup>");
      Info ("");
      Info ("   <combo label=""Construct"" switch=""-l"" line=""2"" noswitch=""1"" nodigit=""1"" >");
      Info ("      <combo-entry label=""GNAT style"" value=""1"" />");
      Info ("      <combo-entry label=""Compact"" value=""2"" />");
      Info ("      <combo-entry label=""Uncompact"" value=""3"" />");
      Info ("   </combo>");
      Info ("   <combo label=""Comment"" switch=""-c"" line=""2"" noswitch=""1"" nodigit=""1"" >");
      Info ("      <combo-entry label=""Unchanged"" value=""0"" />");
      Info ("      <combo-entry label=""GNAT style line indentation"" value=""1"" />");
      Info ("      <combo-entry label=""Standard line indentation"" value=""2"" />");
      Info ("   </combo>");
      Info ("   <check label=""GNAT style beginning"" switch=""-c3"" line=""2"" />");
      Info ("   <check label=""Reformat blocks"" switch=""-c4"" line=""2"" />");
      Info ("   <check label=""Leave special comments unchanged"" switch=""-c5"" line=""2"" />");
      Info ("   <check label=""No TAB character in comments"" switch=""-N"" line=""2"" />");
      Info ("");
      Info ("   <combo label=""Result file format"" switch=""--eol="" line=""3"" noswitch=""default"">");
      Info ("      <combo-entry label=""default"" value=""default"" />");
      Info ("      <combo-entry label=""UNIX"" value=""unix"" />");
      Info ("      <combo-entry label=""DOS"" value=""dos"" />");
      Info ("   </combo>");
      Info ("");
      Info ("   <combo label=""Wide character encoding"" switch=""-W"" line=""3"" noswitch=""b"" nodigit=""b"">");
      Info ("      <combo-entry label=""Hex ESC encoding"" value=""h"" />");
      Info ("      <combo-entry label=""Upper half encoding"" value=""u"" />");
      Info ("      <combo-entry label=""Shift-JIS encoding"" value=""s"" />");
      Info ("      <combo-entry label=""EUC Encoding"" value=""e"" />");
      Info ("      <combo-entry label=""UTF-8 encoding"" value=""8"" />");
      Info ("      <combo-entry label=""Brackets encoding"" value=""b"" />");
      Info ("   </combo>");
      Info ("");
      Info ("   <check label=""Do not set missing end/exit labels"" switch=""-e"" line=""4"" />");
      Info ("   <check label=""Put Form Feed after a pragma Page"" switch=""-ff"" line=""4"" />");
      Info ("   <check label=""Do not place IS on a separate  line"" switch=""--no-separate-is"" line=""4"" />");
      Info ("   <check label=""Use separate line for label(s)"" switch=""--separate-label"" line=""4"" />");
      Info ("   <check label=""Use separate line for LOOP and THEN"" switch=""--separate-loop-then"" line=""4"" />");
      Info ("   <check label=""Do not use separate line for LOOP and THEN"" switch=""--no-separate-loop-then"" line=""4"" />");
      Info ("   <check label=""Use separate lines for USE clauses""");
      Info ("          tip=""Use separate lines for USE clauses in a context clause""");
      Info ("          switch=""--use-on-new-line"" line=""4"" />");
      Info ("   <check label=""Use separate line for statement name""");
      Info ("          tip=""Use separate line for statement name with no extra indentation for statement itself""");
      Info ("          switch=""--separate-stmt-name"" line=""4"" />");
      Info ("</switches>");

      pragma Style_Checks (On);
   end XML_Help;

end GNATPP.Output;
