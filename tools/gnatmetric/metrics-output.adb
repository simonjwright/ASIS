------------------------------------------------------------------------------
--                                                                          --
--                      GNAT METRICS TOOLS COMPONENTS                       --
--                                                                          --
--                       M E T R I C S . O U T P U T                        --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                    Copyright (C) 2003-2017, AdaCore                      --
--                                                                          --
-- GNATMETRIC  is  free software; you can  redistribute it and/or modify it --
-- under terms of the  GNU  General Public License as published by the Free --
-- Software  Foundation;  either version 3, or (at your option)  any  later --
-- version. GNATMETRIC  is  distributed in the hope that it will be useful, --
-- but  WITHOUT  ANY  WARRANTY;   without  even  the  implied  warranty  of --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General --
-- Public License for more details.  You should have received a copy of the --
-- GNU General Public License distributed with GNAT; see file  COPYING3. If --
-- not,  go  to  http://www.gnu.org/licenses  for  a  complete  copy of the --
-- license.                                                                 --
--                                                                          --
-- GNAT Metrics Toolset is maintained by AdaCore (http://www.adacore.com).  --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Characters.Handling;     use Ada.Characters.Handling;
with Ada.Strings.Fixed;           use Ada.Strings.Fixed;
with Ada.Text_IO;                 use Ada.Text_IO;

with GNAT.Directory_Operations;   use GNAT.Directory_Operations;
with GNAT.OS_Lib;                 use GNAT.OS_Lib;

with Asis.Compilation_Units;      use Asis.Compilation_Units;
with Asis.Declarations;           use Asis.Declarations;
with Asis.Elements;               use Asis.Elements;
with Asis.Extensions.Flat_Kinds;  use Asis.Extensions.Flat_Kinds;
with Asis.Text;                   use Asis.Text;

with ASIS_UL.Common;              use ASIS_UL.Common;
with ASIS_UL.Debug;               use ASIS_UL.Debug;
with ASIS_UL.Environment;
with ASIS_UL.Options;
with ASIS_UL.Output;              use ASIS_UL.Output;

with METRICS.ASIS_Utilities;      use METRICS.ASIS_Utilities;
with METRICS.Common;              use METRICS.Common;
with METRICS.Coupling;            use METRICS.Coupling;
with METRICS.Metric_Definitions;  use METRICS.Metric_Definitions;
with METRICS.Options;             use METRICS.Options;
with METRICS.Source_Table;        use METRICS.Source_Table;

package body METRICS.Output is

   -----------------------
   -- Local subprograms --
   -----------------------

   procedure Report_No_EOL (Message : String; Depth : Natural := 0);
   --  Send Message into output file. The message is not appended with
   --  platform-specific EOL. Depth is used to send the indentation if needed.
   --  This procedure should not be used for generating XML output

   procedure Set_Global_Metrics_Output;
   --  Creates or opens, if needed (depending on gnatmetric options), and sets
   --  the output file for outputting the global metrics results
   --
   --  Does nothing for XML output

   ----------------
   -- Brief_Help --
   ----------------

   procedure Brief_Help is
   begin
      pragma Style_Checks ("M200"); -- Allow long lines

      Info ("usage: gnatmetric [options] {filename} {-files filename} [-cargs gcc_switches]");
      Info (" options:");
      Info (" --version - Display version and exit");
      Info (" --help    - Display usage and exit");
      Info ("");
      Info (" -Pproject         - Use project file project. Only one such switch can be used");
      Info (" -U                - process all sources of the argument project");
      Info (" -U main           - process the closure of units rooted at unit main");
      Info (" -Xname=value      - specify an external reference for argument project file");
      Info (" --subdirs=dir     - specify subdirectory to place the result files into");
      Info (" --no_objects_dir  - place results into current dir instead of project dir");
      Info (" -eL               - follow all symbolic links when processing project files");
      Info (" --ignore=filename - do not process sources listed in filename");

      Info ("");
      Info (" -a    - process sources from RTL");
      if False then -- Disable this for now
         Info (" --incremental -- incremental processing on a per-file basis");
      end if;
      Info (" -jn   - n is the maximal number of processes");
      Info (" -v    - verbose mode");
      Info (" -q    - quiet mode");
      Info ("");

      Info (" -nolocal - do not compute detailed metrics for local program units");
      Info ("");

      Info ("Options to control metrics to compute. An option --<metric> turns computing");
      Info ("the metric ON, the corresponding --no-<metric> option turns computing the");
      Info ("metric OFF. If no metric option is specified, all the metrics are computed");
      Info ("and reported. If at least one positive option is  specified, only explicitly");
      Info ("selected metrics are computed.");
      Info ("");

      Info ("Complexity metrics:");
      Info ("  --complexity-all        - all complexity metrics");
      Info ("  --complexity-cyclomatic - McCabe Cyclomatic Complexity");
      Info ("  --complexity-essential  - Essential Complexity");
      Info ("  --complexity-average    - average McCabe Cyclomatic Complexity of a body");
      Info ("  --loop-nesting          - maximal loop nesting level");
      Info ("  --no-static-loop        - do not count static loops for cyclomatic complexity");
      Info ("  -ne                     - do not consider exit statements as gotos when");
      Info ("                            computing Essential Complexity");
      Info ("  --extra-exit-points     - extra exit points in subprograms");
      Info ("");

      Info ("Line metrics:");
      Info ("  --lines-all         - all line metrics");
      Info ("  --lines             - number of all lines");
      Info ("  --lines-code        - number of code lines");
      Info ("  --lines-comment     - number of comment lines");
      Info ("  --lines-eol-comment - number of code lines also containing comments");
      Info ("  --lines-ratio       - comment/code lines percentage");
      Info ("  --lines-blank       - number of blank lines");
      Info ("  --lines-average     - average number of code lines in a body");
      Info ("");

      Info (" Syntax element metrics:");
      Info ("  --syntax-all         - all syntax element metrics");
      Info ("  --declarations       - total number of declarations");
      Info ("  --statements         - total number of statements");
      Info ("  --public-subprograms - number of public subprograms in a compilation unit");
      Info ("  --all-subprograms    - number of subprograms in a compilation unit");
      Info ("  --public-types       - number of public types in a compilation unit");
      Info ("  --all-types          - number of types in a compilation unit");
      Info ("  --unit-nesting       - maximal unit nesting level");
      Info ("  --construct-nesting  - maximal construct nesting level");
      Info ("  --param-number       - number of subprogram parameters");
      Info ("");

      Info (" Coupling metrics. By default they are disabled, options below enable all or");
      Info (" specific coupling metrics, there is no  option to disable coupling metrics");
      Info ("  --coupling-all           - all coupling metrics");
      Info ("  --tagged-coupling-out    - tagged (class) fan-out coupling");
      Info ("  --tagged-coupling-in     - tagged (class) fan-in coupling");
      Info ("  --hierarchy-coupling-out - hierarchy (category) fan-out coupling");
      Info ("  --hierarchy-coupling-in  - hierarchy (category) fan-in coupling");
      Info ("  --unit-coupling-out      - unit fan-out coupling");
      Info ("  --unit-coupling-in       - unit fan-in coupling");
      Info ("  --control-coupling-out   - control fan-out coupling");
      Info ("  --control-coupling-in    - control fan-in coupling");
      Info ("");

      Info (" output file control:");
      Info ("  -d=dirname     - put files with detailed metrics into 'dirname'");
      Info ("  -x             - generate XML output");
      Info ("  -xs            - generate XML output and corresponding schema file");
      Info ("  -nt            - do not generate output in text form, implies '-x'");
      Info ("  -o file-suffix - suffix for the file to put detailed metrics for");
      Info ("                   a source file into (file suffix should follow OS");
      Info ("                   file name conventions and contain '.' or '$' character)");
      Info ("  -og filename   - name of the file to put global metrics info into");
      Info ("                   (if not set, this info is sent to Stdout),");
      Info ("                   ignored if -nt is used");
      Info ("  -ox filename   - name of the file to put XML output into, implies '-x'");
      Info ("  -sfn           - use short source file name in output");
      Info ("");
      Info (" filename        - name of Ada source file for which metrics");
      Info ("                   should be computed (wildcards are allowed)");
      Info (" -files filename - name of the text file containing a list of Ada");
      Info ("                   source files for which metrics should be computed");

      Info (" gcc_switches    - switches to be passed to gcc called by " &
            ASIS_UL.Common.Tool_Name.all);

      pragma Style_Checks ("M79");
   end Brief_Help;

   ---------------
   -- Close_Tag --
   ---------------

   procedure Close_Tag (Tag_Name : String; Depth : Natural := 0) is
   begin
      Report_XML ("</" & Tag_Name & '>', Depth => Depth);
   end Close_Tag;

   ---------------------
   -- Generate_Header --
   ---------------------

   procedure Generate_Header (SF : SF_Id; CU : Compilation_Unit) is
   begin

      if Generate_Text_Output then
         Report ("Metrics computed for " &  Source_Name_For_Output (SF));
         Report ("containing " & CU_Profile (CU));
      end if;

      if Generate_XML_Output then
         Report_XML
           ("<file name=" & '"' &
             Source_Name_For_Output (SF) & """>", Depth => 1);
      end if;

   end Generate_Header;

   --------------------------
   -- Generate_Line_Output --
   --------------------------

   procedure Generate_Line_Output (SF : SF_Id) is
      Tmp : Metric_Count;
   begin

      if Line_Metrics_Set then
         Report ("");
         Report ("=== Code line metrics ===");
      else
         return;
      end if;

      if Source_Status (SF) = Error_Detected then
         Report ("WARNING! errors are detected when processing the file");
         Report ("         COMPUTED LINE METRIC VALUES ARE NOT SAFE");

      else
         Global_Statistics.Computed_Line_Metrics :=
           Global_Statistics.Computed_Line_Metrics + 1;
      end if;

      if Compute_All_Lines then
         Report_No_EOL ("  all lines           :");
         Tmp := Get_All_Lines (SF);

         if Computed (Tmp) then
            Report (Tmp'Img);

            Output_XML_Metric ("all_lines", Tmp, Depth => 2);

            Global_Statistics.Line_Metrics.All_Lines :=
              Global_Statistics.Line_Metrics.All_Lines + Tmp;
         else
            Report (" unknown");
         end if;

      end if;

      if Compute_Code_Lines
        or else
         Compute_Comment_Code_Ratio
      then

         Tmp := Get_Code_Lines (SF);

         Global_Statistics.Line_Metrics.Code_Lines :=
           Global_Statistics.Line_Metrics.Code_Lines + Tmp;

         if Compute_Code_Lines then
            Report_No_EOL ("  code lines          :");
            Report (Tmp'Img);

            Output_XML_Metric ("code_lines", Tmp, Depth => 2);
         end if;

      end if;

      if Compute_Comment_Lines
        or else
         Compute_Comment_Code_Ratio
      then
         Tmp := Get_Comment_Lines (SF);

         Global_Statistics.Line_Metrics.Comment_Lines :=
           Global_Statistics.Line_Metrics.Comment_Lines + Tmp;

         if Compute_Comment_Lines then
            Report_No_EOL ("  comment lines       :");
            Report (Tmp'Img);

            Output_XML_Metric ("comment_lines", Tmp, Depth => 2);
         end if;

      end if;

      if Compute_EOL_Comments
        or else
         Compute_Comment_Code_Ratio
      then
         Tmp := Get_EOL_Comments (SF);

         Global_Statistics.Line_Metrics.EOL_Comments :=
           Global_Statistics.Line_Metrics.EOL_Comments + Tmp;

         if Compute_EOL_Comments then
            Report_No_EOL ("  end-of-line comments:");
            Report (Tmp'Img);

            Output_XML_Metric ("eol_comments", Tmp, Depth => 2);
         end if;

      end if;

      if Compute_Comment_Code_Ratio then
         Report_No_EOL ("  comment percentage  :");

         if Computed (Get_EOL_Comments (SF))
           and then
            Computed (Get_Comment_Lines (SF))
           and then
            Computed (Get_Code_Lines (SF))
         then

            Comment_Code_Ratio :=
                (Float (Get_Comment_Lines (SF)) +
                   Float (Get_EOL_Comments (SF)))
              /
                (Float (Get_Comment_Lines (SF)) +
                   Float (Get_Code_Lines (SF)))
              *
                 100.0;

            Comment_Code_Ratio_To_Print :=
              Comment_Code_Percentage (Comment_Code_Ratio);

            Report (Comment_Code_Ratio_To_Print'Img);

            Output_XML_Metric
              ("comment_percentage",
               Comment_Code_Ratio_To_Print'Img,
               Depth => 2);

         else
            Report (" unknown");
         end if;

      end if;

      if Compute_Blank_Lines then
         Report_No_EOL ("  blank lines         :");
         Tmp := Get_Blank_Lines (SF);

         if Computed (Tmp) then
            Report (Tmp'Img);

            Output_XML_Metric ("blank_lines", Tmp, Depth => 2);

            Global_Statistics.Line_Metrics.Blank_Lines :=
              Global_Statistics.Line_Metrics.Blank_Lines + Tmp;
         else
            Report (" unknown");
         end if;

      end if;

   end Generate_Line_Output;

   --------------
   -- Open_Tag --
   --------------

   procedure Open_Tag (Tag_Name : String) is
   begin
      --  ??? Indentation!!!
      Report_XML ('<' & Tag_Name & '>');
   end Open_Tag;

   -----------------------
   -- Output_XML_Metric --
   -----------------------

   procedure Output_XML_Metric
     (Metric : String;
      Value  : Metric_Count;
      Depth  : Natural := 0)
   is
      Value_Image : constant String := Value'Img;
   begin
      Report_XML
        ("<metric name=" &
         '"' & Metric & '"' &
         '>' & Value_Image (2 .. Value_Image'Last) & "</metric>",
         Depth => Depth);
   end Output_XML_Metric;

   procedure Output_XML_Metric
     (Metric : String;
      Value  : String;
      Depth  : Natural := 0)
   is
   begin
      Report_XML
        ("<metric name=" &
         '"' & Metric & '"' &
         '>' & Trim (Value, Ada.Strings.Both) & "</metric>",
         Depth => Depth);
   end Output_XML_Metric;

   ----------------------------
   -- Print_Gnatmetric_Usage --
   ----------------------------

   procedure Print_Gnatmetric_Usage is
   begin
      Set_Error (Standard_Output);
      Brief_Help;

      New_Line;
      New_Line;
      Put_Line ("Report bugs to report@adacore.com");
   end Print_Gnatmetric_Usage;

   ------------
   -- Report --
   ------------

   procedure Report
     (Message : String;
      Depth   :   Natural := 0)
   is
      Indent : constant String := Depth * Indent_String;
   begin

      if Generate_Text_Output then
         Put      (Indent);
         Put_Line (Message);
      end if;

   end Report;

   ----------------
   -- Report_XML --
   ----------------

   procedure Report_XML
     (Message : String;
      Depth   :   Natural := 0)
   is
      Indent : constant String := Depth * Indent_String;
   begin

      if Generate_XML_Output then
         Put      (XML_Out_File, Indent);
         Put_Line (XML_Out_File, Message);
      end if;

   end Report_XML;

   ------------------------------
   -- Report_Global_Statistics --
   ------------------------------

   procedure Report_Global_Statistics is
      LSLOC : Metric_Count := 0;
      --  Used to compute the logical SLOC (all declarations + all statement)

      ITD : Public_Types_Details
        renames Global_Statistics.Public_Types_Detailed;
   begin
      --  In incremental mode, we shouldn't print anything, because it's not a
      --  complete "summary".

      if ASIS_UL.Options.Incremental_Mode or ASIS_UL.Options.Mimic_gcc then
         return;
      end if;

      if Compute_Comment_Code_Ratio then

         if Global_Statistics.Computed_Line_Metrics > 0 then
            Comment_Code_Ratio :=
                (Float (Global_Statistics.Line_Metrics.Comment_Lines) +
                   Float (Global_Statistics.Line_Metrics.EOL_Comments))
              /
                (Float (Global_Statistics.Line_Metrics.Comment_Lines) +
                   Float (Global_Statistics.Line_Metrics.Code_Lines))
              *
                 100.0;
         end if;

         Comment_Code_Ratio_To_Print :=
           Comment_Code_Percentage (Comment_Code_Ratio);
      end if;

      if Compute_Average_Lines_In_Bodies then

         if Num_Of_Processes_Bodies > 0 then
            Average_Lines_In_Process_Bodies :=
              Float (Lines_In_Process_Bodies) /
                Float (Num_Of_Processes_Bodies);
         else
            Average_Lines_In_Process_Bodies := 0.0;
         end if;

         Average_Lines_In_Process_Bodies_To_Print :=
           Real_Val_To_Print (Average_Lines_In_Process_Bodies);
      end if;

      if Compute_Average_Complexity
        and then
         Units_Compute_Average_Complexity_For >  0
      then
         Average_Cyclomatic_Complexity :=
           Float (Total_Cyclomatic_Complexity) /
           Float (Units_Compute_Average_Complexity_For);

         Average_Cyclomatic_Complexity_To_Print :=
          Real_Val_To_Print (Average_Cyclomatic_Complexity);
      end if;

      if Coupling_Metrics_Set then

         if Print_Coupling_Unit_Table then
            Print_Unit_Table;
         end if;

         Compute_Coupling_Metrics;
      end if;

      if Generate_Text_Output then

         Set_Global_Metrics_Output;

         if Line_Metrics_Set then

            Report ("Line metrics summed over" &
                    Global_Statistics.Computed_Line_Metrics'Img & " units");

            if Compute_All_Lines then
               Report ("  all lines            :" &
                       Global_Statistics.Line_Metrics.All_Lines'Img);
            end if;

            if Compute_Code_Lines then
               Report ("  code lines           :" &
                       Global_Statistics.Line_Metrics.Code_Lines'Img);
            end if;

            if Compute_Comment_Lines then
               Report ("  comment lines        :" &
                       Global_Statistics.Line_Metrics.Comment_Lines'Img);
            end if;

            if Compute_EOL_Comments then
               Report ("  end-of-line comments :" &
                       Global_Statistics.Line_Metrics.EOL_Comments'Img);
            end if;

            if Compute_Comment_Code_Ratio then
               Report ("  comment percentage   :" &
                       Comment_Code_Ratio_To_Print'Img);
            end if;

            if Compute_Blank_Lines then
               Report ("  blank lines          :" &
                       Global_Statistics.Line_Metrics.Blank_Lines 'Img);
            end if;

         end if;

         if Compute_Average_Lines_In_Bodies then

            if Line_Metrics_Set then
               Report ("");
            end if;

            Report ("Average lines in body:" &
                    Average_Lines_In_Process_Bodies_To_Print'Img);
         end if;

         if Global_Element_Metrics_Set then

            if Line_Metrics_Set or else Compute_Average_Lines_In_Bodies then
               Report ("");
            end if;

            Report ("Element metrics summed over" &
                    Global_Statistics.Computed_Element_Metrics'Img & " units");

            if Compute_All_Statements then
               Report ("  all statements      :" &
                       Global_Statistics.Syntax_Metrics.All_Statements'Img);
            end if;

            if Compute_All_Declarations then
               Report ("  all declarations    :" &
                       Global_Statistics.Syntax_Metrics.All_Declarations'Img);
            end if;

            if Compute_All_Declarations and then Compute_All_Statements then

               LSLOC :=
                 Global_Statistics.Syntax_Metrics.All_Statements +
                 Global_Statistics.Syntax_Metrics.All_Declarations;

               Report ("  logical SLOC        :" & LSLOC'Img);

            end if;

            if Compute_Public_Types
              and then
               Global_Statistics.Public_Types          >  0
            then
               Report ("");
               Report (Global_Statistics.Public_Types'Img &
                       " public types in" &
                       Global_Statistics.Computed_Public_Types'Img &
                       " units");

               if Details_Present (ITD) then

                  Report (" including");

                  if ITD.Abstract_Types > 0 then
                     Report
                       (ITD.Abstract_Types'Img & " abstract types",
                        Depth => 1);
                  end if;

                  if ITD.Tagged_Types > 0 then
                     Report (ITD.Tagged_Types'Img & " tagged types",
                             Depth => 1);
                  end if;

                  if ITD.Private_Types > 0 then
                     Report
                       (ITD.Private_Types'Img & " private types", Depth => 1);
                  end if;

                  if ITD.Task_Types > 0 then
                     Report (ITD.Task_Types'Img & " task types", Depth => 1);
                  end if;

                  if ITD.Protected_Types > 0 then
                     Report
                       (ITD.Protected_Types'Img & " protected types",
                        Depth => 1);
                  end if;

               end if;

            end if;

            if Compute_All_Types
              and then
               Global_Statistics.All_Types          >  0
            then
               Report ("");
               Report (Global_Statistics.All_Types'Img &
                       " type declarations in" &
                       Global_Statistics.Computed_All_Types'Img &
                       " units");
            end if;

            if Compute_Public_Subprograms
              and then
               Global_Statistics.Public_Subprograms          >  0
            then
               Report ("");
               Report (Global_Statistics.Public_Subprograms'Img &
                       " public subprograms in" &
                       Global_Statistics.Computed_Public_Subprograms'Img &
                       " units");
            end if;

            if Compute_All_Subprograms
              and then
               Global_Statistics.All_Subprograms          >  0
            then
               Report ("");
               Report (Global_Statistics.All_Subprograms'Img &
                       " subprogram bodies in" &
                       Global_Statistics.Computed_All_Subprograms'Img &
                       " units");
            end if;

         end if;

         if Compute_Average_Complexity
           and then
            Units_Compute_Average_Complexity_For >  0
         then
            if Line_Metrics_Set
              or else
               Compute_Average_Lines_In_Bodies
              or else
               Element_Metrics_Set
            then
               Report ("");
            end if;

            Report ("Average cyclomatic complexity:" &
                    Average_Cyclomatic_Complexity_To_Print'Img);
         end if;

         if Coupling_Metrics_Set then
            Report_Coupling_Metrics
              (Text => True,
               XML  => False);
         end if;

      end if;

      if Generate_XML_Output then

         --  XML output. Here we output all the computed metric values, even
         --  if we have only one unit or if this value is zero.

         --  ??? Indentation!!!

         if Compute_All_Lines then
            Output_XML_Metric
              ("all_lines",
               Global_Statistics.Line_Metrics.All_Lines,
               Depth => 1);
         end if;

         if Compute_Code_Lines then
            Output_XML_Metric
              ("code_lines",
               Global_Statistics.Line_Metrics.Code_Lines,
               Depth => 1);
         end if;

         if Compute_Comment_Lines then
            Output_XML_Metric
              ("comment_lines",
               Global_Statistics.Line_Metrics.Comment_Lines,
               Depth => 1);
         end if;

         if Compute_EOL_Comments then
            Output_XML_Metric
              ("eol_comments",
               Global_Statistics.Line_Metrics.EOL_Comments,
               Depth => 1);
         end if;

         if Compute_Comment_Code_Ratio then
            Output_XML_Metric
              ("comment_percentage",
               Comment_Code_Ratio_To_Print'Img,
               Depth => 1);
         end if;

         if Compute_Blank_Lines then
            Output_XML_Metric
              ("blank_lines",
               Global_Statistics.Line_Metrics.Blank_Lines,
               Depth => 1);
         end if;

         if Compute_Average_Lines_In_Bodies then
            Output_XML_Metric
              ("average_lines_in_bodies",
               Average_Lines_In_Process_Bodies_To_Print'Img,
               Depth => 1);
         end if;

         --  ??? Interface type details

         if Compute_Public_Types then
            Output_XML_Metric
             ("public_types",
              Global_Statistics.Public_Types,
               Depth => 1);
         end if;

         if Compute_All_Types then
            Output_XML_Metric
              ("all_types",
               Global_Statistics.All_Types,
               Depth => 1);
         end if;

         if Compute_Public_Subprograms then
            Output_XML_Metric
              ("public_subprograms",
               Global_Statistics.Public_Subprograms,
               Depth => 1);
         end if;

         if Compute_All_Subprograms then
            Output_XML_Metric
              ("all_subprograms",
               Global_Statistics.All_Subprograms,
               Depth => 1);
         end if;

         if Compute_All_Statements then
            Output_XML_Metric
              ("all_stmts",
               Global_Statistics.Syntax_Metrics.All_Statements,
               Depth => 1);
         end if;

         if Compute_All_Declarations then
            Output_XML_Metric
              ("all_dcls",
               Global_Statistics.Syntax_Metrics.All_Declarations,
               Depth => 1);
         end if;

         if Compute_All_Declarations and then Compute_All_Statements then
            LSLOC :=
              Global_Statistics.Syntax_Metrics.All_Statements +
              Global_Statistics.Syntax_Metrics.All_Declarations;

            Output_XML_Metric ("lsloc", LSLOC, Depth => 1);
         end if;

         if Compute_Average_Complexity
           and then
            Units_Compute_Average_Complexity_For >  0
         then
            Output_XML_Metric
              ("average_complexity",
               Average_Cyclomatic_Complexity_To_Print'Img,
               Depth => 1);
         end if;

         if Coupling_Metrics_Set then
            Report_Coupling_Metrics
              (Text => False,
               XML  => True);
         end if;

         Close_Tag ("global");

      end if;

      if Generate_Text_Output then
         Set_Output (Standard_Output);
      end if;

   end Report_Global_Statistics;

   -------------------
   -- Report_No_EOL --
   -------------------

   procedure Report_No_EOL (Message : String; Depth : Natural := 0) is
      Indent : constant String := Depth * Indent_String;
   begin

      if Generate_Text_Output then
         Put (Current_Output, Indent);
         Put (Current_Output, Message);
      end if;

   end Report_No_EOL;

   -------------------------
   -- Report_Program_Unit --
   -------------------------

   procedure Report_Program_Unit
     (El           : Element;
      Depth        : Natural;
      Library_Item : Boolean := False)
   is
      Arg_Kind  : constant Flat_Element_Kinds := Flat_Element_Kind (El);
      Indent    : constant String             := Depth * Indent_String;
      Def_Name  : constant Element            := Names (El) (1);
      Name_Str  : constant String             :=
        To_String (Defining_Name_Image (Def_Name));
      Unit_Span : constant Span               := Element_Span (El);

      First_Line_Str : constant String := Unit_Span.First_Line'Img;
      First_Col_Str  : constant String := Unit_Span.First_Column'Img;

      function XML_Name_Str return String;
      --  Generates XML name for a defining operator symbol by replacing
      --  '"', '<', '>', '&' with the appropriate strings. For a defining
      --  identifier or defining A_Defining_Expanded_Name returns Name_Str

      function Program_Unit_Kind return String;
      --  Returns the string describing the kind of a program unit

      function Program_Unit_Kind return String is
      begin
         case Arg_Kind is

            --  Specs:

            when A_Task_Type_Declaration =>
               return ("task type");

            when  A_Protected_Type_Declaration =>
               return ("protected type");

            when A_Single_Task_Declaration =>
               return ("task object");

            when A_Single_Protected_Declaration =>
               return ("protected object");

            when A_Procedure_Declaration =>
               return ("procedure");

            when A_Function_Declaration =>
               return ("function");

            when A_Package_Declaration =>
               return ("package");

            when A_Package_Renaming_Declaration =>
               return ("package renaming");

            when A_Procedure_Renaming_Declaration =>
               return ("procedure renaming");

            when A_Function_Renaming_Declaration =>
               return ("function renaming");

            when A_Generic_Package_Renaming_Declaration =>
               return ("generic package renaming");

            when A_Generic_Procedure_Renaming_Declaration =>
               return ("generic procedure renaming");

            when A_Generic_Function_Renaming_Declaration =>
               return ("generic function renaming");

            when A_Generic_Procedure_Declaration =>
               return ("generic procedure");

            when A_Generic_Function_Declaration =>
               return ("generic function");

            when A_Generic_Package_Declaration =>
               return ("generic package");

            when A_Package_Instantiation =>
               return ("package instantiation");

            when A_Procedure_Instantiation =>
               return ("procedure instantiation");

            when A_Function_Instantiation =>
               return ("function instantiation");

            when An_Expression_Function_Declaration =>
               return ("expression function");
            when A_Null_Procedure_Declaration =>
               return ("null procedure");

            --  Bodies:

            when A_Task_Body_Declaration =>
               return ("task body");

            when A_Protected_Body_Declaration =>
               return ("protected body");

            when An_Entry_Body_Declaration =>
               return ("entry body");

            when A_Package_Body_Declaration =>
               return ("package body");

            when A_Procedure_Body_Declaration =>
               return ("procedure body");

            when A_Function_Body_Declaration  =>
               return ("function body");

            when A_Procedure_Body_Stub =>
               return ("procedure body stub");

            when A_Function_Body_Stub =>
               return ("function body stub");

            when A_Package_Body_Stub =>
               return ("package body stub");

            when A_Task_Body_Stub =>
               return ("task body stub");

            when A_Protected_Body_Stub =>
               return ("protected body stub");

            when others =>
               pragma Assert (False, "Program_Unit_Kind: " & Arg_Kind'Img);
               return (Arg_Kind'Img);
         end case;
      end Program_Unit_Kind;

      function XML_Name_Str return String is
      begin

         if Name_Str = """<""" then
            return "&quot;&lt;&quot;";

         elsif Name_Str = """<=""" then
            return "&quot;&lt;=&quot;";

         elsif Name_Str = """>""" then
            return "&quot;&gt;&quot;";

         elsif Name_Str = """>=""" then
            return "&quot;&gt;=&quot;";

         elsif Name_Str = """&""" then
            return "&quot;&amp;&quot;";

         elsif Defining_Name_Kind (Def_Name) = A_Defining_Operator_Symbol then
            return "&quot;" &
                   Name_Str (Name_Str'First + 1 .. Name_Str'Last - 1) &
                   "&quot;";

         else
            return Name_Str;
         end if;
      end XML_Name_Str;

   begin
      Report_No_EOL (Indent);
      Report_No_EOL (Name_Str);
      Report_No_EOL (" (");

      Report_No_EOL (Program_Unit_Kind);

      if Library_Item then

         if Unit_Kind (Enclosing_Compilation_Unit (El)) in A_Subunit then
            Report_No_EOL (" - subunit");
         else
            Report_No_EOL (" - library item");
         end if;

      end if;

      Report_No_EOL (" at lines ");
      Report_No_EOL (Unit_Span.First_Line'Img);
      Report_No_EOL (":");
      Report_No_EOL (Unit_Span.Last_Line'Img);
      Report        (")");

      Report_XML
        ("<unit name=""" & XML_Name_Str                           &
         """ kind=""" & Program_Unit_Kind                         &
         """ line=""" & First_Line_Str (2 .. First_Line_Str'Last) &
         """ col="""  & First_Col_Str  (2 .. First_Col_Str'Last)  & """>",
         Depth => Depth + 2);

   end Report_Program_Unit;

   -------------------------------
   -- Set_Global_Metrics_Output --
   -------------------------------

   procedure Set_Global_Metrics_Output is
   begin

      if Global_File_Name /= null then
         pragma Assert
           (Get_Current_Dir = ASIS_UL.Environment.Tool_Temp_Dir.all &
              Directory_Separator);
         --  If we are here, we for sure are in our temporary directory
         Change_Dir (ASIS_UL.Environment.Tool_Current_Dir.all);

         begin
            if GNAT.OS_Lib.Is_Regular_File (Global_File_Name.all) then
               Open (Global_Out_File, Out_File, Global_File_Name.all);
            else
               Create (Global_Out_File, Out_File, Global_File_Name.all);
            end if;

         exception
            when others =>
               Error ("gnatmetric: can not open " & Global_File_Name.all);
               Error ("check the file name");
         end;

         Set_Output (Global_Out_File);
      else
         Set_Output (Standard_Output);
      end if;

      Set_Error  (Standard_Error);
      Global_Output := Current_Output;
   end Set_Global_Metrics_Output;

   -------------------------
   -- Set_Source_Out_File --
   -------------------------

   procedure Set_Source_Out_File (SF : SF_Id) is
      S : constant String := Source_Out_File (SF);
   begin

      if not Generate_Text_Output then
         return;
      end if;

      if GNAT.OS_Lib.Is_Regular_File (S) then
         Open (Source_Output_File, Out_File, S);
      else
         Create (Source_Output_File, Out_File, S);
      end if;

      Set_Output (Source_Output_File);

      if ASIS_UL.Options.Mimic_gcc then
         if ASIS_UL.Options.Verbose_Mode then
            Put_Line (Standard_Output, "creating " & S);
         elsif Debug_Flag_V then
            Put_Line (Standard_Output, "creating " & File_Name (S));
         end if;
      end if;

   exception
      when others =>
         Error ("gnatmetric: can not write in " & S);
         raise Fatal_Error;
   end Set_Source_Out_File;

   ----------------------
   -- Set_XML_Out_File --
   ----------------------

   procedure Set_XML_Out_File is
   begin

      if Generate_XML_Output then

         begin

            if GNAT.OS_Lib.Is_Regular_File (XML_File_Name.all) then
               Open (XML_Out_File, Out_File, XML_File_Name.all);
            else
               Create (XML_Out_File, Out_File, XML_File_Name.all);
            end if;

         exception
            when others =>
               Generate_XML_Output := False;
               Error ("gnatmetric: can not open " & XML_File_Name.all);
               Error ("check the file name");

               if not Generate_Text_Output then
                  --  No need to do anything - we have no file to output the
                  --  result
                  raise Fatal_Error;
               end if;

         end;

         if Generate_XML_Schema then

            begin

               if GNAT.OS_Lib.Is_Regular_File (XSD_File_Name.all) then
                  Open (XSD_Out_File, Out_File, XSD_File_Name.all);
               else
                  Create (XSD_Out_File, Out_File, XSD_File_Name.all);
               end if;

            exception
               when others =>
                  Generate_XML_Output := False;
                  Error ("gnatmetric: can not open schema file for " &
                         XML_File_Name.all);
                  Error ("check the file name");

                  Generate_XML_Schema := False;

            end;

         end if;

         Report_XML ("<?xml version=""1.0""?>");

         if Generate_XML_Schema then
            Report_XML
              ("<global xmlns:xsi=" &
               """http://www.w3.org/2001/XMLSchema-instance"" " &
               "xsi:noNamespaceSchemaLocation="""               &
               XSD_File_Name.all & """>");
         else
            Open_Tag ("global");
         end if;

      end if;

   end Set_XML_Out_File;

   ----------------------------
   -- Source_Name_For_Output --
   ----------------------------

   function Source_Name_For_Output (SF : SF_Id) return String is
   begin
      if Short_SFN_In_Output then
         return Short_Source_Name (SF);
      else
         return Source_Name (SF);
      end if;
   end Source_Name_For_Output;

   ---------------------
   -- Source_Out_File --
   ---------------------

   function Source_Out_File (SF : SF_Id) return String is
      Dir_From_Prj : constant String := Get_Result_Dir (SF);
   begin
      if Output_Dir.all = "" then
         if Dir_From_Prj = "" then
            return Source_Name (SF) & Out_Suffix.all;
         else
            return Dir_From_Prj & Short_Source_Name (SF) & Out_Suffix.all;
         end if;
      else
         return Output_Dir.all & Directory_Separator &
                Short_Source_Name (SF) & Out_Suffix.all;
      end if;
   end Source_Out_File;

   ----------------------
   -- Write_XML_Schema --
   ----------------------

   procedure Write_XML_Schema is
   begin
      Set_Output (XSD_Out_File);
      pragma Style_Checks (Off);

      Put_Line ("<?xml version=""1.0"" encoding=""UTF-8""?>");
      Put_Line ("<xs:schema xmlns:xs=""http://www.w3.org/2001/XMLSchema"">");
      Put_Line ("        <xs:element name=""global"">");
      Put_Line ("                <xs:complexType>");
      Put_Line ("                        <xs:sequence>");
      Put_Line ("                                <xs:element ref=""file"" minOccurs=""0"" maxOccurs=""unbounded""/>");
      Put_Line ("                                <xs:element ref=""metric"" minOccurs=""0"" maxOccurs=""unbounded""/>");
      Put_Line ("                                <xs:element ref=""coupling"" minOccurs=""0"" maxOccurs=""1""/>");
      Put_Line ("                        </xs:sequence>");
      Put_Line ("                </xs:complexType>");
      Put_Line ("        </xs:element>");
      Put_Line ("        <xs:element name=""file"">");
      Put_Line ("                <xs:complexType>");
      Put_Line ("                        <xs:sequence>");
      Put_Line ("                                <xs:element ref=""metric"" minOccurs=""0"" maxOccurs=""unbounded""/>");
      Put_Line ("                                <xs:element ref=""unit"" minOccurs=""0"" maxOccurs=""unbounded""/>");
      Put_Line ("                        </xs:sequence>");
      Put_Line ("                        <xs:attribute name=""name"" use=""required"" type=""xs:string""/>");
      Put_Line ("                </xs:complexType>");
      Put_Line ("        </xs:element>");
      Put_Line ("        <xs:element name=""unit"">");
      Put_Line ("                <xs:complexType>");
      Put_Line ("                        <xs:sequence>");
      Put_Line ("                                <xs:element ref=""metric"" minOccurs=""0"" maxOccurs=""unbounded""/>");
      Put_Line ("                                <xs:element ref=""unit"" minOccurs=""0"" maxOccurs=""unbounded""/>");
      Put_Line ("                        </xs:sequence>");
      Put_Line ("                        <xs:attribute name=""name"" use=""required"" type=""xs:string""/>");
      Put_Line ("                        <xs:attribute name=""line"" use=""required"" type=""xs:decimal""/>");
      Put_Line ("                        <xs:attribute name=""kind"" type=""xs:string""/>");
      Put_Line ("                        <xs:attribute name=""col"" use=""required"" type=""xs:byte""/>");
      Put_Line ("                </xs:complexType>");
      Put_Line ("        </xs:element>");
      Put_Line ("        <xs:element name=""metric"">");
      Put_Line ("                <xs:complexType>");
      Put_Line ("                        <xs:simpleContent>");
      Put_Line ("                                <xs:extension base=""xs:decimal"">");
      Put_Line ("                                        <xs:attribute name=""name"" use=""required"" type=""xs:string""/>");
      Put_Line ("                                </xs:extension>");
      Put_Line ("                        </xs:simpleContent>");
      Put_Line ("                </xs:complexType>");
      Put_Line ("        </xs:element>");
      Put_Line ("        <xs:element name=""coupling"">");
      Put_Line ("                <xs:complexType>");
      Put_Line ("                        <xs:sequence>");
      Put_Line ("                                <xs:element ref=""file"" minOccurs=""0"" maxOccurs=""unbounded""/>");
      Put_Line ("                        </xs:sequence>");
      Put_Line ("                </xs:complexType>");
      Put_Line ("        </xs:element>");
      Put_Line ("</xs:schema>");

      pragma Style_Checks (On);
      Set_Output (Standard_Output);

      Close (XSD_Out_File);
   end Write_XML_Schema;

end METRICS.Output;
