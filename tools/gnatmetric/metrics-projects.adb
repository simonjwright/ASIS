------------------------------------------------------------------------------
--                                                                          --
--                      GNAT METRICS TOOLS COMPONENTS                       --
--                                                                          --
--                              M E T R I C S                               --
--                                                                          --
--                     M E T R I C S . P R O J E C T S                      --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                    Copyright (C) 2013-2017, AdaCore                      --
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
-- GNATMETRIC is maintained by AdaCore (http://www.adacore.com).            --
--                                                                          --
------------------------------------------------------------------------------

pragma Ada_2012;

with GNAT.OS_Lib;                use GNAT.OS_Lib;

with ASIS_UL.Common;             use ASIS_UL.Common;
with ASIS_UL.Compiler_Options;
with ASIS_UL.Environment;        use ASIS_UL.Environment;
with ASIS_UL.Options;            use ASIS_UL.Options;
with ASIS_UL.Output;             use ASIS_UL.Output;

with METRICS.Common;             use METRICS.Common;
with ASIS_UL.Metrics.Definitions;
with METRICS.Options;            use METRICS.Options;
with METRICS.Output;             use METRICS.Output;

package body METRICS.Projects is

   ----------------------
   -- Print_Tool_Usage --
   ----------------------

   overriding procedure Print_Tool_Usage
     (My_Project : Gnatmetrics_Project_Type)
   is
      pragma Unreferenced (My_Project);
   begin
      METRICS.Output.Print_Gnatmetric_Usage;
   end Print_Tool_Usage;

   --------------------
   -- Scan_Arguments --
   --------------------

   overriding procedure Scan_Arguments
     (My_Project  : in out Gnatmetrics_Project_Type;
      First_Pass  :        Boolean    := False;
      Parser      :        Opt_Parser := Command_Line_Parser;
      In_Switches :        Boolean    := False)
   is
      In_Project_File : constant Boolean := Parser /= Command_Line_Parser;
      Initial_Char    :          Character;
      Common_Arg : Common_Arg_Status;

      Next_Metric_Option : String_Access;
      Selective_Mode     : Boolean := False;

      Line_Metrics_Set_On    : Boolean := True;
      Element_Metrics_Set_On : Boolean := True;
      --  The corresponding flag is set ON when the first specific metric
      --  option is processed and all the line or element metrics are turned
      --  off. It prevents from turning all the metric all again when the next
      --  specific metric is set ON.

   begin
      loop
         Initial_Char :=
          GNAT.Command_Line.Getopt
           (

            "P: U X! eL "            &   --  project-specific options
            "-subdirs= "             &
            "-no_objects_dir "       &
            "-RTS= "                 &
            "-target= "              &

            --  Incremental_Switches     & -- ???Not yet supported.
            --  Note that if we ever support --incremental, we will need to
            --  resolve the conflict between the "-x" switch listed below, and
            --  the one in Incremental_Switches. See "when 'x' =>" in
            --  ASIS_UL.Environment.Scan_Common_Arg for details.

            --  Complexity metrics options:
            "-complexity-cyclomatic -no-complexity-cyclomatic " &
            "-complexity-essential  -no-complexity-essential "  &
            "-complexity-average    -no-complexity-average "    &
            "-loop-nesting          -no-loop-nesting  "         &
            "-extra-exit-points     -no-extra-exit-points  "    &
            "-complexity-all        -no-complexity-all  "       &
            "-no-static-loop "                                  &

            --  Line metrics options:
            " -lines             -no-lines             " &
            " -lines-code        -no-lines-code        " &
            " -lines-comment     -no-lines-comment     " &
            " -lines-eol-comment -no-lines-eol-comment " &
            " -lines-ratio       -no-lines-ratio       " &
            " -lines-blank       -no-lines-blank       " &
            " -lines-average     -no-lines-average     " &
            " -lines-all         -no-lines-all         " &

            --  Syntax element metrics options:
            "-declarations       -no-declarations        " &
            "-statements         -no-statements          " &
            "-public-subprograms -no-public-subprograms  " &
            "-all-subprograms    -no-all-subprograms     " &
            "-public-types       -no-public-types        " &
            "-all-types          -no-all-types           " &
            "-unit-nesting       -no-unit-nesting        " &
            "-construct-nesting  -no-construct-nesting   " &
            "-param-number       -no-param-number        " &
            "-syntax-all         -no-syntax-all          " &

            --  Coupling metrics
            "-coupling-all " &
            "-tagged-coupling-out     -tagged-coupling-in    " &
            "-hierarchy-coupling-out  -hierarchy-coupling-in " &
            "-unit-coupling-out       -unit-coupling-in      " &
            "-control-coupling-out    -control-coupling-in   " &

            "-test " & -- running the tool in test mode in Q4A driver

            --  Old coupling metric control options, kept for upward
            --  compatibility reasons (as non-documented feature)

            "-package-efferent-coupling  -no-package-efferent-coupling  " &
            "-package-afferent-coupling  -no-package-afferent-coupling  " &
            "-category-efferent-coupling -no-category-efferent-coupling " &
            "-category-afferent-coupling -no-category-afferent-coupling " &
            "-no-coupling-all                                           " &

            --  Old metric control options, are kept for upward compatibility
            --  reasons (as non-documented feature)
            "la lcode lcomm leol lb lratio lav " &
            "enu "                               &
            "es ed eps eas ept eat ec "          &
            "nocc noec nonl "                    &

            --  Other options:
            "-version -help "                    & --  print version and usage
            "j! t "                              &
            "ne nolocal  "                       &
            "-ignore= "                          & --  specifies a set of units
            --  to skip
            "d= o= files= og= ox= x xs nt sfn "  &
            "gnat05 "                            & --  Ada 2005 mode
            "a q v dd debug!",
            Parser => Parser);

         Common_Arg := Scan_Common_Arg
           (First_Pass, Parser, In_Switches,
            In_Project_File, Initial_Char);
         case Common_Arg is
            when Arg_Processed => goto Continue; -- Dealt with above
            when Arg_Not_Processed => null; -- Deal with it in 'case' below
            when Quit => return; -- Ignore all other args
         end case;

         case Initial_Char is
            when ASCII.NUL =>
               exit when not
                 More_Arguments
                   (Store_Arguments => In_Project_File or else First_Pass,
                    In_Switches     => In_Switches);
            when  'a' =>
               if not First_Pass then
                  Process_RTL_Units := True;
               end if;
            when  'd' =>

               pragma Assert (Full_Switch (Parser => Parser) /= "debug");
               --  gnatmetric uses -debugX instead of -dX for its debug
               --  switches. The -debugX switches are handled in
               --  Scan_Common_Arg.

               if not First_Pass then
                  if Full_Switch (Parser => Parser) = "dd" then
                     Progress_Indicator_Mode := True;
                  elsif Full_Switch (Parser => Parser) = "d" then
                     Free (Output_Dir);
                     Output_Dir :=
                       new String'(Parameter (Parser => Parser));
                  end if;
               end if;

            when  'e' =>

               if not First_Pass then
                  --  Old non-documented syntax element metrics options:

                  if Element_Metrics_Set_On then
                     Element_Metrics_Off;
                     Element_Metrics_Set_On := False;
                  end if;

                  if Full_Switch (Parser => Parser) = "es" then
                     Compute_All_Statements := True;
                  elsif Full_Switch (Parser => Parser) = "ed" then
                     Compute_All_Declarations := True;
                  elsif Full_Switch (Parser => Parser) = "eps" then
                     Compute_Public_Subprograms := True;
                  elsif Full_Switch (Parser => Parser) = "eas" then
                     Compute_All_Subprograms := True;
                  elsif Full_Switch (Parser => Parser) = "ept" then
                     Compute_Public_Types := True;
                  elsif Full_Switch (Parser => Parser) = "eat" then
                     Compute_All_Types := True;
                  elsif Full_Switch (Parser => Parser) = "enu" then
                     Compute_Progam_Unit_Nesting := True;
                  elsif Full_Switch (Parser => Parser) = "ec" then
                     Compute_Construct_Nesting := True;
                  end if;
               end if;

               if Full_Switch (Parser => Parser) = "eL" then
                  if First_Pass then
                     ASIS_UL.Projects.Follow_Symbolic_Links := True;
                  elsif In_Project_File then
                     Error ("-eL option cannot be set in a project file");
                     raise Parameter_Error;
                  end if;
               end if;

            when 'g' =>

               --  if Full_Switch (Parser => Parser) = "gnat05" then
               null; -- allow for compatibility
               --  end if;

            when 'l' =>

               if not First_Pass then
                  --  Old non-documented line metrics options:
                  if Line_Metrics_Set_On then
                     Line_Metrics_Off;
                     Line_Metrics_Set_On := False;
                  end if;

                  if Full_Switch (Parser => Parser) = "la" then
                     Compute_All_Lines := True;
                  elsif Full_Switch (Parser => Parser) = "lcode" then
                     Compute_Code_Lines := True;
                  elsif Full_Switch (Parser => Parser) = "lcomm" then
                     Compute_Comment_Lines := True;
                  elsif Full_Switch (Parser => Parser) = "leol" then
                     Compute_EOL_Comments := True;
                  elsif Full_Switch (Parser => Parser) = "lb" then
                     Compute_Blank_Lines := True;
                  elsif Full_Switch (Parser => Parser) = "lratio" then
                     Compute_Comment_Code_Ratio := True;
                  elsif Full_Switch (Parser => Parser) = "lav" then
                     Compute_Average_Lines_In_Bodies := True;
                  end if;
               end if;

            when 'n' =>

               if not First_Pass then
                  if Full_Switch (Parser => Parser) = "ne" then
                     ASIS_UL.Metrics.Definitions.Treat_Exit_As_Goto := False;
                  elsif Full_Switch (Parser => Parser) = "nolocal" then
                     Compute_Local_Metrics := False;

                  --  Old non-documented complexity metrics options:
                  elsif Full_Switch (Parser => Parser) = "nocc" then
                     Compute_Cyclomatic_Complexity := False;
                  elsif Full_Switch (Parser => Parser) = "noec" then
                     Compute_Essential_Complexity := False;
                  elsif Full_Switch (Parser => Parser) = "nonl" then
                     Compute_Loop_Nesting := False;
                  elsif Full_Switch (Parser => Parser) = "nt" then
                     Generate_Text_Output := False;
                     Generate_XML_Output  := True;
                  end if;
               end if;

            when 'o' =>

               if not First_Pass then
                  if Full_Switch (Parser => Parser) = "o" then
                     if Parameter (Parser => Parser) = "" then
                        Error ("empty output suffix");
                        raise Parameter_Error;
                     end if;

                     Out_Suffix  := new String'(Parameter (Parser => Parser));
                  elsif Full_Switch (Parser => Parser) = "og" then
                     Global_File_Name :=
                       new String'(Parameter (Parser => Parser));
                  elsif Full_Switch (Parser => Parser) = "ox" then
                     Generate_XML_Output := True;
                     XML_File_Name :=
                       new String'(Parameter (Parser => Parser));
                  else
                     raise Parameter_Error;
                  end if;
               end if;

            when 'P' =>
               if Full_Switch (Parser => Parser) = "P" then
                  if First_Pass then
                     My_Project.Store_Project_Source
                       (Parameter (Parser => Parser));
                  elsif In_Project_File then
                     Error ("project file should not be specified inside " &
                            "another project file");
                     raise Parameter_Error;
                  end if;
               end if;

            when 'q' =>

               if not First_Pass then
                  if Full_Switch (Parser => Parser) = "q" then
                     Quiet_Mode := True;
                  end if;
               end if;

            when 's' =>

               if not First_Pass then
                  if Full_Switch (Parser => Parser) = "sfn" then
                     Short_SFN_In_Output := True;
                  end if;
               end if;

            when 't' =>
               if not First_Pass then
                  if Full_Switch (Parser => Parser) = "t" then
                     Compute_Timing := True;
                  end if;
               end if;

            when 'v' =>

               if Full_Switch (Parser => Parser) = "vP" then
                  if First_Pass then
                     begin
                        ASIS_UL.Projects.Verbosity_Level :=
                          Verbosity_Levels'Value
                            (Parameter (Parser => Parser));
                     exception
                        when Constraint_Error =>
                           Error ("wrong switch parameter " &
                                  Parameter (Parser => Parser) & " for -vP");
                           raise Parameter_Error;
                     end;
                  elsif In_Project_File then
                     Error ("-vP option is not allowed in a project file");
                     raise Parameter_Error;
                  end if;
               end if;

            when 'U' =>
               if Full_Switch (Parser => Parser) = "U" then
                  if First_Pass then
                     if ASIS_UL.Projects.U_Option_Set then
                        Error ("-U can be specified only once");
                        raise Parameter_Error;
                     end if;

                     ASIS_UL.Projects.U_Option_Set := True;
                  elsif In_Project_File then
                     Error ("-U option is not allowed in a project file");
                     raise Parameter_Error;
                  end if;
               end if;

            when 'X' =>
               if Full_Switch (Parser => Parser) = "X" then
                  if First_Pass then
                     ASIS_UL.Projects.Store_External_Variable
                       (Var => Parameter (Parser => Parser));
                  elsif In_Project_File then
                     Error ("external references cannot be set in " &
                            "a project file");
                     raise Parameter_Error;
                  end if;
               end if;

            when 'x' =>
               Generate_XML_Output := True;

               if Full_Switch (Parser => Parser) = "xs" then
                  Generate_XML_Schema := True;
               end if;

            when '-' =>

               if not First_Pass then
                  --  Metric options:

                  Next_Metric_Option :=
                    new String'(Full_Switch (Parser => Parser));

                  if not Selective_Mode
                    and then
                      Next_Metric_Option'Length > 5
                    and then
                      Next_Metric_Option
                          (Next_Metric_Option'First ..
                                   Next_Metric_Option'First + 3)
                       /= "-no-"
                    and then
                      not (Full_Switch (Parser => Parser) = "-subdirs"
                          or else
                           Full_Switch (Parser => Parser) = "-no_objects_dir")
                  then
                     Selective_Mode := True;
                     Complexity_Metrics_Off;
                     Element_Metrics_Off;
                     Line_Metrics_Off;
                  end if;

                  --  Complexity metrics options:
                  if Next_Metric_Option.all = "-complexity-cyclomatic" then
                     Compute_Cyclomatic_Complexity := True;
                  elsif Next_Metric_Option.all =
                          "-no-complexity-cyclomatic"
                  then
                     Compute_Cyclomatic_Complexity := False;
                  elsif Next_Metric_Option.all = "-complexity-essential" then
                     Compute_Essential_Complexity := True;
                  elsif Next_Metric_Option.all =
                          "-no-complexity-essential"
                  then
                     Compute_Essential_Complexity := False;
                  elsif Next_Metric_Option.all = "-complexity-average" then
                     Compute_Average_Complexity := True;
                  elsif Next_Metric_Option.all = "-no-complexity-average" then
                     Compute_Average_Complexity := False;
                  elsif Next_Metric_Option.all = "-loop-nesting" then
                     Compute_Loop_Nesting := True;
                  elsif Next_Metric_Option.all = "-no-loop-nesting" then
                     Compute_Loop_Nesting := False;
                  elsif Next_Metric_Option.all = "-extra-exit-points" then
                     Compute_Extra_Exit_Points := True;
                  elsif Next_Metric_Option.all = "-no-extra-exit-points" then
                     Compute_Extra_Exit_Points := False;
                  elsif Next_Metric_Option.all = "-complexity-all" then
                     Complexity_Metrics_On;
                  elsif Next_Metric_Option.all = "-no-complexity-all" then
                     Complexity_Metrics_Off;
                  elsif Next_Metric_Option.all = "-no-static-loop" then
                     ASIS_UL.Metrics.Definitions.Count_Static_Loop := False;

                  --  Line metrics options:
                  elsif Next_Metric_Option.all = "-lines" then
                     Compute_All_Lines := True;
                  elsif Next_Metric_Option.all = "-no-lines" then
                     Compute_All_Lines := False;
                  elsif Next_Metric_Option.all = "-lines-code" then
                     Compute_Code_Lines := True;
                  elsif Next_Metric_Option.all = "-no-lines-code" then
                     Compute_Code_Lines := False;
                  elsif Next_Metric_Option.all = "-lines-comment" then
                     Compute_Comment_Lines := True;
                  elsif Next_Metric_Option.all = "-no-lines-comment" then
                     Compute_Comment_Lines := False;
                  elsif Next_Metric_Option.all = "-lines-eol-comment" then
                     Compute_EOL_Comments := True;
                  elsif Next_Metric_Option.all = "-no-lines-eol-comment" then
                     Compute_EOL_Comments := False;
                  elsif Next_Metric_Option.all = "-lines-ratio" then
                     Compute_Comment_Code_Ratio := True;
                  elsif Next_Metric_Option.all = "-no-lines-ratio" then
                     Compute_Comment_Code_Ratio := False;
                  elsif Next_Metric_Option.all = "-lines-blank" then
                     Compute_Blank_Lines := True;
                  elsif Next_Metric_Option.all = "-no-lines-blank" then
                     Compute_Blank_Lines := False;
                  elsif Next_Metric_Option.all = "-lines-average" then
                     Compute_Average_Lines_In_Bodies := True;
                  elsif Next_Metric_Option.all = "-no-lines-average" then
                     Compute_Average_Lines_In_Bodies := False;
                  elsif Next_Metric_Option.all = "-lines-all" then
                     Line_Metrics_On;
                  elsif Next_Metric_Option.all = "-no-lines-all" then
                     Line_Metrics_Off;

                  --  Syntax element metrics options:
                  elsif Next_Metric_Option.all = "-declarations" then
                     Compute_All_Declarations := True;
                  elsif Next_Metric_Option.all = "-no-declarations" then
                     Compute_All_Declarations := False;
                  elsif Next_Metric_Option.all = "-statements" then
                     Compute_All_Statements := True;
                  elsif Next_Metric_Option.all = "-no-statements" then
                     Compute_All_Statements := False;
                  elsif Next_Metric_Option.all = "-public-subprograms" then
                     Compute_Public_Subprograms := True;
                  elsif Next_Metric_Option.all = "-no-public-subprograms" then
                     Compute_Public_Subprograms := False;
                  elsif Next_Metric_Option.all = "-all-subprograms" then
                     Compute_All_Subprograms := True;
                  elsif Next_Metric_Option.all = "-no-all-subprograms" then
                     Compute_All_Subprograms := False;
                  elsif Next_Metric_Option.all = "-public-types" then
                     Compute_Public_Types := True;
                  elsif Next_Metric_Option.all = "-no-public-types" then
                     Compute_Public_Types := False;
                  elsif Next_Metric_Option.all = "-all-types" then
                     Compute_All_Types := True;
                  elsif Next_Metric_Option.all = "-no-all-types" then
                     Compute_All_Types := False;
                  elsif Next_Metric_Option.all = "-unit-nesting" then
                     Compute_Progam_Unit_Nesting := True;
                  elsif Next_Metric_Option.all = "-no-unit-nesting" then
                     Compute_Progam_Unit_Nesting := False;
                  elsif Next_Metric_Option.all = "-construct-nesting" then
                     Compute_Construct_Nesting := True;
                  elsif Next_Metric_Option.all = "-no-construct-nesting" then
                     Compute_Construct_Nesting := False;
                  elsif Next_Metric_Option.all = "-param-number" then
                     Compute_Spb_Pars_Num := True;
                  elsif Next_Metric_Option.all = "-no-param-number" then
                     Compute_Spb_Pars_Num := False;
                  elsif Next_Metric_Option.all = "-syntax-all" then
                     Element_Metrics_On;
                  elsif Next_Metric_Option.all = "-no-syntax-all" then
                     Element_Metrics_Off;

                  --  Coupling metrics
                  elsif Next_Metric_Option.all = "-package-efferent-coupling"
                     or else
                        Next_Metric_Option.all = "-tagged-coupling-out"
                  then
                     Compute_OO_Package_Efferent_Coupling := True;
                  elsif Next_Metric_Option.all =
                    "-no-package-efferent-coupling"
                  then
                     Compute_OO_Package_Efferent_Coupling := False;
                  elsif Next_Metric_Option.all = "-package-afferent-coupling"
                     or else
                        Next_Metric_Option.all = "-tagged-coupling-in"
                  then
                     Compute_OO_Package_Afferent_Coupling := True;
                  elsif Next_Metric_Option.all =
                    "-no-package-afferent-coupling"
                  then
                     Compute_OO_Package_Afferent_Coupling := False;
                  elsif Next_Metric_Option.all = "-category-efferent-coupling"
                     or else
                        Next_Metric_Option.all = "-hierarchy-coupling-out"
                  then
                     Compute_Category_Efferent_Coupling := True;
                  elsif Next_Metric_Option.all =
                    "-no-category-efferent-coupling"
                  then
                     Compute_Category_Efferent_Coupling := False;
                  elsif Next_Metric_Option.all = "-category-afferent-coupling"
                     or else
                        Next_Metric_Option.all = "-hierarchy-coupling-in"
                  then
                     Compute_Category_Afferent_Coupling := True;
                  elsif Next_Metric_Option.all =
                    "-no-category-afferent-coupling"
                  then
                     Compute_Category_Afferent_Coupling := False;
                  elsif Next_Metric_Option.all = "-control-coupling-out" then
                     Compute_Control_Efferent_Coupling := True;
                  elsif Next_Metric_Option.all = "-control-coupling-in" then
                     Compute_Control_Afferent_Coupling := True;
                  elsif Next_Metric_Option.all = "-unit-coupling-out" then
                     Compute_Unit_Efferent_Coupling := True;
                  elsif Next_Metric_Option.all = "-unit-coupling-in" then
                     Compute_Unit_Afferent_Coupling := True;
                  elsif Next_Metric_Option.all = "-coupling-all" then
                     Coupling_Metric_On;
                  elsif Next_Metric_Option.all = "-no-coupling-all" then
                     Coupling_Metric_Off;

                  --  Test mode for Q4A
                  elsif Next_Metric_Option.all = "-test" then
                     Test_Mode := True;

                  elsif Full_Switch (Parser => Parser) = "-RTS" then
                     null;
                     --  We should not store --RTS now, we need to resolve
                     --  it to a full path, and we can do it only after
                     --  detecting the target environment
                  else
                     null;
                     pragma Assert (False);
                  end if;

                  Free (Next_Metric_Option);
               else
                  if Full_Switch (Parser => Parser) = "-RTS" then
                     ASIS_UL.Compiler_Options.Store_RTS_Path
                       (Parameter (Parser => Parser));
                  --  controlling the output files location if the project file
                  --  is used
                  elsif Full_Switch (Parser => Parser) = "-subdirs" then
                     Set_Subdir_Name (Parameter (Parser => Parser));
                  elsif Full_Switch (Parser => Parser) = "-no_objects_dir" then
                     No_Object_Dir := True;
                  end if;
               end if;

            when others =>
               raise Parameter_Error;
         end case;

         <<Continue>>
         --  Go here to skip the above case statement in the case when
         --  Scan_Common_Arg already dealt with an argument.
      end loop;

      --  If there is an -asis-tool-args section (which only happens in the
      --  inner invocations of incremental mode), we treat those args like
      --  normal args. We do so by going to that section, and recursively
      --  calling Scan_Arguments. See also ASIS_UL.Projects.Section_Delimiters.

      if Current_Section (Parser => Parser) = "" then
         Goto_Section ("asis-tool-args", Parser => Parser);
         if Current_Section (Parser => Parser) = "-asis-tool-args" then
            Scan_Arguments (My_Project, First_Pass, Parser, In_Switches);
            Goto_Section ("", Parser => Parser);
         else
            pragma Assert (Current_Section (Parser => Parser) = "");
         end if;

         if not First_Pass or else In_Project_File then
            ASIS_UL.Compiler_Options.Process_cargs_Section (Parser);
         end if;

         if Incremental_Mode_By_Default
           and then My_Project.Is_Specified
         then
            pragma Assert (not Mimic_gcc);
            Incremental_Mode := True;
         end if;
      else
         --  We're in the recursive call; do nothing
         pragma Assert
           (Current_Section (Parser => Parser) = "-asis-tool-args");
      end if;

   exception
      when GNAT.Command_Line.Invalid_Switch =>
         Error ("invalid switch : " & Full_Switch (Parser => Parser));
         raise Parameter_Error;

      when GNAT.Command_Line.Invalid_Parameter =>
         Error ("parameter missed for : -" & Full_Switch (Parser => Parser));
         raise Parameter_Error;

   end Scan_Arguments;

   -----------------------
   -- Tool_Package_Name --
   -----------------------

   function Tool_Package_Name
     (My_Project : Gnatmetrics_Project_Type)
      return       String
   is
      pragma Unreferenced (My_Project);
   begin
      return "metrics";
   end Tool_Package_Name;

end METRICS.Projects;
