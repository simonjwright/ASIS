------------------------------------------------------------------------------
--                                                                          --
--                          GNATCHECK COMPONENTS                            --
--                                                                          --
--                   G N A T C H E C K . P R O J E C T S                    --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                     Copyright (C) 2013-2018, AdaCore                     --
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

pragma Ada_2012;

with Ada.Strings.Fixed;

with GNAT.OS_Lib;                use GNAT.OS_Lib;

with Asis.Extensions.Strings;

with ASIS_UL.Common;             use ASIS_UL.Common;
with ASIS_UL.Compiler_Options;
with ASIS_UL.Debug;
with ASIS_UL.Environment;        use ASIS_UL.Environment;
with ASIS_UL.Misc;
with ASIS_UL.Options;            use ASIS_UL.Options;
with ASIS_UL.Output;             use ASIS_UL.Output;
with ASIS_UL.Projects.Aggregate;

with Gnatcheck.Categories;
with Gnatcheck.Diagnoses;
with Gnatcheck.Diagnoses_Old;
with Gnatcheck.Options;          use Gnatcheck.Options;
with Gnatcheck.Output;           use Gnatcheck.Output;
with Gnatcheck.Rules.Rule_Table; use Gnatcheck.Rules.Rule_Table;

package body Gnatcheck.Projects is

   -------------------------------------
   -- Aggregate_Project_Report_Header --
   -------------------------------------

   procedure Aggregate_Project_Report_Header
     (My_Project : Gnatcheck_Project_Type)
   is
   begin
      if XML_Report_ON then
         XML_Report ("<?xml version=""1.0""?>");
         XML_Report_No_EOL ("<gnatcheck-report");

         if Gnatcheck_Prj.Is_Specified then
            XML_Report (" project=""" & Gnatcheck_Prj.Source_Prj & """>");
         else
            XML_Report (">");
         end if;
      end if;

      Gnatcheck.Diagnoses.Print_Report_Header;

      if Text_Report_ON then
         Report ("");
      end if;

      Aggregate_Project_Report_Header (Arg_Project_Type (My_Project));

      if Text_Report_ON then
         Report ("");
      end if;

   end Aggregate_Project_Report_Header;

   ------------------------------------
   -- Close_Aggregate_Project_Report --
   ------------------------------------

   procedure Close_Aggregate_Project_Report
     (My_Project : Gnatcheck_Project_Type)
   is
   begin
      Close_Aggregate_Project_Report (Arg_Project_Type (My_Project));

      if XML_Report_ON then
         XML_Report ("</gnatcheck-report>");
      end if;
   end Close_Aggregate_Project_Report;

   ----------------------
   -- Print_Tool_Usage --
   ----------------------

   procedure Print_Tool_Usage (My_Project : Gnatcheck_Project_Type) is
      pragma Unreferenced (My_Project);
   begin
      Gnatcheck.Output.Print_Gnatcheck_Usage;
   end Print_Tool_Usage;

   -----------------------------------------
   -- Report_Aggregated_Project_Exit_Code --
   -----------------------------------------

   procedure Report_Aggregated_Project_Exit_Code
     (Aggregate_Prj : Gnatcheck_Project_Type;
      Exit_Code     : Integer)
   is
      pragma Unreferenced (Aggregate_Prj);
   begin
      if Text_Report_ON then
         Report ("Exit code is" & Exit_Code'Img & " (" &
                 (case Exit_Code is
                     when 0 => "no rule violation detected",
                     when 1 => "rule violation(s) detected",
                     when 2 => "tool failure, results cannot be trusted",
                     when 3 => "no rule check performed",
                     when others => "unknown")        & ")");
      end if;

      if XML_Report_ON then
         XML_Report ("<exit-code>" & ASIS_UL.Misc.Image (Exit_Code) &
                     "</exit-code>",
                     Indent_Level => 3);

         XML_Report ("</aggregated-project>",
                     Indent_Level => 2);
      end if;
   end Report_Aggregated_Project_Exit_Code;

   --------------------
   -- Scan_Arguments --
   --------------------

   overriding procedure Scan_Arguments
     (My_Project  : in out Gnatcheck_Project_Type;
      First_Pass  :         Boolean    := False;
      Parser      :         Opt_Parser := Command_Line_Parser;
      In_Switches :         Boolean    := False)
   is
      In_Project_File : constant Boolean := Parser /= Command_Line_Parser;
      Initial_Char    :          Character;
      Common_Arg : Common_Arg_Status;
      Success : Boolean;

      function Get_Coding_Standard_Kind
        (Par  : String)
         return Coding_Standard_Kinds;
      --  Computes the coding standard kind from its Parameter. If the
      --  Parametervalue does not correspond to any coding standard kinds,
      --  generates the corresponding diagnosis and daises Fatal_Error

      function Get_Coding_Standard_Kind
        (Par  : String)
         return Coding_Standard_Kinds
      is
         Result : Coding_Standard_Kinds;
      begin
         Result := Coding_Standard_Kinds'Value (Par);

         if Result = Not_A_Coding_Standard then
            raise Constraint_Error;
         end if;

         return Result;
      exception
         when Constraint_Error =>
            Error ("unknown coding standard kind specified");
            raise Fatal_Error;
      end Get_Coding_Standard_Kind;

      procedure Process_Sections;
      --  Processes option sections. At the moment we have two such sections -
      --  'cargs' for compiler arguments and 'rules' for switching rules ON and
      --  OFF and changing the rule status to feature detection.

      procedure Process_Sections is
      begin

         ASIS_UL.Compiler_Options.Process_cargs_Section (Parser => Parser);

         --  Processing the 'rules' section
         Goto_Section ("rules", Parser => Parser);

         Gnatcheck.Options.Check_Param_Redefinition :=
           Gnatcheck.Options.Check_Param_Redefinition or else
           ASIS_UL.Debug.Debug_Flag_W;

         loop
            case GNAT.Command_Line.Getopt ("* from=", Parser => Parser) is
            --  We do not want to depend on the set of the currently
            --  implemented rules
               when ASCII.NUL =>
                  exit;
               when 'f' =>
                  Process_Rule_File (Parameter (Parser => Parser));

                  if not More_Then_One_Rule_File_Set then
                     Rule_File_Name :=
                       new String'(Parameter (Parser => Parser));
                     More_Then_One_Rule_File_Set := True;
                  else
                     Free (Rule_File_Name);
                  end if;

               when others =>
                  Process_Rule_Option
                    (Ada.Strings.Fixed.Trim
                       (Full_Switch (Parser => Parser), Ada.Strings.Both),
                    Defined_At => "");
                  --  We use the call to Trim here because there can be a rule
                  --  option in quotation marks
                  Individual_Rules_Set := True;
            end case;
         end loop;

      end Process_Sections;

   --  Start of processing for Scan_Arguments

   begin
      loop
         Initial_Char :=
           GNAT.Command_Line.Getopt
             ("v q t h hc? hx s? "     &
              "l m? files= a "         &
              "P: U X! vP! eL A: "     &   --  project-specific options
              "-target= "              &
              "-subdirs= "             &
              "-no_objects_dir "       &
              "j! "                    &
              "d? "                    &   --  debug mode/options

              "-deletion-file= "       &
              Incremental_Switches     &
              "o= "                    & -- See Scan_Common_Arg
              "ox= "                   &
              "I: "                    &
              "-RTS= "                 &

              "log "                   &
              "-include-file= "        &   --  specify user-defined
              --  part of the report file
              "-old-report-format "    &   --  generate report file
              --  using the old format
             "-check-redefinition "    &
             --  check if rule parameter is defined more than once
              "-no-column "            &
              "-exemption "            &   --  rule exemption ON
              "-show-rule "            &   --  show rule name in diagnosis
              "-version -help "        &   --  print version and usage
              "-ignore= "              &   --  specify a set of units to skip
              "-test "                 &   --  test mode (all rules ON)
              "nt xml "                &
              "-write-rules= "         &   --  template rule file name
--       "-dump-code-standard=  "           &   --  coding standard type
              "gnat05",                    --  Ada 2005 mode
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
            when 'A' =>
               if Full_Switch (Parser => Parser) = "A" then
                  if First_Pass then
                     Aggregated_Project := True;
                     ASIS_UL.Projects.Aggregate.Store_Aggregated_Project
                       (Parameter);
                  elsif In_Project_File then
                     Error ("project file should not be specified inside " &
                            "another project file");
                     raise Parameter_Error;
                  end if;
               end if;

            when 'a' =>

               if not First_Pass then
                  if Full_Switch (Parser => Parser) = "a" then
                     Process_RTL_Units := True;
                  end if;
               end if;

            when 'e' =>
               if Full_Switch (Parser => Parser) = "eL" then
                  if First_Pass then
                     ASIS_UL.Projects.Follow_Symbolic_Links := True;
                  elsif In_Project_File then
                     Error ("-eL option cannot be set in a project file");
                     raise Parameter_Error;
                  end if;
               end if;

            when 'h' =>
               if not First_Pass then
                  if Full_Switch (Parser => Parser) = "h" then
                     Generate_Rules_Help := True;
                  elsif Full_Switch (Parser => Parser) = "hc" then
                     Generate_Category_Help := True;
                     Gnatcheck.Categories.Process_Category_Help_Parameter
                       (Parameter (Parser => Parser), Success);

                     if not Success then
                        raise Parameter_Error;
                     end if;

                  elsif Full_Switch (Parser => Parser) = "hx" then
                     Generate_XML_Help := True;
                  end if;
               end if;

            when 'g' =>

               null;
               pragma Assert (Full_Switch (Parser => Parser) = "gnat05");
               --  allow for compatibility

            when 'l' =>

               if not First_Pass then
                  if Full_Switch (Parser => Parser)  = "l" then
                     Gnatcheck.Options.Full_Source_Locations := True;
                     Asis.Extensions.Strings.Set_Full_Names (True);
                  elsif Full_Switch (Parser => Parser)  = "log" then
                     Log_Mode := True;
                  end if;
               end if;

            when 'm' =>

               if not First_Pass then
                  begin
                     Gnatcheck.Options.Max_Diagnoses :=
                       Natural'Value (Parameter (Parser => Parser));

                     if Gnatcheck.Options.Max_Diagnoses > 1000 then
                        Error ("Parameter (Parser => Parser) of '-m' option " &
                               "too big, max allowed is 1000");
                        raise Parameter_Error;
                     end if;

                  exception
                     when Constraint_Error =>
                     Error ("Wrong Parameter of '-m' option: " &
                            Parameter (Parser => Parser));
                     raise Parameter_Error;
                  end;
               end if;

            when 'n' =>

               if not First_Pass then
                  if Full_Switch (Parser => Parser) = "nt" then
                     if Incremental_Mode then
                        Error ("XML output is not allowed " &
                               "in incremental mode");
                        raise Parameter_Error;
                     end if;

                     Text_Report_ON := False;
                     XML_Report_ON  := True;
                  end if;
               end if;

            when 'o' =>

               if not First_Pass then
                  if Full_Switch (Parser => Parser) = "o" then
                     Set_Report_File_Name (Parameter (Parser => Parser));
                  elsif Full_Switch (Parser => Parser) = "ox" then

                     if Incremental_Mode then
                        Error ("XML output is not allowed " &
                               "in incremental mode");
                        raise Parameter_Error;
                     end if;

                     Set_XML_Report_File_Name (Parameter (Parser => Parser));
                     XML_Report_ON := True;
                  end if;
               end if;

            when 'P' =>
               if Full_Switch (Parser => Parser) = "P" then
                  if First_Pass then
                     My_Project.Store_Project_Source (Parameter);
                  elsif In_Project_File then
                     Error ("project file should not be specified inside " &
                            "another project file");
                     raise Parameter_Error;
                  end if;
               end if;

            when 'q' =>
               if not First_Pass then
                  Quiet_Mode := True;
               end if;

            when 's' =>
               if not First_Pass then
                  Gnatcheck.Diagnoses_Old.Process_Report_File_Format_Parameter
                    (Parameter (Parser => Parser), Success);

                  if not Success then
                     raise Parameter_Error;
                  end if;
               end if;

            when 't' =>
               if not First_Pass then
                  Compute_Timing := True;
               end if;

            when 'v' =>
               if Full_Switch (Parser => Parser) = "vP" then
                  if First_Pass then
                     begin
                        ASIS_UL.Projects.Verbosity_Level :=
                          Verbosity_Levels'Value (Parameter);
                     exception
                        when Constraint_Error =>
                           Error ("wrong switch parameter " &
                                  Parameter & " for -vP");
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

            when 'x' =>

               if not First_Pass then
                  if Full_Switch (Parser => Parser) = "xml" then
                     if Incremental_Mode then
                        Error ("XML output is not allowed " &
                               "in incremental mode");
                        raise Parameter_Error;
                     end if;

                     XML_Report_ON  := True;
                  end if;
               end if;

            when 'X' =>
               if Full_Switch (Parser => Parser) = "X" then
                  if First_Pass then
                     ASIS_UL.Projects.Store_External_Variable
                       (Var => Parameter);
                  elsif In_Project_File then
                     Error ("external references cannot be set in " &
                            "a project file");
                     raise Parameter_Error;
                  end if;
               end if;
            when '-' =>
               if not First_Pass then
                  if Full_Switch (Parser => Parser) =
                       "-check-redefinition"
                  then
                     Gnatcheck.Options.Check_Param_Redefinition := True;
                  elsif Full_Switch (Parser => Parser) =
                       "-dump-code-standard"
                  then
                     Coding_Standard_Kind :=
                       Get_Coding_Standard_Kind (Parameter (Parser => Parser));
                  elsif Full_Switch (Parser => Parser) = "-no-column" then
                     Gnatcheck.Options.No_Column_Num_In_Diagnoses := True;
                  elsif Full_Switch (Parser => Parser) = "-exemption" then
                     Gnatcheck.Options.Print_Exemption_Section := True;
                  elsif Full_Switch (Parser => Parser) = "-include-file" then
                     Gnatcheck.Diagnoses_Old.Process_User_Filename
                       (Parameter (Parser => Parser));
                  elsif Full_Switch (Parser => Parser) = "-show-rule" then
                     Gnatcheck.Options.Mapping_Mode := True;
                  elsif Full_Switch (Parser => Parser) = "-test" then
                     ASIS_UL.Options.Test_Mode := True;
                     ASIS_UL.Options.Warning_Mode := Quiet;
                  elsif Full_Switch (Parser => Parser) =
                          "-old-report-format"
                  then
                     Gnatcheck.Options.Qualification_Report := False;
                  elsif Full_Switch (Parser => Parser) = "-write-rules" then
                     Generate_Coding_Standard := True;
                     Set_Coding_Standard_File_Name
                       (Parameter (Parser => Parser));
                  elsif Full_Switch (Parser => Parser) = "-RTS" then
                     --  We do not store --RTS option for gcc now - we have
                     --  to resolve its parameter to the full path, and we
                     --  can do this only when target is fully detected.
                     null;
                  end if;
               else
                  if Full_Switch (Parser => Parser) = "-RTS" then
                     ASIS_UL.Compiler_Options.Store_RTS_Path
                       (Parameter (Parser => Parser));
                     ASIS_UL.Compiler_Options.Custom_RTS :=
                       new String'(Parameter (Parser => Parser));
                  elsif Full_Switch (Parser => Parser) = "-subdirs" then
                     Set_Subdir_Name (Parameter (Parser => Parser));
                  elsif Full_Switch (Parser => Parser) = "-no_objects_dir" then
                     No_Object_Dir := True;
                  elsif Full_Switch (Parser => Parser) = "-deletion-file" then
                     Deletion_File_Name := new String'
                       (Parameter (Parser => Parser));
                  end if;
               end if;

            when others =>
               if not Mimic_gcc then
                  --  Ignore unrecognized switches in the inner invocation
                  Error
                    ("unrecognized switch: " & Full_Switch (Parser => Parser));
                  raise Parameter_Error;
               end if;
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

         if not First_Pass then
            Process_Sections;
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
         Error ("missing Parameter (Parser => Parser) for: -" &
                Full_Switch (Parser => Parser));
         raise Parameter_Error;

   end Scan_Arguments;

   ------------------------
   -- Section_Delimiters --
   ------------------------

   function Section_Delimiters
     (My_Project : Gnatcheck_Project_Type)
      return       String
   is
      pragma Unreferenced (My_Project);
   begin
      return "cargs rules asis-tool-args";
   end Section_Delimiters;

   -----------------------
   -- Tool_Package_Name --
   -----------------------

   function Tool_Package_Name
     (My_Project : Gnatcheck_Project_Type)
      return       String
   is
      pragma Unreferenced (My_Project);
   begin
      return "check";
   end Tool_Package_Name;

end Gnatcheck.Projects;
