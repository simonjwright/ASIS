------------------------------------------------------------------------------
--                                                                          --
--                           GNATELIM COMPONENTS                            --
--                                                                          --
--                    G N A T E L I M . P R O J E C T S                     --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                    Copyright (C) 2013-2018, AdaCore                      --
--                                                                          --
-- GNATELIM  is  free software;  you can  redistribute it and/or  modify it --
-- under the terms of the  GNU  General Public License  as published by the --
-- Free Software Foundation; either version 3 or (at your option) any later --
-- version. GNATELIM is distributed in the hope that it will be useful, but --
-- WITHOUT ANY WARRANTY; without even the implied warranty of  MERCHANTABI- --
-- LITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public Li- --
-- cense for  more details.  You should  have  received  a copy of the  GNU --
-- General Public License distributed with GNAT; see file COPYING3. If not, --
-- go to http://www.gnu.org/licenses for a complete copy of the license.    --
--                                                                          --
-- The original version  of  Gnatelim  was developed by  Alain  Le  Guennec --
-- It is now maintained by AdaCore (http://www.adacore.com)                 --
--                                                                          --
------------------------------------------------------------------------------

with System.Multiprocessors;

with GNAT.OS_Lib;                use GNAT.OS_Lib;

with ASIS_UL.Common;             use ASIS_UL.Common;
with ASIS_UL.Compiler_Options;   use ASIS_UL.Compiler_Options;
with ASIS_UL.Debug;              use ASIS_UL.Debug;
with ASIS_UL.Environment;        use ASIS_UL.Environment;
with ASIS_UL.Options;            use ASIS_UL.Options;
with ASIS_UL.Output;             use ASIS_UL.Output;
with ASIS_UL.Source_Table;       use ASIS_UL.Source_Table;

with Gnatelim.Output;
with Gnatelim.Options;

package body Gnatelim.Projects is

   ----------------------
   -- Print_Tool_Usage --
   ----------------------

   overriding procedure Print_Tool_Usage (My_Project : Gnatelim_Project_Type)
   is
      pragma Unreferenced (My_Project);
   begin
      Gnatelim.Output.Print_Gnatelim_Usage;
   end Print_Tool_Usage;

   --------------------
   -- Scan_Arguments --
   --------------------

   overriding procedure Scan_Arguments
     (My_Project  : in out Gnatelim_Project_Type;
      First_Pass  :        Boolean    := False;
      Parser      :        Opt_Parser := Command_Line_Parser;
      In_Switches :        Boolean    := False)
   is
      In_Project_File : constant Boolean := Parser /= Command_Line_Parser;
   begin
      loop
         case GNAT.Command_Line.Getopt
                (
                 "P: U X! eL "         &   --  project-specific options
                 "-RTS= "              &
                 "-target= "           &
                 "t v q  "             &
                 "j! "                 &

                 "log? "               & --  Specifying the log file
                 "wq ws wn wf "        & --  Warning message control
                 "o= "                 & --  Specify the output file
                 "files= "             & --  Specifies a set of files
                 --  to process
                 "main= "              & --  specifies the main subprogram
                 "-version -help "     & --  print version and usage
                 "-no-elim-dispatch "  & --  Turns OFF elimination of
                 --  dispatching operations,

                 --  Non-documented options:
                 "d? "                 & --  debug mode/options
                 "a "                  & --  Process RTL components, no effect
                 --  at the moment
                 "-ignore= "           & --  specifies a set of units not to
                 --  generate pragmas for
                 "-elim-dispatching "  & --  Turns ON elimination of
                 --  dispatching operations, does nothing for now, elimination
                 --  of dispatching operations is ON by default

                 "-old-pragma-format " & --  Generate UNIT_NAME pragma
                 --  Parameter (Parser => Parser)

                 --  obsolete features, for backward compatibility
                 "b: I: C: -GCC=: -GNATMAKE= m",
                 Parser => Parser)
         is

            when ASCII.NUL =>
               exit when not
                 More_Arguments
                   (Store_Arguments => In_Project_File or else First_Pass,
                    In_Switches     => In_Switches);
            when 'a' =>
               if not First_Pass then
                  Process_RTL_Units := True;
               end if;
            when 'b' =>
               null;  --  obsolete features, for backward compatibility

            when 'C' =>
               --  In old gnatelim this switch has been used to specify a
               --  configuration file, so:
               if not First_Pass then
                  Store_GNAT_Option_With_Path
                    ("gnatec", Parameter (Parser => Parser));
               end if;
            when 'd' =>

               if not First_Pass then
                  if Full_Switch (Parser => Parser) = "d" then
                     Set_Debug_Options (Parameter (Parser => Parser));
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

            when 'f' =>

               if Full_Switch (Parser => Parser) = "files" then
                  if First_Pass then
                     Read_Args_From_File (Parameter);

                  elsif In_Project_File then
                     if In_Switches then
                        Error ("-files option is not allowed " &
                               "for Switches attribute");
                        raise Parameter_Error;
                     else
                        Read_Args_From_File (Parameter (Parser => Parser));
                     end if;
                  end if;
               end if;

            when 'j' =>

               if not First_Pass then
                  begin
                     J_Specified := True;
                     ASIS_UL.Options.Process_Num :=
                       Natural'Value (Parameter (Parser => Parser));

                     if ASIS_UL.Options.Process_Num = 0 then
                        ASIS_UL.Options.Process_Num :=
                          Positive (System.Multiprocessors.Number_Of_CPUs);
                     end if;
                  exception
                     when Constraint_Error =>
                        Error ("Wrong Parameter of '-j' option: " &
                               Parameter (Parser => Parser));
                        raise Parameter_Error;
                  end;
               end if;

            when 'I' =>
               if not First_Pass then
                  Store_I_Option (Parameter (Parser => Parser));
               end if;

            when 'l' =>

               if not First_Pass then
                  if Full_Switch (Parser => Parser) = "log" then
                     Log_Mode := True;
                     Set_Log_File_Name (Parameter (Parser => Parser));
                  end if;
               end if;

            when 'm' =>

               if not First_Pass then
                  if Full_Switch (Parser => Parser) = "main" then
                     Free (ASIS_UL.Options.Main_Subprogram_Name);
                     ASIS_UL.Options.Main_Subprogram_Name :=
                       new String'(Parameter (Parser => Parser));
                  --  elsif Full_Switch (Parser => Parser) = "m" then
                  --     null; --  Obsolete switch, for backwards compatibility
                  end if;
               end if;

            when 'o' =>

               if not First_Pass then
                  if Full_Switch (Parser => Parser) = "o" then
                     Set_Pipe_Mode (False);
                     Set_Report_File_Name (Parameter (Parser => Parser));
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

            when 't' =>
               if not First_Pass then
                  if Full_Switch (Parser => Parser) = "t" then
                     Compute_Timing := True;
                  end if;
               end if;

            when 'v' =>
               if not First_Pass then
                  Verbose_Mode := True;
               end if;

            when 'w' =>

               if not First_Pass then
                  if Full_Switch (Parser => Parser) = "wq" then
                     Warning_Mode := Quiet;
                  elsif Full_Switch (Parser => Parser) = "ws" then
                     Warning_Mode := Short;
                  elsif Full_Switch (Parser => Parser) = "wn" then
                     Warning_Mode := Normal;
                  elsif Full_Switch (Parser => Parser) = "wf" then
                     Warning_Mode := Full;
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
                       (Var => Parameter);
                  elsif In_Project_File then
                     Error ("external references cannot be set in " &
                            "a project file");
                     raise Parameter_Error;
                  end if;
               end if;

            when '-' =>

               if Full_Switch (Parser => Parser) = "-help" then
                  if In_Project_File then
                     Error ("project file should not contain '--help' option");
                     raise Parameter_Error;
                  end if;

                  Print_Usage := True;
                  return;
               elsif Full_Switch (Parser => Parser) = "-version" then
                  if In_Project_File then
                     Error
                       ("project file should not contain '--version' option");
                     raise Parameter_Error;
                  end if;

                  Print_Version := True;
                  return;
               end if;

               if not First_Pass then
                  if Full_Switch (Parser => Parser) = "-GCC" then
                     null;  --  obsolete features, for backward compatibility
                  elsif Full_Switch (Parser => Parser) = "-GNATMAKE" then
                     null;  --  obsolete features, for backward compatibility
                  elsif Full_Switch (Parser => Parser) =
                          "-elim-dispatching"
                  then
                     Gnatelim.Options.Eliminate_Dispatching_Operations := True;
                  elsif Full_Switch (Parser => Parser) =
                          "-no-elim-dispatch"
                  then
                     Gnatelim.Options.Eliminate_Dispatching_Operations :=
                       False;
                  elsif Full_Switch (Parser => Parser) = "-ignore" then
                     if Is_Regular_File (Parameter (Parser => Parser)) then
                        ASIS_UL.Options.Exempted_Units :=
                          new String'(Normalize_Pathname
                                        (Parameter (Parser => Parser)));
                     else
                        Error (Parameter (Parser => Parser) & " not found");
                        raise Parameter_Error;
                     end if;
                  elsif Full_Switch (Parser => Parser) =
                          "-old-pragma-format"
                  then
                     --  Junk for now, should be changed to opposite when
                     --  K108-003 is complete
                     Gnatelim.Options.Long_Pragma_Format := False;
                  elsif Full_Switch (Parser => Parser) = "-RTS" then
                     --  We do not store --RTS option for gcc now - we have
                     --  to resolve its parameter to the full path, and we
                     --  can do this only when target is fully detected.
                     null;
                  end if;
               else
                  if Full_Switch (Parser => Parser) = "-RTS" then
                     Store_RTS_Path (Parameter (Parser => Parser));
                  elsif Full_Switch (Parser => Parser) = "-target" then
                     Target := new String'(Parameter (Parser => Parser));
                  end if;
               end if;

            when others =>
               raise Parameter_Error;
         end case;
      end loop;

      if not First_Pass then
         Process_cargs_Section (Parser => Parser);
      end if;

   exception
      when GNAT.Command_Line.Invalid_Switch =>
         Error ("invalid switch : " & Full_Switch (Parser => Parser));
         raise Parameter_Error;

      when GNAT.Command_Line.Invalid_Parameter =>
         Error ("missing Parameter (Parser => Parser) for: " &
                Full_Switch (Parser => Parser));
         raise Parameter_Error;

   end Scan_Arguments;

   -----------------------
   -- Tool_Package_Name --
   -----------------------

   function Tool_Package_Name
     (My_Project : Gnatelim_Project_Type)
      return       String
   is
      pragma Unreferenced (My_Project);
   begin
      return "Eliminate";
   end Tool_Package_Name;

end Gnatelim.Projects;
