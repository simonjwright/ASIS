------------------------------------------------------------------------------
--                                                                          --
--                            GNAT2XML COMPONENTS                           --
--                                                                          --
--                     G N A T 2 X M L . P R O J E C T S                    --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                     Copyright (C) 2013-2018, AdaCore                     --
--                                                                          --
-- Gnat2xml is free software; you can redistribute it and/or modify it      --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software Foundation;  either version 2,  or  (at your option)  any later --
-- version. Gnat2xml is distributed  in the hope  that it will be useful,   --
-- but WITHOUT ANY WARRANTY; without even the implied warranty of MER-      --
-- CHANTABILITY or  FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General  --
-- Public License for more details. You should have received a copy of the  --
-- GNU General Public License distributed with GNAT; see file COPYING. If   --
-- not, write to the Free Software Foundation, 59 Temple Place Suite 330,   --
-- Boston, MA 02111-1307, USA.                                              --
-- The gnat2xml tool was derived from the Avatox sources.                   --
------------------------------------------------------------------------------

pragma Ada_2012;

with Ada.Directories;          use Ada.Directories;
with Ada.Strings.Unbounded;    use Ada.Strings.Unbounded;

with GNAT.OS_Lib;

with GNATCOLL.Projects;        use GNATCOLL.Projects;

with ASIS_UL.Common;           use ASIS_UL.Common;
with ASIS_UL.Compiler_Options; use ASIS_UL.Compiler_Options;
with ASIS_UL.Debug;            use ASIS_UL.Debug;
with ASIS_UL.Environment;      use ASIS_UL.Environment;
with ASIS_UL.Formatted_Output; use ASIS_UL.Formatted_Output;
with ASIS_UL.Options;          use ASIS_UL.Options;
with ASIS_UL.Output;           use ASIS_UL.Output;

with Gnat2xml.Command_Line;    use Gnat2xml.Command_Line;

package body Gnat2xml.Projects is

   ----------------------
   -- Print_Tool_Usage --
   ----------------------

   overriding procedure Print_Tool_Usage
     (My_Project : Gnat2xml_Project_Type)
   is
      pragma Unreferenced (My_Project);
   begin
      Gnat2xml_Usage;
   end Print_Tool_Usage;

   ------------------------------
   -- Register_Tool_Attributes --
   ------------------------------

   procedure Register_Tool_Attributes (My_Project : Gnat2xml_Project_Type) is
      Pkg : constant String :=
        Tool_Package_Name (Arg_Project_Type'Class (My_Project));
      Dummy : String_Access;
   begin
      Dummy := new String'
        (Register_New_Attribute
           (Name    => "Default_Switches",
            Pkg     => Pkg,
            Is_List => True,
            Indexed => True));

      if Dummy.all /= "" then
         Error ("cannot parse project file: " & Dummy.all);
         raise Fatal_Error;
      end if;
      Free (Dummy);

      Dummy := new String'
        (Register_New_Attribute
           (Name    => "Switches",
            Pkg     => Pkg,
            Is_List => True,
            Indexed => True));

      if Dummy.all /= "" then
         Error ("cannot parse project file: " & Dummy.all);
         raise Fatal_Error;
      end if;
      Free (Dummy);

   end Register_Tool_Attributes;

   --------------------
   -- Scan_Arguments --
   --------------------

   overriding procedure Scan_Arguments
     (My_Project  : in out Gnat2xml_Project_Type;
      First_Pass  :        Boolean    := False;
      Parser      :        Opt_Parser := Command_Line_Parser;
      In_Switches :        Boolean    := False)
   is
      In_Project_File : constant Boolean := Parser /= Command_Line_Parser;
      Initial_Char    :          Character;
      Common_Arg : Common_Arg_Status;
   begin
      if First_Pass then
         Warning_Mode := Quiet;
         --  Otherwise an average run would generate too much diagnoses
         --  about problems in reformatting

         Process_RTL_Units := True;
         --  We don't care about this

      end if;

      loop
         Initial_Char :=
           GNAT.Command_Line.Getopt
             ("P: U X! eL "             &   --  project-specific options
              "-subdirs= "              &
              "-no_objects_dir "        &
              Incremental_Switches      &
              "j! " &
              "-rep-clauses " &
              "I: -RTS= -target= v q d? " &
              "-compact " &
              "files= " &
              "-ignore= "         &   --  specify a set of units to skip
              "h -help -version " &
              "m: -output-dir=",
               Parser => Parser);

         --  Print command-line options for debugging

         if False -- disable for now
           and then ASIS_UL.Debug.Debug_Flag_1
           and then (not First_Pass)
           and then Initial_Char /= ASCII.NUL
         then
            declare
               Param : constant String :=
                 Strip_Prefix (Parameter (Parser => Parser), "=");
            begin
               Put
                 ("\1 \2: ""\3"" = ""\4""\n",
                  (if First_Pass then "1" else "2"),
                    (1 => Initial_Char),
                    Full_Switch (Parser => Parser),
                    Param);

               --  If the parameter refers to an existing file, print out the
               --  contents of that file.

               if Param /= ""
                 and then Exists (Param)
                 and then Kind (Param) = Ordinary_File
               then
                  declare
                     File_Content : GNAT.OS_Lib.String_Access :=
                       Read_File (Param);
                  begin
                     Put ("----------------\n\1----------------\n",
                          File_Content.all);
                     GNAT.OS_Lib.Free (File_Content);
                  end;
               end if;
            end;
         end if;

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
               if Full_Switch (Parser => Parser) = "h" then
                  if In_Project_File then
                     Error ("project file should not contain '-h' option");
                     raise Parameter_Error;
                  end if;

                  Print_Usage := True;
                  return;
               else
                  pragma Assert (False);
               end if;

            when 'm' => --  Obsolete alternative to --output-dir
               if not First_Pass then
                  if Full_Switch (Parser => Parser) = "m" then
                     --  In Incremental_Mode, -m won't work, because gnatmake
                     --  and gprbuild process it with a different meaning. In
                     --  non-Incremental_Mode, we continue to allow -m for
                     --  compatibility.

                     if Incremental_Mode then
                        Error ("-m is obsolete; use --output-dir="
                                 & Parameter (Parser => Parser) & " instead");
                        raise Parameter_Error;
                     end if;

                     Out_Dir := new String'
                       (Full_Name (Parameter (Parser => Parser)));
                  else
                     pragma Assert (False);
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
                  if Full_Switch (Parser => Parser) = "q" then
                     Quiet_Mode := True;
                  else
                     pragma Assert (False);
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
                  if Full_Switch (Parser => Parser) = "x" then
                     null; -- ignore
                  else
                     pragma Assert (False);
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
                  if Full_Switch (Parser => Parser) = "-RTS" then
                     null;
                     --  We should not store --RTS now, we need to resolve
                     --  it to a full path, and we can do it only after
                  elsif Full_Switch (Parser => Parser) = "-rep-clauses" then
                     Generate_Representation_Clauses := True;
                  elsif Full_Switch (Parser => Parser) = "-compact" then
                     Gnat2xml.Command_Line.Options.Compact_XML := True;
                  else
                     pragma Assert (False);
                  end if;
               else
                  if Full_Switch (Parser => Parser) = "-RTS" then
                     Store_RTS_Path (Parameter (Parser => Parser));
                  elsif Full_Switch (Parser => Parser) = "-subdirs" then
                     Set_Subdir_Name (Parameter (Parser => Parser));
                  elsif Full_Switch (Parser => Parser) = "-no_objects_dir" then
                     No_Object_Dir := True;
                  end if;
               end if;

            when others =>
               if not Mimic_gcc then
                  --  Ignore unrecognized switches in the inner invocation
                  Error
                    ("unrecognized switch: " & Full_Switch (Parser => Parser));
                  raise Program_Error;
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

      --  Set_Gnat2xml_Options (Options);
      --  Moved to Check_Parameters when the direct project support is added.

   exception
      when GNAT.Command_Line.Invalid_Switch =>
         Error ("invalid switch : " & Full_Switch (Parser => Parser));
         raise Parameter_Error;

      when GNAT.Command_Line.Invalid_Parameter =>
         Error ("parameter missed for : -" & Full_Switch (Parser => Parser));
         raise Parameter_Error;

   end Scan_Arguments;

end Gnat2xml.Projects;
