------------------------------------------------------------------------------
--                                                                          --
--                            GNATPP COMPONENTS                             --
--                                                                          --
--                      G N A T P P . P R O J E C T S                       --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                    Copyright (C) 2013-2018, AdaCore                      --
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

with Ada.Directories;
with Ada.Text_IO;                       use Ada.Text_IO;
with System.WCh_Con;                    use System.WCh_Con;

with ASIS_UL.Common;                    use ASIS_UL.Common;
with ASIS_UL.Compiler_Options;          use ASIS_UL.Compiler_Options;
with ASIS_UL.Environment;               use ASIS_UL.Environment;
with ASIS_UL.Misc;                      use ASIS_UL.Misc;
with ASIS_UL.Options;                   use ASIS_UL.Options;
with ASIS_UL.Output;                    use ASIS_UL.Output;
with ASIS_UL.String_Utilities;

with GNATPP.Options;                    use GNATPP.Options;
with GNATPP.Output;                     use GNATPP.Output;

with Pp.Formatting; use Pp.Formatting;
with Pp.Formatting.Dictionaries;
pragma Unreferenced (Pp.Formatting.Dictionaries); -- ???

package body GNATPP.Projects is

   ----------------------
   -- Print_Tool_Usage --
   ----------------------

   overriding procedure Print_Tool_Usage
     (My_Project : Gnatpp_Project_Type)
   is
      pragma Unreferenced (My_Project);
   begin
      GNATPP.Output.Print_Gnatpp_Usage;
   end Print_Tool_Usage;

   --------------------
   -- Scan_Arguments --
   --------------------

   overriding procedure Scan_Arguments
     (My_Project  : in out Gnatpp_Project_Type;
      First_Pass  :        Boolean    := False;
      Parser      :        Opt_Parser := Command_Line_Parser;
      In_Switches :        Boolean    := False)
   is
      In_Project_File : constant Boolean := Parser /= Command_Line_Parser;
      Initial_Char    :          Character;
      Common_Arg : Common_Arg_Status;
      Nat             : Natural;
   begin
      if First_Pass then
         Warning_Mode := Quiet;
         --  Otherwise an average gnatpp run would generate too much diagnoses
         --  about problems in reformatting

         Process_RTL_Units := True;
         --  gnatpp does not care about this
      end if;

      loop
         Initial_Char :=
           GNAT.Command_Line.Getopt
             ("P: U X! eL "           &   --  project-specific options
              "-subdirs= "             &
              "-no_objects_dir "       &
              "-target= "              &

              "-file-name-file= " &
              "-output-dir= " &
              Incremental_Switches      &

              "j! t "                                &

              "aL aU aM cl! c0 c1 c2 c3 c4 c5 "      &
              "-comments-only "                      &
              "e ff i! kL kU l1 l2 l3 M! "           &
              "hx "                                  &
              "N notab nL nU nM nD D- D: "           &
              "neL neU neM neD "                     &
              "ntL ntU ntM ntD "                     &
              "nnL nnU nnM nnD "                     &
              "pL pU pM "                            &
              "A0 A1 A2 A3 A4 A5 T! "                &
              "-no-end-id "                          &
              "-no-separate-is "                     &
              "-separate-label "                     &
              "-separate-loop-then "                 &
              "-no-separate-loop-then "              &
              "-use-on-new-line "                    &
              "-separate-stmt-name "                 &
              "-split-line-before-op "               &
              "-RM-style-spacing "                   &
              "I: gnatec! -RTS= v w q d? "           &
              "gnat05 "                              & -- Ada 2005 mode
              --  output file control
              "-eol= files= pipe of= r rf rnb "   &
              "o= " & -- See Scan_Common_Arg
              --  encoding of the result file(s)
              "Wh Wu Ws We W8 Wb "                   &
              "-par_threshold= -call_threshold= "    &
              "-insert-blank-lines "                 &
              "-preserve-blank-lines "               &
              "-version -help "                      &
                "-ignore= "                            & --  specify a set of
                --                                           units to skip
              "-test "                               &
              "-decimal-grouping= -based-grouping= " &
              "-pp-off= -pp-on= ",
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
               if not First_Pass then
                  if Full_Switch (Parser => Parser) = "A0" then
                     Align_Colons_In_Decl := False;
                     Align_Asign_In_Decl  := False;
                     Align_Asign_In_Stmts := False;
                     Align_Arrows         := False;
                     Align_Ats            := False;
                  elsif Full_Switch (Parser => Parser) = "A1" then
                     Align_Colons_In_Decl := True;
                  elsif Full_Switch (Parser => Parser) = "A2" then
                     Align_Colons_In_Decl := True;
                     Align_Asign_In_Decl := True;
                  elsif Full_Switch (Parser => Parser) = "A3" then
                     Align_Asign_In_Stmts := True;
                  elsif Full_Switch (Parser => Parser) = "A4" then
                     Align_Arrows := True;
                  elsif Full_Switch (Parser => Parser) = "A5" then
                     Align_Ats := True;
                  end if;
               end if;

            when 'a' =>

               if not First_Pass then
                  if Full_Switch (Parser => Parser) = "aL" then
                     PP_Attribute_Casing := Lower_Case;
                  elsif Full_Switch (Parser => Parser) = "aU" then
                     PP_Attribute_Casing := Upper_Case;
                  elsif Full_Switch (Parser => Parser) = "aM" then
                     PP_Attribute_Casing := Mixed;
                  end if;
               end if;

            when 'c' =>

               if not First_Pass then
                  if Full_Switch (Parser => Parser) = "c0" then
                     Format_Comments := False;
                  elsif Full_Switch (Parser => Parser) = "c1" then
                     GNAT_Comment_Inden      := True;
                     Standard_Comment_Indent := False;
                  elsif Full_Switch (Parser => Parser) = "c2" then
                     GNAT_Comment_Inden      := False;
                     Standard_Comment_Indent := True;
                  elsif Full_Switch (Parser => Parser) = "c3" then
                     GNAT_Comment_Start := True;
                  elsif Full_Switch (Parser => Parser) = "c4" then
                     Reformat_Comment_Block := True;
                  elsif Full_Switch (Parser => Parser) = "c5" then
                     Preserve_Special_Comments := True;
                  elsif Full_Switch (Parser => Parser) = "cl" then
                     Nat :=
                       Get_Nat_Switch_Parameter (Parameter (Parser => Parser));

                     if Nat not in 1 .. 9 then
                        Error ("wrong continuation line indentation (" &
                               Parameter (Parser => Parser) & ")");
                        raise Parameter_Error;
                     else
                        PP_Cont_Line_Indentation := Nat;
                        PP_Cont_Line_Indentation_Set := True;
                     end if;

                  end if;
               end if;

            when 'D' =>

               if not First_Pass then
                  if Full_Switch (Parser => Parser) = "D-" then
                     Use_Predefined_Casing := False;
                  else
                     Use_Dictionary := True;
                     --  ???Temporary code. We can't Scan_Dictionary here,
                     --  because Tree_Read destroys Namet.
--                     Pp.Formatting.Dictionaries.
--                       Scan_Dictionary (Parameter (Parser => Parser));
                     ASIS_UL.String_Utilities.String_Vectors.Append
                       (Dictionary_File_Names,
                        Ada.Directories.Full_Name
                          (Parameter (Parser => Parser)));
                  end if;
               end if;

            when 'e' =>
               if not First_Pass then
                  if Full_Switch (Parser => Parser) = "e" then
                     End_Labels := False;
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

            when 'f' =>
               if Full_Switch (Parser => Parser) = "ff" then
                  if not First_Pass then
                     Add_FF := True;
                  end if;
               end if;

            when 'h' =>

               if not First_Pass then
                  if Full_Switch (Parser => Parser) = "hx" then
                     ASIS_UL.Options.Generate_XML_Help := True;
                  end if;
               end if;

            when 'i' =>
               if not First_Pass then
                  Nat :=
                    Get_Nat_Switch_Parameter (Parameter (Parser => Parser));

                  if Nat not in 1 .. 9 then
                     Error
                       ("wrong indentation (" &
                        Parameter (Parser => Parser) & ")");
                     raise Parameter_Error;
                  else
                     PP_Indentation := Nat;
                  end if;
               end if;

            when 'k' =>

               if not First_Pass then
                  if Full_Switch (Parser => Parser) = "kL" then
                     PP_Keyword_Casing := Lower_Case;
                  elsif Full_Switch (Parser => Parser) = "kU" then
                     PP_Keyword_Casing := Upper_Case;
                  end if;
               end if;

            when 'l' =>

               if not First_Pass then
                  if Full_Switch (Parser => Parser) = "l1" then
                     Compact_Layout  := True;
                     Add_Empty_Lines := True;
                  elsif Full_Switch (Parser => Parser) = "l2" then
                     Compact_Layout  := True;
                     Add_Empty_Lines := False;
                  elsif Full_Switch (Parser => Parser) = "l3" then
                     Compact_Layout  := False;
                     Add_Empty_Lines := False;
                  end if;
               end if;

            when 'M' =>

               if not First_Pass then
                  Nat :=
                    Get_Nat_Switch_Parameter (Parameter (Parser => Parser));

                  if Nat not in 32 .. 256 then
                     Put      ("gnatpp: wrong max line length (");
                     Put      (Parameter (Parser => Parser));
                     Put_Line (")");
                     raise Parameter_Error;
                  else
                     Max_Line_Length := Nat;
                  end if;
               end if;

            when 'n' =>

               if not First_Pass then
                  if Full_Switch (Parser => Parser) = "nL" then
                     PP_Name_Casing := Lower_Case;

                     if not PP_Enum_Literal_Casing_Specified then
                        PP_Enum_Literal_Casing := Lower_Case;
                     end if;
                  elsif Full_Switch (Parser => Parser) = "nU" then
                     PP_Name_Casing := Upper_Case;

                  elsif Full_Switch (Parser => Parser) = "nM" then
                     PP_Name_Casing := Mixed;

                  elsif Full_Switch (Parser => Parser) = "nD" then
                     PP_Name_Casing := As_Declared;

                  elsif Full_Switch (Parser => Parser) = "neL" then
                     PP_Enum_Literal_Casing           := Lower_Case;
                     PP_Enum_Literal_Casing_Specified := True;
                  elsif Full_Switch (Parser => Parser) = "neU" then
                     PP_Enum_Literal_Casing           := Upper_Case;
                     PP_Enum_Literal_Casing_Specified := True;
                  elsif Full_Switch (Parser => Parser) = "neM" then
                     PP_Enum_Literal_Casing           := Mixed;
                     PP_Enum_Literal_Casing_Specified := True;
                  elsif Full_Switch (Parser => Parser) = "neD" then
                     PP_Enum_Literal_Casing           := As_Declared;
                     PP_Enum_Literal_Casing_Specified := True;
                  elsif Full_Switch (Parser => Parser) = "ntL" then
                     PP_Type_Casing           := Lower_Case;
                     PP_Type_Casing_Specified := True;
                  elsif Full_Switch (Parser => Parser) = "ntU" then
                     PP_Type_Casing           := Upper_Case;
                     PP_Type_Casing_Specified := True;
                  elsif Full_Switch (Parser => Parser) = "ntM" then
                     PP_Type_Casing           := Mixed;
                     PP_Type_Casing_Specified := True;
                  elsif Full_Switch (Parser => Parser) = "ntD" then
                     PP_Type_Casing           := As_Declared;
                     PP_Type_Casing_Specified := True;
                  elsif Full_Switch (Parser => Parser) = "nnL" then
                     PP_Nnumbers_Casing           := Lower_Case;
                     PP_Nnumbers_Casing_Specified := True;
                  elsif Full_Switch (Parser => Parser) = "nnU" then
                     PP_Nnumbers_Casing           := Upper_Case;
                     PP_Nnumbers_Casing_Specified := True;
                  elsif Full_Switch (Parser => Parser) = "nnM" then
                     PP_Nnumbers_Casing           := Mixed;
                     PP_Nnumbers_Casing_Specified := True;
                  elsif Full_Switch (Parser => Parser) = "nnD" then
                     PP_Nnumbers_Casing           := As_Declared;
                     PP_Nnumbers_Casing_Specified := True;
                  elsif Full_Switch (Parser => Parser) = "notab" then
                     --  -notab is an obsolete feature, replaced with -N
                     No_Tab_In_Comments := True;
                  end if;
               end if;

            when 'N' =>
               if not First_Pass then
                  No_Tab_In_Comments := True;
               end if;

            when 'p' =>

               if not First_Pass then
                  if Full_Switch (Parser => Parser) = "pL" then
                     PP_Pragma_Casing := Lower_Case;
                  elsif Full_Switch (Parser => Parser) = "pU" then
                     PP_Pragma_Casing := Upper_Case;
                  elsif Full_Switch (Parser => Parser) = "pM" then
                     PP_Pragma_Casing := Mixed;
                  elsif Full_Switch (Parser => Parser) = "pipe" then
                     Output_Mode := Pipe;
                  end if;
               end if;

            when 'q' =>
               if not First_Pass then
                  Quiet_Mode := True;
               end if;

            when 'r' =>

               if not First_Pass then
                  if Full_Switch (Parser => Parser) = "r" then
                     Output_Mode := Replace;
                  elsif Full_Switch (Parser => Parser) = "rnb" then
                     Output_Mode := Replace_No_Backup;
                  elsif Full_Switch (Parser => Parser) = "rf" then
                     Output_Mode := Force_Replace;
                  end if;
               end if;

            when 't' =>
               if not First_Pass then
                  if Full_Switch (Parser => Parser) = "t" then
                     Compute_Timing := True;
                  end if;
               end if;

            when 'T' =>

               if not First_Pass then
                  Case_Threshold :=
                    Get_Nat_Switch_Parameter (Parameter (Parser => Parser));
               end if;

            when 'g' | '-' =>

               if not First_Pass then
                  if Full_Switch (Parser => Parser) = "-RTS" then
                     null;
                     --  We should not store --RTS now, we need to resolve
                     --  it to a full path, and we can do it only after
                     --  detecting the target environment
                  elsif Full_Switch (Parser => Parser) = "gnatec" then
                     Store_GNAT_Option_With_Path
                       (Full_Switch (Parser => Parser),
                        Parameter (Parser => Parser));
                  elsif Full_Switch (Parser => Parser) = "-eol" then
                     Out_File_Format :=
                       Get_Out_File_Format (Parameter (Parser => Parser));
                  elsif Full_Switch (Parser => Parser) = "-no-end-id" then
                     End_Id := False;
                  elsif Full_Switch (Parser => Parser) = "-no-separate-is" then
                     Separate_Line_For_IS := False;
                  elsif Full_Switch (Parser => Parser) = "-separate-label" then
                     Separate_Line_For_Label := True;
                  elsif Full_Switch (Parser => Parser) =
                        "-separate-loop-then"
                  then
                     Separate_Line_For_THEN_and_LOOP := True;
                  elsif Full_Switch (Parser => Parser) =
                        "-no-separate-loop-then"
                  then
                     No_Separate_Line_For_THEN_and_LOOP := True;
                  elsif Full_Switch (Parser => Parser) =
                        "-use-on-new-line"
                  then
                     Separate_Line_For_USE := True;
                  elsif Full_Switch (Parser => Parser) =
                        "-separate-stmt-name"
                  then
                     Separate_Line_For_Stmt_Name := True;
                  elsif Full_Switch (Parser => Parser) =
                        "-split-line-before-op"
                  then
                     Split_Line_Before_Op := True;
                  elsif Full_Switch (Parser => Parser) =
                        "-RM-style-spacing"
                  then
                     RM_Style_Spacing := True;
                  elsif Full_Switch (Parser => Parser) = "gnat05" then
                     null; -- allow for compatibility
                  elsif Full_Switch (Parser => Parser) = "-par_threshold" then
                     Nat :=
                       Get_Nat_Switch_Parameter (Parameter (Parser => Parser));
                     Par_Specs_Threshold := Nat;
                  elsif Full_Switch (Parser => Parser) = "-call_threshold" then
                     Nat :=
                       Get_Nat_Switch_Parameter (Parameter (Parser => Parser));
                     Par_Associations_Threshold := Nat;
                  elsif Full_Switch (Parser => Parser) = "-test" then
                     Test_Mode := True;
                  elsif Full_Switch (Parser => Parser) =
                        "-decimal-grouping"
                  then
                     Nat :=
                       Get_Nat_Switch_Parameter (Parameter (Parser => Parser));
                     Decimal_Grouping := Nat;
                  elsif Full_Switch (Parser => Parser) = "-based-grouping" then
                     Nat :=
                       Get_Nat_Switch_Parameter (Parameter (Parser => Parser));
                     Based_Grouping := Nat;
                  elsif Full_Switch (Parser => Parser) = "-pp-off" then
                     Pp_Off_String :=
                       new String'(Parameter (Parser => Parser));
                  elsif Full_Switch (Parser => Parser) = "-pp-on" then
                     Pp_On_String := new String'(Parameter (Parser => Parser));
                  elsif Full_Switch (Parser => Parser) = "-comments-only" then
                     Comments_Only := True;
                  elsif Full_Switch (Parser => Parser) =
                    "-insert-blank-lines"
                  then
                     Insert_Blank_Lines := True;
                     Preserve_Blank_Lines := False;
                  elsif Full_Switch (Parser => Parser) =
                    "-preserve-blank-lines"
                  then
                     Insert_Blank_Lines := False;
                     Preserve_Blank_Lines := True;
                  elsif Full_Switch (Parser => Parser) = "-file-name-file" then
                     File_Name_File_Name := new String'
                       (Parameter (Parser => Parser));
                  else
                     raise Parameter_Error;
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

            when 'o' =>

               if not First_Pass then
                  if Full_Switch (Parser => Parser) = "o" then
                     Output_Mode := Create_File;
                  elsif Full_Switch (Parser => Parser) = "of" then
                     Output_Mode := Force_Create_File;
                  else
                     raise Parameter_Error;
                  end if;

                  Res_File_Name := new String'(Parameter (Parser => Parser));
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

            when 'W' =>

               if not First_Pass then
                  if Full_Switch (Parser => Parser) = "Wh" then
                     Output_Encoding := WCEM_Hex;
                  elsif Full_Switch (Parser => Parser) = "Wu" then
                     Output_Encoding := WCEM_Upper;
                  elsif Full_Switch (Parser => Parser) = "Ws" then
                     Output_Encoding := WCEM_Shift_JIS;
                  elsif Full_Switch (Parser => Parser) = "We" then
                     Output_Encoding := WCEM_EUC;
                  elsif Full_Switch (Parser => Parser) = "W8" then
                     Output_Encoding := WCEM_UTF8;
                  elsif Full_Switch (Parser => Parser) = "Wb" then
                     Output_Encoding := WCEM_Brackets;
                  end if;
               end if;

            when 'w' =>

               if not First_Pass then
                  Warning_Mode := Full;
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

      --  If the output encoding was specified on the command line, set the
      --  input encoding to the same. See also code in ASIS_Processing. Note
      --  that if the user also specifies an input encoding ("-cargs -gnatWx"),
      --  that -gnatWx will appear on the command line after this one.

      if not First_Pass or else In_Project_File then

         case Output_Encoding is
            when WCEM_Default => null;
            when WCEM_Hex => Store_Option ("-gnatWh");
            when WCEM_Upper => Store_Option ("-gnatWu");
            when WCEM_Shift_JIS => Store_Option ("-gnatWs");
            when WCEM_EUC => Store_Option ("-gnatWe");
            when WCEM_UTF8 => Store_Option ("-gnatW8");
            when WCEM_Brackets => Store_Option ("-gnatWb");
         end case;

      end if;

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
           and then Output_Mode in Out_Directory | Replace_Modes
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
         Error ("Parameter missed for : -" & Full_Switch (Parser => Parser));
         raise Parameter_Error;

   end Scan_Arguments;

end GNATPP.Projects;
