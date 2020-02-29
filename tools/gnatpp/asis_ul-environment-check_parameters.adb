------------------------------------------------------------------------------
--                                                                          --
--                            GNATPP COMPONENTS                             --
--                                                                          --
-- A S I S _ U L . E N V I R O N M E N T . C H E C K  _ P A R A M E T E R S --
--                                                                          --
--              (adapted for gnatpp from ASIS Utility Library)              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                     Copyright (C) 2009-2017, AdaCore                     --
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

with System.WCh_Con;
use type System.WCh_Con.WC_Encoding_Method;

with ASIS_UL.Tree_Creation;

with GNATPP.Options;           use GNATPP.Options;
with GNATPP.Output;            use GNATPP.Output;
with GNATPP.Projects;

separate (ASIS_UL.Environment)
procedure Check_Parameters is
begin
   if Verbose_Mode and then not Mimic_gcc then
      --  In incremental mode, we want Verbose_Mode to print this only in the
      --  outer invocation.
      Print_Version_Info (2003);
   end if;

   if ASIS_UL.Options.Generate_XML_Help then
      XML_Help;
      return;
   end if;

   --  First, read all the argument files using all available path information
   if ASIS_UL.Options.No_Argument_File_Specified then
      Error ("No input source file set");
      raise Parameter_Error;
   end if;

   Read_Args_From_Temp_Storage
     (Duplication_Report =>
        not GNATPP.Projects.Is_Specified (GNATPP.Options.Gnatpp_Prj),
      Arg_Project        => GNATPP.Options.Gnatpp_Prj);

   Nothing_To_Do := Last_Source < First_SF_Id;

   if Nothing_To_Do then
      Error ("No existing file to process");
      --  All the rest does not make any sense
      return;
   end if;

   if ASIS_UL.Options.Exempted_Units /= null
     and then Incremental_Mode
   then
      Error ("--ignore option cannot be used in incremental mode");
      Brief_Help;
      raise Parameter_Error;
   end if;

   if RM_Style_Spacing then
      --  RM-style colons are incompatible with aligning them

      Align_Colons_In_Decl := False;
      Align_Asign_In_Decl  := False;
      Align_Asign_In_Stmts := False;
      Align_Arrows         := False;
      Align_Ats            := False;
   end if;

   if GNATPP.Options.Gnatpp_Prj.Is_Specified then
      GNATPP.Projects.Set_Global_Result_Dirs (GNATPP.Options.Gnatpp_Prj);
      GNATPP.Projects.Set_Individual_Source_Options
        (GNATPP.Options.Gnatpp_Prj);
      Set_Arg_List;
   end if;

   if ASIS_UL.Options.Exempted_Units /= null then
      Process_Exemptions (ASIS_UL.Options.Exempted_Units.all);
   end if;

   Total_Sources := Total_Sources_To_Process;

   if Total_Sources = 0 then
      Error ("No existing file to process");
      --  All the rest does not make any sense
      return;
   end if;

   Sources_Left := Total_Sources;

   --  Check that GNAT_Comment_Inden and Standard_Comment_Indent
   --  are not set together

   if GNAT_Comment_Inden and then Standard_Comment_Indent then
      Put      (Standard_Error,
                "gnatpp: comment processing modes -c1 and -c2 ");
      Put_Line (Standard_Error, "can not be set together");
      raise Parameter_Error;
   end if;

   --  If --output-dir= was given on the command line, set Output_Mode to the
   --  corresponding mode.

   if Out_Dir /= null then
      Output_Mode := Out_Directory;
   end if;

   if Incremental_Mode
     and then Output_Mode not in Out_Directory | Replace_Modes
   then
      Error ("in --incremental mode, must also specify " &
               "-rnb, -rf, -r, or --output-dir=");
      raise Parameter_Error;
   end if;

   if Output_Mode = Pipe then

      if Out_File_Format /= Default then
         Put_Line (Standard_Error,
                   "gnatpp: out file format can not be set in pipe mode");
         raise Parameter_Error;

      end if;

      --  Check that the out file format is not set for the pipe output mode:

      if Out_File_Format /= Default then
         Put_Line (Standard_Error,
                   "gnatpp: out file format can not be set in pipe mode");
         raise Parameter_Error;

      end if;

      --  Check that the out file encoding is not set for the pipe output mode,
      --  and set the needed value for the Form parameter for the Create
      --  and Open procedures:

      if Output_Encoding /= WCEM_Default then
         Put_Line (Standard_Error,
                   "gnatpp: out file encoding can not be set in pipe mode");
         raise Parameter_Error;
      end if;

   else
      Set_Form_String;
   end if;

   --  Check that there is no contradictory settings for THEN and LOOP
   --  keyword layout

   if Separate_Line_For_THEN_and_LOOP and then
      No_Separate_Line_For_THEN_and_LOOP
   then
      Put      (Standard_Error,
                "gnatpp: --separate-loop-then and --no-separate-loop-then");
      Put_Line (Standard_Error,
                " can not be set together");
      raise Parameter_Error;
   end if;

   if Last_Source = First_SF_Id then
      Multiple_File_Mode := False;

      --  If we have only one source to reformat, we have to check
      --  the settings of the output file, if it is set

      Progress_Indicator_Mode := False;
      --  We do not need this in case of one file, and we may be in the
      --  mode of outputting the reformatted source into Stdout

      if Output_Mode in Create_Modes then
         --  We have to set the output file here, before we get into the
         --  temporary directory

         if Res_File_Name /= null and then
            Is_Regular_File (Res_File_Name.all)
         then

            if Output_Mode = Create_File then
               Put (Standard_Error, "gnatpp: file ");
               Put (Standard_Error, Res_File_Name.all);
               Put (Standard_Error, " exists. Use '-of' option");
               Put (Standard_Error, " to override");
               New_Line (Standard_Error);
               raise Parameter_Error;
            end if;

         end if;

         Res_File_Name := new String'(Normalize_Pathname (Res_File_Name.all));

      end if;

   else
      --  If we have more than one file to reformat, we can not have options
      --  '-o' or '-of' set

      Multiple_File_Mode := True;

      if Output_Mode in Create_Modes then
         Put (Standard_Error, "gnatpp: explicit output file name is not ");
         Put (Standard_Error, "allowed when multiple ");
         Put_Line (Standard_Error, "argument sources set");
         raise Parameter_Error;
      end if;

   end if;

   --  And  now - some preparations:

   --  Compute the default continuation line indentation, if needed

   if not PP_Cont_Line_Indentation_Set then
      PP_Cont_Line_Indentation := PP_Indentation;

      if PP_Cont_Line_Indentation > 1 then
         PP_Cont_Line_Indentation := PP_Cont_Line_Indentation - 1;
      end if;

   end if;

   --  Set casing for specific kinds of names

   if not PP_Enum_Literal_Casing_Specified then
      PP_Enum_Literal_Casing := PP_Name_Casing;
   end if;

   if not PP_Type_Casing_Specified then
      PP_Type_Casing := PP_Name_Casing;
   end if;

   if not PP_Nnumbers_Casing_Specified then
      PP_Nnumbers_Casing := PP_Name_Casing;
   end if;

   if Test_Mode then
      End_Labels := False;
   end if;

   --  Check that user did not specify --pp-off=X and --pp-on=X, where X = X

   if Pp_Off_String /= null and then Pp_On_String /= null then
      if Pp_Off_String.all = Pp_On_String.all then
         Put_Line
           (Standard_Error,
            "gnatpp: cannot specify --pp-off and --pp-on with same string");
         raise Parameter_Error;
      end if;
   end if;

   Set_Arg_List;

   ASIS_UL.Tree_Creation.Set_Max_Processes;

   if J_Specified
     and then Output_Mode in Replace_Modes
     and then not Incremental_Mode
   then
      Error ("-j cannot be used with " &
             (case Output_Mode is
                 when Replace => "-r",
                 when Force_Replace => "-rf",
                 when Replace_No_Backup => "-rnb",
                 when others => "") &
             " option unless --incremental is used");
      raise Parameter_Error;
   end if;

end Check_Parameters;
