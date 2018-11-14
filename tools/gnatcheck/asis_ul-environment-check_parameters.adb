------------------------------------------------------------------------------
--                                                                          --
--                          GNATCHECK COMPONENTS                            --
--                                                                          --
-- A S I S _ U L . E N V I R O N M E N T . C H E C K  _ P A R A M E T E R S --
--                                                                          --
--            (adapted for gnatcheck from ASIS Utility Library)             --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                     Copyright (C) 2005-2018, AdaCore                     --
--                                                                          --
-- Asis Utility Library (ASIS UL) is free software; you can redistribute it --
-- and/or  modify  it  under  terms  of  the  GNU General Public License as --
-- published by the Free Software Foundation; either version 3, or (at your --
-- option)  any later version.  ASIS UL  is distributed in the hope that it --
-- will  be  useful,  but  WITHOUT  ANY  WARRANTY; without even the implied --
-- warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the --
-- GNU  General Public License for more details. You should have received a --
-- copy of the  GNU General Public License  distributed with GNAT; see file --
-- COPYING3. If not,  go to http://www.gnu.org/licenses for a complete copy --
-- of the license.                                                          --
--                                                                          --
-- ASIS UL is maintained by AdaCore (http://www.adacore.com).               --
--                                                                          --
------------------------------------------------------------------------------

pragma Ada_2012;

with ASIS_UL.Debug;
with ASIS_UL.Output;
with ASIS_UL.Projects;
with ASIS_UL.Source_Table;
with ASIS_UL.Tree_Creation;

with Gnatcheck.Categories;
with Gnatcheck.Compiler;         use Gnatcheck.Compiler;
with Gnatcheck.Ids;              use Gnatcheck.Ids;
with Gnatcheck.Options;
with Gnatcheck.Output;           use Gnatcheck.Output;
with Gnatcheck.Projects;         use Gnatcheck.Projects;
with Gnatcheck.Rules;            use Gnatcheck.Rules;
with Gnatcheck.Rules.Rule_Table; use Gnatcheck.Rules.Rule_Table;

separate (ASIS_UL.Environment)
procedure Check_Parameters is
begin
   if Verbose_Mode
     and then
       not (Mimic_gcc or else Aggregated_Project)
   then
      --  In incremental mode or when procrssing aggregated projects one by
      --  one, we want Verbose_Mode to print this only in the outer invocation.
      Print_Version_Info (2004);
   end if;

   if Print_Version then
      Print_Tool_Version (2004);
      Nothing_To_Do := True;
      return;
   end if;

   if Print_Usage then
      Print_Gnatcheck_Usage;
      Nothing_To_Do := True;
      return;
   end if;

   --  We generate the rule help inconditionally.
   if Gnatcheck.Options.Generate_Rules_Help
     and then
      not Aggregated_Project
   then
      Rules_Help;
   end if;

   if Gnatcheck.Options.Generate_Category_Help
     and then
      not Aggregated_Project
   then
      Gnatcheck.Categories.Category_Help
        (Category_Name => "", --  Root category
         From_Status   => Gnatcheck.Options.Rule_Report_Status,
         Recursively   => Gnatcheck.Options.Recursive_Help,
         Level         => 0);
   end if;

   if ASIS_UL.Options.Generate_XML_Help
     and then
      not Aggregated_Project
   then
      Gnatcheck.Categories.XML_Categories_Help;
   end if;

   if Gnatcheck.Options.Generate_Coding_Standard
     and then
      not Aggregated_Project
   then
      Gnatcheck.Output.Write_Coding_Standard;
   end if;

   if In_Aggregate_Project then
      --  We have to skip most of the checks because this gnatcheck call does
      --  not do anything except spawhing another gnatcheck calls for
      --  individual projects
      goto Processing_Aggregate_Project;
   end if;

   --  Now check if we have anything to do:

   if ASIS_UL.Options.No_Argument_File_Specified then

      if Gnatcheck.Options.Generate_Rules_Help
       or else
         Gnatcheck.Options.Generate_Category_Help
       or else
         ASIS_UL.Options.Generate_XML_Help
       or else
         Gnatcheck.Options.Generate_Coding_Standard
      then
         Nothing_To_Do := True;
         return;
      else
         Error ("No input source file set");
         raise Parameter_Error;
      end if;

   end if;

   Read_Args_From_Temp_Storage
     (Duplication_Report =>
        not Gnatcheck.Projects.Is_Specified (Gnatcheck.Options.Gnatcheck_Prj),
      Arg_Project => Gnatcheck.Options.Gnatcheck_Prj);

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

   if Gnatcheck.Options.Gnatcheck_Prj.Is_Specified then
      Gnatcheck.Projects.Set_Individual_Source_Options
        (Gnatcheck.Options.Gnatcheck_Prj);
   end if;

   Set_Compiler_Checks;

   Analyze_Compiler_Output :=
     Use_gnaty_Option or else Use_gnatw_Option or else Check_Restrictions;

   if Analyze_Compiler_Output then
      if Use_gnatw_Option
        or else
         Check_Restrictions
         --  We may change settings for Check_Restrictions mode if and when
         --  we have an option to turn off all the compiler warnings except
         --  those that are related to restrictions only
      then
         Store_Option ("-gnatwn");
         Store_Option (Get_Warning_Option);
      else
         --  '-gnatws' disables all the warnings except style-related
         Store_Option ("-gnatws");
      end if;

      Store_Option ("-gnatyN");
   end if;

   if Use_gnatw_Option
     and then
      Gnatcheck.Options.Mapping_Mode
   then
      Store_Option ("-gnatw.d");
   end if;

   if Use_gnaty_Option then
      Store_Option (Get_Style_Option);
   end if;

   --  In Mimic_gcc mode, we need to use the full path name of the
   --  Gnatcheck_Config_File, because the current directory when running the
   --  compiler is Tool_Current_Dir.

   if Check_Restrictions or else Use_gnatw_Option then
      Store_Option
        ("-gnatec=" &
           (if Mimic_gcc
              then Compose (Tool_Temp_Dir.all, Gnatcheck_Config_File)
              else Gnatcheck_Config_File));
   end if;

   Suppess_Compiler_Check := not (Use_gnatw_Option
                                or else
                                  Use_gnaty_Option
                                or else
                                  Check_Restrictions);

   for Rule in All_Rules.First .. All_Rules.Last loop

      if All_Rules.Table (Rule).Diagnosis /= null
       and then
         Is_Enable (All_Rules.Table (Rule).all)
      then
         --  Note, that if a rule does not have its own diagnoses, this
         --  means that it is implemented by some other rules, so it
         --  should not go into the report
         Gnatcheck.Options.Active_Rule_Present := True;
         exit;
      end if;
   end loop;

   --  If debug flag J is specified, we have to add '-gnatdJ' to the compiler
   --  call options to have the corresponding diagnoses annotations for
   --  compiler-based checks:

   if ASIS_UL.Debug.Debug_Flag_JJ then
      Store_Option ("-gnatdJ");
   end if;

   if not (Gnatcheck.Options.Active_Rule_Present
           or else
             Analyze_Compiler_Output)
   then
      if ASIS_UL.Options.Test_Mode then
         Activate_Rules_In_Test_Mode;
         Process_RTL_Units := True;
      else
         --  Here we have some sources specified, so this situation definitely
         --  is wrong!
         Error ("No rule to check specified");
         raise Parameter_Error;
      end if;
   end if;

   --  Check is the active set of rules requires building the global
   --  structure

   for J in First_Rule .. All_Rules.Last loop

      if All_Rules.Table (J).Rule_State = Enabled then

         if All_Rules.Table (J).all in Global_Rule_Template'Class then
            ASIS_UL.Options.Buld_Call_Graph         := True;
            Gnatcheck.Options.Analyse_Expanded_Code := True;
            Init_Global_Structure
              (Global_Rule_Template'Class (All_Rules.Table (J).all));
         end if;

         if Checked_On_Expanded_Code (All_Rules.Table (J).all) then
            Gnatcheck.Options.Analyse_Expanded_Code := True;
         end if;

         if All_Rules.Table (J).all in Text_Rule_Template'Class then
            Gnatcheck.Options.Analyse_Source_Text := True;
         end if;

      end if;

   end loop;

   if ASIS_UL.Options.Exempted_Units /= null then
      Process_Exemptions (ASIS_UL.Options.Exempted_Units.all);
   end if;

   Total_Sources := Total_Sources_To_Process;

   if Total_Sources = 0 then
      Error ("No existing file to process");
      --  All the rest does not make any sense
      return;
   end if;

   --  If we are here - we have sources to check and rules to apply
   Sources_Left  := Total_Sources;

   --  We need to reset the argument list, because new warning and style
   --  control options may be extracted from the corresponding rules

   Set_Arg_List;

   --  Check for non-documented --no-column option:

   if Gnatcheck.Options.Full_Source_Locations
     and then
       Gnatcheck.Options.No_Column_Num_In_Diagnoses
   then
      Error ("-l and --no-column cannot be set together");
      raise Parameter_Error;
   end if;

   ASIS_UL.Tree_Creation.Set_Max_Processes;

   <<Processing_Aggregate_Project>>

   if Gnatcheck.Options.Gnatcheck_Prj.Is_Specified then
      Gnatcheck.Projects.Set_Global_Result_Dirs
        (Gnatcheck.Options.Gnatcheck_Prj);
   end if;

   ASIS_UL.Output.Set_Report_Files;

end Check_Parameters;
