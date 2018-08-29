------------------------------------------------------------------------------
--                                                                          --
--                          GNATMETRIC COMPONENTS                           --
--                                                                          --
-- A S I S _ U L . E N V I R O N M E N T . C H E C K  _ P A R A M E T E R S --
--                                                                          --
--            (adapted for gnatmetric from ASIS Utility Library)            --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                     Copyright (C) 2010-2017, AdaCore                     --
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

with ASIS_UL.Projects;
with ASIS_UL.Source_Table;
with ASIS_UL.Tree_Creation;

with METRICS.Common;             use METRICS.Common;
with METRICS.Metric_Definitions; use METRICS.Metric_Definitions;
with METRICS.Options;            use METRICS.Options;
with METRICS.Output;             use METRICS.Output;
with METRICS.Projects;
with METRICS.Source_Table;       use METRICS.Source_Table;

separate (ASIS_UL.Environment)
procedure Check_Parameters is
   Tmp : String_Access;

   function Get_XSD_Name_Name (S : String) return String;
   --  Assuming that S is the name for the XML output, generates the name for
   --  the corresponding schema file.

   function Get_XSD_Name_Name (S : String) return String is
      Result    : constant String := Normalize_Pathname (S);
      Firts_Idx : constant Natural := Result'First;
      Last_Idx  : Natural          := Result'Last;
   begin

      if Result (Last_Idx - 3 .. Last_Idx) = ".xml" then
         Last_Idx := Last_Idx - 4;
      end if;

      return Result (Firts_Idx .. Last_Idx) & ".xsd";

   end Get_XSD_Name_Name;

begin

   if Verbose_Mode and then not Mimic_gcc then
      --  In incremental mode, we want Verbose_Mode to print this only in the
      --  outer invocation.

      if Options.Print_Version then
         Print_Tool_Version (2005);
         GNAT.OS_Lib.OS_Exit (0);
      end if;
   end if;

   if Options.Print_Usage then
      Print_Gnatmetric_Usage;
      GNAT.OS_Lib.OS_Exit (0);
   end if;

   --  '--ignore' cannot be used in incremental mode

   if ASIS_UL.Options.Exempted_Units /= null
     and then Incremental_Mode
   then
      Error ("--ignore option cannot be used in incremental mode");
      Brief_Help;
      raise Parameter_Error;
   end if;

   --  First, read all the argument files using all available path information
   if ASIS_UL.Options.No_Argument_File_Specified then
      Error ("No input source file set");
      Brief_Help;
      raise Parameter_Error;
   end if;

   Read_Args_From_Temp_Storage
     (Duplication_Report =>
      not METRICS.Projects.Is_Specified (METRICS.Options.Gnatmetric_Prj),
      Arg_Project => METRICS.Options.Gnatmetric_Prj);

   Nothing_To_Do := Last_Source < First_SF_Id;

   if Nothing_To_Do then
      Error ("No existing file to process");
      --  All the rest does not make any sense
      return;
   end if;

   if METRICS.Options.Gnatmetric_Prj.Is_Specified then
      METRICS.Projects.Set_Global_Result_Dirs (METRICS.Options.Gnatmetric_Prj);
      METRICS.Projects.Set_Individual_Source_Options
        (METRICS.Options.Gnatmetric_Prj);
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

   Sources_Left  := Total_Sources;
   Set_Source_Metrics_Table;

   Init_Global_Statistics;

   --  If the output directory is set, check the name can be used as a
   --  directory name, create the output directory if needed and set
   --  Output_Dir to the full name of this directory

   if Output_Dir.all /= "" then

      if Is_Regular_File (Output_Dir.all) then
         Error ("gnatmetric: cannot create the output directory");
         raise Parameter_Error;

      elsif not Is_Directory (Output_Dir.all) then

         begin
            Make_Dir (Output_Dir.all);
         exception
            when Directory_Error =>
               Error ("gnatmetric: cannot create the output directory");
               raise Parameter_Error;
         end;

      end if;

      Tmp := new String'(Normalize_Pathname (Output_Dir.all));
      Free (Output_Dir);
      Output_Dir := new String'(Tmp.all);
      Free (Tmp);

   end if;

   --  Check that if we have to compute average complexity, we can do this:

   if Compute_Average_Complexity
    and then
      not Compute_Local_Metrics
   then
      Compute_Average_Complexity := False;
   end if;

   --  Check if we have to compute coupling metrics:
   Compute_Coupling_Metric    := Coupling_Metrics_Set;
   Compute_OO_Coupling_Metric := OO_Coupling_Metrics_Set;
   Compute_Control_Coupling_Metric :=
     Compute_Control_Efferent_Coupling or else
     Compute_Control_Afferent_Coupling;
   Compute_Unit_Coupling_Metric :=
     Compute_Unit_Efferent_Coupling or else Compute_Unit_Afferent_Coupling;

   --  Check if we have at least one metric to compute:
   if not (Unit_Metrics_Set
         or else
           Compute_Average_Lines_In_Bodies
         or else
           Compute_Average_Complexity
         or else
           Coupling_Metrics_Set)
   then
      Error ("gnatmetric: no metric to compute is specified");
      Brief_Help;
      raise Fatal_Error;
   end if;

   --  Setting the default values:

   if Out_Suffix = null then
      Out_Suffix := new String'(".metrix");
   end if;

   if XML_File_Name = null and then
      Generate_XML_Output
   then
      XML_File_Name := new String'(Get_Global_Report_Dir & "metrix.xml");
   elsif XML_File_Name /= null then
      Generate_XML_Output := True;
   end if;

   if Generate_XML_Schema then
      XSD_File_Name :=
        new String'(Get_XSD_Name_Name (XML_File_Name.all));
   end if;

   --  Test mode:
   if Test_Mode then
      Complexity_Metrics_On;
      Element_Metrics_On;
      Line_Metrics_On;
      Coupling_Metric_On;
      Process_RTL_Units := True;
   end if;

   Set_XML_Out_File;

   Set_Arg_List;
   ASIS_UL.Tree_Creation.Set_Max_Processes;

end Check_Parameters;
