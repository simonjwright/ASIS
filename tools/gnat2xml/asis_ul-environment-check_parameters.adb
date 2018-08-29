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
-- GNATPP is free software; you can redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion.  GNATPP is  distributed in the  hope that it will  be  useful, but --
-- WITHOUT ANY WARRANTY; without even the implied warranty of  MERCHANTABI- --
-- LITY or  FITNESS  FOR A  PARTICULAR  PURPOSE. See the GNU General Public --
-- License  for more details. You  should  have  received a copy of the GNU --
-- General Public License  distributed with GNAT; see file COPYING. If not, --
-- write to the Free Software Foundation,  51 Franklin Street, Fifth Floor, --
-- Boston, MA 02110-1301, USA.                                              --
--                                                                          --
-- GNATPP is maintained by AdaCore (http://www.adacore.com)                 --
--                                                                          --
------------------------------------------------------------------------------

pragma Ada_2012;

with Gnat2xml.Command_Line; use Gnat2xml.Command_Line;
with Gnat2xml.Projects;

separate (ASIS_UL.Environment)
procedure Check_Parameters is
begin
   if Verbose_Mode then
      Print_Version_Info (2012);
   end if;

   if Incremental_Mode and Out_Dir = null then
      Error ("in --incremental mode, must also specify --output-dir=");
      raise Parameter_Error;
   end if;

   Set_Gnat2xml_Options (Gnat2xml.Command_Line.Options);
   --  Moved here from parameter processing routine to Check_Parameters when
   --  the direct project support is added.

   --  First, read all the argument files using all available path information
   if ASIS_UL.Options.No_Argument_File_Specified then
      Error ("No input source file set");
      raise Parameter_Error;
   end if;

   Read_Args_From_Temp_Storage
     (Duplication_Report =>
        not Gnat2xml.Projects.Is_Specified (Gnat2xml.Projects.Gnat2xml_Prj),
      Arg_Project        => Gnat2xml.Projects.Gnat2xml_Prj);

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
      raise Parameter_Error;
   end if;

   if Gnat2xml.Projects.Gnat2xml_Prj.Is_Specified then
      Gnat2xml.Projects.Set_Global_Result_Dirs
        (Gnat2xml.Projects.Gnat2xml_Prj);
      Gnat2xml.Projects.Set_Individual_Source_Options
        (Gnat2xml.Projects.Gnat2xml_Prj);
   end if;

   Set_Arg_List;

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

   if Last_Source = First_SF_Id then
      Multiple_File_Mode := False;

      --  If we have only one source to reformat, we have to check the settings
      --  of the output file, if it is set

      Progress_Indicator_Mode := False;
      --  We do not need this in case of one file, and we may be in the mode of
      --  outputting the reformatted source into Stdout
   end if;

   if not Verbose_Mode then
      Progress_Indicator_Mode := False;
   end if;

end Check_Parameters;
