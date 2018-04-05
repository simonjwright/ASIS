------------------------------------------------------------------------------
--                                                                          --
--                           GNATELIM COMPONENTS
--                                                                          --
-- A S I S _ U L . E N V I R O N M E N T . C H E C K  _ P A R A M E T E R S --
--                                                                          --
--            (adapted for gnatelim from ASIS Utility Library)              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                    Copyright (C) 2008-2017, AdaCore                      --
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

with GNATCOLL.VFS; use GNATCOLL.VFS;

with ASIS_UL.Common;
with ASIS_UL.Options;
with ASIS_UL.Output;
with ASIS_UL.Projects;
with ASIS_UL.Source_Table;
with ASIS_UL.Tree_Creation;

with Gnatelim.Options;
with Gnatelim.Output;           use Gnatelim.Output;
with Gnatelim.Projects;

separate (ASIS_UL.Environment)
procedure Check_Parameters is
   Tmp      : String_Access;
   Res      : Virtual_File;
begin

   if Options.Print_Version then
      Print_Tool_Version (2004);
      Options.Nothing_To_Do := True;
      return;
   end if;

   if Options.Print_Usage then
      Print_Gnatelim_Usage;
      Options.Nothing_To_Do := True;
      return;
   end if;

   Read_Args_From_Temp_Storage
     (Duplication_Report => False,
      Arg_Project        => Gnatelim.Options.Gnatelim_Prj);

   if ASIS_UL.Options.Verbose_Mode then
      ASIS_UL.Output.Print_Version_Info (1997);
   end if;

   if ASIS_UL.Options.Main_Subprogram_Name /= null then

      Free (Tmp);

      if not Is_Regular_File (ASIS_UL.Options.Main_Subprogram_Name.all) then

         if Gnatelim.Projects.Is_Specified (Gnatelim.Options.Gnatelim_Prj) then
            Res := Gnatelim.Projects.Create
                     (Gnatelim.Options.Gnatelim_Prj,
                      +ASIS_UL.Options.Main_Subprogram_Name.all);
            if Res = No_File then
               Free (Tmp);
            else
               Tmp := new String'(Res.Display_Full_Name);
            end if;
         else
            if Source_Search_Path /= null then
               Tmp := Locate_Regular_File
                       (File_Name => ASIS_UL.Options.Main_Subprogram_Name.all,
                        Path      => Source_Search_Path.all);
            end if;
         end if;

         if Tmp = null then
            Error ("file specified as main subprogram not found (" &
                   ASIS_UL.Options.Main_Subprogram_Name.all & ")");
            raise Fatal_Error;
         else
            Free (ASIS_UL.Options.Main_Subprogram_Name);
            ASIS_UL.Options.Main_Subprogram_Name := new String'
              (Normalize_Pathname
                 (Tmp.all,
                  Resolve_Links  => False,
                  Case_Sensitive => True));
            Free (Tmp);
         end if;

      else
         Tmp :=
           new String'(Normalize_Pathname
                         (ASIS_UL.Options.Main_Subprogram_Name.all,
                          Resolve_Links  => False,
                          Case_Sensitive => True));

         Free (ASIS_UL.Options.Main_Subprogram_Name);
         ASIS_UL.Options.Main_Subprogram_Name := new String'(Tmp.all);
      end if;

      Free (Tmp);

   end if;

   if ASIS_UL.Options.Main_Subprogram_Name = null
     and then
      Last_Source /= First_SF_Id
   then
      Error ("No main unit specified");
      raise Parameter_Error;
   end if;

   if ASIS_UL.Options.Main_Subprogram_Name /= null
     and then
      Last_Source < First_SF_Id
   then
      Gnatelim.Options.Compute_Closure := True;
   elsif ASIS_UL.Options.Main_Subprogram_Name = null
     and then
      Last_Source = First_SF_Id
   then
      ASIS_UL.Options.Main_Subprogram_Name :=
        new String'(Source_Name (Last_Source));
      Gnatelim.Options.Compute_Closure := True;
   end if;

   Add_Source_To_Process
     (Fname              => ASIS_UL.Options.Main_Subprogram_Name.all,
      Arg_Project        => Gnatelim.Options.Gnatelim_Prj,
      Duplication_Report => False);

   Total_Sources := Natural (Last_Source);
   Sources_Left  := Total_Sources;

   ASIS_UL.Output.Set_Report_Files;

   if not ASIS_UL.Options.Nothing_To_Do
     and then
      Gnatelim.Options.Gnatelim_Prj.Is_Specified
   then
      Gnatelim.Projects.Set_Global_Result_Dirs (Gnatelim.Options.Gnatelim_Prj);
      Gnatelim.Projects.Set_Individual_Source_Options
        (Gnatelim.Options.Gnatelim_Prj);
      Set_Arg_List;
   end if;

   ASIS_UL.Tree_Creation.Set_Max_Processes;

end Check_Parameters;
