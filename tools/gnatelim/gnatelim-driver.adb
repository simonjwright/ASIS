------------------------------------------------------------------------------
--                                                                          --
--                           GNATELIM COMPONENTS                            --
--                                                                          --
--                      G N A T E L I M . D R I V E R                       --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                    Copyright (C) 2009-2017, AdaCore                      --
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

with Ada.Calendar;
with Ada.Command_Line;

with GNAT.OS_Lib;          use GNAT.OS_Lib;

with Asis.Exceptions;

with ASIS_UL.Common;
with  ASIS_UL.Compiler_Options;
with ASIS_UL.Environment;
with ASIS_UL.Global_State.CG.Gnatelim;
with ASIS_UL.Misc;
with ASIS_UL.Options;      use ASIS_UL.Options;
with ASIS_UL.Output;
with ASIS_UL.Source_Table; use ASIS_UL.Source_Table;
with ASIS_UL.Source_Table.Processing;

with Gnatelim.Closure;
with Gnatelim.Options;
with Gnatelim.Output;

procedure Gnatelim.Driver is
   Time_Start : constant Ada.Calendar.Time := Ada.Calendar.Clock;
   Exect_Time :          Duration;
   use type Ada.Calendar.Time;

   Success : Boolean := False;

   procedure Set_Exemption (Fname : String);
   --  Marks the argument file in the source table as exempted (that is, no
   --  pragma should be generated for subprograms declared in this file).
   --  Generates a warning if Fname does not point to argument file.

   procedure Set_Exemption (Fname : String) is
      SF : constant SF_Id := File_Find (Fname, Use_Short_Name => True);
   begin
      if Present (SF) then
         Set_Source_Info (SF, Gnatelim.Options.Ignore_Unit);
      else
         ASIS_UL.Output.Warning ("exemption: source " & Fname & " not found");
      end if;
   end Set_Exemption;

   procedure Process_Exemptions is new
     ASIS_UL.Misc.Parse_File_List (Set_Exemption);

begin
   ASIS_UL.Output.Set_Pipe_Mode;
   ASIS_UL.Compiler_Options.Add_I_Options_To_Source_Search_Path := True;
   ASIS_UL.Environment.Initialize (Gnatelim.Options.Gnatelim_Prj);

   if Nothing_To_Do then
      ASIS_UL.Output.Close_Log_File;
      GNAT.OS_Lib.OS_Exit (0);
   end if;

   --  Processing:

   if Gnatelim.Options.Compute_Closure then

      Gnatelim.Closure.Try_Get_Sources_From_Binder (Success);

      if Success then
         ASIS_UL.Source_Table.Processing.Process_Sources
           (Add_Needed_Sources => True);
      else
         Gnatelim.Closure.Process_Closure;
      end if;

   else
      ASIS_UL.Source_Table.Processing.Process_Sources;
   end if;

   --  Processing list of units to be ignored. We can do it only now, when we
   --  have all the sources stored in source table.

   if Gnatelim.Options.Exempted_Units /= null then
      Process_Exemptions (Gnatelim.Options.Exempted_Units.all);
   end if;

   --  Finalize:
   ASIS_UL.Global_State.CG.Transitive_Closure;

   ASIS_UL.Global_State.CG.Gnatelim.Mark_Used_Subprograms;

   ASIS_UL.Global_State.Print_Global_Structure;

   Gnatelim.Output.Report_Unused_Subprograms;

   ASIS_UL.Output.Close_Report_Files;

   if ASIS_UL.Common.Tool_Failures > 0 then
      ASIS_UL.Output.Info
        ("Total tool failures :" & ASIS_UL.Common.Tool_Failures'Img);
   end if;

   ASIS_UL.Environment.Clean_Up;
   Gnatelim.Options.Gnatelim_Prj.Clean_Up;

   if ASIS_UL.Options.Compute_Timing then
      Exect_Time := Ada.Calendar.Clock - Time_Start;
      ASIS_UL.Output.Info ("Execution time:" & Exect_Time'Img);
   end if;

   ASIS_UL.Output.Close_Log_File;

   if ASIS_UL.Common.Tool_Failures > 0 then
      GNAT.OS_Lib.OS_Exit (1);
   else
      GNAT.OS_Lib.OS_Exit (0);
   end if;
exception
   when ASIS_UL.Common.Fatal_Error =>
      --  Just a trap; all the diagnostic messages should already
      --  have been generated.
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
      ASIS_UL.Environment.Clean_Up;
      Gnatelim.Options.Gnatelim_Prj.Clean_Up;

   when Ex : Asis.Exceptions.ASIS_Inappropriate_Context          |
             Asis.Exceptions.ASIS_Inappropriate_Container        |
             Asis.Exceptions.ASIS_Inappropriate_Compilation_Unit |
             Asis.Exceptions.ASIS_Inappropriate_Element          |
             Asis.Exceptions.ASIS_Inappropriate_Line             |
             Asis.Exceptions.ASIS_Inappropriate_Line_Number      |
             Asis.Exceptions.ASIS_Failed                         =>

      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
      ASIS_UL.Output.Report_Unhandled_ASIS_Exception (Ex);
      ASIS_UL.Environment.Clean_Up;
      Gnatelim.Options.Gnatelim_Prj.Clean_Up;

   when Ex : others =>
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
      ASIS_UL.Output.Report_Unhandled_Exception (Ex);
      ASIS_UL.Environment.Clean_Up;
      Gnatelim.Options.Gnatelim_Prj.Clean_Up;

      GNAT.OS_Lib.OS_Exit (1);
end Gnatelim.Driver;
