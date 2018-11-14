------------------------------------------------------------------------------
--                                                                          --
--                          GNATCHECK COMPONENTS                            --
--                                                                          --
--                     G N A T C H E C K . D R I V E R                      --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                     Copyright (C) 2004-2018, AdaCore                     --
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
-- GNATCHECK is maintained by AdaCore (http://www.adacore.com).             --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Calendar;
with Ada.Command_Line;

with Asis.Exceptions;

with ASIS_UL.Debug;
with ASIS_UL.Common;
with ASIS_UL.Environment;
with ASIS_UL.Global_State;
with ASIS_UL.Options;
with ASIS_UL.Output;
with ASIS_UL.Projects.Aggregate;
with ASIS_UL.Source_Table;
with ASIS_UL.Source_Table.Gnatcheck_Processing;

with Namet;
with Snames;

with Gnatcheck.Options;
with Gnatcheck.Projects;

procedure Gnatcheck.Driver is
   --  For the current state of the Asis Utility Library, GNATCHECK seems to be
   --  too complicated to be based on some standard driver. The main problem is
   --  that at some point we will have to add processing of the compiler
   --  warnings that goes beyond the typical framework for the ASIS processing.

   Time_Start : constant Ada.Calendar.Time := Ada.Calendar.Clock;
   Exect_Time :          Duration;
   use type Ada.Calendar.Time;

begin
   --  The following allows to use the front-end Snames routines when analyzing
   --  rule parameters
   Namet.Initialize;
   Snames.Initialize;

   ASIS_UL.Environment.Initialize (Gnatcheck.Options.Gnatcheck_Prj);

   if ASIS_UL.Options.Nothing_To_Do then
      ASIS_UL.Environment.Clean_Up;
      Gnatcheck.Projects.Clean_Up (Gnatcheck.Options.Gnatcheck_Prj);
   else
      ASIS_UL.Source_Table.Gnatcheck_Processing.Initialize;

      if ASIS_UL.Options.In_Aggregate_Project then
         --  In this case we spawn gnatcheck for each project being aggregated
         ASIS_UL.Projects.Aggregate.Process_Aggregated_Projects
           (Gnatcheck.Options.Gnatcheck_Prj);
      elsif ASIS_UL.Options.Incremental_Mode then
         --  In Incremental_Mode, we invoke the builder instead of doing the
         --  normal processing. The inner invocations of gnatcheck invoked by
         --  the builder will do the normal tool processing.

         ASIS_UL.Environment.Call_Builder;
         ASIS_UL.Source_Table.Gnatcheck_Processing.Delete_ALI_Files;
      else
         ASIS_UL.Source_Table.Gnatcheck_Processing.Process_Sources;
         ASIS_UL.Source_Table.Gnatcheck_Processing.Finalize;
      end if;

      ASIS_UL.Environment.Clean_Up;
      Gnatcheck.Projects.Clean_Up (Gnatcheck.Options.Gnatcheck_Prj);

      ASIS_UL.Source_Table.Gnatcheck_Processing.Define_Exit_Code;

      if ASIS_UL.Options.Compute_Timing then
         Exect_Time := Ada.Calendar.Clock - Time_Start;
         ASIS_UL.Output.Info ("Execution time:" & Exect_Time'Img);
      end if;

      if not ASIS_UL.Options.In_Aggregate_Project then
         ASIS_UL.Global_State.Print_Global_Structure;
      end if;

   end if;

   ASIS_UL.Source_Table.Gnatcheck_Processing.Exit_Gnatcheck
     (ASIS_UL.Source_Table.Gnatcheck_Processing.Exit_Code);

exception
   when ASIS_UL.Common.Fatal_Error =>
      --  Just a trap; all the diagnostic messages should already
      --  have been generated. But just in case:

      if ASIS_UL.Debug.Debug_Flag_L then

         if ASIS_UL.Debug.Debug_Flag_G then
            ASIS_UL.Global_State.Print_Global_Structure;
         end if;

         if ASIS_UL.Debug.Debug_Flag_S then
            ASIS_UL.Source_Table.Source_Table_Debug_Image;
         end if;

      end if;

      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
      ASIS_UL.Environment.Clean_Up;
      Gnatcheck.Projects.Clean_Up (Gnatcheck.Options.Gnatcheck_Prj);

      ASIS_UL.Source_Table.Gnatcheck_Processing.Exit_Gnatcheck
        (ASIS_UL.Source_Table.Gnatcheck_Processing.E_Fatal);

   when Ex : Asis.Exceptions.ASIS_Inappropriate_Context          |
             Asis.Exceptions.ASIS_Inappropriate_Container        |
             Asis.Exceptions.ASIS_Inappropriate_Compilation_Unit |
             Asis.Exceptions.ASIS_Inappropriate_Element          |
             Asis.Exceptions.ASIS_Inappropriate_Line             |
             Asis.Exceptions.ASIS_Inappropriate_Line_Number      |
             Asis.Exceptions.ASIS_Failed                         =>

      if ASIS_UL.Debug.Debug_Flag_L then

         if ASIS_UL.Debug.Debug_Flag_G then
            ASIS_UL.Global_State.Print_Global_Structure;
         end if;

         if ASIS_UL.Debug.Debug_Flag_S then
            ASIS_UL.Source_Table.Source_Table_Debug_Image;
         end if;

      end if;

      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
      ASIS_UL.Output.Report_Unhandled_ASIS_Exception (Ex);
      ASIS_UL.Environment.Clean_Up;
      Gnatcheck.Projects.Clean_Up (Gnatcheck.Options.Gnatcheck_Prj);

      ASIS_UL.Source_Table.Gnatcheck_Processing.Exit_Gnatcheck
        (ASIS_UL.Source_Table.Gnatcheck_Processing.E_Fatal);

   when Ex : others =>
      if ASIS_UL.Debug.Debug_Flag_L then

         if ASIS_UL.Debug.Debug_Flag_G then
            ASIS_UL.Global_State.Print_Global_Structure;
         end if;

         if ASIS_UL.Debug.Debug_Flag_S then
            ASIS_UL.Source_Table.Source_Table_Debug_Image;
         end if;

      end if;

      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
      ASIS_UL.Output.Report_Unhandled_Exception (Ex);
      ASIS_UL.Environment.Clean_Up;
      Gnatcheck.Projects.Clean_Up (Gnatcheck.Options.Gnatcheck_Prj);

      ASIS_UL.Source_Table.Gnatcheck_Processing.Exit_Gnatcheck
        (ASIS_UL.Source_Table.Gnatcheck_Processing.E_Fatal);

end Gnatcheck.Driver;
