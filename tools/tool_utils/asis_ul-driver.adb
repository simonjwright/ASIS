------------------------------------------------------------------------------
--                                                                          --
--                     ASIS UTILITY LIBRARY COMPONENTS                      --
--                                                                          --
--                       A S I S _ U L . D R I V E R                        --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                     Copyright (C) 2013-2017, AdaCore                     --
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
with Ada.Calendar;
with Ada.Command_Line;

with GNAT.OS_Lib;

with Asis.Exceptions;

with ASIS_UL.Common;
with ASIS_UL.Output;
with ASIS_UL.Environment;
with ASIS_UL.Options;
with ASIS_UL.Source_Table.Processing;
with ASIS_UL.Utilities;

procedure ASIS_UL.Driver
  (Prj : in out ASIS_UL.Projects.Arg_Project_Type'Class)
is
   Time_Start : constant Ada.Calendar.Time := Ada.Calendar.Clock;
   Exect_Time :          Duration;
   use type Ada.Calendar.Time;
begin
   ASIS_UL.Environment.Initialize (Prj);
   ASIS_UL.Source_Table.Processing.Initialize;

   --  In Incremental_Mode, we invoke the builder instead of doing the normal
   --  tool processing. The inner invocations of this tool invoked by the
   --  builder will do the normal tool processing.

   if ASIS_UL.Options.Incremental_Mode then
      ASIS_UL.Environment.Call_Builder;
   else
      ASIS_UL.Source_Table.Processing.Process_Sources;
   end if;

   ASIS_UL.Source_Table.Processing.Finalize;

   ASIS_UL.Environment.Clean_Up;

   if ASIS_UL.Options.Compute_Timing then
      Exect_Time := Ada.Calendar.Clock - Time_Start;
      ASIS_UL.Output.Info ("Execution time:" & Exect_Time'Img);
   end if;

   ASIS_UL.Output.Close_Log_File;
   Prj.Clean_Up;

   if not ASIS_UL.Options.Incremental_Mode then
      if not ASIS_UL.Source_Table.Processing
        .All_Files_Successfully_Processed
      then
         GNAT.OS_Lib.OS_Exit (1);
      end if;
   end if;

   Utilities.Main_Done := True;

exception
   when ASIS_UL.Common.Fatal_Error =>
      --  Just a trap; all the diagnostic messages should already
      --  have been generated.
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
      ASIS_UL.Environment.Clean_Up;
      Prj.Clean_Up;
      GNAT.OS_Lib.OS_Exit (1);

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
      Prj.Clean_Up;
      GNAT.OS_Lib.OS_Exit (1);

   when Ex : others =>
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
      ASIS_UL.Output.Report_Unhandled_Exception (Ex);
      ASIS_UL.Environment.Clean_Up;
      Prj.Clean_Up;
      GNAT.OS_Lib.OS_Exit (1);

end ASIS_UL.Driver;
