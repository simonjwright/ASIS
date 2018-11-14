------------------------------------------------------------------------------
--                                                                          --
--                           GNATTEST COMPONENTS                            --
--                                                                          --
--                      G N A T T E S T . D R I V E R                       --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                     Copyright (C) 2011-2017, AdaCore                     --
--                                                                          --
-- GNATTEST  is  free  software;  you  can redistribute it and/or modify it --
-- under terms of the  GNU  General Public License as published by the Free --
-- Software  Foundation;  either  version  2, or (at your option) any later --
-- version.  GNATTEST  is  distributed  in the hope that it will be useful, --
-- but  WITHOUT  ANY  WARRANTY;   without  even  the  implied  warranty  of --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General --
-- Public License for more details.  You should have received a copy of the --
-- GNU  General  Public License distributed with GNAT; see file COPYING. If --
-- not, write to the  Free  Software  Foundation, 51 Franklin Street, Fifth --
-- Floor, Boston, MA 02110-1301, USA.,                                      --
--                                                                          --
-- GNATTEST is maintained by AdaCore (http://www.adacore.com).              --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Command_Line;

with Asis.Exceptions;

with GNATtest.Aggregator;
with GNATtest.Environment;
with GNATtest.Skeleton.Generator;
with GNATtest.Harness.Generator;
with GNATtest.Common;
with GNATtest.Options;
with GNATtest.Mapping;

procedure GNATtest.Driver is
begin
   GNATtest.Environment.Initialize;
   case GNATtest.Options.GNATtest_Mode is
      when GNATtest.Options.Generation =>
         if not GNATtest.Options.Harness_Only then
            GNATtest.Skeleton.Generator.Process_Sources;
         end if;
         GNATtest.Harness.Generator.Process_Sources;
         GNATtest.Mapping.Generate_Mapping_File;
      when GNATtest.Options.Aggregation =>
         GNATtest.Aggregator.Process_Drivers_List;
   end case;
   GNATtest.Environment.Clean_Up;
   if GNATtest.Options.Strict_Execution
     and then GNATtest.Environment.Source_Compilation_Failed
   then
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
   else
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Success);
   end if;
exception
   when GNATtest.Common.Fatal_Error =>
      --  Just a trap; all the diagnostic messages should already
      --  have been generated.
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
      GNATtest.Environment.Clean_Up;

   when Ex : Asis.Exceptions.ASIS_Inappropriate_Context          |
             Asis.Exceptions.ASIS_Inappropriate_Container        |
             Asis.Exceptions.ASIS_Inappropriate_Compilation_Unit |
             Asis.Exceptions.ASIS_Inappropriate_Element          |
             Asis.Exceptions.ASIS_Inappropriate_Line             |
             Asis.Exceptions.ASIS_Inappropriate_Line_Number      |
             Asis.Exceptions.ASIS_Failed                         =>

      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
      GNATtest.Common.Report_Unhandled_ASIS_Exception (Ex);
      GNATtest.Environment.Clean_Up;

   when Ex : others =>
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
      GNATtest.Common.Report_Unhandled_Exception (Ex);
      GNATtest.Environment.Clean_Up;

end GNATtest.Driver;
