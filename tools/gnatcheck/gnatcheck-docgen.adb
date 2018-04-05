------------------------------------------------------------------------------
--                                                                          --
--                          GNATCHECK COMPONENTS                            --
--                                                                          --
--                     G N A T C H E C K . D O C G E N                      --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                     Copyright (C) 2008-2014, AdaCore                     --
--                                                                          --
-- GNATCHECK  is  free  software;  you can redistribute it and/or modify it --
-- under terms of the  GNU  General Public License as published by the Free --
-- Software Foundation;  either version 2, or ( at your option)  any  later --
-- version.  GNATCHECK  is  distributed in the hope that it will be useful, --
-- but  WITHOUT  ANY  WARRANTY;   without  even  the  implied  warranty  of --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General --
-- Public License for more details.  You should have received a copy of the --
-- GNU  General Public License distributed with GNAT; see file  COPYING. If --
-- not,  write to the  Free Software Foundation,  51 Franklin Street, Fifth --
-- Floor, Boston, MA 02110-1301, USA.                                       --
--                                                                          --
-- GNATCHECK is maintained by AdaCore (http://www.adacore.com).             --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Command_Line;
with GNAT.Command_Line;  use GNAT.Command_Line;
with GNAT.Directory_Operations;

with ASIS_UL.Common;
with ASIS_UL.Output;

with Gnatcheck.Documentation;
with Gnatcheck.Rules;

procedure Gnatcheck.Docgen is
   Success : Boolean;
begin
   --  Do we need any option here and any conditional generation???
   Initialize_Option_Scan;

   begin
      loop
         case GNAT.Command_Line.Getopt
           ("all rs= req qst")
         is
            when ASCII.NUL =>
               exit;
            when 'a' =>
               Gnatcheck.Documentation.Set_Max_Details;
            when 'r' =>

               if Full_Switch = "rs" then
                  Gnatcheck.Documentation.Process_Rule_Status_Par
                    (Parameter, Success);

                  if not Success then
                     ASIS_UL.Output.Error
                       ("wrong parameter for -rs option : " & Parameter);
                     raise ASIS_UL.Common.Fatal_Error;
                  end if;

               elsif Full_Switch = "req" then
                  Gnatcheck.Documentation.Add_Requirements := True;
               end if;

            when 'q' =>
               if Full_Switch = "qst" then
                  Gnatcheck.Documentation.Add_Questions := True;
               end if;
            when others =>
               raise ASIS_UL.Common.Fatal_Error;
         end case;
      end loop;
   exception
      when GNAT.Command_Line.Invalid_Switch =>
         ASIS_UL.Output.Error ("invalid switch : " & Full_Switch);
         raise ASIS_UL.Common.Fatal_Error;

      when GNAT.Command_Line.Invalid_Parameter =>
         ASIS_UL.Output.Error ("missing parameter for: -" & Full_Switch);
         raise ASIS_UL.Common.Fatal_Error;
   end;
   --  Go to the docs dir:

   begin
      GNAT.Directory_Operations.Change_Dir ("docs");
   exception
      when GNAT.Directory_Operations.Directory_Error =>
         ASIS_UL.Output.Error ("tool can be used only in gnatcheck " &
                               "source directory");
         raise ASIS_UL.Common.Fatal_Error;
   end;

   --  First, get the list of currently available rules:
   Gnatcheck.Rules.Register_Rules;

   Gnatcheck.Documentation.Set_Doc_File;
   Gnatcheck.Documentation.Compose_Documentation;
   Gnatcheck.Documentation.Clean_Up;

   GNAT.Directory_Operations.Change_Dir ("..");
exception
   when ASIS_UL.Common.Fatal_Error =>
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
      Gnatcheck.Documentation.Clean_Up;

   when Ex : others =>
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
      ASIS_UL.Output.Report_Unhandled_Exception (Ex);
      Gnatcheck.Documentation.Clean_Up;
end Gnatcheck.Docgen;
