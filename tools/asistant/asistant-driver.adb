------------------------------------------------------------------------------
--                                                                          --
--             ASIS Tester And iNTerpreter (ASIStant) COMPONENTS            --
--                                                                          --
--                      A S I S T A N T . D R I V E R                       --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1997-2013, Free Software Foundation, Inc.         --
--                                                                          --
-- ASIStant is free software; you can redistribute it and/or modify it      --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software Foundation;  either version 2,  or  (at your option)  any later --
-- version. ASIStant is distributed  in the hope  that it will be useful,   --
-- but WITHOUT ANY WARRANTY; without even the implied warranty of MER-      --
-- CHANTABILITY or  FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General  --
-- Public License for more details. You should have received a copy of the  --
-- GNU General Public License distributed with GNAT; see file COPYING. If   --
-- not, write to the Free Software Foundation, 59 Temple Place Suite 330,   --
-- Boston, MA 02111-1307, USA.                                              --
--                                                                          --
-- ASIStant is an evolution of ASIStint tool that was created by            --
-- Vasiliy Fofanov as part of a collaboration between Software Engineering  --
-- Laboratory of the Swiss Federal Institute of Technology in Lausanne,     --
-- Switzerland, and the Scientific Research Computer Center of the Moscow   --
-- University, Russia, supported by the Swiss National Science Foundation   --
-- grant #7SUPJ048247, "Development of ASIS for GNAT with industry quality" --
--                                                                          --
-- ASIStant is distributed as a part of the ASIS implementation for GNAT    --
-- (ASIS-for-GNAT) and is maintained by AdaCore (http://www.adacore.com).   --
--                                                                          --
------------------------------------------------------------------------------

with ASIStant.FuncArr, ASIStant.Interfacing, ASIStant.Text_IO;
use  ASIStant.FuncArr, ASIStant.Interfacing, ASIStant.Text_IO;

with Ada.Command_Line; use Ada.Command_Line;
with Ada.Characters.Handling; use Ada.Characters.Handling;

procedure ASIStant.Driver is

------------------------------------------------------------------------------
--  ASIStant driver procedure
------------------------------------------------------------------------------

   Script_Arg : Natural := 0;
   Level : Integer;
   No_Cmd_Line : Boolean := False;

begin

   for Arg in 1 .. Argument_Count loop
      if Argument (Arg) 'Length > 1 and then Argument (Arg)(1 .. 2) = "-o" then
         Level := Character'Pos (Argument (Arg)(3)) - Character'Pos ('0');
         if Level < 0 or else Level > 6 then
            ATIPut_Line ("Incorrect console output level.", 5);
            return;
         end if;
         ConsoleOutputLevel := Level;
      else
         Script_Arg := Arg;
      end if;
   end loop;

   Initialize_Query_Arrays;

   if Script_Arg = 0 then
      Set_Exit_Status (Exit_Status (Parse_Cmd_Line));
   else
      No_Cmd_Line := (Level = 5);
      --  Special case: -o5 scriptname; asistant will always quit whenever
      --  it would normally switch to interactive mode.
      Set_Exit_Status (Exit_Status (Parse_Cmd_Line
         (To_Wide_String (Argument (Script_Arg)), No_Cmd_Line)));
   end if;

end ASIStant.Driver;
