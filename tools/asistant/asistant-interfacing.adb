------------------------------------------------------------------------------
--                                                                          --
--             ASIS Tester And iNTerpreter (ASIStant) COMPONENTS            --
--                                                                          --
--                  A S I S T A N T . I N T E R F A C I N G                 --
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

with Ada.Exceptions;
with Ada.IO_Exceptions;
with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Text_IO;

with Asis.Implementation;

with ASIStant.Text_IO;  use ASIStant.Text_IO;
with ASIStant.Batch_IO; use ASIStant.Batch_IO;

with ASIStant.S_Parser; use ASIStant.S_Parser;
with ASIStant.L_Parser; use ASIStant.L_Parser;
with ASIStant.Common;   use ASIStant.Common;
with ASIStant.Exec;     use ASIStant.Exec;

package body ASIStant.Interfacing is

------------------------------------------------------------------------------
--  This package is an upper-level interface to ASIStant
------------------------------------------------------------------------------

   function Parse_Cmd_Line (ScriptName : Wide_String := "";
                            No_Cmd_Line : Boolean := False) return Integer is
      I, J : Integer;
      C : Character;
   begin

      Print_ASIStant_Header;
      ATIPut ("  Asis Version: ", 3);
      ATIPut_Line (Asis.Implementation.ASIS_Version, 3);
      ATINew_Line (3);

      if ScriptName'Length > 0 then
         OpenScript (ScriptName, NORMAL);
      end if;

      loop
         begin

            if not Is_Active (CurTokStream) then
               if No_Cmd_Line and then Script = 1 and then
                 ScriptMode /= INACTIVE
               then
                  --  The topmost script has just finished
                  return 1;
               end if;
               Get_Next_Cmd;
            end if;

            if ScriptMode = INTERACT then
               Ada.Text_IO.Get_Immediate (C);

               case C is
                  when 'C' | 'c' => --  Continue normally
                     ScriptMode := NORMAL;
                  when 'P' | 'p' => --  Pause script
                     Pause_Script;
                  when 'Q' | 'q' =>  --  Exit script
                     raise ASIStant_ERROR;
                  when others =>
                     null;
               end case;
            end if;

            Reset_Tree;

            I := CurTokStream.Prev_Token_End;

            if I < 1 then
               I := 1;
            else
               I := Next_Line (CurTokStream.Text.all
                       (I .. CurTokStream.Text.all'Last));
               if I = 0 then
                  I := 1;
               end if;
            end if;

            --  Parse statement
            Get_Stmt (CurTokStream);

            if CurTokStream.Prev_Token_End > 0 then
               J := Next_Line (CurTokStream.Text.all
                     (CurTokStream.Prev_Token_End ..
                        CurTokStream.Text.all'Last)) - 1;

               if I > J then
                  J := CurTokStream.Text'Last;
               end if;

               if ScriptMode /= INACTIVE then
                  ATIPut (CurTokStream.Text (I .. J), 2);
               else
                  ATIPut (CurTokStream.Text (I .. J), 0);
               end if;

               if CurStat.Free > 1 then
                  Exec_ATI_Command (1);

                  if Exec_Result /= -1 then
                     return Exec_Result;
                  end if;

               end if;
            end if;
         exception

            when Ada.IO_Exceptions.End_Error =>
               --  No more input. Exit to avoid endless loop when fed
               --  with a file on standard input.
               return 0;

            when ASIStant_ASIS_ERROR =>
               --  all handling is done in ASIStant.Call;
               null;

            when ASIStant_ERROR =>
               if No_Cmd_Line then
                  return 1;
               end if;
               --  Purge script if active, otherwise purge command line
               if ScriptMode /= INACTIVE then
                  CloseScript;
               else
                  CurTokStream.Cur_Token_Start := 0;
               end if;

            when E : others =>
               ATIPut_Line (
                  "Raised unhandled exception " &
                   To_Wide_String (Ada.Exceptions.Exception_Name (E)), 5);
               ATIPut_Line (
                  "   " &
                   To_Wide_String (Ada.Exceptions.Exception_Message (E)), 5);
               if No_Cmd_Line then
                  return 1;
               end if;
               --  Purge script if active, otherwise purge command line
               if ScriptMode /= INACTIVE then
                  CloseScript;
               else
                  CurTokStream.Cur_Token_Start := 0;
               end if;
         end;
      end loop;

   end Parse_Cmd_Line;

end ASIStant.Interfacing;
