------------------------------------------------------------------------------
--                                                                          --
--             ASIS Tester And iNTerpreter (ASIStant) COMPONENTS            --
--                                                                          --
--                    A S I S T A N T . B A T C H _ I O                     --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1997-2013, Free Software Foundation, Inc.         --
--                                                                          --
-- ASIStant  is  free  software;  you can  redistribute it and/or modify it --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software Foundation;  either version 2,  or  (at your option)  any later --
-- version.  ASIStant is  distributed  in the hope  that it will be useful, --
-- but  WITHOUT  ANY  WARRANTY;  without  even the implied warranty of MER- --
-- CHANTABILITY or  FITNESS FOR  A PARTICULAR PURPOSE.  See the GNU General --
-- Public License for more details.  You should have received a copy of the --
-- GNU General Public License  distributed with GNAT;  see file COPYING. If --
-- not,  write to the  Free Software Foundation,  51 Franklin Street, Fifth --
-- Floor, Boston, MA 02110-1301, USA.                                       --
--                                                                          --
-- ASIStant  is an evolution of  ASIStint tool that was created by  Vasiliy --
-- Fofanov  as  part  of  a  collaboration  between  Software   Engineering --
-- Laboratory  of the  Swiss  Federal Institute of Technology in  Lausanne, --
-- Switzerland,  and the Scientific Research Computer Center of the  Moscow --
-- University, Russia,  supported by the  Swiss National Science Foundation --
-- grant #7SUPJ048247, "Development of ASIS for GNAT with industry quality" --
--                                                                          --
-- ASIStant  is  distributed as a part of the  ASIS implementation for GNAT --
-- (ASIS-for-GNAT) and is maintained by AdaCore (http://www.adacore.com).   --
------------------------------------------------------------------------------

with Ada.Strings.Wide_Fixed;  use Ada.Strings.Wide_Fixed;

with ASIStant.Call;           use ASIStant.Call;
with ASIStant.Common;         use ASIStant.Common;
with ASIStant.DeAlloc;        use ASIStant.DeAlloc;
with ASIStant.Get;            use ASIStant.Get;
with ASIStant.L_Parser;       use ASIStant.L_Parser;
with ASIStant.Text_IO;        use ASIStant.Text_IO;

package body ASIStant.Batch_IO is

------------------------------------------------------------------------------
--  Logs and scripts interface
------------------------------------------------------------------------------

   procedure Script_IO (NP : Node_Position) is

      N : Node := CurStat.Tree (NP);
      Mode : Script_Mode;

   begin

      case Identify_Function (NP) is
         when FT_EXECUTE | FT_RUN | FT_IRUN =>

            if Identify_Function (NP) = FT_IRUN then
               Mode := INTERACT;
            else
               Mode := NORMAL;
            end if;

            if N.NValue = 0 then
               --  Resume current script
               if Script = 0 then
                  Error (ERR_NOSCRIPT);
               end if;
               if ScriptMode = INACTIVE then
                  CurTokStream := ScriptStream (Script);
               end if;
               ScriptMode := Mode;
            else
               --  Initialize script
               N := CurStat.Tree (N.NValue);
               OpenScript (Get_String (N.NValue).all, Mode);
            end if;

         when FT_PAUSE =>
            if N.NValue = 0 then
               Pause_Script;
            else
               Error (ERR_BADPARAM);
            end if;

         when FT_EXIT =>
            if Script = 0 or ScriptMode = INACTIVE then
               Error (ERR_NOSCRIPT);
            end if;
            --  Deactivate current script
            CurTokStream.Cur_Token_Start := 0;

         when others => Error (ERR_INTERNAL);
      end case;

   end Script_IO;

   procedure Log_IO (NP : Node_Position) is

      N : Node := CurStat.Tree (NP);

   begin

      if N.NValue = 0 then
         --  Close current log
         if Log then
            CloseLog;
         else
            Error (ERR_NEEDFILENAME);
         end if;
      else
         --  Open new log
         N := CurStat.Tree (N.NValue);
         OpenLog (Get_String (N.NValue).all);
      end if;

   end Log_IO;

   Cur_Line  : Wide_String (1 .. 100);

   procedure Get_Next_Cmd is

      Last : Integer;

   begin

      if Script > 0 and ScriptMode /= INACTIVE then
         CloseScript;
      end if;
      if Script = 0 or ScriptMode = INACTIVE then
         DeAlloc.Free (CurTokStream.Text);
         CurTokStream := (null, 1, 0, 0, 0);
         ATIPut (">", 3);
         ATIGet (Cur_Line, Last);
         Cur_Line (Last + 1) := Nul_Char;
         CurTokStream.Text := new Wide_String (1 .. Last + 1);
         Move (Cur_Line (1 .. Last + 1), CurTokStream.Text.all);
--      To_Upper(CurTokStream.Text.All);
      end if;
   end Get_Next_Cmd;

   procedure Pause_Script is
   begin
      if Script = 0 then
         Error (ERR_NOSCRIPT);
      end if;
      if ScriptMode /= INACTIVE then
         ScriptMode := INACTIVE;
         ScriptStream (Script) := CurTokStream;
         CurTokStream.Text := null;
         Get_Next_Cmd;
      end if;
   end Pause_Script;

end ASIStant.Batch_IO;
