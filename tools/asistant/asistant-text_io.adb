------------------------------------------------------------------------------
--                                                                          --
--             ASIS Tester And iNTerpreter (ASIStant) COMPONENTS            --
--                                                                          --
--                     A S I S T A N T . T E X T _ I O                      --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1997-2012, Free Software Foundation, Inc.         --
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

with Ada.Characters.Handling; use Ada.Characters.Handling;

with ASIStant.DeAlloc;        use  ASIStant.DeAlloc;
with ASIStant.FuncEnum;       use ASIStant.FuncEnum;
with File;                    use File;

package body ASIStant.Text_IO is

------------------------------------------------------------------------------
--  Text I/O supporting scripts and logs
------------------------------------------------------------------------------

   Session_Log : Boolean := False;
   Keybd_Log   : Boolean := False;
   SessionFD   : File_Type;
   KeybdFD     : File_Type;

   procedure Load_Script (S : Wide_String; Pos : Integer);
   --  ??? Should be documented

   procedure ATIPut (S : Wide_String; Level : Integer := 5) is
      L1, L2  : Natural;
      LF : Boolean;
   begin
      L1 := S'First; L2 := L1;
      while L2 <= S'Last loop
         L1 := L2;
         while L2 < S'Last and S (L2) /= Nul_Char loop
            L2 := L2 + 1;
         end loop;
         if S (L2) = Nul_Char then
            L2 := L2 - 1;
            LF := True;
         else
            LF := False;
         end if;

         if Level >= ConsoleOutputLevel then
            Put (S (L1 .. L2));
         end if;

         if Log and Level >= OutputLevel then
            Put (LogFD, S (L1 .. L2));
         end if;

         if not Session_Log then
            Create (SessionFD, Out_File, "session.log");
            Session_Log := True;
         end if;

         Put (SessionFD, S (L1 .. L2));

         L2 := L2 + 2;
         if LF then
            ATINew_Line (Level);
         end if;
      end loop;
   end ATIPut;

   procedure ATINew_Line (Level : Integer := 5) is
   begin
      if Level >= ConsoleOutputLevel then
         New_Line (1);
      end if;
      if Log and Level >= OutputLevel then
         New_Line (LogFD, 1);
      end if;

      New_Line (SessionFD, 1);

   end ATINew_Line;

   procedure ATIPut_Line (S : Wide_String; Level : Integer := 5) is
   begin
      ATIPut (S, Level);
      ATINew_Line (Level);
   end ATIPut_Line;

   procedure ATIPut (I : Integer; Level : Integer := 5) is
   begin
      ATIPut (Integer'Wide_Image (I), Level);
   end ATIPut;

   procedure ATIGet (S : out Wide_String; Last : out Integer) is
   begin
      Get_Line (S, Last);

      if Log then
         ATIPut_Line (S (S'First .. Last), 2);
      else
         ATIPut_Line (S (S'First .. Last), 0);
      end if;

      if not Keybd_Log then
         Create (KeybdFD, Out_File, "input.log");
         Keybd_Log := True;
      end if;

      Put_Line (KeybdFD, S (S'First .. Last));

      Last := Last + 1;
      S (Last) := Nul_Char;

   end ATIGet;

   procedure Load_Script (S : Wide_String; Pos : Integer) is
      L, Last  : Integer;
      ScriptFD : File_Type;
      SP : String_Ptr;
   begin
      L := File_Length (S);
      Open (ScriptFD, In_File, To_String (S), "wcem=8");
      Last := L + 1;
      SP := new Wide_String (1 .. Last);
      L := 1;
      while not End_Of_File (ScriptFD) loop
         Get_Line (ScriptFD, SP.all (L .. Last), L);
         SP.all (L + 1) := Nul_Char;
         L := L + 2;
      end loop;
      Close (ScriptFD);

      ScriptStream (Pos).Text := new Wide_String (1 .. L - 1);
      ScriptStream (Pos).Text.all := SP.all (1 .. L - 1);
      Free (SP);
   end Load_Script;

   procedure OpenScript (S : Wide_String; Mode : Script_Mode) is
   begin
      if Script = MAX_SCRIPTDEPTH then
         ATIPut_Line ("Maximum script depth reached.", 4);
         return;
      end if;

      ScriptStream (Script + 1) := (null, 1, 0, 0, 0);
      Load_Script (S, Script + 1);

--     ASIStant.String_Handling.To_Upper(ScriptStream(Script+1).Text.All);
      if Script > 0 then
         ScriptModes (Script) := ScriptMode;
         if ScriptMode /= INACTIVE then
            ScriptStream (Script) := CurTokStream;
         end if;
      end if;

      Script := Script + 1;
      ScriptMode := Mode;
      CurTokStream := ScriptStream (Script);

      exception
         when others =>
            ATIPut_Line ("Couldn't open script file " & S &
                        ". Check the filename.", 4);
   end OpenScript;

   procedure CloseScript is
   begin
      if Script = 0 then
         return;
      end if;
      Script := Script - 1;
      if Script = 0 or else ScriptModes (Script) = INACTIVE then
         ScriptMode := INACTIVE;
      else
         Free (CurTokStream.Text);
         CurTokStream := ScriptStream (Script);
      end if;
   end CloseScript;

   procedure OpenLog (S : Wide_String) is
   begin
      if Log then
         ATIPut_Line ("Current log closed.", 3);
         CloseLog;
      end if;
      Create (LogFD, Out_File, To_String (S));
      Log := True;
   exception
      when others =>
         ATIPut_Line ("Couldn't open log file " & S &
                      ". Check the filename.", 4);
   end OpenLog;

   procedure CloseLog is
   begin
      Close (LogFD);
      Log := False;
   end CloseLog;

end ASIStant.Text_IO;
