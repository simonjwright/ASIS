------------------------------------------------------------------------------
--                                                                          --
--             ASIS Tester And iNTerpreter (ASIStant) COMPONENTS            --
--                                                                          --
--                     A S I S T A N T . T E X T _ I O                      --
--                                                                          --
--                                 S p e c                                  --
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

with Ada.Characters.Handling;
with Ada.Wide_Text_IO;  use Ada.Wide_Text_IO;

with ASIStant.Common, ASIStant.L_Parser;
use  ASIStant.Common, ASIStant.L_Parser;

package ASIStant.Text_IO is

------------------------------------------------------------------------------
--  Text I/O supporting scripts and logs
------------------------------------------------------------------------------

   OutputLevel : Integer := 1;
   --  Output Level can be 0 through 5 and is changed with LOGLEVEL command.
   --  All output with level of not less than OutputLevel is sent to current
   --  log, all other is only displayed if the ConsoleOutputLevel is not more
   --  than the OutputLevel. Levels (default is 1):
   --  Level 5 (always sent to log): ASIS exception handlers; PRINT output.
   --  Level 4: Monitor warnings.
   --  Level 3: Monitor standard messages.
   --  Level 2: Command prompt; input backup.
   --  Level 0: Internal debugging information.
   --  ASIStant keeps track of all I/O throughout the session in the file
   --  SESSION.LOG. OutputLevel is ignored for this file and all level 0
   --  output will be recorded. Therefore, it is not recommended to set
   --  OutputLevel to 0

   ConsoleOutputLevel : Integer := 1;
   --  ConsoleOutputLevel can be 0 through 6 and is changed with -o command
   --  line parameter. All output with level of not less than
   --  ConsoleOutputLevel is displayed. If it is set to 6, nothing is displayed

   procedure ATIGet      (S : out Wide_String; Last : out Integer);
   procedure ATIPut      (S : Wide_String; Level : Integer := 5);
   procedure ATINew_Line (Level : Integer := 5);
   procedure ATIPut_Line (S : Wide_String; Level : Integer := 5);
   procedure ATIPut      (I : Integer; Level : Integer := 5);

   type Script_Mode is (INACTIVE, NORMAL, INTERACT);
   procedure OpenScript  (S : Wide_String; Mode : Script_Mode);
   procedure CloseScript;
   procedure OpenLog     (S : Wide_String);
   procedure CloseLog;

   Script         : Natural := 0;
   ScriptStream   : array (1 .. MAX_SCRIPTDEPTH) of Token_Stream;
   Log            : Boolean := False;
   LogFD          : File_Type;

   ScriptMode     : Script_Mode := INACTIVE;
   ScriptModes    : array (1 .. MAX_SCRIPTDEPTH) of Script_Mode;

   Nul_Char       : Wide_Character :=
      Ada.Characters.Handling.To_Wide_Character (ASCII.NUL);

end ASIStant.Text_IO;
