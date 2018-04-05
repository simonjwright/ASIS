------------------------------------------------------------------------------
--                                                                          --
--             ASIS Tester And iNTerpreter (ASIStant) COMPONENTS            --
--                                                                          --
--                      A S I S T A N T . C O M M O N                       --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1997-2013, Free Software Foundation, Inc.         --
--                                                                          --
-- ASIStant  is  free  software;  you  can redistribute it and/or modify it --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software Foundation;  either version 2,  or  (at your option)  any later --
-- version.  ASIStant  is distributed  in the hope  that it will be useful, --
-- but  WITHOUT  ANY  WARRANTY;  without  even the implied warranty of MER- --
-- CHANTABILITY or  FITNESS FOR A PARTICULAR PURPOSE.  See  the GNU General --
-- Public License for  more details. You should have received a copy of the --
-- GNU  General Public License distributed with GNAT;  see file COPYING. If --
-- not,  write to the  Free Software Foundation,  51 Franklin Street, Fifth --
-- Floor, Boston, MA 02110-1301, USA.                                       --
--                                                                          --
-- ASIStant  is  an  evolution of ASIStint tool that was created by Vasiliy --
-- Fofanov   as  part  of  a  collaboration  between  Software  Engineering --
-- Laboratory  of  the  Swiss  Federal Institute of Technology in Lausanne, --
-- Switzerland,  and  the Scientific Research Computer Center of the Moscow --
-- University,  Russia,  supported by the Swiss National Science Foundation --
-- grant #7SUPJ048247, "Development of ASIS for GNAT with industry quality" --
--                                                                          --
-- ASIStant  is  distributed  as a part of the ASIS implementation for GNAT --
-- (ASIS-for-GNAT) and is maintained by AdaCore (http://www.adacore.com).   --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Characters.Handling;

with ASIStant.Text_IO; use ASIStant.Text_IO;

with Gnatvsn;

package body ASIStant.Common is

------------------------------------------------------------------------------
--  This package contains definitions common to ASIStant packages.
------------------------------------------------------------------------------

   function Error_Message (E : Error_Type) return Wide_String;
   --  Maps Error_Type to error messages

   function Error_Message (E : Error_Type) return Wide_String is
   --  Maps Error_Type to error messages
   begin
      case E is
         when ERR_INTERNAL =>
            return "Internal error";
         when ERR_NOTIMPLEMENTED =>
            return "Not implemented";
         when ERR_NOTSUPPORTED =>
            return "Not supported";

         when ERR_NOSCRIPT =>
            return "No active script";
         when ERR_NEEDFILENAME =>
            return "Filename required";

         when ERR_BADBOOLEAN =>
            return "Invalid boolean value";
         when ERR_BADEXPR =>
            return "Invalid expression";
         when ERR_BADID =>
            return "Invalid identifier";
         when ERR_BADINTEGER =>
            return "Invalid integer";
         when ERR_BADLISTELEM =>
            return "Invalid index";
         when ERR_BADPARAM =>
            return "Invalid parameter";
         when ERR_BADPARAMLIST =>
            return "Invalid list of parameters";
         when ERR_BADSTRING =>
            return "Invalid string";
         when ERR_BADVARNAME =>
            return "There is a built-in query with the name";
         when ERR_BADVARTYPE =>
            return "Wrong variable type";
         when ERR_NEEDCHAR =>
            return "Character required";
         when ERR_NEEDFUNCTION =>
            return "Function required";
         when ERR_NEEDPARAM =>
            return "Parameter required";
         when ERR_NOTINRANGE =>
            return "Value out of range";
         when ERR_TOOMANYPARAMS =>
            return "Too many parameters";
         when ERR_UNKNOWNVAR =>
            return "Unknown variable or query";

         when ERR_TABLEFULL =>
            return "Variable table full";

         when ERR_UNKNOWNSYNTAX =>
            return "Unknown syntax for query";
         when ERR_UNKNOWNQUERY =>
            return "Unknown query";
      end case;
   end Error_Message;

   procedure Error (ErrNo :  Error_Type;
                    ErrStr : Wide_String  := "";
                    Level :  Natural := 5) is
   begin
      LastErr := ErrNo;

      if DebugPrint then
         ATIPut ("ASIStant error: " & Error_Message (ErrNo), Level);

         if ErrStr /= "" then
            ATIPut_Line (" " & ErrStr & ".", Level);
         else
            ATIPut_Line (".", Level);
         end if;

      end if;

      raise ASIStant_ERROR;
   end Error;

   procedure Print_ASIStant_Header is
   begin
      ATIPut_Line ("ASIStant - ASIS Tester And iNTerpreter, v1.5", 3);

      ATIPut ("(C) 1997-", 3);
      ATIPut
        (Ada.Characters.Handling.To_Wide_String (Gnatvsn.Current_Year), 3);
      ATIPut_Line (", Free Software Foundation, Inc.", 3);
   end Print_ASIStant_Header;

end ASIStant.Common;
