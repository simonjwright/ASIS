------------------------------------------------------------------------------
--                                                                          --
--             ASIS Tester And iNTerpreter (ASIStant) COMPONENTS            --
--                                                                          --
--                    A S I S T A N T . L _ P A R S E R                     --
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

with Ada.Strings;             use Ada.Strings;
with Ada.Strings.Wide_Fixed;  use Ada.Strings.Wide_Fixed;

with ASIStant.Common;          use ASIStant.Common;
with ASIStant.String_Handling; use ASIStant.String_Handling;

package body ASIStant.L_Parser is

------------------------------------------------------------------------------
--  This package provides token parsing for the ASIStant language interpreter
------------------------------------------------------------------------------

   procedure Next_Token (TS : in out Token_Stream) is
   --  advances to the next token in stream

      TStart : Natural;
      TEnd   : Natural := TS.Cur_Token_End;

   begin

      TS.Prev_Token_Start := TS.Cur_Token_Start;
      TS.Prev_Token_End   := TS.Cur_Token_End;

      loop

         if TEnd = 0 and TS.Cur_Token_Start = 0 then
            return;
         end if;

         TStart := TEnd + 1;

         while TStart < TS.Text'Last
               and then Is_In (TS.Text (TStart), CS_White_Spaces)
         loop
            TStart := TStart + 1;
         end loop;

         if TStart >= TS.Text'Last then
            TS.Cur_Token_Start := 0;
            TS.Cur_Token_End   := 0;
            return;
         end if;

         if not Is_In (TS.Text (TStart), CS_Special_Tokens) then
            Find_Token (TS.Text (TStart .. TS.Text'Last),
                        CS_Token_Separators,
                        Outside,
                        TStart,
                        TEnd);
            exit;
         end if;

         TEnd := TStart;

         if TStart < TS.Text'Last and then TS.Text (TStart) = '-' then
            if TS.Text (TStart + 1) = '-' then
               --  Ada-style comments
               TEnd := Next_Line (TS.Text (TEnd .. TS.Text'Last)) - 1;
            elsif TS.Text (TStart + 1) in '0' .. '9' then
               --  Negative integer
               Find_Token (TS.Text (TStart + 1 .. TS.Text'Last),
                           CS_Token_Separators,
                           Outside,
                           TStart,
                           TEnd);
               TStart := TStart - 1;
               exit;
            else
               Error (ERR_BADINTEGER);
            end if;
         elsif TStart < TS.Text'Last and then TS.Text (TStart) = '"' then
            TEnd := TStart + 1;
            loop
               TEnd := Find_Symbol (TS.Text (TEnd .. TS.Text'Last), '"');
               if TEnd = 0 then
                  Error (ERR_BADSTRING);
               end if;
               exit when TEnd = TS.Text'Last or else TS.Text (TEnd + 1) /= '"';
               --  Otherwise, this is an Ada-style quote in string
               TEnd := TEnd + 2;
            end loop;
            exit;
         else
            exit;
         end if;

      end loop;

      TS.Cur_Token_Start := TStart;
      TS.Cur_Token_End   := TEnd;

   end Next_Token;

   procedure Prev_Token (TS : in out Token_Stream) is
   --  undoes the last Next_Token call, but may retreat only one token back

   begin

      if TS.Prev_Token_Start = 0 then
         return;
      end if;

      TS.Cur_Token_Start  := TS.Prev_Token_Start;
      TS.Cur_Token_End    := TS.Prev_Token_End;
      TS.Prev_Token_Start := 0;
      TS.Prev_Token_End   := 0;

   end Prev_Token;

   function Cur_Token (TS : Token_Stream) return Wide_String is
   --  returns current token in stream
   begin

      return TS.Text (TS.Cur_Token_Start .. TS.Cur_Token_End);

   end Cur_Token;

   function  Is_ID (TS : Token_Stream) return Boolean is
   --  returns True if the current token is correct ID, False otherwise;
   --  the correct ID is a combination of letters, digits and underline
   --  characters starting with a letter or underline.
   --  Side effect: '.' is replaced by '_'
   begin

      if not Is_Active (TS) then
         return False;
      end if;

      if TS.Text (TS.Cur_Token_Start) not in 'A' .. 'Z'
         and TS.Text (TS.Cur_Token_Start) not in 'a' .. 'z'
      then
         return False;
      end if;

      for I in TS.Cur_Token_Start + 1 .. TS.Cur_Token_End loop

         if TS.Text (I) /= '_' and TS.Text (I) /= '.' and
            TS.Text (I) not in 'A' .. 'Z' and
            TS.Text (I) not in 'a' .. 'z' and
            TS.Text (I) not in '0' .. '9'
         then
            return False;
         end if;

         if TS.Text (I) = '.' then
            TS.Text (I) := '_';
         end if;
      end loop;

      return True;

   end Is_ID;

   function  Is_Active  (TS : Token_Stream) return Boolean is
   --  returns True if the token stream still has tokens, False otherwise
   begin
      return TS.Cur_Token_Start /= 0;
   end Is_Active;

   function Next_Line (S : Wide_String) return Natural is
   --  Skips to next line in stream (warning: when there are no more lines in
   --  stream, the value is returned that is outside the range. Boundary check
   --  should be provided outside)
   begin

      for i in S'Range loop
         if S (i) = To_Wide_Character (ASCII.NUL) then
            return i + 1;
         end if;
      end loop;

      return 0;

   end Next_Line;

end ASIStant.L_Parser;
