------------------------------------------------------------------------------
--                                                                          --
--             ASIS Tester And iNTerpreter (ASIStant) COMPONENTS            --
--                                                                          --
--                    A S I S T A N T . L _ P A R S E R                     --
--                                                                          --
--                                 S p e c                                  --
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

with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Strings.Wide_Maps; use Ada.Strings.Wide_Maps;
with ASIStant.FuncEnum; use ASIStant.FuncEnum;

package ASIStant.L_Parser is

------------------------------------------------------------------------------
--  This package provides token parsing for the ASIStant language
------------------------------------------------------------------------------

   CS_White_Spaces     : Wide_Character_Set :=
       To_Set (To_Wide_String (ASCII.NUL & ASCII.HT & ' '));
   CS_Special_Tokens   : Wide_Character_Set :=
       To_Set ("""-+*/;,()");
   CS_Token_Separators : Wide_Character_Set :=
       CS_White_Spaces or CS_Special_Tokens;
   --  Sets of symbols that require special processing and/or separate tokens

   type Token_Stream is
      record
         Text             : String_Ptr;
         Cur_Token_Start  : Natural := 0;
         Cur_Token_End    : Natural := 0;
         Prev_Token_Start : Natural := 0;
         Prev_Token_End   : Natural := 0;
      end record;
      --  this structure is used for token parsing. Text accesses the string
      --  being parsed, other fields are necessary to keep track of the current
      --  and previous tokens. Initial value (Cur_Token_Start,Cur_Token_End)
      --  of an active token stream should be (1,0)

   CurTokStream : Token_Stream := (null, 0, 0, 0, 0);

   procedure Next_Token (TS : in out Token_Stream);
   --  advances to the next token in stream

   procedure Prev_Token (TS : in out Token_Stream);
   --  undoes the last Next_Token call, but may retreat only one token back

   function  Cur_Token  (TS : Token_Stream) return Wide_String;
   --  returns current token in stream

   function  Is_ID      (TS : Token_Stream) return Boolean;
   --  returns True if the current token is correct ID, False otherwise;
   --  the correct ID is a combination of letters, digits and underline
   --  characters starting with a letter

   function  Is_Active  (TS : Token_Stream) return Boolean;
   --  returns True if the token stream still has tokens, False otherwise

   function Next_Line   (S : Wide_String) return Natural;
   --  Skips to next line in token stream

end ASIStant.L_Parser;
