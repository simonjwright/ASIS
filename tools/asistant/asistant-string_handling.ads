------------------------------------------------------------------------------
--                                                                          --
--             ASIS Tester And iNTerpreter (ASIStant) COMPONENTS            --
--                                                                          --
--              A S I S T A N T . S T R I N G _ H A N D L I N G             --
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

package ASIStant.String_Handling is

------------------------------------------------------------------------------
--  Low-level string analysis
------------------------------------------------------------------------------

   function  Word_End (Str : Wide_String; ID : Boolean := False)
      return Integer;
   --  Searches for the first space character or EOL. If ID is set to TRUE,
   --  the function also stops on the first non-alpha-numeric character

   function  Skip_Spc (Str : Wide_String) return Integer;
   --  Skips all spaces

   function  Find_Symbol (Str : Wide_String; C : Wide_Character)
      return Integer;
   --  Searches for the character

   function  Skip_Symbol (Str : Wide_String; C : Wide_Character)
      return Integer;
   --  Skips all spaces and any encountered after that specified characters

   function  Skip_Prefix (S : Wide_String; N : Natural := 4)
      return Wide_String;
   --  Skips first N symbols in S

   procedure Read_Integer (S : Wide_String; Val, New1st : out Integer);
   --  Reads integer and points to the first character after it

   procedure To_Upper (S : in out Wide_String);
   --  Converts all characters in S except string literals to UPPER CASE

   procedure To_Proper (S : in out Wide_String);
   function  To_Proper (S : Wide_String) return Wide_String;
   --  Converts all characters in S to Proper_Case

end ASIStant.String_Handling;
