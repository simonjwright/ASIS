------------------------------------------------------------------------------
--                                                                          --
--             ASIS Tester And iNTerpreter (ASIStant) COMPONENTS            --
--                                                                          --
--              A S I S T A N T . S T R I N G _ H A N D L I N G             --
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

with Ada.Characters.Handling; use  Ada.Characters.Handling;
with ASIStant.Common; use ASIStant.Common;

package body ASIStant.String_Handling is

   --------------
   -- Word_End --
   --------------

   function Word_End (Str : Wide_String; ID : Boolean := False)
     return Integer is
      P : Integer := Str'Last;
   begin
      for i in Str'First .. Str'Last loop

         if ID                    and
            (Str (i) in 'A' .. 'Z' or
            Str (i) in 'a' .. 'z'  or
            Str (i) = '_'            or
            Str (i) in '0' .. '9')
         then
            P := i;
         else
            exit;
         end if;

         if Str (i) /= ' ' then
            P := i;
         else
            exit;
         end if;

      end loop;
      return P;
   end Word_End;

   --------------
   -- Skip_Spc --
   --------------

   function Skip_Spc (Str : Wide_String) return Integer is
   begin
      for i in Str'First .. Str'Last loop
         if Str (i) /= ' ' then
            return i;
         end if;
      end loop;
      return Str'Last;
   end Skip_Spc;

   -----------------
   -- Skip_Symbol --
   -----------------

   function Skip_Symbol (Str : Wide_String; C : Wide_Character)
     return Integer is
      P : Integer;
   begin
      P := Skip_Spc (Str);
      if P > 0 and then Str (P) = C then
         P := P + 1;
      else
         P := 0;
      end if;
      return P;
   end Skip_Symbol;

   -----------------
   -- Skip_Prefix --
   -----------------

   function Skip_Prefix (S : Wide_String; N : Natural  := 4)
     return Wide_String is
   begin
      return S (N + 1 .. S'Last);
   end Skip_Prefix;

   function Find_Symbol (Str : Wide_String; C : Wide_Character)
     return Integer is
      P : Integer  := Str'First;
   begin
      while P <= Str'Last loop
         if Str (P) = C then
            return P;
         else
            P := P + 1;
         end if;
      end loop;
      return 0;
   end Find_Symbol;

   ------------------
   -- Read_Integer --
   ------------------

   procedure Read_Integer (S : Wide_String; Val, New1st : out Integer) is
   begin
      New1st  := S'First;
      Val  := 0;

      while S (New1st) in '0' .. '9' loop
         Val := Val * 10 +
                Wide_Character'Pos (S (New1st)) - Wide_Character'Pos ('0');
         New1st := New1st + 1;
         exit when New1st > S'Last;
      end loop;
   end Read_Integer;

   --------------
   -- To_Upper --
   --------------

   procedure To_Upper (S : in out Wide_String) is
      I : Natural  := S'First;
   begin
      loop
         exit when I > S'Last;
         if S (I) in 'a' .. 'z' then
            S (I)  := To_Wide_Character (To_Upper (To_Character (S (I))));
         elsif S (I) = '"' then
            I  := Find_Symbol (S (I + 1 .. S'Last), '"');
            if I = 0 then
               Error (ERR_BADSTRING);
            end if;
            I  := I + 1;
         end if;
         I  := I + 1;
      end loop;
   end To_Upper;

   ---------------
   -- To_Proper --
   ---------------

   procedure To_Proper (S : in out Wide_String) is
      Next_To_Upper : Boolean := True;
   begin
      for I in S'Range loop
         if S (I) = '_' then
            Next_To_Upper := True;
         elsif Next_To_Upper and then
           S (I) in 'a' .. 'z'
         then
            S (I)  := To_Wide_Character (To_Upper (To_Character (S (I))));
            Next_To_Upper := False;
         elsif not Next_To_Upper and then
           S (I) in 'A' .. 'Z'
         then
            S (I)  := To_Wide_Character (To_Lower (To_Character (S (I))));
         else
            Next_To_Upper := False;
         end if;
      end loop;
   end To_Proper;

   ---------------
   -- To_Proper --
   ---------------

   function  To_Proper (S : Wide_String) return Wide_String is
      SS : Wide_String := S;
   begin
      To_Proper (SS);
      return SS;
   end To_Proper;

end ASIStant.String_Handling;
