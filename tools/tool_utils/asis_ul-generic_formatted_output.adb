------------------------------------------------------------------------------
--                                                                          --
--                            GNAT2XML COMPONENTS                           --
--                                                                          --
--              G E N E R I C _ F O R M A T T E D _ O U T P U T             --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                     Copyright (C) 2012-2016, AdaCore                     --
--                                                                          --
-- Gnat2xml is free software; you can redistribute it and/or modify it      --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software Foundation;  either version 2,  or  (at your option)  any later --
-- version. Gnat2xml is distributed  in the hope  that it will be useful,   --
-- but WITHOUT ANY WARRANTY; without even the implied warranty of MER-      --
-- CHANTABILITY or  FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General  --
-- Public License for more details. You should have received a copy of the  --
-- GNU General Public License distributed with GNAT; see file COPYING. If   --
-- not, write to the Free Software Foundation, 59 Temple Place Suite 330,   --
-- Boston, MA 02111-1307, USA.                                              --
-- The gnat2xml tool was derived from the Avatox sources.                   --
------------------------------------------------------------------------------

pragma Ada_2012;

with Ada.Finalization; use Ada.Finalization;

with ASIS_UL.Utilities;

with ASIS_UL.String_Utilities; use ASIS_UL.String_Utilities;

package body ASIS_UL.Generic_Formatted_Output is

   Column : Natural := 1;

   procedure Raw_Put_Char (C : Char_Type);
   --  Put the character and adjust Column

   procedure Put (S : Str_Type);
   --  Put_Char all the characters

   type Finalization is new Limited_Controlled with null record;
   procedure Finalize (X : in out Finalization);
   The_Finalization : Finalization;
   pragma Unreferenced (The_Finalization);
   --  Declare a singleton object to check that the indentation isn't messed up
   --  -- we should end up at zero indentation.

   ----------------
   -- Cur_Column --
   ----------------

   function Cur_Column return Positive is
   begin
      return Column;
   end Cur_Column;

   --------------
   -- Finalize --
   --------------

   procedure Finalize (X : in out Finalization) is
      pragma Unreferenced (X);
   begin
      if ASIS_UL.Utilities.Main_Done then
         if Indentation /= 0 then
            raise Program_Error with "Indentation should be zero at end.";
         end if;
      end if;
   end Finalize;

   ------------
   -- Indent --
   ------------

   procedure Indent
     (Indentation_Amount : Natural := Default_Indentation_Amount)
   is
   begin
      Indentation := Indentation + Indentation_Amount;
   end Indent;

   -------------
   -- Outdent --
   -------------

   procedure Outdent
     (Indentation_Amount : Natural := Default_Indentation_Amount)
   is
   begin
      Indentation := Indentation - Indentation_Amount;
   end Outdent;

   ---------
   -- Put --
   ---------

   procedure Put
     (T                      : Template;
      X1, X2, X3, X4, X5, X6 : Str_Type := (1 .. 0 => <>))
   is
      J    : Positive                  := T'First;
      Used : array (1 .. 6) of Boolean := (others => False);

   begin
      if not Output_Enabled then
         return;
      end if;

      while J <= T'Last loop
         if T (J) = '\' then
            J := J + 1;
            case T (J) is
               when 'n' =>
                  Put_Char (Char_Type'Val (W_Char'Pos (NL)));

               when 't' =>
                  Put_Char (Char_Type'Val (W_Char'Pos (W_HT)));

               when '\' =>
                  Put_Char (Char_Type'Val (W_Char'Pos ('\')));

               when 'i' =>
                  Indent;

               when 'o' =>
                  Outdent;

               when '1' =>
                  Used (1) := True;
                  Put (X1);

               when '2' =>
                  Used (2) := True;
                  Put (X2);

               when '3' =>
                  Used (3) := True;
                  Put (X3);

               when '4' =>
                  Used (4) := True;
                  Put (X4);

               when '5' =>
                  Used (5) := True;
                  Put (X5);

               when '6' =>
                  Used (6) := True;
                  Put (X6);

               when others =>
                  raise Program_Error;
            end case;

         else
            Put_Char (Char_Type'Val (Character'Pos (T (J))));
         end if;
         J := J + 1;
      end loop;

      if not Used (1) then
         pragma Assert (X1'Length = 0);
      end if;
      if not Used (2) then
         pragma Assert (X2'Length = 0);
      end if;
      if not Used (3) then
         pragma Assert (X3'Length = 0);
      end if;
      if not Used (4) then
         pragma Assert (X4'Length = 0);
      end if;
      if not Used (5) then
         pragma Assert (X5'Length = 0);
      end if;
      if not Used (6) then
         pragma Assert (X6'Length = 0);
      end if;
   end Put;

   procedure Put (S : Str_Type) is
   begin
      for J in S'Range loop
         Put_Char (S (J));
      end loop;
   end Put;

   --------------
   -- Put_Char --
   --------------

   procedure Put_Char (C : Char_Type) is
   begin
      pragma Assert (Output_Enabled); -- too slow to check on every char
      if False and then not Output_Enabled then
         return;
      end if;

      if Column = 1 and then C /= Char_Type'Val (W_Char'Pos (NL)) then
         for J in 1 .. Indentation mod 60 loop
            --  The "mod 60" is so we don't indent by huge amounts
            Raw_Put_Char (Char_Type'Val (W_Char'Pos (' ')));
         end loop;
      end if;
      Raw_Put_Char (C);
   end Put_Char;

   ------------------
   -- Raw_Put_Char --
   ------------------

   procedure Raw_Put_Char (C : Char_Type) is
   begin
      Basic_Put_Char (C);

      if C = Char_Type'Val (W_Char'Pos (NL)) then
         Column := 1;

      else
         Column := Column + 1;
      end if;
   end Raw_Put_Char;

   -------------------
   -- Tab_To_Column --
   -------------------

   procedure Tab_To_Column (Column : Positive) is
   begin
      while Cur_Column < Column loop
         Put_Char (Char_Type'Val (W_Char'Pos (' ')));
      end loop;
   end Tab_To_Column;

end ASIS_UL.Generic_Formatted_Output;
