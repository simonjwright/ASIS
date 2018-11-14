------------------------------------------------------------------------------
--                                                                          --
--                            GNAT2XML COMPONENTS                           --
--                                                                          --
--                 G N A T 2 X M L . G N A T 2 T O K E N S                  --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                     Copyright (C) 2012-2017, AdaCore                     --
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

with Ada.Command_Line; use Ada.Command_Line;

with ASIS_UL.Formatted_Output; use ASIS_UL.Formatted_Output;
with Pp.Buffers;        use Pp.Buffers;
with Pp.Scanner;        use Pp.Scanner;

procedure Gnat2xml.Gnat2tokens is
   Slocs : Boolean := True;
begin
   if Argument_Count = 0 then
      raise Program_Error with "missing arguments";
   end if;

   for X in 1 .. Argument_Count loop
      if Argument (X) = "--debug" then
         Debug_Mode := True;

      elsif Argument (X) = "--noslocs" then
         Slocs := False;

      else
         declare
            Source_Name : constant String := Argument (X);
            Buf         : Buffer;
            Tokens      : Token_Vectors.Vector;
            BOM_Seen    : Boolean;
         begin
            Put ("-- Processing \1\n", Source_Name);
            Read_Ada_File (Buf, Source_Name,
                           BOM_Seen => BOM_Seen, Expand_Tabs => True);
            pragma Assert (not BOM_Seen);
            Get_Tokens (Buf, Tokens, Pp_Off_On_Delimiters => (others => <>));
            Put_Tokens (Tokens, Slocs => Slocs);
            Put ("\n\n\n");
         end;
      end if;
   end loop;

   Main_Done := True;
end Gnat2xml.Gnat2tokens;
