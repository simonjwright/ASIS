------------------------------------------------------------------------------
--                                                                          --
--                            GNAT2XML COMPONENTS                           --
--                                                                          --
--                G N A T 2 X M L . B U F F E R S . T E S T                 --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                     Copyright (C) 2013-2016, AdaCore                     --
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

with ASIS_UL.Utilities;
with ASIS_UL.Formatted_Output; use ASIS_UL.Formatted_Output;

procedure Pp.Buffers.Test is

   Buf : Buffer;

   Marks : Marker_Vectors.Vector;

   procedure Assert (Condition : Boolean);

   procedure Ins (Buf : in out Buffer; S : W_Str);

   procedure Check_Marks;
   --  Check that Slicing the marks works

   procedure Assert (Condition : Boolean) is
   begin
      if not Condition then
         Dbg_Out.Output_Enabled := True;
         Dump_Buffer (Buf);
         raise Program_Error;
      end if;
   end Assert;

   procedure Check_Marks is

      function Mark_LT (X, Y : Marker) return Boolean;

      function Mark_LT (X, Y : Marker) return Boolean is
      begin
         return Mark_LT (Buf, X, Y);
      end Mark_LT;

      package Sorting is new Marker_Vectors.Generic_Sorting ("<" => Mark_LT);

      Buf_Contents : constant W_Str := To_W_Str (Buf);

   begin
      Sorting.Sort (Marks);

      --  Check slices three times: First at end, second after Reset, third in
      --  the middle.

      pragma Assert (At_End (Buf));

      for Count in 1 .. 3 loop
         for J in 1 .. Last_Index (Marks) loop
            for K in J .. Last_Index (Marks) loop
               declare
                  F  : constant Marker := Marks (J);
                  L  : constant Marker := Marks (K);
                  S1 : constant W_Str  := Slice (Buf, F, L);
                  S2 : constant W_Str  :=
                    Buf_Contents (Position (Buf, F) .. Position (Buf, L) - 1);

               begin
                  Assert (S1 = S2);
               end;
            end loop;
         end loop;

         case Count is
            when 1 =>
               Reset (Buf);

            when 2 =>
               for X in 1 .. Last_Position (Buf) loop
                  Move_Forward (Buf);
               end loop;

            when 3 =>
               while not At_End (Buf) loop
                  Move_Forward (Buf);
               end loop;
         end case;
      end loop;
   end Check_Marks;

   procedure Ins (Buf : in out Buffer; S : W_Str) is
      Was_At_End : Boolean;

   begin
      for C of S loop
         if C = '|' then
            Append (Marks, Mark (Buf, '|'));
            Assert (At_Point (Buf, Last_Element (Marks)));
            Assert (Mark (Buf, '-') = Last_Element (Marks));

         else
            Was_At_End := At_End (Buf);
            Insert (Buf, C);
            if not Is_Empty (Marks) and then Was_At_End then
               Assert (not At_Point (Buf, Last_Element (Marks)));
            end if;
         end if;
      end loop;
   end Ins;

   Main_Done : Boolean renames ASIS_UL.Utilities.Main_Done;

--  Start of processing for Pp.Buffers.Test

begin
   Put ("Pp.Buffers.Test\n");

   Dbg_Out.Output_Enabled := True;
   Assert (At_End (Buf));
   Assert (To_W_Str (Buf) = "");

   Ins (Buf, "|A|B|C|");
   Dump_Buffer (Buf);
   Assert (To_Debug_String (Buf) = "|A|B|C|");
   Check_Marks;

   Clear (Buf);
   Clear (Marks);
   Ins (Buf, "X|Z");
   Dump_Buffer (Buf);
   Assert (Char_At (Buf, Last_Element (Marks)) = 'Z');
   Reset (Buf);
   Move_Forward (Buf);
   Dump_Buffer (Buf);
   Assert (Char_At (Buf, Last_Element (Marks)) = 'Z');
   Insert (Buf, 'Y');
   Dump_Buffer (Buf);
   Assert (Char_At (Buf, Last_Element (Marks)) = 'Z');

   Clear (Buf);
   Clear (Marks);
   Ins (Buf, "ABC");
   Dump_Buffer (Buf);

   for J in 1 .. 3 loop
      Reset (Buf);
      if J = 1 then
         Assert (To_Debug_String (Buf) = "ABC");
      end if;
      Ins (Buf, "|");
      Move_Forward (Buf);
      Ins (Buf, "|");
      Move_Forward (Buf);
      Ins (Buf, "|");
      Move_Forward (Buf);
      Ins (Buf, "|");
      Dump_Buffer (Buf);
      Assert (At_End (Buf));
      Assert (To_Debug_String (Buf) = "|A|B|C|");
      Check_Marks;
   end loop;

   Clear (Buf);
   Clear (Marks);
   Ins (Buf, "A|B|C");
   Dump_Buffer (Buf);
   Assert (To_Debug_String (Buf) = "A|B|C");
   Check_Marks;

   Clear (Buf);
   Clear (Marks);
   Ins (Buf, "ABC");
   Reset (Buf);
   Dump_Buffer (Buf);
   Assert (To_Debug_String (Buf) = "ABC");
   Move_Forward (Buf);
   Ins (Buf, "|");
   Move_Forward (Buf);
   Ins (Buf, "|");
   Move_Forward (Buf);
   Dump_Buffer (Buf);
   Assert (At_End (Buf));
   Assert (To_Debug_String (Buf) = "A|B|C");
   Reset (Buf);
   Ins (Buf, "|X");
   Move_Forward (Buf);
   Move_Forward (Buf);
   Move_Forward (Buf);
   Ins (Buf, "X|");
   Dump_Buffer (Buf);
   Assert (At_End (Buf));
   Assert (To_Debug_String (Buf) = "X|A|B|CX|");
   Check_Marks;

   for Count in 1 .. 3 loop
      Clear (Buf);
      Clear (Marks);
      Assert (At_End (Buf));
      Assert (To_W_Str (Buf) = "");

      Ins (Buf, "|Hello|World|");
      Assert (At_End (Buf));
      Assert (To_W_Str (Buf) = "HelloWorld");

      for Count2 in 1 .. 3 loop
         Reset (Buf);
         Assert (To_W_Str (Buf) = "HelloWorld");

         while not At_End (Buf) loop
            Move_Forward (Buf);
         end loop;
      end loop;

      Reset (Buf);

      while not At_End (Buf) loop
         Move_Forward (Buf);
         Ins (Buf, ",| ");
      end loop;

      Assert (To_W_Str (Buf) = "H, e, l, l, o, W, o, r, l, d, ");

      Reset (Buf);
      Ins (Buf, " ");
      Assert (To_W_Str (Buf) = " H, e, l, l, o, W, o, r, l, d, ");

      while not At_End (Buf) loop
         Move_Forward (Buf);
      end loop;
      Ins (Buf, "!");
      Assert (To_W_Str (Buf) = " H, e, l, l, o, W, o, r, l, d, !");
      Assert (At_End (Buf));
   end loop;

   Dump_Buffer (Buf);

   Put ("end Pp.Buffers.Test\n");

   Main_Done := True;
end Pp.Buffers.Test;
