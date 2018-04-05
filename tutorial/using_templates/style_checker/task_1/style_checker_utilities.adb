------------------------------------------------------------------------------
--                                                                          --
--                       ASIS TUTORIAL COMPONENTS                           --
--                                                                          --
--               S T Y L E _ C H E C K E R _ U T I L I T I E S              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--            Copyright (c) 2000, Free Software Foundation, Inc.            --
--                                                                          --
-- ASIS  Application  Templates are  free software; you can redistribute it --
-- and/or  modify it under  terms  of the  GNU  General  Public  License as --
-- published by the Free Software Foundation; either version 2, or (at your --
-- option) any later version. ASIS Application Templates are distributed in --
-- the hope that they will be useful, but  WITHOUT  ANY  WARRANTY; without  --
-- even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR --
-- PURPOSE. See the GNU General Public License for more details. You should --
-- have  received a copy of the GNU General Public License distributed with --
-- distributed  with  GNAT;  see  file  COPYING. If not, write to the Free  --
-- Software Foundation, 59 Temple Place - Suite 330, Boston, MA 02111-1307, --
-- USA.                                                                     --
--                                                                          --
-- ASIS Tutorial was developed and are now maintained by Ada Core           --
-- Technologies Inc (http://www.gnat.com).                                  --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Wide_Text_IO;

with Asis.Text;

package body Style_Checker_Utilities is

   ----------------------------
   -- Report_Style_Violation --
   ----------------------------

   procedure Report_Style_Violation
     (The_Element : Asis.Element;
      Diagnosis   : Wide_String)
   is
   begin
      --  May be, this code is not very elegant or very effective, but
      --  what wee need is some simple code to output the location of
      --  The_Element

      Ada.Wide_Text_IO.Put (Diagnosis);
      Ada.Wide_Text_IO.Put (" at lines");

      if Asis.Text.Is_Text_Available (The_Element) then
         Ada.Wide_Text_IO.Put (Asis.Text.Line_Number'Wide_Image
           (Asis.Text.First_Line_Number (The_Element)));

         Ada.Wide_Text_IO.Put (" -");

         Ada.Wide_Text_IO.Put (Asis.Text.Line_Number'Wide_Image
           (Asis.Text.Last_Line_Number (The_Element)));
      else
         Ada.Wide_Text_IO.Put (" <not available>");
      end if;

      Ada.Wide_Text_IO.New_Line;

   end Report_Style_Violation;

end Style_Checker_Utilities;