------------------------------------------------------------------------------
--                                                                          --
--                       ASIS TUTORIAL COMPONENTS                           --
--                                                                          --
--                    M E T R I C S _ U T I L I T I E S                     --
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

package body Metrics_Utilities is

   ----------------------------
   -- Print_Metric_Counterts --
   ----------------------------

   procedure Print_Metric_Counterts is
      package Natural_IO is new Ada.Wide_Text_IO.Integer_IO (Natural);
   begin
      Ada.Wide_Text_IO.Put ("Total statements  :");
      Natural_IO.Put       (Total_Statements);
      Ada.Wide_Text_IO.New_Line;

      Ada.Wide_Text_IO.Put ("Total declarations:");
      Natural_IO.Put       (Total_Declarations);
      Ada.Wide_Text_IO.New_Line;
   end Print_Metric_Counterts;

   ---------------------------
   -- Reset_Metric_Counters --
   ---------------------------

   procedure Reset_Metric_Counters is
   begin
      Total_Statements   := 0;
      Total_Declarations := 0;
   end Reset_Metric_Counters;

end Metrics_Utilities;