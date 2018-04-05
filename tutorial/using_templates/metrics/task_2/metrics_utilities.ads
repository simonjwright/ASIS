------------------------------------------------------------------------------
--                                                                          --
--                       ASIS TUTORIAL COMPONENTS                           --
--                                                                          --
--                    M E T R I C S _ U T I L I T I E S                     --
--                                                                          --
--                                 S p e c                                  --
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

--  This package defines the data structures and the utility programs needed
--  to implement an ASIS Metrics tool in the framework of the ASIS tutorial
--  provided by ASIS-for-GNAT (Task 2).

package Metrics_Utilities is

   --  Metric counters:
   Total_Statements   : Natural := 0;
   Total_Declarations : Natural := 0;

   --  Metric counters added for Task 2:
   Simple_Statements   : Natural := 0;
   Compound_Statements : Natural := 0;
   Defining_Names      : Natural := 0;

   procedure Reset_Metric_Counters;
   --  Sets all the metric counters to zero.

   procedure Print_Metric_Counterts;
   --  Outputs the values of the metric counters.

end Metrics_Utilities;