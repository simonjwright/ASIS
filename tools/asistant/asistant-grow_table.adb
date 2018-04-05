------------------------------------------------------------------------------
--                                                                          --
--             ASIS Tester And iNTerpreter (ASIStant) COMPONENTS            --
--                                                                          --
--                   A S I S T A N T . G R O W _ T A B L E                  --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 2005-2013, Free Software Foundation, Inc.         --
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

procedure ASIStant.Grow_Table is
   Tmp : Table_Ptr := Table;
begin
   Table := new Table_Type (0 .. Table_Size + 20);
   Table.all (0 .. Table_Size) := Tmp.all;
   Table_Size := Table_Size + 20;
   Free_Proc (Tmp);
end ASIStant.Grow_Table;
