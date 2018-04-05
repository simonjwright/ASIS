------------------------------------------------------------------------------
--                                                                          --
--             ASIS Tester And iNTerpreter (ASIStant) COMPONENTS            --
--                                                                          --
--                       A S I S T A N T . T A B L E                        --
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

with ASIStant.Common; use ASIStant.Common;
with ASIStant.FuncEnum; use ASIStant.FuncEnum;

package ASIStant.Table is

------------------------------------------------------------------------------
--  This package provides handling of ASIStant language variable tables
------------------------------------------------------------------------------

   subtype Var_Name is Name_String;

   type Var_Info is
      record
         Name   : Var_Name;
         VType  : Var_Type;
         IValue : Integer;
         SValue : String_Ptr;
      end record;

   Var_Unknown : constant Var_Info :=
      ((1 .. MAX_ID_LENGTH => ' '), Par_Absent, 0, null);

   type V_Table is array (Positive range <>) of Var_Info;
   type V_TablePtr is access all V_Table;

   type Var_Table is
      record
         Free  : Positive;
         Max   : Positive;
         Table : V_TablePtr;
      end record;

   CurTable : Var_Table :=
      (1, MAX_VARIABLES, new V_Table (1 .. MAX_VARIABLES));

   function  Get_Var (T : Var_Table; N : Wide_String) return Var_Info;
   --  Scans for the variable in table T. Returns Var_Unknown if fails.

   function Get_Var_Value (VI : Var_Info) return Query_Result;
   --  Returns variable value information

   function Store_Var_Value (QR : Query_Result) return Var_Info;
   --  Stores value information in variable

   procedure Modify_Var (T : in out Var_Table; V : Var_Info);
   --  Adds/changes variable

end ASIStant.Table;
