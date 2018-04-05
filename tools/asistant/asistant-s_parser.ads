------------------------------------------------------------------------------
--                                                                          --
--             ASIS Tester And iNTerpreter (ASIStant) COMPONENTS            --
--                                                                          --
--                    A S I S T A N T . S _ P A R S E R                     --
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

with ASIStant.Common;   use ASIStant.Common;
with ASIStant.L_Parser; use ASIStant.L_Parser;
with ASIStant.FuncEnum; use ASIStant.FuncEnum;

package ASIStant.S_Parser is

------------------------------------------------------------------------------
--  This package provides parsing for the ASIStant language
------------------------------------------------------------------------------

   type Node_Type is (
     NT_BOOLEAN,
     NT_FUNCTION,
     NT_INTEGER,
     NT_PARAMLIST,
     NT_STRING,
     NT_VARIABLE
   );

   subtype Node_Position is Natural range 0 .. MAX_TREENODES;

   type Node is
     record
       NType     : Node_Type;
       VType     : Var_Type;
       SValue    : String_Ptr;
       IValue    : Integer;
       NValue    : Node_Position;
       Next_Node : Node_Position;
     end record;

   type TStorage is array (Node_Position) of Node;

   type Statement_Tree is
     record
       Free      : Node_Position := 1;
       Tree      : TStorage;
     end record;

   CurStat : Statement_Tree;

   procedure Reset_Tree;
   procedure Get_Func (TS : in out Token_Stream);
   procedure Get_Expr (TS : in out Token_Stream);
   procedure Get_Stmt (TS : in out Token_Stream);

end ASIStant.S_Parser;
