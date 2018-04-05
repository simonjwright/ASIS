------------------------------------------------------------------------------
--                                                                          --
--             ASIS Tester And iNTerpreter (ASIStant) COMPONENTS            --
--                                                                          --
--                        A S I S T A N T . I F 1                           --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1997-2012, Free Software Foundation, Inc.         --
--                                                                          --
-- ASIStant  is  free  software;  you can  redistribute it and/or modify it --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software Foundation;  either version 2,  or  (at your option)  any later --
-- version.  ASIStant is  distributed  in the hope  that it will be useful, --
-- but  WITHOUT  ANY  WARRANTY;  without  even the implied warranty of MER- --
-- CHANTABILITY or  FITNESS FOR  A PARTICULAR PURPOSE.  See the GNU General --
-- Public License for more details.  You should have received a copy of the --
-- GNU General Public License  distributed with GNAT;  see file COPYING. If --
-- not,  write to the  Free Software Foundation,  51 Franklin Street, Fifth --
-- Floor, Boston, MA 02110-1301, USA.                                       --
--                                                                          --
-- ASIStant  is an evolution of  ASIStint tool that was created by  Vasiliy --
-- Fofanov  as  part  of  a  collaboration  between  Software   Engineering --
-- Laboratory  of the  Swiss  Federal Institute of Technology in  Lausanne, --
-- Switzerland,  and the Scientific Research Computer Center of the  Moscow --
-- University, Russia,  supported by the  Swiss National Science Foundation --
-- grant #7SUPJ048247, "Development of ASIS for GNAT with industry quality" --
--                                                                          --
-- ASIStant  is  distributed as a part of the  ASIS implementation for GNAT --
-- (ASIS-for-GNAT) and is maintained by AdaCore (http://www.adacore.com).   --
--                                                                          --
------------------------------------------------------------------------------

with ASIStant.Common;   use ASIStant.Common;
with ASIStant.Evaluate; use ASIStant.Evaluate;
with ASIStant.Exec;     use ASIStant.Exec;
with ASIStant.FuncEnum; use ASIStant.FuncEnum;

package body ASIStant.If1 is

------------------------------------------------------------------------------
--  Package for ASIStant conditional operation
------------------------------------------------------------------------------

   procedure If1 (N : Node_Position) is

      NPtr1, NPtr2, NPtr3 : Node_Position;
      QR :    Query_Result;
   begin

      if CurStat.Tree (N).NValue = 0 then
         Error (ERR_NEEDPARAM);
      end if;

      NPtr1 := CurStat.Tree (N).NValue;

      if CurStat.Tree (NPtr1).Next_Node = 0 then
         Error (ERR_NEEDPARAM);
      end if;

      NPtr2 := CurStat.Tree (NPtr1).Next_Node;
      NPtr3 := CurStat.Tree (NPtr2).Next_Node;

      NPtr1 := CurStat.Tree (NPtr1).NValue;   --  condition
      NPtr2 := CurStat.Tree (NPtr2).NValue;   --  <THEN> ATI command

      if NPtr3 /= 0 then
         --  <ELSE> ATI command
         NPtr3 := CurStat.Tree (NPtr3).NValue;   --  condition

         if CurStat.Tree (NPtr3).Next_Node /= 0 then
            --  Only 1 or 2 branches allowed
            Error (ERR_TOOMANYPARAMS);
         end if;
      else
         NPtr3 := 0;
      end if;

      QR := Evaluate_Node (NPtr1);

      if QR.RType /= Par_Boolean then
         Error (ERR_BADBOOLEAN);
      end if;

      if QR.B then
         Exec_ATI_Command (NPtr2);
      else
         Exec_ATI_Command (NPtr3);
      end if;

   end If1;

end ASIStant.If1;
