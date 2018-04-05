------------------------------------------------------------------------------
--                                                                          --
--             ASIS Tester And iNTerpreter  (ASIStant) COMPONENTS            --
--                                                                          --
--                        A S I S T A N T . E X E C                         --
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
------------------------------------------------------------------------------

with ASIStant.Batch_IO; use ASIStant.Batch_IO;
with ASIStant.Call;     use ASIStant.Call;
with ASIStant.Common;   use  ASIStant.Common;
with ASIStant.Evaluate; use ASIStant.Evaluate;
with ASIStant.Get;      use ASIStant.Get;
with ASIStant.Help;
with ASIStant.If1;
with ASIStant.Print;
with ASIStant.Set;
with ASIStant.FuncEnum; use ASIStant.FuncEnum;
with ASIStant.Text_IO;

package body ASIStant.Exec is

------------------------------------------------------------------------------
--  This package executes ASIStant commands
------------------------------------------------------------------------------

   procedure Exec_ATI_Command  (N : Node_Position) is
      i : Integer;
   begin

      Exec_Result := -1;
      case Identify_Function (N) is
         when FT_INFO      => ASIStant.Help.ASISHelp;
         when FT_HELP      => ASIStant.Help.Help (N);

         when FT_CALL      =>
            declare
               QR : constant Query_Result := Evaluate_Node (N);
            begin
               if QR.RType /= Par_Absent then
                  --  ??? Non-empty result is now ignored
                  null;
               end if;
            end;

         when FT_IF       => ASIStant.If1.If1 (N);
         when FT_SET      => ASIStant.Set.Set (N);

         when FT_LOGLEVEL =>
            if CurStat.Tree (N).NValue /= 0 then
               --  Expect integer
               i := Get_Integer (CurStat.Tree (N + 1).NValue);
               if i < 0 or i > 5 then
                  Error (ERR_BADPARAM);
               end if;
               Text_IO.OutputLevel := i;
            else
               Error (ERR_NEEDPARAM);
            end if;

         when FT_LOG      => ASIStant.Batch_IO.Log_IO (N);
         when FT_RUN | FT_IRUN | FT_PAUSE | FT_EXIT
                          => ASIStant.Batch_IO.Script_IO (N);

         when FT_PRINT    => ASIStant.Print.Print (N);
         when FT_PRINTDETAIL =>
            if CurStat.Tree (N).NValue /= 0 then
               --  Expect string
               ASIStant.Print.Print_Detail
                  (Get_String (CurStat.Tree (N + 1).NValue).all);
            else
               Error (ERR_NEEDPARAM);
            end if;

         when FT_QUIT     =>
            if CurStat.Tree (N).NValue /= 0 then
               --  Expect integer
               Exec_Result := Get_Integer (CurStat.Tree (N + 1).NValue);
            else
               Exec_Result := 0;
            end if;

         when FT_EXECUTE  => null;
      end case;
   end Exec_ATI_Command;

end ASIStant.Exec;
