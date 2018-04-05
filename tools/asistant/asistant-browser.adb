------------------------------------------------------------------------------
--                                                                          --
--             ASIS Tester And iNTerpreter (ASIStant) COMPONENTS            --
--                                                                          --
--                      A S I S T A N T . B R O W S E R                     --
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

with Ada.Text_IO; --  Required for Get_Immediate

with Asis.Elements;
with Asis.Exceptions;  use Asis.Exceptions;
with Asis.Errors;      use Asis.Errors;
with Asis.Implementation;
with Asis.Extensions.Flat_Kinds; use Asis.Extensions.Flat_Kinds;

with ASIStant.Help;    use ASIStant.Help;
with ASIStant.Print;   use ASIStant.Print;
with ASIStant.Text_IO; use ASIStant.Text_IO;

with ASIStant.Find_Element;
with ASIStant.Browser.Iterator; use ASIStant.Browser.Iterator;

with ASIStant.FuncEnum, ASIStant.FuncArr;
use  ASIStant.FuncEnum, ASIStant.FuncArr;

package body ASIStant.Browser is

------------------------------------------------------------------------------
--  Browser capability for ASIStant
------------------------------------------------------------------------------

   Browser_Prompt : constant Wide_String :=
      "N=Next,P=Previous,D=Down,U=Up,\=Detail,?=QHelp," &
      "Space=Query,G=GotoSLOC,Q=Quit-->";

   function Browse (E : Asis.Element) return Asis.Element is
   --  Gateway to the Browser capability

      Current : Asis.Element := E;
      Next    : Asis.Element := Asis.Nil_Element;
      C       : Character;
      S       : Wide_String (1 .. 50);
      N, Line, Col : Natural;
      sw      : FuncEnum.Switch_Index;
      Success : Boolean;
      --  Set False if  any error occured during interpreting a given comand,
      --  Set True otherwise

   begin

      if Asis.Elements.Is_Nil (E) then
         ATIPut_Line ("Browser: Cannot browse from Nil_Element.");
         return Asis.Nil_Element;
      end if;

      Print_Result ((RType => Par_Element, E => Current));
      ATINew_Line;
      ATIPut_Line (Browser_Prompt);

      loop

         Success := True;

         if not (Asis.Elements.Is_Nil (Next) or else
                 Asis.Elements.Is_Identical (Current, Next))
         then
            Current := Next;
            ATINew_Line;
            Print_Result ((RType => Par_Element, E => Current));
            ATINew_Line;
            ATIPut_Line (Browser_Prompt);
         end if;

         Ada.Text_IO.Get_Immediate (C);

         case C is
            when 'Q' | 'q' =>
               ATIPut_Line ("Q:");
               return Current;
            when 'G' | 'g' =>
               begin
                  ATIPut ("Enter Line position:");
                  ATIGet (S, N);
                  Line := Integer'Wide_Value (S (1 .. N - 1));
                  ATIPut ("Enter Column position:");
                  ATIGet (S, N);
                  Col := Integer'Wide_Value (S (1 .. N - 1));

                  Next := ASIStant.Find_Element
                     (Asis.Elements.Enclosing_Compilation_Unit (Current),
                      Line, Col);
                  Current := Asis.Nil_Element;
               exception
                  when Constraint_Error =>
                     ATIPut_Line ("Invalid parameter: " & S (1 .. N - 1));
               end;
            when 'D' | 'd' =>
               ATIPut ("Down:");
               Next := Down (Current);
            when 'U' | 'u' =>
               ATIPut ("Up:");
               Next := Up (Current);
            when 'N' | 'n' =>
               ATIPut ("Next:");
               Next := Iterator.Next (Current);
            when 'P' | 'p' =>
               ATIPut ("Previous:");
               Next := Previous (Current);
            when '\' =>
               ATIPut ("PRINT detail: ");
               ATIGet (S, N);
               Print_Detail (S (1 .. 2));
               Next := Current;
               Current := Asis.Nil_Element;
            when ' ' =>
               ATIPut ("Query name: ");
               ATIGet (S, N);
               begin
                  sw := FuncEnum.Switch_Index'Wide_Value (S (1 .. N - 1));
                  Next := FuncArr.FElemRetElem (sw) (Current);
               exception
                  when Constraint_Error =>
                     Success := False;

                     ATIPut_Line
                       ("Unknown or illegal query " & S (1 .. N - 1));

                  when ASIS_Inappropriate_Context          |
                       ASIS_Inappropriate_Container        |
                       ASIS_Inappropriate_Compilation_Unit |
                       ASIS_Inappropriate_Element          |
                       ASIS_Inappropriate_Line             |
                       ASIS_Inappropriate_Line_Number      |
                       ASIS_Failed                         =>

                     Success := False;

                     ATIPut_Line ("Exception is raised by ASIS query "
                                 & S (1 .. N - 1));

                     ATIPut_Line ("Status : " &
                     Asis.Errors.Error_Kinds'Wide_Image
                       (Asis.Implementation.Status), 5);

                     ATIPut_Line ("Diagnosis : ", 5);
                     ATIPut_Line (Asis.Implementation.Diagnosis, 5);

                     Asis.Implementation.Set_Status;

                  when others =>
                     Success := False;

                     ATIPut_Line ("Unexpected bug in browser");
                     ATIPut_Line ("Please report to report@adacore.com");

               end;
            when '?' =>
               Display_Kind_Help (Flat_Element_Kind (Current));
               Success := False;

            when others =>
               Success := False;
               ATIPut_Line ("Unknown browser command");
         end case;

         if (Asis.Elements.Is_Nil (Next)) and then Success then
            ATIPut_Line (" Cannot go in this direction.");
         end if;

      end loop;
   end Browse;

   function Browse (C : Asis.Compilation_Unit) return Asis.Element is
   --  Gateway to the Browser capability for Compilation_Units
   begin
      return Browse (Asis.Elements.Unit_Declaration (C));
   end Browse;

end ASIStant.Browser;
