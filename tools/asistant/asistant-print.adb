------------------------------------------------------------------------------
--                                                                          --
--             ASIS Tester And iNTerpreter (ASIStant) COMPONENTS            --
--                                                                          --
--                       A S I S T A N T . P R I N T                        --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1997-2013, Free Software Foundation, Inc.         --
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

with Ada.Characters.Handling;

with Asis;              use Asis;
with Asis.Text;         use Asis.Text;

with Asis.Compilation_Units;
with Asis.Elements;
with Asis.Ada_Environments;

with ASIStant.Common;   use ASIStant.Common;
with ASIStant.Evaluate; use ASIStant.Evaluate;
with ASIStant.Text_IO;  use ASIStant.Text_IO;
with ASIStant.XTable;   use ASIStant.XTable;

--  ASIS-for-GNAT-specific components

with Asis.Extensions.Flat_Kinds; use Asis.Extensions.Flat_Kinds;
with Asis.Data_Decomposition.Debug; use Asis.Data_Decomposition.Debug;

with Asis.Extensions.Strings;

package body ASIStant.Print is

------------------------------------------------------------------------------
--  This package provides printing of the ASIStant variables
------------------------------------------------------------------------------

   --  Keys that control output of Compilation_Units and Elements;
   --  set by Print_Detail and from inside the Browser capability.

   Print_Debug_Image : Boolean := False;
   Print_Text_Image  : Boolean := False;

   ------------------
   -- Print_Detail --
   ------------------

   procedure Print_Detail (S : Wide_String) is
   begin
      if S'Length > 2 then
         raise ASIStant_ERROR;
      end if;

      if S (S'First) = 'D' then
         Print_Debug_Image := True;

      elsif S (S'First) = 'd' then
         Print_Debug_Image := False;

      else
         raise ASIStant_ERROR;
      end if;

      if S (S'First + 1) = 'T' then
         Print_Text_Image := True;

      elsif S (S'First + 1) = 't' then
         Print_Text_Image := False;

      else
         raise ASIStant_ERROR;
      end if;

   exception
      when others =>
         Error (ERR_BADPARAM, " " & S);
   end Print_Detail;

   ------------------
   -- Print_Result --
   ------------------

   procedure Print_Result (QR : Query_Result) is
   begin
      case QR.RType is

         when Par_Absent   =>
            Error (ERR_BADPARAM);

         when Par_Boolean    =>
            ATIPut_Line (Boolean'Wide_Image (QR.B));

         when Par_CUnit   =>
            begin
               ATIPut_Line (Asis.Compilation_Units.Debug_Image (QR.C));
            exception
               when others =>
                  Error (ERR_INTERNAL, "CUnits.Debug_Image failed");
            end;

         when Par_CUnitList =>
            ATIPut ("Compilation_Unit_List size:");
            ATIPut_Line (Integer'Wide_Image (QR.CL.all'Length));

            for i in QR.CL.all'Range loop

               begin
                  Print_Result ((RType => Par_CUnit, C => QR.CL.all (i)));
               exception
                  when others =>
                     null;  --  Keep processing the CUnit_List
               end;
            end loop;

         when Par_Element =>
            begin

               ATIPut (Flat_Element_Kinds'Wide_Image
                 (Flat_Element_Kind (QR.E)));

               if not Asis.Elements.Is_Nil (QR.E) then

                  ATIPut (" at ");

                  if Is_Text_Available (QR.E) then
                     Print_Result
                       ((RType => Par_Span, Sp => Element_Span (QR.E)));
                  else
                     ATIPut_Line
                       (Ada.Characters.Handling.To_Wide_String
                          (Asis.Extensions.Strings.Build_GNAT_Location
                             (QR.E)));
                  end if;

                  if Print_Text_Image
                   and then
                     Is_Text_Available (QR.E)
                  then
                     declare
                        E_Lines : constant Line_List := Lines (QR.E);
                     begin
                        ATIPut_Line ("Text Image:");
                        for L in E_Lines'Range loop
                           ATIPut_Line ("  " & Integer'Wide_Image (L) & ":  "
                                        & Line_Image (E_Lines (L)));
                        end loop;
                     end;
                  end if;

               else
                  ATINew_Line;
               end if;

               if Print_Debug_Image then
                  ATIPut_Line (Asis.Elements.Debug_Image (QR.E));
               end if;

            exception
               when others =>
                  Error (ERR_INTERNAL, "while displaying Element value");
            end;

         when Par_ElemList =>
            ATIPut ("Element_List size:");
            ATIPut_Line (Integer'Wide_Image (QR.EL.all'Length));

            for i in QR.EL.all'Range loop
               begin
                  Print_Result ((RType => Par_Element, E => QR.EL.all (i)));
               exception
                  when others =>
                     null; --  Keep processing the Element_List
               end;
            end loop;

         when Par_Integer =>
            ATIPut_Line (Integer'Wide_Image (QR.I));

         when Par_Context =>
            ATIPut_Line
              (Asis.Ada_Environments.Debug_Image (ATIContext (QR.I)));

         when Par_String  =>
            ATIPut_Line (QR.S.all);

         when Par_Span    =>
            ATIPut_Line ("(" & Integer'Wide_Image (QR.Sp.First_Line) & " :"
                         & Integer'Wide_Image (QR.Sp.First_Column) & " )-("
                         & Integer'Wide_Image (QR.Sp.Last_Line) & " :"
                         & Integer'Wide_Image (QR.Sp.Last_Column) & " )");

         when Par_Relationship =>
            ATIPut_Line ("Consistent units:" &
                         Integer'Wide_Image (QR.R.all.Consistent'Last));
            --  Print_Result ((Par_CUnitList, QR.R.all.Consistent'Access));
            ATIPut_Line ("Inconsistent units:" &
                         Integer'Wide_Image (QR.R.all.Inconsistent'Last));
            --  Print_Result ((Par_CUnitList, QR.R.all.Inconsistent'Access));
            ATIPut_Line ("Missing units:" &
                         Integer'Wide_Image (QR.R.all.Missing'Last));
            --  Print_Result ((Par_CUnitList, QR.R.all.Missing'Access));
            ATIPut_Line ("Circular units:" &
                         Integer'Wide_Image (QR.R.all.Circular'Last));
            --  Print_Result ((Par_CUnitList, QR.R.all.Circular'Access));

         when Par_DDA_Array_Component =>
            ATIPut_Line (Debug_Image (QR.AC));

         when Par_DDA_Record_Component =>
            ATIPut_Line (Debug_Image (QR.RC));

         when Par_DDA_Array_Component_List =>
            ATIPut_Line ("Array_Component_List size:" &
                         Integer'Wide_Image (QR.ACL.all'Length));
            for i in QR.ACL.all'Range loop
               begin
                  Print_Result ((RType => Par_DDA_Array_Component,
                                 AC => QR.ACL.all (i)));
               exception
                  when others =>
                     null; --  Keep processing the List
               end;
            end loop;

         when Par_DDA_Record_Component_List =>
            ATIPut_Line ("Record_Component_List size:" &
                         Integer'Wide_Image (QR.RCL.all'Length));
            for i in QR.RCL.all'Range loop
               begin
                  Print_Result ((RType => Par_DDA_Record_Component,
                                 RC => QR.RCL.all (i)));
               exception
                  when others =>
                     null; --  Keep processing the List
               end;
            end loop;

         when Par_Line =>
            if Print_Text_Image then
               ATIPut_Line (Line_Image (QR.L));
            end if;
            if Print_Debug_Image then
               ATIPut_Line (Debug_Image (QR.L));
            end if;

         when Par_Line_List =>
            ATIPut_Line ("Line_List size:" &
                         Integer'Wide_Image (QR.LL.all'Length) &
                         "  First:" &
                         Integer'Wide_Image (QR.LL.all'First) &
                         "  Last:" &
                         Integer'Wide_Image (QR.LL.all'Last));

            for i in QR.LL.all'Range loop
               begin
                  Print_Result ((RType => Par_Line, L => QR.LL.all (i)));
               exception
                  when others =>
                     null; --  Keep processing the Line_List
               end;
            end loop;

         when others =>
            Error (ERR_BADPARAM);
      end case;

   exception
      when others =>
         Error (ERR_INTERNAL, "Print_Result");
   end Print_Result;

   -----------
   -- Print --
   -----------

   procedure Print (N : Node_Position) is
      NPtr : Node_Position;

   begin
      if CurStat.Tree (N).NValue = 0 then
         Error (ERR_NEEDPARAM);
      end if;

      NPtr := CurStat.Tree (N).NValue; -- NT_PARAMLIST

      if CurStat.Tree (NPtr).Next_Node /= 0 then
         Error (ERR_TOOMANYPARAMS);
      end if;

      NPtr := CurStat.Tree (NPtr).NValue; --  Parameter of Print

      Print_Result (Evaluate_Node (NPtr));
   end Print;

end ASIStant.Print;
