------------------------------------------------------------------------------
--                                                                          --
--             ASIS Tester And iNTerpreter (ASIStant) COMPONENTS            --
--                                                                          --
--                     A S I S T A N T . F U N C E N U M                    --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--             Copyright (C) 2019, Free Software Foundation, Inc.           --
--                                                                          --
-- ASIStant  is  free  software;  you can redistribute  it and/or modify it --
-- under terms of the  GNU  General Public License as published by the Free --
-- Software Foundation;  either version 3, or ( at your option)  any  later --
-- version.  GNATCHECK  is  distributed in the hope that it will be useful, --
-- but  WITHOUT  ANY  WARRANTY;   without  even  the  implied  warranty  of --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General --
-- Public License for more details.  You should have received a copy of the --
-- GNU General Public License distributed with GNAT; see file  COPYING3. If --
-- not,  go  to  http://www.gnu.org/licenses  for  a  complete  copy of the --
-- license.                                                                 --
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

with Asis.Elements;

package body ASIStant.FuncEnum is
   use Asis;
   use Asis.Text;

   function "=" (Left, Right : Query_Result) return Boolean is
      Result : Boolean;
      Shift  : ASIS_Integer;
   begin

      if Left.RType /= Right.RType then
         return False;
      end if;

      case Left.RType is
         when Par_Absent =>
            return True;
         when Par_String =>
            if Left.S = null then
               if Right.S = null then
                  return True;
               else
                  return False;
               end if;
            else
               if Right.S = null then
                  return False;
               else
                  return Left.S.all = Right.S.all;
               end if;
            end if;

         when Par_Boolean =>
            return Left.B = Right.B;

         when Par_CUnit     =>
            return Asis.Compilation_Units.Is_Equal (Left.C, Right.C);

         when Par_CUnitList =>
            if Left.CL = null and then Right.CL = null then
               return True;
            elsif Left.CL /= null and then Right.CL /= null then
               if Left.CL'Length /= Right.CL'Length then
                  return False;
               else
                  Result := True;

                  Shift := Right.CL'First - Left.CL'First;

                  for J in Left.CL'Range loop
                     if not Asis.Compilation_Units.Is_Equal
                              (Left.CL (J), Right.CL (J + Shift))
                     then
                        Result := False;
                        exit;
                     end if;
                  end loop;

                  return Result;
               end if;
            else
               return False;
            end if;

         when Par_Element   =>
            return Asis.Elements.Is_Equal (Left.E, Right.E);

         when Par_ElemList  =>

            if Left.EL = null and then Right.EL = null then
               return True;
            elsif Left.EL /= null and then Right.EL /= null then
               if Left.EL'Length /= Right.EL'Length then
                  return False;
               else
                  Result := True;

                  Shift := Right.EL'First - Left.EL'First;

                  for J in Left.EL'Range loop
                     if not Asis.Elements.Is_Equal
                       (Left.EL (J), Right.EL (J + Shift))
                     then
                        Result := False;
                        exit;
                     end if;
                  end loop;

                  return Result;
               end if;
            else
               return False;
            end if;

         when Par_Context | Par_Integer =>
            return Left.I = Right.I;

         when Par_Line =>
            return Asis.Text.Is_Equal (Left.L, Right.L);

         when Par_Line_List =>

            if Left.LL = null and then Right.LL = null then
               return True;
            elsif Left.LL /= null and then Right.LL /= null then
               if Left.LL'Length /= Right.LL'Length then
                  return False;
               else
                  Result := True;

                  Shift := Right.LL'First - Left.LL'First;

                  for J in Left.LL'Range loop
                     if not Asis.Text.Is_Equal
                              (Left.LL (J), Right.LL (J + Shift))
                     then
                        Result := False;
                        exit;
                     end if;
                  end loop;

                  return Result;
               end if;
            else
               return False;
            end if;

         when Par_Relationship =>
            --  This is a junk comparision, but the proper solution is really
            --  annoying to implement, and there is no known use of
            --  relationship in asistant
            return Left.R = Right.R;

         when Par_Span      =>
            return Left.Sp = Right.Sp;

         when Par_DDA_Array_Component =>
            return Asis.Data_Decomposition.Is_Equal (Left.AC, Right.AC);

         when Par_DDA_Array_Component_List =>

            if Left.ACL = null and then Right.ACL = null then
               return True;
            elsif Left.ACL /= null and then Right.ACL /= null then
               if Left.ACL'Length /= Right.ACL'Length then
                  return False;
               else
                  Result := True;

                  Shift := Right.ACL'First - Left.ACL'First;

                  for J in Left.ACL'Range loop
                     if not Asis.Data_Decomposition.Is_Equal
                              (Left.ACL (J), Right.ACL (J + Shift))
                     then
                        Result := False;
                        exit;
                     end if;
                  end loop;

                  return Result;
               end if;
            else
               return False;
            end if;

         when Par_DDA_Record_Component =>
            return Asis.Data_Decomposition.Is_Equal (Left.RC, Right.RC);

         when Par_DDA_Record_Component_List =>

            if Left.RCL = null and then Right.RCL = null then
               return True;
            elsif Left.RCL /= null and then Right.RCL /= null then
               if Left.RCL'Length /= Right.RCL'Length then
                  return False;
               else
                  Result := True;

                  Shift := Right.RCL'First - Left.RCL'First;

                  for J in Left.RCL'Range loop
                     if not Asis.Data_Decomposition.Is_Equal
                              (Left.RCL (J), Right.RCL (J + Shift))
                     then
                        Result := False;
                        exit;
                     end if;
                  end loop;

                  return Result;
               end if;
            else
               return False;
            end if;

         when others =>
            return True;
      end case;

   end "=";

end ASIStant.FuncEnum;
