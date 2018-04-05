------------------------------------------------------------------------------
--                                                                          --
--             ASIS Tester And iNTerpreter (ASIStant) COMPONENTS            --
--                                                                          --
--                    A S I S T A N T . S _ P A R S E R                     --
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
------------------------------------------------------------------------------

with Ada.Strings.Wide_Fixed; use Ada.Strings.Wide_Fixed;

with ASIStant.DeAlloc;       use ASIStant.DeAlloc;

package body ASIStant.S_Parser is

------------------------------------------------------------------------------
--  This package provides parsing for the ASIStant language
------------------------------------------------------------------------------

   procedure Reset_Tree is
      NType : Node_Type;
   begin
      for i in 1 .. CurStat.Free - 1 loop
         NType := CurStat.Tree (i).NType;
         if NType = NT_FUNCTION or NType = NT_VARIABLE or
            NType = NT_STRING or NType = NT_INTEGER
         then
            Free (CurStat.Tree (i).SValue);
         end if;
      end loop;
      CurStat.Free := 1;
   end Reset_Tree;

   procedure Get_Func (TS : in out Token_Stream) is
      Cur : Node_Position;
      P   : Natural;
   begin

      --  Get function name
      if not Is_ID (TS) then
         Error (ERR_BADID, Cur_Token (TS));
      end if;
      Cur := CurStat.Free;
      CurStat.Tree (Cur).NType  := NT_FUNCTION;
      P := Cur_Token (TS)'Length;
      CurStat.Tree (Cur).SValue := new Wide_String (1 .. P);
      Move (Cur_Token (TS), CurStat.Tree (Cur).SValue.all);
      CurStat.Tree (Cur).NValue := 0;
      CurStat.Free := CurStat.Free + 1;
      Next_Token (TS);

      --  Check existence of parameters
      if not Is_Active (TS) or else TS.Text (TS.Cur_Token_Start) /= '(' then
         return;
      end if;

      --  Skip '('
      Next_Token (TS);
      CurStat.Tree (Cur).NValue := CurStat.Free;

      --  Get parameters
      loop

         --  Current parameter
         if not Is_Active (TS) or else TS.Text (TS.Cur_Token_Start) = ')' then
            Error (ERR_NEEDPARAM);
         end if;
         Cur := CurStat.Free;
         CurStat.Free := CurStat.Free + 1;
         CurStat.Tree (Cur).NType  := NT_PARAMLIST;
         CurStat.Tree (Cur).NValue := CurStat.Free;

         Get_Expr (TS);

         --  Skip parameter separator
         if Is_Active (TS) and then TS.Text (TS.Cur_Token_Start) = ')' then
            CurStat.Tree (Cur).Next_Node := 0;
            Next_Token (TS); --  Skip ')'
            exit;
         else
            CurStat.Tree (Cur).Next_Node := CurStat.Free;
         end if;

         if not Is_Active (TS) or else TS.Text (TS.Cur_Token_Start) /= ',' then
            Error (ERR_BADPARAMLIST);
         end if;
         Next_Token (TS); --  Skip ','
      end loop;

   end Get_Func;

   procedure Get_Expr (TS : in out Token_Stream) is
      Cur : Node_Position;
      P   : Natural;
   begin

      if not Is_Active (TS) then
         Error (ERR_BADEXPR);
      end if;

      --  Determine expression type
      if Is_ID (TS) then
         Next_Token (TS);
         if Is_Active (TS) and then TS.Text (TS.Cur_Token_Start) = '(' then
            --  Function call with parameters
            Prev_Token (TS);
            Get_Func (TS);
         else
            --  Function call without parameters or variable
            Prev_Token (TS);
            Cur := CurStat.Free;
            begin
               --  Possibly boolean
               CurStat.Tree (Cur).IValue :=
                 Boolean'Pos (Boolean'Wide_Value (Cur_Token (TS)));

               CurStat.Tree (Cur).NType  := NT_BOOLEAN;
               CurStat.Tree (Cur).NValue := 0;
               CurStat.Tree (Cur).Next_Node := 0;
               CurStat.Free := CurStat.Free + 1;
               Next_Token (TS);
            exception
               when Constraint_Error =>
                  --  Value is not boolean
                  CurStat.Tree (Cur).NType  := NT_VARIABLE;
                  P := Cur_Token (TS)'Length;
                  CurStat.Tree (Cur).SValue := new Wide_String (1 .. P);
                  Move (Cur_Token (TS), CurStat.Tree (Cur).SValue.all);
                  CurStat.Tree (Cur).NValue := 0;
                  CurStat.Free := CurStat.Free + 1;
                  Next_Token (TS);
            end;
         end if;
      else
         if Cur_Token (TS) (TS.Cur_Token_Start) = '"' then
            --  String
            Cur := CurStat.Free;
            CurStat.Tree (Cur).NType  := NT_STRING;

            declare
               S : Wide_String (1 .. Cur_Token (TS)'Length - 2);
               C : Natural := 0;
               I : Natural := TS.Cur_Token_Start + 1;
            begin
               loop
                  exit when I > TS.Cur_Token_End - 1;
                  C := C + 1;
                  S (C) := Cur_Token (TS) (I);
                  if Cur_Token (TS) (I) = '"' then  --  Skip double quote
                     I := I + 1;
                  end if;
                  I := I + 1;
               end loop;
               CurStat.Tree (Cur).SValue := new Wide_String (1 .. C);
               CurStat.Tree (Cur).SValue.all := S (1 .. C);
            end;

            CurStat.Tree (Cur).NValue := 0;
            CurStat.Free := CurStat.Free + 1;
            Next_Token (TS);
         else
            if Cur_Token (TS) (TS.Cur_Token_Start) in '0' .. '9' or
               Cur_Token (TS) (TS.Cur_Token_Start) = '-'
            then
               --  Possibly integer
               Cur := CurStat.Free;
               CurStat.Tree (Cur).NType  := NT_INTEGER;
               P := Cur_Token (TS)'Length;
               CurStat.Tree (Cur).SValue := new Wide_String (1 .. P);

               Move (Cur_Token (TS), CurStat.Tree (Cur).SValue.all);

               CurStat.Tree (Cur).NValue := 0;
               CurStat.Free := CurStat.Free + 1;
               Next_Token (TS);
            else
               --  Unknown or empty expression
               Error (ERR_BADEXPR);
            end if;
         end if;
      end if;
   end Get_Expr;

   procedure Get_Stmt (TS : in out Token_Stream) is
   begin

      if TS.Cur_Token_Start = 1 and TS.Cur_Token_End = 0 then
         --  Initialize token stream
         Next_Token (TS);
      end if;

      if not Is_Active (TS) then
         return;
      end if;

      --  Upper level may only contain functions
      Get_Func (TS);

      --  The statement may or may not end with semicolon
      if Is_Active (TS) and then
        Cur_Token (TS) (TS.Cur_Token_Start) = ';'
      then
         Next_Token (TS);
      end if;

   end Get_Stmt;

end ASIStant.S_Parser;
