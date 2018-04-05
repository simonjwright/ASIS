------------------------------------------------------------------------------
--                                                                          --
--             ASIS Tester And iNTerpreter (ASIStant) COMPONENTS            --
--                                                                          --
--                         A S I S T A N T . G E T                          --
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

with ASIStant.Common;   use ASIStant.Common;
with ASIStant.Evaluate; use ASIStant.Evaluate;

package body ASIStant.Get is

------------------------------------------------------------------------------
--  This package provides retrieval of data from the ASIStant syntax tree
------------------------------------------------------------------------------

   function Get_String  (N : Node_Position) return String_Ptr is
      QR : constant Query_Result := Evaluate_Node (N);
   begin
      if QR.RType /= Par_String then
         Error (ERR_BADSTRING);
      end if;
      return QR.S;
   end Get_String;

   function Get_Integer (N : Node_Position) return Integer is
      QR : constant Query_Result := Evaluate_Node (N);
   begin
      if QR.RType /= Par_Integer then
         Error (ERR_BADINTEGER);
      end if;
      return QR.I;
   end Get_Integer;

end ASIStant.Get;
