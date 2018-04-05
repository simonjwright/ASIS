------------------------------------------------------------------------------
--                                                                          --
--                           GNATELIM COMPONENTS
--                                                                          --
--     A S I S _ U L . S O U R C E _ T A B L E . P R O C E S S I N G .      --
--                      A S I S _ P R O C E S S I N G                       --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                     Copyright (C) 2008-2009, AdaCore                     --
--                                                                          --
-- GNATELIM  is  free software;  you can  redistribute it and/or  modify it --
-- under the terms of the  GNU  General Public License  as published by the --
-- Free Software Foundation; either version 2 or (at your option) any later --
-- version. GNATELIM is distributed in the hope that it will be useful, but --
-- WITHOUT ANY WARRANTY; without even the implied warranty of  MERCHANTABI- --
-- LITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public Li- --
-- cense for  more details.  You should  have  received  a copy of the  GNU --
-- General Public License distributed with GNAT; see file COPYING.  If not, --
-- write to the  Free Software Foundation, 51 Franklin Street, Fifth Floor, --
-- Boston, MA 02110-1301, USA.                                              --
--                                                                          --
-- The original version  of  Gnatelim  was developed by  Alain  Le  Guennec --
-- It is now maintained by AdaCore (http://www.adacore.com)                 --
--                                                                          --
------------------------------------------------------------------------------

with Asis.Compilation_Units;
with Asis.Elements;

with ASIS_UL.Global_State.CG;

separate (ASIS_UL.Source_Table.Processing)
procedure ASIS_Processing (CU : Asis.Compilation_Unit; SF : SF_Id) is
   Unit : constant Asis.Element := Asis.Elements.Unit_Declaration (CU);
begin

   Set_CU_Name (SF, To_String (Asis.Compilation_Units.Unit_Full_Name (CU)));

   ASIS_UL.Global_State.CG.Check_For_Main_Unit (SF, CU, Unit);

   ASIS_UL.Global_State.CG.Collect_CG_Info_From_Construct (Unit);
   Set_Source_Status (SF, Processed);
exception
   when Fatal_Error =>
      Set_Source_Status (SF, Error_Detected);
   when Ex : others =>
      Tool_Failures := Tool_Failures + 1;
      Report_Unhandled_Exception (Ex);
      Set_Source_Status (SF, Error_Detected);
end ASIS_Processing;
