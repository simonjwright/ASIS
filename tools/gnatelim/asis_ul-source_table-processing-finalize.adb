------------------------------------------------------------------------------
--                                                                          --
--                           GNATELIM COMPONENTS
--                                                                          --
--                ASIS_UL.SOURCE_TABLE.PROCESSING.FINALIZE                  --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                     Copyright (C) 2008-2016, AdaCore                     --
--                                                                          --
-- GNATELIM  is  free software;  you can  redistribute it and/or  modify it --
-- under the terms of the  GNU  General Public License  as published by the --
-- Free Software Foundation; either version 3 or (at your option) any later --
-- version. GNATELIM is distributed in the hope that it will be useful, but --
-- WITHOUT ANY WARRANTY; without even the implied warranty of  MERCHANTABI- --
-- LITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public Li- --
-- cense for  more details.  You should  have  received  a copy of the  GNU --
-- General Public License distributed with GNAT; see file COPYING3. If not, --
-- go to http://www.gnu.org/licenses for a complete copy of the license.    --
--                                                                          --
-- The original version  of  Gnatelim  was developed by  Alain  Le  Guennec --
-- It is now maintained by AdaCore (http://www.adacore.com)                 --
--                                                                          --
------------------------------------------------------------------------------

with ASIS_UL.Global_State;
with ASIS_UL.Global_State.CG;
with ASIS_UL.Global_State.CG.Gnatelim;
with ASIS_UL.Output;

with Gnatelim.Output;

separate (ASIS_UL.Source_Table.Processing)
procedure Finalize is
begin

   ASIS_UL.Global_State.CG.Transitive_Closure;

   ASIS_UL.Global_State.CG.Gnatelim.Mark_Used_Subprograms;

   if Debug_Flag_3 then
      ASIS_UL.Global_State.Print_Global_Structure;
      Source_Table_Debug_Image;
   end if;

   Gnatelim.Output.Report_Unused_Subprograms;

   ASIS_UL.Output.Close_Report_Files;

   if Tool_Failures > 0 then
      Info ("Total tool failures :" & Tool_Failures'Img);
   end if;

end Finalize;
