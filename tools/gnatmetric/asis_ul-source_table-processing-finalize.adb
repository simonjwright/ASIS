------------------------------------------------------------------------------
--                                                                          --
--                          GNATMETRIC COMPONENTS                           --
--                                                                          --
--     A S I S _ U L . S O U R C E _ T A B L E . P R O C E S S I N G .      --
--                             F I N A L I Z E                              --
--                                                                          --
--            (adapted for gnatmetric from ASIS Utility Library)            --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                     Copyright (C) 2009-2010, AdaCore                     --
--                                                                          --
-- GNAT Metrics Toolset  is free software;  you can  redistribute it and/or --
-- modify it under terms of the  GNU General Public License as published by --
-- the Free Software Foundation;  either version 2, or (at your option) any --
-- later version.  GNAT Metrics Toolset is  distributed in the hope that it --
-- will be useful, but  WITHOUT ANY WARRANTY; without even the implied war- --
-- ranty of  MERCHANTABILITY or  FITNESS FOR A PARTICULAR PURPOSE.  See the --
-- GNU General Public License for more details.  You should have received a --
-- copy of the  GNU General Public License distributed with  GNAT; see file --
-- COPYING.  If not,  write to the  Free Software  Foundation,  51 Franklin --
-- Street, Fifth Floor, Boston, MA 02110-1301, USA.                         --
--                                                                          --
-- GNAT Metrics Toolset is maintained by AdaCore (http://www.adacore.com).  --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Text_IO;    use Ada.Text_IO;

with METRICS.Common; use  METRICS.Common;
with METRICS.Output; use  METRICS.Output;

separate (ASIS_UL.Source_Table.Processing)
procedure Finalize is
begin
   METRICS.Output.Report_Global_Statistics;

   if Global_File_Name /= null then

      if Global_Output /= null
        and then
         Is_Open (Global_Output.all)
      then
         Close (Global_Out_File);
      end if;

   end if;

   if Is_Open (XML_Out_File) then
      Close (XML_Out_File);

      if Generate_XML_Schema then
         Write_XML_Schema;
      end if;
   end if;

end Finalize;
