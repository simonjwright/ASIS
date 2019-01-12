------------------------------------------------------------------------------
--                                                                          --
--                          GNATCHECK COMPONENTS                            --
--                                                                          --
--                   G N A T C H E C K . P R O J E C T S                    --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                     Copyright (C) 2013-2018, AdaCore                     --
--                                                                          --
-- GNATCHECK  is  free  software;  you can redistribute it and/or modify it --
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
-- GNATCHECK is maintained by AdaCore (http://www.adacore.com).             --
--                                                                          --
------------------------------------------------------------------------------

pragma Ada_2012;

--  This package adjusts the general project support for ASIS tools for
--  gnatcheck needs.

with GNAT.Command_Line; use GNAT.Command_Line;

with ASIS_UL.Projects;  use ASIS_UL.Projects;

package Gnatcheck.Projects is

   type Gnatcheck_Project_Type is new Arg_Project_Type with null record;

   overriding function Tool_Package_Name
     (My_Project : Gnatcheck_Project_Type)
      return       String;
   --  Returns "check"

   overriding function Section_Delimiters
     (My_Project : Gnatcheck_Project_Type)
      return       String;
   --  Returns "cargs rules"

   overriding procedure Scan_Arguments
     (My_Project  : in out Gnatcheck_Project_Type;
      First_Pass  :         Boolean    := False;
      Parser      :         Opt_Parser := Command_Line_Parser;
      In_Switches :         Boolean    := False);

   overriding procedure Print_Tool_Usage (My_Project : Gnatcheck_Project_Type);

   overriding procedure Close_Aggregate_Project_Report
     (My_Project : Gnatcheck_Project_Type);

   overriding procedure Aggregate_Project_Report_Header
     (My_Project : Gnatcheck_Project_Type);

   overriding procedure Report_Aggregated_Project_Exit_Code
     (Aggregate_Prj : Gnatcheck_Project_Type;
      Exit_Code     : Integer);

end Gnatcheck.Projects;
