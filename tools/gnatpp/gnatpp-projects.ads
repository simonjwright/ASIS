------------------------------------------------------------------------------
--                                                                          --
--                            GNATPP COMPONENTS                             --
--                                                                          --
--                      G N A T P P . P R O J E C T S                       --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                       Copyright (C) 2013, AdaCore                        --
--                                                                          --
-- GNATPP  is free software; you can redistribute it and/or modify it under --
-- terms  of  the  GNU  General  Public  License  as  published by the Free --
-- Software Foundation;  either version 3, or ( at your option)  any  later --
-- version.  GNATCHECK  is  distributed in the hope that it will be useful, --
-- but  WITHOUT  ANY  WARRANTY;   without  even  the  implied  warranty  of --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General --
-- Public License for more details.  You should have received a copy of the --
-- GNU General Public License distributed with GNAT; see file  COPYING3. If --
-- not,  go  to  http://www.gnu.org/licenses  for  a  complete  copy of the --
-- license.                                                                 --
--                                                                          --
-- GNATPP is maintained by AdaCore (http://www.adacore.com)                 --
--                                                                          --
------------------------------------------------------------------------------

--  This package adjusts the general project support for ASIS tools for
--  gnatpp needs.

pragma Ada_2012;

with GNAT.Command_Line; use GNAT.Command_Line;

with ASIS_UL.Projects;  use ASIS_UL.Projects;

package GNATPP.Projects is

   type Gnatpp_Project_Type is new Arg_Project_Type with null record;

   overriding function Tool_Package_Name
     (My_Project : Gnatpp_Project_Type)
      return       String is ("Pretty_Printer");

   overriding procedure Scan_Arguments
     (My_Project  : in out Gnatpp_Project_Type;
      First_Pass  :        Boolean    := False;
      Parser      :        Opt_Parser := Command_Line_Parser;
      In_Switches :        Boolean    := False);

   overriding procedure Print_Tool_Usage
     (My_Project : Gnatpp_Project_Type);

end GNATPP.Projects;
