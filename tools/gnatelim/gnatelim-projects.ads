------------------------------------------------------------------------------
--                                                                          --
--                           GNATELIM COMPONENTS                            --
--                                                                          --
--                    G N A T E L I M . P R O J E C T S                     --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                    Copyright (C) 2013-2018, AdaCore                      --
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

--  This package adjusts the general project support for ASIS tools for
--  gnatelim needs.

pragma Ada_2012;

with GNAT.Command_Line; use GNAT.Command_Line;

with ASIS_UL.Projects;  use ASIS_UL.Projects;

package Gnatelim.Projects is

   type Gnatelim_Project_Type is new Arg_Project_Type with null record;

   overriding function Compute_Project_Closure
     (My_Project  : Gnatelim_Project_Type)
      return        Boolean is (U_Option_Set);
   --  gnatelim requires the main unit to be specified explicitly.

   overriding procedure Print_Tool_Usage (My_Project : Gnatelim_Project_Type);

   overriding procedure Scan_Arguments
     (My_Project  : in out Gnatelim_Project_Type;
      First_Pass  :        Boolean    := False;
      Parser      :        Opt_Parser := Command_Line_Parser;
      In_Switches :        Boolean    := False);
   --  This procedure differs from a typical parameter scanner for ASIS tools
   --  in the following aspects:
   --
   --  * '-U [main]' option is not supported;
   --  * '--subdirs' and '--no_objects_dir' options are not supported;

   overriding function Tool_Package_Name
     (My_Project : Gnatelim_Project_Type)
      return       String;
   --  Returns "Eliminate"

end Gnatelim.Projects;
