------------------------------------------------------------------------------
--                                                                          --
--                           GNATSTUB COMPONENTS                            --
--                                                                          --
--                     G N A T S T U B . P R O J E C T S                    --
--                                                                          --
--                                  S p e c                                 --
--                                                                          --
--                    Copyright (C) 2013-2014, AdaCore                      --
--                                                                          --
-- Gnatstub  is  free  software;  you can  redistribute it and/or modify it --
-- under the terms of the  GNU  General Public License  as published by the --
-- Free Software Foundation; either version 3 or (at your option) any later --
-- version. Gnatstub is distributed in the hope that it will be useful, but --
-- WITHOUT ANY WARRANTY; without even the implied warranty of  MERCHANTABI- --
-- LITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public Li- --
-- cense for  more details.  You should  have  received  a copy of the  GNU --
-- General Public License distributed with GNAT; see file COPYING3. If not, --
-- go to http://www.gnu.org/licenses for a complete copy of the license.    --
--                                                                          --
-- Gnatstub  was  originally  developed  by  Alexei  Kuchumov  as a part of --
-- collaboration  between  Software  Engineering  Laboratory  of  the Swiss --
-- Federal  Institute  of  Technology  in  Lausanne,  Switzerland, and  the --
-- Scientific  Research  Computer  Center  of the  Moscow State University, --
-- Russia.  This  work  was  supported  by  a grant from the Swiss National --
-- Science Foundation,  no 7SUPJ048247,  funding a project  "Development of --
-- ASIS for GNAT with industry quality".                                    --
--                                                                          --
-- Gnatstub is now maintained by AdaCore (http://www.adacore.com).          --
------------------------------------------------------------------------------

--  This package adjusts the general project support for ASIS tools for
--  gnatstub needs.

pragma Ada_2012;

with GNAT.Command_Line; use GNAT.Command_Line;

with ASIS_UL.Projects;  use ASIS_UL.Projects;

package Gnatstub.Projects is

   type Gnatstub_Project_Type is new Arg_Project_Type with null record;

   overriding function Compute_Project_Closure
     (My_Project  : Gnatstub_Project_Type)
      return        Boolean is (False);
   --  gnatstub has exactly one argument and it requires it to be specified
   --  explicitly.

   overriding procedure Print_Tool_Usage (My_Project : Gnatstub_Project_Type);

   overriding procedure Scan_Arguments
     (My_Project  : in out Gnatstub_Project_Type;
      First_Pass  :        Boolean    := False;
      Parser      :        Opt_Parser := Command_Line_Parser;
      In_Switches :        Boolean    := False);
   --  This procedure differs from a typical parameter scanner for ASIS tools
   --  in the following aspects:
   --
   --  * '-U [main]' option is not supported;
   --  * '--subdirs' and '--no_objects_dir' options are not supported;
   --  * '-files=filename" is not supported

   overriding function Tool_Package_Name
     (My_Project : Gnatstub_Project_Type)
      return       String;
   --  Returns "Gnatstub"

end Gnatstub.Projects;
