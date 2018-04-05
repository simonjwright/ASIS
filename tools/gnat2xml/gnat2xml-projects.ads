------------------------------------------------------------------------------
--                                                                          --
--                            GNAT2XML COMPONENTS                           --
--                                                                          --
--                     G N A T 2 X M L . P R O J E C T S                    --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                       Copyright (C) 2013, AdaCore                        --
--                                                                          --
-- Gnat2xml is free software; you can redistribute it and/or modify it      --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software Foundation;  either version 2,  or  (at your option)  any later --
-- version. Gnat2xml is distributed  in the hope  that it will be useful,   --
-- but WITHOUT ANY WARRANTY; without even the implied warranty of MER-      --
-- CHANTABILITY or  FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General  --
-- Public License for more details. You should have received a copy of the  --
-- GNU General Public License distributed with GNAT; see file COPYING. If   --
-- not, write to the Free Software Foundation, 59 Temple Place Suite 330,   --
-- Boston, MA 02111-1307, USA.                                              --
-- The gnat2xml tool was derived from the Avatox sources.                   --
------------------------------------------------------------------------------

--  This package adjusts the general project support for ASIS tools for
--  gnat2xml needs.

with GNAT.Command_Line; use GNAT.Command_Line;

with ASIS_UL.Projects;  use ASIS_UL.Projects;

package Gnat2xml.Projects is

   type Gnat2xml_Project_Type is new Arg_Project_Type with null record;

   overriding procedure Scan_Arguments
     (My_Project  : in out Gnat2xml_Project_Type;
      First_Pass  :        Boolean    := False;
      Parser      :        Opt_Parser := Command_Line_Parser;
      In_Switches :        Boolean    := False);

   overriding procedure Print_Tool_Usage
     (My_Project : Gnat2xml_Project_Type);

   procedure Register_Tool_Attributes (My_Project : Gnat2xml_Project_Type);
   --  Registers Default_Switches and Switches attributes in package
   --  Tool_Package_Name. (Both attributes are indexed and have a list as
   --  a value).

   Gnat2xml_Prj : Gnat2xml_Project_Type;

end Gnat2xml.Projects;
