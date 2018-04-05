------------------------------------------------------------------------------
--                                                                          --
--                          GNAT2XML COMPONENTS                             --
--                           AVATOX COMPONENTS                              --
--                                                                          --
--                           Avatox_Versioning                              --
--                                                                          --
--                                                                          --
--                Copyright (C) 2006, McKae Technologies.                   --
--                Copyright (C) 2012-2013, AdaCore, Inc.                    --
--                                                                          --
-- Avatox is free software; you can redistribute it and/or modify it        --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software Foundation;  either version 2,  or  (at your option)  any later --
-- version. Avatox is distributed in the hope  that it will be useful,      --
-- but WITHOUT ANY WARRANTY; without even the implied warranty of MER-      --
-- CHANTABILITY or  FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General  --
-- Public License for more details. You should have received a copy of the  --
-- GNU General Public License distributed with GNAT; see file COPYING. If   --
-- not, write to the Free Software Foundation, 59 Temple Place Suite 330,   --
-- Boston, MA 02111-1307, USA.                                              --
--                                                                          --
-- Avatox is based off the Display_Source software distributed as part of   --
-- the ASIS implementation for GNAT, and therefore inherits its GPL         --
-- licensing.  Ada Core Technologies maintains the Display_Source program   --
-- and its copyright is held by the Free Software Foundation.               --
--                                                                          --
-- Avatox is now maintained by McKae Technologies (http://www.mckae.com)    --
-- The gnat2xml tool was derived from the Avatox sources.                   --
------------------------------------------------------------------------------

pragma Ada_2012;

package Gnat2xml_Versioning is

   procedure Print_Version_Info
     (Tool_Name          : String;
      First_Release_Year : String);
   --  Prints version information. First_Release_Year is the year the tool was
   --  first released. Do not update the call site with the current year; it's
   --  supposed to be the _first_ year in which some version of the product was
   --  released.

   ----------------------------------------------------------------

   --  Obsolete avatox code follows.

   --  Product name, acronym for Ada, Via Asis, TO Xml
   Product_Name : constant String := "Avatox";

   --  Product vendor
   Vendor : constant String := "McKae Technologies (www.mckae.com)";

   --  Current Avatox version
   Version : constant String := "1.8";

end Gnat2xml_Versioning;
