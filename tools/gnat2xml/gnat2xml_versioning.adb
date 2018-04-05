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

with Gnatvsn;

with ASIS_UL.Formatted_Output;

package body Gnat2xml_Versioning is

   ------------------------
   -- Print_Version_Info --
   ------------------------

   procedure Print_Version_Info
     (Tool_Name          : String;
      First_Release_Year : String)
   is
      use ASIS_UL.Formatted_Output;

   begin
      Put ("\1 ", Tool_Name);

      --  We don't want the version string in the schema, because it messes up
      --  the 'diff' done in the Makefile.

      if Tool_Name = "gnat2xsd" then
         null;

      elsif Tool_Name = "gnat2xml" then
         Put ("\1\n", Gnatvsn.Gnat_Version_String);

      else
         raise Program_Error;
      end if;

      --  "\1\2" below is something like "2012" or "2012-2013"

      Put
        ("Copyright (C) \1\2, AdaCore, Inc.\n",
         (if First_Release_Year = Gnatvsn.Current_Year then ""
          else First_Release_Year & "-"),
         Gnatvsn.Current_Year);
   end Print_Version_Info;

end Gnat2xml_Versioning;
