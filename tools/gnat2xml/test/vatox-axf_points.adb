------------------------------------------------------------------------------
--                                                                          --
--                           AVATOX COMPONENTS                              --
--                                                                          --
--                        VATOX (Via Asis To Xml)                           --st
--                                                                          --
--                                                                          --
--                Copyright (c) 2006, McKae Technologies.                   --
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
--                                                                          --                                                                      --
-- Avatox is now maintained by McKae Technologies (http://www.mckae.com)    --                                               --
------------------------------------------------------------------------------

with Avatox_Versioning;
with Vatox.Axf_Pedigrees;
pragma Elaborate (Vatox.Axf_Pedigrees);

package body Vatox.Axf_Points is

begin
   -- Register the AXF_Point pedigree
   Vatox.Axf_Pedigrees.Register
     (AXF_Point_Tag,
      Vatox.Axf_Pedigrees.Processor_Info,
      Avatox_Versioning.Product_Name,
      Avatox_Versioning.Vendor,
      Avatox_Versioning.Version);

end Vatox.Axf_Points;
