------------------------------------------------------------------------------
--                                                                          --
--                           AVATOX COMPONENTS                              --
--                                                                          --
--                    AVATOX (Ada, Via Asis, To Xml)                        --
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
--                                                                          --
-- Avatox is maintained by McKae Technologies (http://www.mckae.com)        --
--                                                                          --
------------------------------------------------------------------------------
with Mckae.XML.EZ_Out.Generic_Medium;
with Ada.Text_IO;
use  Ada.Text_IO;

pragma Elaborate_All (Mckae.Xml.Ez_Out.Generic_Medium);

package Vatox.Xml_File_Output is
   new Mckae.XML.EZ_Out.Generic_Medium
  (Output_Medium       => Ada.Text_IO.File_Type,
   Max_Element_Nesting => 20_000);
