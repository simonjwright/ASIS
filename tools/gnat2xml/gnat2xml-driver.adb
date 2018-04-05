------------------------------------------------------------------------------
--                                                                          --
--                            GNAT2XML COMPONENTS                           --
--                                                                          --
--                       G N A T 2 X M L . D R I V E R                      --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                      Copyright (C) 2013-2016, AdaCore                    --
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

with ASIS_UL.Common;
with ASIS_UL.Driver;

with Gnat2xml.Projects;

procedure Gnat2xml.Driver is
begin
   ASIS_UL.Common.Preprocessing_Allowed := False;
   ASIS_UL.Driver (Gnat2xml.Projects.Gnat2xml_Prj);
end Gnat2xml.Driver;
