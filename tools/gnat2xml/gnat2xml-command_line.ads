------------------------------------------------------------------------------
--                                                                          --
--                            GNAT2XML COMPONENTS                           --
--                                                                          --
--                 G N A T 2 X M L . C O M M A N D _ L I N E                --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                  Copyright (C) 2012-2014, AdaCore, Inc.                  --
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

pragma Ada_2012;

--  Command Line Arguments. ???This is mostly superseded by ASIS_UL.

package Gnat2xml.Command_Line is

   --  Call Set_Gnat2xml_Options first (exactly once). Then call
   --  Gnat2xml_Options as many times as you like to get the argument values.

   type Gnat2xml_Options_Type is record
      Compact_XML : Boolean := False;
      --  True if the --compact switch was given, which means we should
      --  generate less verbose XML. In particular, we should generate XML
      --  according to the ada-schema.compact.xsd schema, rather than the
      --  ada-schema.xsd one. This also intersperses the source text with the
      --  XML.
   end record;

   Options : Gnat2xml_Options_Type;
   --  ???

   procedure Set_Gnat2xml_Options (Options : Gnat2xml_Options_Type);
   function Gnat2xml_Options return access constant Gnat2xml_Options_Type;

   procedure Gnat2xml_Usage;
   --  Prints a usage message

end Gnat2xml.Command_Line;
