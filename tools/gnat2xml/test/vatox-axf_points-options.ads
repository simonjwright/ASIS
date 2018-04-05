------------------------------------------------------------------------------
--                                                                          --
--                           AVATOX COMPONENTS                              --
--                                                                          --
--                        VATOX (Via Asis To Xml)                           --
--                                                                          --
--                Copyright (c) 2007, McKae Technologies.                   --
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
-- Avatox is now maintained by McKae Technologies (http://www.mckae.com)    --                                               --
------------------------------------------------------------------------------

with Ada.Containers.Ordered_Sets;

package Vatox.Axf_Points.Options is

   -- axfPoint content options
   type Axf_Point_Options is (Axf_Cross_References,
                              Axf_Decl_Enclosures,
                              Axf_Terminal_Reps
                             );

   -- Maintains the selected AXF Point generation options
   type Axf_Options_Selections is private;

   -- Set an AXF option.
   procedure Set (Selections : in out Axf_Options_Selections;
                  -- AXF options selections

                  Option     : in     Axf_Point_Options
                  -- Option to set
                 );

   -- Set all the AXF content options
   procedure Set_All (Selections : in out Axf_Options_Selections
                      -- AXF options selections
                     );

   -- Return whether an AXF option has been set
   function Is_Set (Selections : Axf_Options_Selections;
                    -- Current AXF options selections

                    Option     : Axf_Point_Options
                    -- Option to check on
                   ) return Boolean;

private
   package Options_Set is new
     Ada.Containers.Ordered_Sets (Axf_Point_Options);

   type Axf_Options_Selections is new Options_Set.Set with null record;

end Vatox.Axf_Points.Options;
