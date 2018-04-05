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

package body Vatox.Axf_Points.Options is

   procedure Set (Selections : in out Axf_Options_Selections;
                  Option     : in     Axf_Point_Options) is
   begin
      Selections.Include (Option);
   end Set;

   ---------------------------------------------------------------------------

   procedure Set_All (Selections : in out Axf_Options_Selections) is
   begin
      for I in Axf_Point_Options loop
         Set (Selections, I);
      end loop;
   end Set_All;

   ---------------------------------------------------------------------------

   function Is_Set (Selections : Axf_Options_Selections;
                    Option     : Axf_Point_Options) return Boolean is
   begin
      return Selections.Contains(Option);
   end Is_Set;

   ---------------------------------------------------------------------------

end Vatox.Axf_Points.Options;
