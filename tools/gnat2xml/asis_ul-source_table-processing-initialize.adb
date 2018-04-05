------------------------------------------------------------------------------
--                                                                          --
--                            GNATPP COMPONENTS                             --
--                                                                          --
--     A S I S _ U L . S O U R C E _ T A B L E . P R O C E S S I N G .      --
--                           I N I T I A L I Z E                            --
--                                                                          --
--             (adapted for gnat2xml from ASIS Utility Library)             --
--                                 B o d y                                  --
--                                                                          --
--                      Copyright (C) 2014, AdaCore                         --
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
------------------------------------------------------------------------------

pragma Ada_2012;

with ASIS_UL.String_Utilities; use ASIS_UL.String_Utilities.String_Vectors;

separate (ASIS_UL.Source_Table.Processing)
procedure Initialize is
begin
   if Incremental_Mode then
      Append (ASIS_UL.Environment.Extra_Inner_Pre_Args,
              String'("-asis-tool-args"));
      Append (ASIS_UL.Environment.Extra_Inner_Post_Args,
              String'("-asis-tool-args"));
   end if;
end Initialize;
