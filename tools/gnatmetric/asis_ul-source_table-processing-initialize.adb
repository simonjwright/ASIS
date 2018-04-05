------------------------------------------------------------------------------
--                                                                          --
--                            GNATPP COMPONENTS                             --
--                                                                          --
--     A S I S _ U L . S O U R C E _ T A B L E . P R O C E S S I N G .      --
--                           I N I T I A L I Z E                            --
--                                                                          --
--            (adapted for gnatmetric from ASIS Utility Library)            --
--                                 B o d y                                  --
--                                                                          --
--                       Copyright (C) 2014, AdaCore                        --
--                                                                          --
-- GNATPP is free software; you can redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion.  GNATPP is  distributed in the  hope that it will  be  useful, but --
-- WITHOUT ANY WARRANTY; without even the implied warranty of  MERCHANTABI- --
-- LITY or  FITNESS  FOR A  PARTICULAR  PURPOSE. See the GNU General Public --
-- License  for more details. You  should  have  received a copy of the GNU --
-- General Public License  distributed with GNAT; see file COPYING. If not, --
-- write to the Free Software Foundation,  51 Franklin Street, Fifth Floor, --
-- Boston, MA 02110-1301, USA.                                              --
--                                                                          --
-- GNATPP is maintained by AdaCore (http://www.adacore.com)                 --
--                                                                          --
------------------------------------------------------------------------------

pragma Ada_2012;

with ASIS_UL.Environment;
with ASIS_UL.Options;
with ASIS_UL.String_Utilities; use ASIS_UL.String_Utilities.String_Vectors;

separate (ASIS_UL.Source_Table.Processing)
procedure Initialize is
begin
   if not ASIS_UL.Options.Nothing_To_Do then
      if Incremental_Mode then
         Append (ASIS_UL.Environment.Extra_Inner_Pre_Args,
                 String'("-asis-tool-args"));
         Append (ASIS_UL.Environment.Extra_Inner_Post_Args,
                 String'("-asis-tool-args"));
      end if;
   end if;
end Initialize;
