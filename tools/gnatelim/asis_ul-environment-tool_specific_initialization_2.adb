------------------------------------------------------------------------------
--                                                                          --
--                           GNATELIM COMPONENTS
--                                                                          --
--                 A S I S _ U L . E N V I R O N M E N T .                  --
--       T O O L_ S P E C I F I C _ I N I T I A L I Z A T I O N _ 2         --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                       Copyright (C) 2008, AdaCore                        --
--                                                                          --
-- GNATELIM  is  free software;  you can  redistribute it and/or  modify it --
-- under the terms of the  GNU  General Public License  as published by the --
-- Free Software Foundation; either version 2 or (at your option) any later --
-- version. GNATELIM is distributed in the hope that it will be useful, but --
-- WITHOUT ANY WARRANTY; without even the implied warranty of  MERCHANTABI- --
-- LITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public Li- --
-- cense for  more details.  You should  have  received  a copy of the  GNU --
-- General Public License distributed with GNAT; see file COPYING.  If not, --
-- write to the  Free Software Foundation, 51 Franklin Street, Fifth Floor, --
-- Boston, MA 02110-1301, USA.                                              --
--                                                                          --
-- The original version  of  Gnatelim  was developed by  Alain  Le  Guennec --
-- It is now maintained by AdaCore (http://www.adacore.com)                 --
--                                                                          --
------------------------------------------------------------------------------

with ASIS_UL.Global_State;
separate (ASIS_UL.Environment)
procedure Tool_Specific_Initialization_2 is
begin
   ASIS_UL.Global_State.Initialize;
end Tool_Specific_Initialization_2;
