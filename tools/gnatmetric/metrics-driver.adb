------------------------------------------------------------------------------
--                                                                          --
--                      GNAT METRICS TOOLS COMPONENTS                       --
--                                                                          --
--                              M E T R I C S                               --
--                                                                          --
--                       M E T R I C S . D R I V E R                        --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                     Copyright (C) 2013-2014, AdaCore                     --
--                                                                          --
-- GNATMETRIC  is  free software; you can  redistribute it and/or modify it --
-- under terms of the  GNU  General Public License as published by the Free --
-- Software  Foundation;  either version 3, or (at your option)  any  later --
-- version. GNATMETRIC  is  distributed in the hope that it will be useful, --
-- but  WITHOUT  ANY  WARRANTY;   without  even  the  implied  warranty  of --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General --
-- Public License for more details.  You should have received a copy of the --
-- GNU General Public License distributed with GNAT; see file  COPYING3. If --
-- not,  go  to  http://www.gnu.org/licenses  for  a  complete  copy of the --
-- license.                                                                 --
--                                                                          --
-- GNATMETRIC is maintained by AdaCore (http://www.adacore.com).            --
--                                                                          --
------------------------------------------------------------------------------

with ASIS_UL.Driver;

with METRICS.Options;

procedure METRICS.Driver is
begin
   ASIS_UL.Driver (METRICS.Options.Gnatmetric_Prj);
end METRICS.Driver;
