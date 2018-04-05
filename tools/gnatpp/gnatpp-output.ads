------------------------------------------------------------------------------
--                                                                          --
--                            GNATPP COMPONENTS                             --
--                                                                          --
--                        G N A T P P . O U T P U T                         --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                    Copyright (C) 2001-2015, AdaCore                      --
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

--  This package defines various output routines

package GNATPP.Output is

   procedure Brief_Help;
   --  Prints the brief help into the error stream.

   procedure Print_Gnatpp_Usage;
   --  Similar to Brief_Help, but corresponds to the general format generated
   --  by other GNAT tools for '--help' option, and sends the output into
   --  Stdout

   procedure XML_Help;
   --  Prints the XML description of the tool's options to be used to create
   --  the gnatpp GUI in GPS.

   procedure Set_Form_String;
   --  Sets the value used for the Form parameter of Create and Open
   --  procedures, this value defines the encoding used for the output file(s).

end GNATPP.Output;
