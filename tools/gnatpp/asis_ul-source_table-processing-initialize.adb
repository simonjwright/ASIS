------------------------------------------------------------------------------
--                                                                          --
--                            GNATPP COMPONENTS                             --
--                                                                          --
--     A S I S _ U L . S O U R C E _ T A B L E . P R O C E S S I N G .      --
--                           I N I T I A L I Z E                            --
--                                                                          --
--              (adapted for gnatpp from ASIS Utility Library)              --
--                                 B o d y                                  --
--                                                                          --
--                    Copyright (C) 2009-2015, AdaCore                      --
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

with Ada.Text_IO;
with Ada.Directories; use Ada;

with ASIS_UL.Environment;
with ASIS_UL.Options;
with ASIS_UL.String_Utilities; use ASIS_UL.String_Utilities.String_Vectors;

with GNATPP.Options;      use GNATPP.Options;
with GNATPP.Output;       use GNATPP.Output;

separate (ASIS_UL.Source_Table.Processing)
procedure Initialize is
   File_Name_File : Text_IO.File_Type;
begin
   if not ASIS_UL.Options.Nothing_To_Do then
      if Mimic_gcc then
         pragma Assert (Directories.Exists (File_Name_File_Name.all),
                        File_Name_File_Name.all & " not found");
      else
         File_Name_File_Name := new String'
           (Directories.Compose (Tool_Temp_Dir.all, "file_names"));

         --  Create an empty file name file, so ASIS_Processing can append to
         --  it. (Small annoyance: the file is not actually empty; it contains
         --  a single blank line, and Finalize has to work around that.)

         Text_IO.Create (File_Name_File,
                         Name => File_Name_File_Name.all);
         Text_IO.Close (File_Name_File);

         if Incremental_Mode then
            Append (ASIS_UL.Environment.Extra_Inner_Pre_Args,
                    String'("-asis-tool-args"));
            Append (ASIS_UL.Environment.Extra_Inner_Post_Args,
                    String'("-asis-tool-args"));
            Append (Extra_Inner_Pre_Args,
                    String'("--file-name-file=" & File_Name_File_Name.all));
         end if;
      end if;
   end if;
end Initialize;
