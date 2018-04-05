------------------------------------------------------------------------------
--                                                                          --
--                          GNATMETRIC COMPONENTS                           --
--                                                                          --
--     A S I S _ U L . S O U R C E _ T A B L E . P R O C E S S I N G .      --
--                             F I N A L I Z E                              --
--                                                                          --
--                (adapted for gnatpp from ASIS Utility Library)            --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                      Copyright (C) 2014-2016, AdaCore                    --
--                                                                          --
-- GNATPP is free software; you can redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion.  GNATPP is  distributed in the  hope that it will  be  useful, but --
-- WITHOUT ANY WARRANTY; without even the implied warranty of  MERCHANTABI- --
-- LITY or FITNESS  FOR A PARTICULAR  PURPOSE.  See the GNU  General Public --
-- License  for more  details.  You should have received a copy of the  GNU --
-- General  Public  License   distributed  with  GNAT;  see  file COPYING3. --
-- If  not,  go to  http://www.gnu.org/licenses  for a complete copy of the --
-- license.                                                                 --
--                                                                          --
-- GNATPP is maintained by AdaCore (http://www.adacore.com)                 --
--                                                                          --
------------------------------------------------------------------------------

with Text_IO;    use Text_IO;
with GNAT.OS_Lib;

with ASIS_UL.String_Utilities; use ASIS_UL.String_Utilities;
with GNATPP.Options; use GNATPP.Options;

separate (ASIS_UL.Source_Table.Processing)
procedure Finalize is
   --  See also comments on File_Name_File_Name in GNATPP.Options.

   --  If this is the outer process of an incremental build, or it is a
   --  non-incremental build, we move all the temp files to the output files.
   --  We don't need any file locking here, because all the inner processes
   --  that were writing to the File_Name_File have finished.

   File_Name_File : File_Type;
   Ignored : Boolean;
   Count : Natural := 0; -- number of files moved
begin
   if not Mimic_gcc
     and then
      not Nothing_To_Do
   then
      Open (File_Name_File, In_File, Name => File_Name_File_Name.all);

      --  The File_Name_File contains an initial blank line, due to Text_IO
      --  weirdness, so we need to discard it.

      declare
         Discard : constant String := Get_Line (File_Name_File);
         pragma Unreferenced (Discard);
      begin
         null;
      end;

      --  Read pairs of lines from the file name file, and do the moves.

      while not End_Of_File (File_Name_File) loop
         Count := Count + 1;
         declare
            Temp_Output_Name : constant String := Get_Line (File_Name_File);
            Output_Name : constant String := Get_Line (File_Name_File);
         begin
            Move_File (Old_Name => Temp_Output_Name, New_Name => Output_Name);
         end;
      end loop;

      Close (File_Name_File);

      if not Debug_Flag_N then
         GNAT.OS_Lib.Delete_File (File_Name_File_Name.all, Ignored);
         --  No point in complaining on failure
      end if;

      if Incremental_Mode and then Count = 0 then
         Put_Line ("files are up to date");
      end if;
   end if;
end Finalize;
