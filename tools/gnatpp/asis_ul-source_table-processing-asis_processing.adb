------------------------------------------------------------------------------
--                                                                          --
--                            GNATPP COMPONENTS                             --
--                                                                          --
--     A S I S _ U L . S O U R C E _ T A B L E . P R O C E S S I N G .      --
--                      A S I S _ P R O C E S S I N G                       --
--                                                                          --
--              (adapted for gnatpp from ASIS Utility Library)              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                     Copyright (C) 2009-2017, AdaCore                     --
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
-- Boston,                                                                  --
--                                                                          --
-- GNATPP is maintained by AdaCore (http://www.adacore.com)                 --
--                                                                          --
------------------------------------------------------------------------------

pragma Ada_2012;

with Ada.Directories; use Ada.Directories;
with Ada.Exceptions;
with Ada.Finalization;
with Ada.Text_IO; use Ada;
with Ada_Trees.PP;
with Ada.Wide_Text_IO;

with System.WCh_Con;
with Opt;

with GNATPP.Output; use GNATPP;
with GNATPP.Options; use GNATPP.Options;

separate (ASIS_UL.Source_Table.Processing)
procedure ASIS_Processing (CU : Asis.Compilation_Unit; SF : SF_Id) is
   --  See also comments on File_Name_File_Name in GNATPP.Options.

   pragma Assert (not Incremental_Mode);

   --  We initially write the output to Temp_Output_Name, then later rename it
   --  to Output_Name (except in Pipe mode). These are full pathnames. If we
   --  are overwriting the Source_Name, and it's a link link-->file, we want to
   --  overwrite file. But we put the temp file in the directory containing
   --  link, in case the directory containing file is not writable.

   function Get_Output_Name (Resolve_Links : Boolean) return String is
     (case Output_Mode is
        when Pipe => "", -- not used
        when Create_Modes => Res_File_Name.all,
        when Replace_Modes => Normalize_Pathname
                                (Source_Name (SF),
                                 Resolve_Links  => Resolve_Links,
                                 Case_Sensitive => True),

        when Default => Source_Name (SF) & PP_Suffix,
        when Out_Directory => Compose (Out_Dir.all, Short_Source_Name (SF)));

   Output_Name : constant String := Get_Output_Name (Resolve_Links => True);

   Temp_Output_Name : constant String :=
       (if Output_Mode = Pipe then "" -- means standard output
        else Get_Output_Name (Resolve_Links => False) & "__GNATPP-TEMP");

   Output_Written : Boolean;
   --  True if Tree_To_Ada wrote the output to Temp_Output_Name. It always
   --  does, except in Replace_Modes if the output would be identical to the
   --  input.

   procedure Write_File_Name_File;
   --  If the Output_Mode /= Pipe, and Output_Written is True, add a pair of
   --  lines to the file name file.

   procedure Set_Output_Encoding;
   --  Set the output encoding method. Default is as for the input.

   procedure Write_File_Name_File is
      use Text_IO;
      Lock_File_Name : constant String := File_Name_File_Name.all & ".lock";

      procedure Do_Writes;
      --  Write the two file names to the file name file. This is split out
      --  into a procedure so we can call it with and without file locking, as
      --  appropriate.

      procedure Do_Writes is
         File_Name_File : File_Type;
      begin
         Open (File_Name_File,
               Mode => Append_File,
               Name => File_Name_File_Name.all);
         Put_Line (File_Name_File, Temp_Output_Name);
         Put_Line (File_Name_File, Output_Name);
         Close (File_Name_File);
      end Do_Writes;

   --  Start of processing for Write_File_Name_File

   begin
      if Output_Mode /= Pipe then
         --  In -r, -rf, and -rnb modes, if the output was identical to the
         --  input, Output_Written will be False, so there is no
         --  Temp_Output_Name file, so we don't move it in that case. This can
         --  also happen if the exception handler at the end of Tree_To_Ada is
         --  executed.

         pragma Assert
           (if Output_Mode not in Replace_Modes then Output_Written);
         if not Output_Written then
            return;
         end if;

         if Mimic_gcc and then (Verbose_Mode or else Debug_Flag_V) then
            Put_Line
              ((if Output_Mode in Replace_Modes
                  then "updating "
                  else "creating ") &
               (if Debug_Flag_V then Short_Source_Name (SF) else Output_Name));
         end if;

         --  The temp file was created, so write a pair (Temp_Output_Name,
         --  Output_Name) of lines to the file name file, so Finalize will know
         --  to rename temp --> output. This is done under lock, in case this
         --  is an inner process of an incremental build, and the -j switch of
         --  the builder is used to invoke this in parallel.

         if Outer_Parallel then
            pragma Assert (Mimic_gcc);
            Lock_File (Lock_File_Name);
            declare
               --  We create a dummy object whose finalization calls
               --  Unlock_File, so we don't leave stale lock files around even
               --  in case of unhandled exceptions.

               type Dummy_Type is new Ada.Finalization.Limited_Controlled with
                 null record;
               procedure Finalize (Ignore : in out Dummy_Type);
               procedure Finalize (Ignore : in out Dummy_Type) is
               begin
                  Unlock_File (Lock_File_Name);
               end Finalize;

               Dummy : Dummy_Type;

            begin
               Do_Writes;
            end;

         --  Otherwise, it's safe to do the writes without any locking. We want
         --  to avoid locking when possible, because it reduces the likelihood
         --  of stale locks left lying around. It's a little more efficient,
         --  too.

         else
            Do_Writes;
         end if;
      end if;
   exception
      when Lock_Error =>
         ASIS_UL.Output.Error ("cannot create " & Lock_File_Name);
         ASIS_UL.Output.Error ("delete it by hand if stale");
         raise;
   end Write_File_Name_File;

   procedure Set_Output_Encoding is
      use System.WCh_Con;
   begin
      --  If the output encoding was not specified on the command line, make it
      --  the same as the input encoding, which is in
      --  Opt.Wide_Character_Encoding_Method. See also Check_Parameters.
      --  Otherwise, it is an error if they are not the same, which happens if
      --  the user specified both "-gnatWx" and "Wy", where x /= y.

      if Output_Encoding = WCEM_Default then
         Output_Encoding := Opt.Wide_Character_Encoding_Method;
      elsif Output_Encoding /=
        Opt.Wide_Character_Encoding_Method
      then
         ASIS_UL.Output.Error
           ("input and output wide character encodings conflict");
         raise Fatal_Error;
      end if;

      The_Formatting_Options.Output_Encoding :=
        Output_Encoding;

      GNATPP.Output.Set_Form_String;
      --  This is the second time we're calling Set_Form_String; the first time
      --  was too early, before we had a correct value for
      --  Opt.Wide_Character_Encoding_Method. ???Could be cleaned up.
      --  Also, do we really need two different types for encodings?
   end Set_Output_Encoding;

   use Wide_Text_IO;

begin
   case Output_Mode is
      when Pipe | Replace_Modes | Default =>
         pragma Assert (Res_File_Name = null);
         pragma Assert (Out_Dir = null);
      when Create_Modes =>
         pragma Assert (Res_File_Name /= null);
         pragma Assert (Out_Dir = null);
      when Out_Directory =>
         pragma Assert (Res_File_Name = null);
         pragma Assert (Out_Dir /= null);

         if Out_Dir.all =
           Containing_Directory (Source_Name (SF))
         then
            Error ("--output-dir=" & Out_Dir.all);
            Error (" contains input file " & Short_Source_Name (SF));
            Error (" skipping " & Short_Source_Name (SF));
            Error (" use -rnb to update source files in place");
            return;
         end if;
   end case;

   Set_Output_Encoding;

   if Output_Mode = Replace and then
      Is_Regular_File (Source_Name (SF) & NPP_Suffix)
   then
      Put (Standard_Error, "gnatpp: file ");
      Put (Standard_Error,
           To_Wide_String (Source_Name (SF) & NPP_Suffix));
      Put (Standard_Error, " exists. Use '-rf' option to override");
      New_Line (Standard_Error);
      return;
   end if;

   if Output_Mode in Replace | Force_Replace then

      if Verbose_Mode then
         Put (Standard_Error, "gnatpp: creating the back-up copy ");
         Put (Standard_Error, "of the original source ");
         Put (Standard_Error, To_Wide_String (Source_Name (SF)));
         New_Line (Standard_Error);
      end if;

      declare
         Success : Boolean;
      begin
         Copy_File
           (Name     => Source_Name (SF),
            Pathname => Source_Name (SF) & NPP_Suffix,
            Success  => Success,
            Mode     => Overwrite);

         if not Success then
            Put (Standard_Error, "gnatpp: can not create ");
            Put (Standard_Error, "the back-up copy for ");
            Put (Standard_Error, To_Wide_String (Source_Name (SF)));
            New_Line (Standard_Error);
         end if;
      end;

   end if;

   begin
      Ada_Trees.PP.Asis_To_Ada
        (CU,
         Short_Source_Name (SF),
         The_Formatting_Options,
         Output_Name => Temp_Output_Name, Form_String => Form_String.all,
         Do_Diff => Output_Mode in Replace_Modes,
         Output_Written => Output_Written);
      Set_Source_Status (SF, Processed);

      Write_File_Name_File;
   exception
      when others =>
         Set_Source_Status (SF, Error_Detected);
         raise;
   end;
end ASIS_Processing;
