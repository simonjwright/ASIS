------------------------------------------------------------------------------
--                                                                          --
--                     ASIS UTILITY LIBRARY COMPONENTS                      --
--                                                                          --
--             A S I S _ U L . C O M P I L E R _ O P T I O N S              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                     Copyright (C) 2004-2018, AdaCore                     --
--                                                                          --
-- Asis Utility Library (ASIS UL) is free software; you can redistribute it --
-- and/or  modify  it  under  terms  of  the  GNU General Public License as --
-- published by the Free Software Foundation; either version 3, or (at your --
-- option)  any later version.  ASIS UL  is distributed in the hope that it --
-- will  be  useful,  but  WITHOUT  ANY  WARRANTY; without even the implied --
-- warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the --
-- GNU  General Public License for more details. You should have received a --
-- copy of the  GNU General Public License  distributed with GNAT; see file --
-- COPYING3. If not,  go to http://www.gnu.org/licenses for a complete copy --
-- of the license.                                                          --
--                                                                          --
-- ASIS UL is maintained by AdaCore (http://www.adacore.com).               --
--                                                                          --
------------------------------------------------------------------------------

pragma Ada_2012;

with Ada.Strings;       use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Text_IO;       use Ada.Text_IO;

with GNATCOLL.Projects; use GNATCOLL.Projects;
with GNATCOLL.Traces;   use GNATCOLL.Traces;
with GNATCOLL.VFS;      use GNATCOLL.VFS;

with ASIS_UL.Common;    use ASIS_UL.Common;
with ASIS_UL.Debug;     use ASIS_UL.Debug;
with ASIS_UL.Options;
with ASIS_UL.Output;    use ASIS_UL.Output;

with Opt;               use all type Opt.Ada_Version_Type;
with GNAT.Table;

package body ASIS_UL.Compiler_Options is

   RTS_Path : String_Access;
   --  Used to store path to RTS specified as a parameter of '--RTS=...' tool
   --  option. This is needed before the start of argument project file
   --  analysis

   package Compiler_Switches is new GNAT.Table (
      Table_Component_Type => String_Access,
      Table_Index_Type     => Natural,
      Table_Low_Bound      => 1,
      Table_Initial        => 20,
      Table_Increment      => 100,
      Table_Name           => "Compiler options");

   package I_Options is new GNAT.Table (
      Table_Component_Type => String_Access,
      Table_Index_Type     => Natural,
      Table_Low_Bound      => 1,
      Table_Initial        => 20,
      Table_Increment      => 100,
      Table_Name           => "Source search path options");

   function Get_Full_Path_To_RTS (RTS : String) return String;
   --  Assumes that RTS is a short name of run-time (no path information),
   --  returns the full path to the run-time.

   --------------------------
   -- Get_Full_Path_To_RTS --
   --------------------------

   function Get_Full_Path_To_RTS (RTS : String) return String is
      Env  : Project_Environment_Access;
      Dirs : File_Array_Access;
      Tmp  : String_Access;
      Idx  : Natural;

      gnatls_to_call : constant String :=
        (if Target = null or else Target.all = ""    then "gnatls"
         else                        Target.all & "-gnatls");
   begin
      GNATCOLL.Traces.Parse_Config_File;
      Initialize (Env);
      Env.Set_Path_From_Gnatls (gnatls_to_call & " --RTS=" & RTS, Tmp);
      Dirs := new File_Array'(Env.Predefined_Object_Path);

      for J in Dirs'Range loop
         Idx := Index (Dirs (J).Display_Full_Name, "adalib");

         if Idx /= 0 then
            declare
               Result : constant String   := Dirs (J).Display_Full_Name;
               F_Idx  : constant Positive := Result'First;
            begin
               Free (RTS_Path);
               RTS_Path := new String'(Trim (Result (F_Idx .. Idx - 2), Both));
               --  Full path to RTS may be neeeded by other tools.

               return RTS_Path.all;
            end;
         end if;
      end loop;

      Error ("cannot detect the full path to runtime " & RTS);
      raise Fatal_Error;

   end Get_Full_Path_To_RTS;

   ------------------
   -- Get_RTS_Path --
   ------------------

   function Get_RTS_Path return String is
   begin
      if RTS_Path = null then
         return "";
      else
         return RTS_Path.all;
      end if;
   end Get_RTS_Path;

   ---------
   -- PAL --
   ----------

   procedure PAL is
   begin
      if Arg_List = null then
         Info ("Arg_List is empty");
      else
         Info_No_EOL ("Arg_List is:");

         for J in Arg_List'Range loop
            Info_No_EOL (" " & Arg_List (J).all);
         end loop;

         Info ("");
      end if;
   end PAL;

   ----------------------------------
   -- Process_ADA_PRJ_INCLUDE_FILE --
   ----------------------------------

   procedure Process_ADA_PRJ_INCLUDE_FILE is
      ADA_PRJ_INCLUDE_FILE_Name : String_Access :=
        Getenv ("ADA_PRJ_INCLUDE_FILE");

      ADA_PRJ_INCLUDE_FILE_File : File_Type;

      Next_Dir     : String (1 .. 1024);
      Next_Dir_Len : Natural;
      Tmp          : String_Access;
   begin
      I_Options_Specified := I_Options.Last >= I_Options.First;

      if not Is_Regular_File (ADA_PRJ_INCLUDE_FILE_Name.all) then
         Need_Tmp_ADA_PRJ_INCLUDE_FILE := True;
         return;
      end if;

      ADA_PRJ_INCLUDE_FILE_Full_Name :=
        new String'(Normalize_Pathname (ADA_PRJ_INCLUDE_FILE_Name.all));

      Open (File => ADA_PRJ_INCLUDE_FILE_File,
            Mode => In_File,
            Name => ADA_PRJ_INCLUDE_FILE_Name.all);

      while not End_Of_File (ADA_PRJ_INCLUDE_FILE_File) loop
         Get_Line (File => ADA_PRJ_INCLUDE_FILE_File,
                   Item => Next_Dir,
                   Last => Next_Dir_Len);

         if Source_Search_Path = null then
            Source_Search_Path := new String'
              (Normalize_Pathname (Next_Dir (1 .. Next_Dir_Len)));
         else
            Tmp := new String'(Source_Search_Path.all);
            Free (Source_Search_Path);
            Source_Search_Path := new String'
              (Tmp.all & Path_Separator &
               Normalize_Pathname (Next_Dir (1 .. Next_Dir_Len)));
            Free (Tmp);
         end if;

         --  We append these directories to the directories specified by
         --  '-Idir' options in case if we have to rewrite the file to add
         --  directories set by '-Idir' to it. As the result of this appending,
         --  I_Options will contain all the directories that should be in the
         --  source search path for the compiler/binder calls.
         I_Options.Append
           (new String'(Normalize_Pathname (Next_Dir (1 .. Next_Dir_Len))));
      end loop;

      Close (ADA_PRJ_INCLUDE_FILE_File);
      Free (ADA_PRJ_INCLUDE_FILE_Name);
   end Process_ADA_PRJ_INCLUDE_FILE;

   ---------------------------
   -- Process_cargs_Section --
   ---------------------------

   procedure Process_cargs_Section
     (Parser : Opt_Parser := Command_Line_Parser)
   is
   begin
      Goto_Section
        ((if ASIS_UL.Options.Mimic_gcc
            then "inner-cargs"
            else "cargs"), Parser => Parser);
      --  See doc for -inner-cargs in asis_ul-environment.adb.

      loop
         case GNAT.Command_Line.Getopt
                ("* " &
                 --  options that need path parameter normalization
                 "I: gnatec! gnatem! gnatep!",
                 Parser => Parser) is
            when 'I' | 'g' =>
               if Full_Switch (Parser => Parser) = "gnatec"
                 or else
                  Full_Switch (Parser => Parser) = "gnatep"
                 or else
                  Full_Switch (Parser => Parser) = "gnatem"
               then
                  Store_GNAT_Option_With_Path
                    (Full_Switch (Parser => Parser),
                     Parameter (Parser => Parser));
               elsif Full_Switch (Parser => Parser) = "I" then
                  Store_I_Option (Parameter (Parser => Parser));
               else
                  Store_Option (Full_Switch (Parser => Parser));
               end if;

            when ASCII.NUL =>
               exit;

            when others =>
               Store_Option (Full_Switch (Parser => Parser));
         end case;
      end loop;

      Process_ADA_PRJ_INCLUDE_FILE;
      Set_Arg_List;

   end Process_cargs_Section;

   procedure Process_cargs_Section_Old
     (Parser           : Opt_Parser := Command_Line_Parser;
      Preprocessing_Allowed : Boolean := True)
   is
   begin

      Goto_Section
        ((if ASIS_UL.Options.Mimic_gcc
            then "inner-cargs"
            else "cargs"), Parser => Parser);
      --  See doc for -inner-cargs in asis_ul-environment.adb.

      loop
         case
            GNAT.Command_Line.Getopt
             ("* -RTS= I: gnatec! gnatep! gnateD? " &

              "gnatWh gnatWu gnatWs gnatWe gnatW8 gnatWb " &

              --  switches to be ignored:

              "gnatQ gnatG gnatd! "                 &
              "gnatR? "                             &
              "gnat83 "                             &
              "gnat95 "                             &
              "gnat05 gnat2005 "                    &
              "gnat12 gnat2012 "                    &
              "gnatD gnats",
               Parser => Parser)
         is
            when ASCII.NUL =>
               exit;

            when 'I' | 'g' =>

               if Full_Switch (Parser => Parser) = "gnatec" then
                  Store_GNAT_Option_With_Path
                    (Full_Switch (Parser => Parser),
                     Parameter (Parser => Parser));
               elsif Full_Switch (Parser => Parser) = "I" then
                  Store_I_Option (Parameter (Parser => Parser));
               elsif Full_Switch (Parser => Parser) = "gnatep"
                   or else
                     Full_Switch (Parser => Parser) = "gnateD"
               then
                  if Preprocessing_Allowed then
                     if Full_Switch (Parser => Parser) = "gnatep" then
                        Store_GNAT_Option_With_Path
                          (Full_Switch (Parser => Parser),
                           Parameter (Parser => Parser));
                     else
                        Store_Option
                          ('-' & Full_Switch (Parser => Parser) &
                           Parameter (Parser => Parser));
                     end if;
                  else
                     Error ("cannot preprocess argument file, " &
                            "do preprocessing as a separate step");
                     raise Parameter_Error;
                  end if;

               elsif Full_Switch (Parser => Parser) = "gnat83" then
                  Store_Option ('-' & Full_Switch (Parser => Parser));
                  Opt.Ada_Version := Ada_83;
               elsif Full_Switch (Parser => Parser) = "gnat95" then
                  Store_Option ('-' & Full_Switch (Parser => Parser));
                  Opt.Ada_Version := Ada_95;
               elsif Full_Switch (Parser => Parser) in
                       "gnat05" | "gnat2005"
               then
                  Store_Option ('-' & Full_Switch (Parser => Parser));
                  Opt.Ada_Version := Ada_2005;
               elsif Full_Switch (Parser => Parser) in
                       "gnat12" | "gnat2012"
               then
                  Store_Option ('-' & Full_Switch (Parser => Parser));
                  Opt.Ada_Version := Ada_2012;

               elsif Full_Switch (Parser => Parser) in
                 "gnatWh" | "gnatWu" | "gnatWs" | "gnatWe" |
                 "gnatW8" | "gnatWb"
               then
                  Store_Option ('-' & Full_Switch (Parser => Parser));

               --  Ignore everything else: -gnatD/Q/G/R/s/d

               else
                  null;
               end if;

            when '-' =>

               if Full_Switch (Parser => Parser) = "-RTS" then
                  Custom_RTS := new String'(Parameter (Parser => Parser));
                  Store_Option
                    ("-" & Full_Switch (Parser => Parser) &
                     "=" & Parameter (Parser => Parser));
               end if;

            when others =>
               --  We don't want miscellaneous switches passed to gcc.
               --  They can, for example, cause errors for switches not
               --  supported on all targets.

               null;
         end case;
      end loop;

      Process_ADA_PRJ_INCLUDE_FILE;
      Set_Arg_List;

   end Process_cargs_Section_Old;

   ------------------
   -- Set_Arg_List --
   ------------------

   procedure Set_Arg_List is
   begin
      --  Note that this is sometimes called more than once (e.g. first from
      --  Process_cargs_Section, then from Check_Parameters).
      Arg_List := new Argument_List'
        (String_List (Compiler_Switches.Table (1 .. Compiler_Switches.Last)));
   end Set_Arg_List;

   ----------------------------
   -- Store_Full_Path_To_RTS --
   ----------------------------

   procedure Store_Full_Path_To_RTS is
   begin
      if Get_RTS_Path /= "" then
         Compiler_Switches.Append
           (new String'("--RTS=" &
            Get_Full_Path_To_RTS (Get_RTS_Path)));
      end if;
   end Store_Full_Path_To_RTS;

   --------------------
   -- Store_I_Option --
   --------------------

   procedure Store_I_Option (Path : String) is
   begin

      if Path = "-" then
         Compiler_Switches.Append (new String'("-I-"));
      else
         I_Options.Append
           (new String'(Normalize_Pathname (Path)));
      end if;

   end Store_I_Option;

   ---------------------
   -- Store_I_Options --
   ---------------------

   procedure Store_I_Options is
      ADA_PRJ_INCLUDE_FILE_File : File_Type;
      Tmp                       : String_Access;
   begin
      if I_Options_Specified then

         if Add_I_Options_To_Source_Search_Path then
            Free (Source_Search_Path);
         end if;

         if ADA_PRJ_INCLUDE_FILE_Full_Name /= null then
            Open (File => ADA_PRJ_INCLUDE_FILE_File,
                  Mode => Out_File,
                  Name => ADA_PRJ_INCLUDE_FILE_Full_Name.all);
         else
            ADA_PRJ_INCLUDE_FILE_Full_Name :=
              new String'(Normalize_Pathname ("tmp_ADA_PRJ_INCLUDE_FILE"));
            Create (File => ADA_PRJ_INCLUDE_FILE_File,
                    Mode => Out_File,
                    Name => ADA_PRJ_INCLUDE_FILE_Full_Name.all);

            Setenv
              ("ADA_PRJ_INCLUDE_FILE",
               ADA_PRJ_INCLUDE_FILE_Full_Name.all);
         end if;

         for J in I_Options.First .. I_Options.Last loop
            Put_Line
              (ADA_PRJ_INCLUDE_FILE_File,
               I_Options.Table (J).all);

            if Add_I_Options_To_Source_Search_Path then
               if Source_Search_Path = null then
                  Source_Search_Path := new String'(I_Options.Table (J).all);
               else
                  Tmp := new String'(Source_Search_Path.all);
                  Free (Source_Search_Path);
                  Source_Search_Path :=
                    new String'(Tmp.all & Path_Separator &
                                I_Options.Table (J).all);
                  Free (Tmp);
               end if;
            end if;

         end loop;

         Close (ADA_PRJ_INCLUDE_FILE_File);

      end if;

      if Debug_Flag_S then
         Info ("***Source search path***");

         if Source_Search_Path /= null then
            Info (">>" & Source_Search_Path.all & "<<");
         else
            Info ("... is not set");
         end if;

      end if;

      if Debug_Flag_C then
         Tmp := Getenv ("ADA_PRJ_INCLUDE_FILE");

         if Is_Regular_File (Tmp.all) then
            Info ("***ADA_PRJ_INCLUDE_FILE***");

            declare
               Next_Dir     : String (1 .. 1024);
               Next_Dir_Len : Natural;
            begin
               Info ("File name >>" & Tmp.all & "<<");

               Open (File => ADA_PRJ_INCLUDE_FILE_File,
                     Mode => In_File,
                     Name => ADA_PRJ_INCLUDE_FILE_Full_Name.all);

               while not End_Of_File (ADA_PRJ_INCLUDE_FILE_File) loop
                  Get_Line (File => ADA_PRJ_INCLUDE_FILE_File,
                            Item => Next_Dir,
                            Last => Next_Dir_Len);

                  Info (">>" & Next_Dir (1 .. Next_Dir_Len) & "<<");
               end loop;

               Close (ADA_PRJ_INCLUDE_FILE_File);
            end;

            Free (Tmp);
         end if;
      end if;

   end Store_I_Options;

   ---------------------------------
   -- Store_GNAT_Option_With_Path --
   ---------------------------------

   procedure Store_GNAT_Option_With_Path (Option : String; Path : String) is
      First_Idx : Natural          := Path'First;
      Last_Idx  : constant Natural := Path'Last;
   begin
      if Path (First_Idx) = '=' then
         First_Idx := First_Idx + 1;
      end if;

      Compiler_Switches.Append
        (new String'
           ('-' & Option & '=' &
            Normalize_Pathname (Path (First_Idx .. Last_Idx))));

   end Store_GNAT_Option_With_Path;

   ------------------
   -- Store_Option --
   ------------------

   procedure Store_Option (Switch : String) is
   begin
      Compiler_Switches.Append (new String'(Switch));
   end Store_Option;

   --------------------
   -- Store_RTS_Path --
   --------------------

   procedure Store_RTS_Path (S : String) is
   begin
      Free (RTS_Path);
      RTS_Path := new String'(S);
   end Store_RTS_Path;

end ASIS_UL.Compiler_Options;
