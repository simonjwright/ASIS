------------------------------------------------------------------------------
--                                                                          --
--                            GNAT2XML COMPONENTS                           --
--                                                                          --
--                 G N A T 2 X M L . C O M M A N D _ L I N E                --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                     Copyright (C) 2012-2017, AdaCore                     --
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
-- The gnat2xml tool was derived from the Avatox sources.                   --
------------------------------------------------------------------------------

pragma Ada_2012;

with ASIS_UL.Formatted_Output; use ASIS_UL.Formatted_Output;

package body Gnat2xml.Command_Line is

   Options_Set : Boolean := False;
   --  True if Set_Gnat2xml_Options has been called.

   The_Options : aliased Gnat2xml_Options_Type;

   function Gnat2xml_Options return access constant Gnat2xml_Options_Type is
   begin
      pragma Assert (Options_Set);
      return The_Options'Access;
   end Gnat2xml_Options;

   procedure Set_Gnat2xml_Options (Options : Gnat2xml_Options_Type) is
   begin
      pragma Assert (not Options_Set);
      Options_Set := True;
      The_Options := Options;
   end Set_Gnat2xml_Options;

   procedure Gnat2xml_Usage is
   begin
      pragma Style_Checks ("M200"); -- Allow long lines

      Put ("usage: gnat2xml [options] {filename} {-files filename} " &
            "[-cargs gcc_switches]\n");
      Put (" options:\n");
      --    Undocumented developer switches:
      --   -d1 -- print command-line args and current dir
      --   -d9 -- verbose debug mode
      --   -dc -- print gcc commands

      Put (" -Pproject        - Use project file project\n");
      Put (" -U               - check all sources of the argument project\n");
      Put (" -U main          - check the closure of units rooted at unit main\n");
      Put (" -Xname=value     - specify an external reference for argument project file\n");
      Put (" -eL              - follow all symbolic links when processing project files\n");

      Put ("\n");
      Put (" --incremental      - incremental processing on a per-file basis\n");
      Put (" --output-dir=dir   - generate one .xml file for each Ada source file, in\n");
      Put ("           directory 'dir'. (Default is to generate the XML to standard\n");
      Put ("           output.)\n");
      Put (" --subdirs=dir      - subdirectory to place the result files in\n");
      Put (" --no_objects_dir   - place results into current dir instead of project dir\n");
      Put ("\n");
      Put (" -I <include-dir>   - directories to search for dependencies\n");
      Put ("\n");
      Put (" --compact          - debugging version, with interspersed source,\n");
      Put ("       and a more compact representation of ""sloc"".\n");
      Put ("       Note that this version does not validate.\n");
      Put (" --rep-clauses      - generate representation clauses\n");
      Put ("\n");
      Put (" -files=filename   - the name of a text file containing a list\n");
      Put ("                     of Ada source files to process\n");
      Put (" --ignore=filename - do not process sources listed in filename\n");
      Put (" -jn               - n is the maximal number of processes\n");
      Put (" --RTS=<dir>       - the same as gcc --RTS option\n");
      Put ("\n");
      Put (" -q        - quiet\n");
      Put (" -v        - verbose\n");
      Put (" -h\n");
      Put (" --help    - print this message and quit, ignoring all other options\n");
      Put (" --version - print version and quit, ignoring all other options\n");
      pragma Style_Checks ("M79");
   end Gnat2xml_Usage;

end Gnat2xml.Command_Line;
