------------------------------------------------------------------------------
--                                                                          --
--                           GNATELIM COMPONENTS                            --
--                                                                          --
--                      G N A T E L I M . O U T P U T                       --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                     Copyright (C) 1998-2018, AdaCore                     --
--                                                                          --
-- GNATELIM  is  free software;  you can  redistribute it and/or  modify it --
-- under the terms of the  GNU  General Public License  as published by the --
-- Free Software Foundation; either version 3 or (at your option) any later --
-- version. GNATELIM is distributed in the hope that it will be useful, but --
-- WITHOUT ANY WARRANTY; without even the implied warranty of  MERCHANTABI- --
-- LITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public Li- --
-- cense for  more details.  You should  have  received  a copy of the  GNU --
-- General Public License distributed with GNAT; see file COPYING3. If not, --
-- go to http://www.gnu.org/licenses for a complete copy of the license.    --
--                                                                          --
-- The original version  of  Gnatelim  was developed by  Alain  Le  Guennec --
-- It is now maintained by AdaCore (http://www.adacore.com)                 --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Characters.Handling;          use Ada.Characters.Handling;
with Ada.Text_IO;                      use Ada.Text_IO;
with Ada.Strings.Fixed;                use Ada.Strings.Fixed;

with Asis.Extensions.Strings;          use Asis.Extensions.Strings;

with ASIS_UL.Common;
with ASIS_UL.Global_State;             use ASIS_UL.Global_State;
with ASIS_UL.Global_State.CG;          use ASIS_UL.Global_State.CG;
with ASIS_UL.Global_State.CG.Gnatelim; use ASIS_UL.Global_State.CG.Gnatelim;
with ASIS_UL.Output;                   use ASIS_UL.Output;
with ASIS_UL.Source_Table;             use ASIS_UL.Source_Table;

with Gnatelim.Options;

package body Gnatelim.Output is

   -----------------------
   -- Local_Subprograms --
   -----------------------

   procedure Generate_Eliminate_Pragma (N : GS_Node_Id);
   --  Generates an Eliminate pragma for a subprogram indicated by N

   function Strip_Column (SLOC_Str : String) return String;
   --  Strips the column part(s) of SLOC, we need this to correspond to the
   --  existing format of Eliminate pragmas

   ----------------
   -- Brief_Help --
   ----------------

   procedure Brief_Help is
   begin
      pragma Style_Checks (Off);

      Info ("usage: gnatelim [options] -main=main_unit_name {filename} [-cargs gcc_switches]");

      Info ("");
      Info ("options:");
      Info (" --version - Display version and exit");
      Info (" --help    - Display usage and exit");
      Info ("");
      Info (" -Pproject     - Use project file project. Only one such switch can be used.");
      Info (" -Xname=value  - specify an external reference for argument project file");
      Info (" -eL           - follow all symbolic links when processing project files");
      Info (" -U            - process all sources of argument project instead of processing");
      Info ("                 the closure of main (does not require the main to be built)");
      Info ("");
      Info (" -files=filemane    - name of text file containing a list of Ada units");
      Info ("                      to analyse");

      Info (" -l[=log_file_name] - create a log file. log_file_name specifies the log name,");
      Info ("                      if not present 'gnatelim.log' is used");
      Info (" --no-elim-dispatch - do not generate pragmas for dispatching operations");
      Info (" --ignore=filename  - do not generate pragmas for units listed in filename");
      Info (" -jn                - n is the maximal number of processes to carry out");
      Info ("                      tree creations");
      Info (" -q                 - quiet mode");
      Info (" -v                 - verbose mode");
      Info (" -t                 - output execution time");
      Info (" -wq                - quiet warning mode - some warnings are suppressed");
      Info (" -o=filename        - send output to filename");

      Info ("");

      Info ("filename            - name of the Ada source file to be analyzed.");
      Info ("                      Wildcards are allowed");
      Info ("main_unit_name      - name of main subprogram of the partition to analyse");
      Info ("");
      Info ("gcc_switches        - switches to be passed to gcc called by " & ASIS_UL.Common.Tool_Name.all);
      Info ("");

      pragma Style_Checks (On);
   end Brief_Help;

   -------------------------------
   -- Generate_Eliminate_Pragma --
   -------------------------------

   procedure Generate_Eliminate_Pragma (N : GS_Node_Id) is
   begin
      Report_No_EOL ("pragma Eliminate (");

      if Gnatelim.Options.Long_Pragma_Format then
         Report_No_EOL (GS_Enclosed_CU_Name (N));
         Report_No_EOL (", ");
      end if;

      Report_No_EOL (GS_Node_Name (N));
      Report_No_EOL (", ");
      Report_No_EOL ("Source_Location => """);
      Report_No_EOL
        (Strip_Column (Old_Format (Get_String (GS_Node_SLOC (N)))));
      Report_No_EOL ("""");

      Report (");");

   end Generate_Eliminate_Pragma;

   --------------------------
   -- Print_Gnatelim_Usage --
   --------------------------

   procedure Print_Gnatelim_Usage is
   begin
      Set_Error (Standard_Output);
      Brief_Help;

      New_Line;
      New_Line;
      Put_Line ("Report bugs to report@adacore.com");
   end Print_Gnatelim_Usage;

   -------------------------------
   -- Report_Unused_Subprograms --
   -------------------------------

   procedure Report_Unused_Subprograms is
      No_Pragma_Reported : Boolean := True;
   begin

      Report ("---------------------------------------------------------");
      Report ("--  List of unused entities to be placed in gnat.adc.  --");
      Report ("---------------------------------------------------------");

      for J in First_GS_Node .. Last_Node loop

         if not (Present (Enclosing_Source (J))
               and then
                 Source_Info (Enclosing_Source (J)) =
                   ASIS_UL.Source_Table.Ignore_Unit)
           and then
            Is_Subprogram_Node (J)
           and then
            Body_Analyzed (J)
           and then
            not Is_Abstract_Subprogram_Node (J)
           and then
            not Is_Implicit_Subprogram_Node (J)
           and then
            not Is_Of_No_Interest (J)
           and then
            not Is_Used (J)
         then
            Generate_Eliminate_Pragma (J);
            No_Pragma_Reported := False;
         end if;

      end loop;

      if No_Pragma_Reported then
         Report ("--  No unused entities.");
      end if;

   end Report_Unused_Subprograms;

   ------------------
   -- Strip_Column --
   ------------------

   function Strip_Column (SLOC_Str : String) return String is
      Result      : String   := SLOC_Str;
      Result_Last : Natural  := Result'First - 1;

      First_Idx   : Positive := SLOC_Str'First;
      Last_Idx    : Natural;
      --  Indexes that cut the next part of SLOC_Str to copy into Result

      SLOC_Str_Last : constant Positive := SLOC_Str'Last;
   begin
      Main_Loop : loop
         Last_Idx := Index (SLOC_Str (First_Idx .. SLOC_Str_Last), ":");
         --  First colon, indicates the line

         if Last_Idx = 0 then
            --  No colon any more, but we need to add closing ']' for SLOCs in
            --  expanded generics:
            for J in reverse SLOC_Str'Range loop

               if SLOC_Str (J) = ']' then
                  Result_Last          := Result_Last + 1;
                  Result (Result_Last) := ']';
               else
                  exit Main_Loop;
               end if;

            end loop;

            exit Main_Loop;
         end if;

         Last_Idx := Last_Idx + 1;

         while SLOC_Str (Last_Idx + 1) /= ':' loop
            --  Go to the second colon, which is for column number
            Last_Idx := Last_Idx + 1;
         end loop;

         Result (Result_Last + 1 .. Result_Last + (Last_Idx - First_Idx) + 1)
           := SLOC_Str (First_Idx .. Last_Idx);

         Result_Last := Result_Last + (Last_Idx - First_Idx) + 1;

         First_Idx := Last_Idx + 2;

         while Is_Digit (SLOC_Str (First_Idx)) loop
            First_Idx := First_Idx + 1;
            exit Main_Loop when First_Idx > SLOC_Str_Last;
         end loop;

      end loop Main_Loop;

      return Result (Result'First .. Result_Last);
   end Strip_Column;

end Gnatelim.Output;
