------------------------------------------------------------------------------
--                                                                          --
--                           AVATOX COMPONENTS                              --
--                                                                          --
--                        VATOX (Via Asis To Xml)                           --
--                                                                          --
--                                                                          --
--                Copyright (c) 2006, McKae Technologies.                   --
--                                                                          --
-- Avatox is free software; you can redistribute it and/or modify it        --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software Foundation;  either version 2,  or  (at your option)  any later --
-- version. Avatox is distributed in the hope  that it will be useful,      --
-- but WITHOUT ANY WARRANTY; without even the implied warranty of MER-      --
-- CHANTABILITY or  FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General  --
-- Public License for more details. You should have received a copy of the  --
-- GNU General Public License distributed with GNAT; see file COPYING. If   --
-- not, write to the Free Software Foundation, 59 Temple Place Suite 330,   --
-- Boston, MA 02111-1307, USA.                                              --
--                                                                          --
-- Avatox is maintained by McKae Technologies (http://www.mckae.com)        --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Directories;
with Ada.Text_Io;
use  Ada.Text_IO;

with Mckae.Environment.Command_Line_Processor;

with Vatox.Axf_Points.Options;
with Vatox.Xsl_Transformation;

package body Vatox.Environment is

   ------------------
   -- Always_Equal --
   ------------------

   function Always_Equal (L, R : Filter_Information) return Boolean is
   begin
      return True;
   end Always_Equal;

   -----------
   -- Usage --
   -----------

   procedure Usage is
      XSL_Enabled : constant Boolean := Xsl_Transformation.Is_XSL_Available;
   begin
      Put_Line ("USAGE: avatox Unit(.ads|.adb) ... [-I <include-dir>...]");
      Put_Line ("               [-o <output-file>|-m <directory>]");
      Put_Line ("               [ -k] [ -t] [ -x] [-v] [[-c | -s] [-p]]");
      Put_Line ("               [-e ""<regexp>"" ...] [-f ""<exp>"" ...]");
      Put_Line ("               [-ne ""<regexp>"" ...] [-nf ""<exp>"" ...]");
      Put_Line ("               [-a] [-axfxr] [-axfsc] [-axftr]");
      if Xsl_Enabled then
         Put_Line ("               [-xsl <stylesheet-file>] [-xslto <output-file>]");
         Put_Line ("               [-xslext <file-extension>]");
         Put_Line ("               [-xslp <param=value> ...]");
      end if;
      New_Line;
      Put_Line ("   Unit(.ads|.adb) ...");
      Put_Line ("          The units, or starting units if supporting or closure");
      Put_Line ("          units are selected, to represent as AXF XML");
      Put_Line ("   -a     add all axfPoint annotations");
      Put_Line ("   -c     Process the application's unit closure, which is valid");
      Put_Line ("          only for subprograms that are valid as a main program.");
      Put_Line ("          If more than one unit is specified, only those suitable");
      Put_Line ("          as main program units will be processed (conflicts with");
      Put_Line ("          the - s option). [DISABLED]");
      Put_Line ("   -e     Filter unit names against the provided regular expression");
      Put_Line ("          Multiple -e instances may be supplied. Ignored when not");
      Put_Line ("          processing multiple units with -c or -s.  See note on");
      Put_Line ("          filtering below.");
      Put_Line ("   -f     Filter unit names against the provided file-type");
      Put_Line ("          expression (i.e., *, ?, []).  Multiple -f instances may");
      Put_Line ("          be supplied.  Ignored when not processing multiple units");
      Put_Line ("          with -c or -s.  See note on filtering below.");
      Put_Line ("   -k     krunch output text into a continuous stream -- no indenting");
      Put_Line ("   -m     Multiple files are created for the AXF representations");
      Put_Line ("          of the units. Applies only to the sets of units processed");
      Put_Line ("          when the -c or -s option is provided.  The files are placed");
      Put_Line ("          into the specified directory. (Use -m. for current");
      Put_Line ("          directory.)");
      Put_Line ("   -ne    'not -e'.  Filters for the unit names that do not pass the");
      Put_Line ("          expression filter.");
      Put_Line ("   -nf    'not -f'.  Filters for the unit names that do not pass the");
      Put_Line ("          expression filter.");
      Put_Line ("   -p     Include predefined Ada and vendor-supplied units in the");
      Put_Line ("          analysis, with the exception of packages Standard and System.");
      Put_Line ("          Valid only with -c and -s.");
      Put_Line ("   -s     Process the given unit, and those supporting its compilation");
      Put_Line ("          (conflicts with the -c option.)");
      Put_Line ("   -t     tree files, preexisting or generated, are retained");
      Put_Line ("   -v     Verbose status and messaging");
      Put_Line ("   -x     convert ASIS terms to XML style element names.");
      New_Line;
      Put_Line ("  axfPoint Annotations.  For more info see the Avatox User Guide (pdf)");
      Put_Line ("  and AVATOX_XML_FORMAT files.");
      Put_Line ("   -axfxr Cross-reference");
      Put_Line ("   -axfsc Enclosing scope");
      Put_Line ("   -axftr Terminal representations of language elements");
      New_Line;
         Put_Line ("  XSL stylesheet support.  For more info see the Avatox User Guide.");
      if Xsl_Enabled then
         Put_Line ("   -xsl   The stylesheet file to apply to the AXF file(s).");
         Put_Line ("   -xslto Single output file for transformation result.");
         Put_Line ("          (Incompatible with -m.)");
         Put_Line ("   -xslext File extension to append to files generated via the");
         Put_Line ("          -m option.  Default extension is AXT (Avatox XML");
         Put_Line ("          Transformed.");
         Put_Line ("   -xslp  XSL parameters of the form:  param=value");
         Put_Line ("          Multiple -xslp specifications are allowed, and each");
         Put_Line ("          parameter specification should be enclosed in quotes.");
      else
         Put_Line ("    Disabled");
      end if;
      New_Line;
      Put_Line ("  Filtering units:");
      Put_Line ("          Ada is not a case-sensitive programming language, so when");
      Put_Line ("          filtering compilation unit names, each unit's name is");
      Put_Line ("          first converted to lower case.  Therefore all filter");
      Put_Line ("          strings must match to lower-case letters.  Upper-case");
      Put_Line ("          characters in a filter string will not match.");
      Put_Line ("          If multiple filters are specified, they are applied in the");
      Put_Line ("          order in which they appear on the command line, and a mix");
      Put_Line ("          of -e, -f, -ne, and -nf filters is allowed.  Filter");
      Put_Line ("          expressions ought to be enclosed in quotes.");
   end Usage;

   ----------------
   -- Show_Usage --
   ----------------
   procedure Show_Usage is
   begin
      New_Line;
      Usage;
   end Show_Usage;

   ---------------
   -- Add_Files --
   ---------------
   procedure Add_Files (Unit_Name  : in     Unbounded_String;
                        Unit_Names : in out File_Name_Entries) is
   begin
      Unit_Names.Include (Unit_Name);
   end Add_Files;

   ------------------------------
   -- Process_Args_And_Options --
   ------------------------------

   procedure Process_Args_And_Options
     (The_Node_Information :    out Vatox.Traversal.Info_Node;
      Unit_Names           :    out File_Name_Entries;
      Output_Filename      :    out Unbounded_String;
      Delete_Trees         : in out Boolean;
      Asis_Params          : in out Unbounded_String;
      Refed_Units          : in out Units_Needed;
      Unit_Breadth         : in out Unit_Breadths;
      Axf_Directory        : in out Unbounded_String;
      Multiple_Files       :    out Boolean;
      Filter_Entries       :    out Filtering_Entries;
      Continue             :    out Boolean)
   is
      use Vatox;

      XSL_Enabled : constant Boolean := Xsl_Transformation.Is_XSL_Available;
      Xsl_Option_Selected : Boolean := False;

      type Avatox_Cmd_Options is (Include,
                                  Output_File,
                                  Be_Verbose,
                                  Krunch_Output,
                                  Retain_Trees,
                                  Xml_Style,

                                  -- Matching/No matching filter options
                                  Regexp_Filter,
                                  Globexp_Filter,
                                  Not_Regexp_Filter,
                                  Not_Globexp_Filter,

                                  Axf_File_Directory,
                                  Closure,
                                  Supporting,
                                  Predefined,

                                  -- The following command options are those
                                  -- that control the generation of specific
                                  -- axfPoint elements
                                  All_Axf_Points,
                                  Axf_Cross_Reference,
                                  Axf_Decl_Enclosures,
                                  Axf_Terminal_Reps,

                                  Xsl_Filename,	-- XSL stylesheet filename
                                  Xsl_To, 	-- File into which to place
                                  		-- transformed output
                                  Xsl_Param, 	-- XSL Key/Value pair
                                  Xsl_Extension	-- File extension to append to
                                  		-- multiple files
                                 );

      subtype Matching_Filters is Avatox_Cmd_Options range
        Regexp_Filter .. Globexp_Filter;

      subtype Non_Matching_Filters is Avatox_Cmd_Options range
        Not_Regexp_Filter .. Not_Globexp_Filter;

      subtype Axf_Point_Cmds is Avatox_Cmd_Options range
        Axf_Cross_Reference .. Axf_Terminal_Reps;

      subtype Xsl_Cmds is Avatox_Cmd_Options range
        Xsl_Filename .. Xsl_Extension;

      subtype Non_Xsl_Cmds is Avatox_Cmd_Options range
        Include .. Avatox_Cmd_Options'Pred(Xsl_Filename);

      Axf_Option_For_Cmd : array (Axf_Point_Cmds)
      of Axf_Points.Options.Axf_Point_Options
        := (Axf_Cross_Reference => Axf_Points.Options.Axf_Cross_References,
            Axf_Decl_Enclosures => Axf_Points.Options.Axf_Decl_Enclosures,
            Axf_Terminal_Reps   => Axf_Points.Options.Axf_Terminal_Reps);

      package Avatox_Cmd_Line is new Mckae.Environment.Command_Line_Processor (Avatox_Cmd_Options);

      Option_Table : constant Avatox_Cmd_Line.Command_Line_Item_Mappings :=
        (Avatox_Cmd_Line.Set_Option (Include, 'I',
         Case_Sensitive  => True,
         Argumented      => True),
         Avatox_Cmd_Line.Set_Option (Output_File, 'o',
           Case_Sensitive  => True,
           Argumented      => True),
         Avatox_Cmd_Line.Set_Option (Be_Verbose, 'v',
           Case_Sensitive  => True,
           Argumented      => False),
         Avatox_Cmd_Line.Set_Option (All_Axf_Points, "a",
           Case_Sensitive  => True,
           Argumented      => False),
         Avatox_Cmd_Line.Set_Option (Krunch_Output, "k",
           Case_Sensitive  => True,
           Argumented      => False),
         Avatox_Cmd_Line.Set_Option (Retain_Trees, "t",
           Case_Sensitive  => True,
           Argumented      => False),
         Avatox_Cmd_Line.Set_Option (Xml_Style, "x",
           Case_Sensitive  => True,
           Argumented      => False),
         Avatox_Cmd_Line.Set_Option (Regexp_Filter, "e",
           Case_Sensitive  => True,
           Argumented      => True),
         Avatox_Cmd_Line.Set_Option (Globexp_Filter, "f",
           Case_Sensitive  => True,
           Argumented      => True),
         Avatox_Cmd_Line.Set_Option (Axf_File_Directory, "m",
           Case_Sensitive => True,
           Argumented     => True),
         Avatox_Cmd_Line.Set_Option (Not_Regexp_Filter, "ne",
           Case_Sensitive => True,
           Argumented     => True),
         Avatox_Cmd_Line.Set_Option (Not_Globexp_Filter, "nf",
           Case_Sensitive => True,
           Argumented     => True),
         Avatox_Cmd_Line.Set_Option (Closure, "c",
           Case_Sensitive => True,
           Argumented     => False),
         Avatox_Cmd_Line.Set_Option (Supporting, "s",
           Case_Sensitive => True,
           Argumented     => False),
         Avatox_Cmd_Line.Set_Option (Predefined, "p",
           Case_Sensitive => True,
           Argumented     => False),
         Avatox_Cmd_Line.Set_Option (Axf_Cross_Reference, "axfxr",
           Case_Sensitive => True,
           Argumented     => False),
         Avatox_Cmd_Line.Set_Option (Axf_Decl_Enclosures, "axfsc",
           Case_Sensitive => True,
           Argumented     => False),
         Avatox_Cmd_Line.Set_Option (Axf_Terminal_Reps, "axftr",
           Case_Sensitive => True,
           Argumented     => False),
         Avatox_Cmd_Line.Set_Option (Xsl_Filename, "xsl",
           Case_Sensitive => True,
           Argumented     => True),
         Avatox_Cmd_Line.Set_Option (Xsl_To, "xslto",
           Case_Sensitive => True,
           Argumented     => True),
         Avatox_Cmd_Line.Set_Option (Xsl_Param, "xslp",
           Case_Sensitive => True,
           Argumented     => True),
         Avatox_Cmd_Line.Set_Option (Xsl_Extension, "xslext",
           Case_Sensitive => True,
           Argumented     => True)
        );

      Item               : Avatox_Cmd_Line.Command_Line_Items;
      Multiple_Filenames : Boolean := False;
      The_Unit_Name      : Unbounded_String;
      Xsl_File_Given     : Boolean := False;
      Xsl_Extensioned    : Boolean := False;
      Xsl_Target_File    : Boolean := False;
      Xsl_Params_Given   : Boolean := False;

   begin
      Avatox_Cmd_Line.Install_Option_Mappings (Option_Table);
      Avatox_Cmd_Line.Set_Invalid_Options_Message;
      Avatox_Cmd_Line.Set_Usage (Usage'Access);

      Continue := True;

      Multiple_Files  := False;
      Output_Filename := Null_Unbounded_String;
      The_Unit_Name   := Null_Unbounded_String;
      Unit_Names      := File_Name_Handling.Empty_Set;
      Refed_Units     := Unit_Only;
      Unit_Breadth    := Single_Unit;

      loop
         Item := Avatox_Cmd_Line.Next_Item;
         case Item.Item_Kind is
         when Avatox_Cmd_Line.CLI_Complete =>
            exit;
         when Avatox_Cmd_Line.CLI_Option =>
            case Item.Option is
               when Include =>
                  Append
                    (Source   => Asis_Params,
                     New_Item => " -I" & Avatox_Cmd_Line.Argument_Strings.To_String
                       (Item.Option_Argument));

               when Output_File =>
                  Multiple_Filenames := Output_Filename /= Null_Unbounded_String;
                  Output_Filename := To_Unbounded_String
                    (Avatox_Cmd_Line.Argument_Strings.To_String
                       (Item.Option_Argument));
                  exit when Multiple_Filenames;

               when Be_Verbose =>
                  The_Node_Information.Verbose := True;

              when Xml_Style =>
                  The_Node_Information.Xml_Style := True;

               when Regexp_Filter | Globexp_Filter
                    | Not_Regexp_Filter | Not_Globexp_Filter =>
                  declare
                     use Gnat.Regexp;
                     Filter : constant String
                       := Avatox_Cmd_Line.Argument_Strings.To_String
                         (Item.Option_Argument);
                  begin
                     Filter_Entry_Handling.Append
                       (Filter_Entries,
                        (Compile
                           (Filter,
                            (Item.Option = Globexp_Filter) or
                              (Item.Option = Not_Globexp_Filter)),
                         (Item.Option in Matching_Filters)));
                  exception
                     when Error_In_Regexp =>
                        Put_Line("Invalid expression: " & Filter);
                        Continue := False;
                        exit;
                  end;

               when Krunch_Output =>
                  The_Node_Information.Krunch := True;

               when Retain_Trees =>
                  Delete_Trees := False;

               when Axf_File_Directory =>
                  Axf_Directory := To_Unbounded_String
                    (Avatox_Cmd_Line.Argument_Strings.To_String
                       (Item.Option_Argument));
                  Multiple_Files := True;

               when Closure =>
                  if Unit_Breadth = Single_Unit then
                     Unit_Breadth := Full_Closure;
                  else
                     Unit_Breadth := Error_Case;
                     exit;  -- Bail upon error
                  end if;

               when Supporting =>
                  if Unit_Breadth = Single_Unit then
                     Unit_Breadth := Supporting;
                  else
                     Unit_Breadth := Error_Case;
                     exit;  -- Bail on error
                  end if;

               when Predefined =>
                  if Refed_Units = Unit_Only then
                     Refed_Units := Predefined;
                  else
                     Refed_Units := Error_Case;
                     exit;
                  end if;

               when All_Axf_Points =>
                  Axf_Points.Options.Set_All (The_Node_Information.Axf_Points);
                  The_Node_Information.Xml_Style  := True;

                when Axf_Cross_Reference
                  | Axf_Decl_Enclosures
                  | Axf_Terminal_Reps =>
                  Axf_Points.Options.Set
                    (The_Node_Information.Axf_Points,
                     Axf_Option_For_Cmd(Item.Option));
                  The_Node_Information.Xml_Style  := True;

                  -- XSL options are invalid if this implementation of Avatox
                  -- is not XSL-enabled
               when Xsl_Filename =>
                  Xsl_Option_Selected := True;
                  exit when not XSL_Enabled;
                  Xsl_Transformation.Set_Xsl_Filename
                    (The_Node_Information.Xsl_Info,
                     Avatox_Cmd_Line.Argument_Strings.To_String
                       (Item.Option_Argument));
                  Xsl_File_Given := True;

               when Xsl_To =>
                  Xsl_Option_Selected := True;
                  exit when not XSL_Enabled;
                  Xsl_Transformation.Set_Output_Filename
                    (The_Node_Information.Xsl_Info,
                     Avatox_Cmd_Line.Argument_Strings.To_String
                       (Item.Option_Argument));
                  Xsl_Target_File := True;

               when Xsl_Param =>
                  Xsl_Option_Selected := True;
                  exit when not XSL_Enabled;
                  Xsl_Transformation.Add_Parameter_Pair
                    (The_Node_Information.Xsl_Info,
                     Avatox_Cmd_Line.Argument_Strings.To_String
                       (Item.Option_Argument), Added => Continue);
                  if Continue then
                     Xsl_Params_Given := True;
                  else
                     Put_Line ("Error in XSL parameter: "
                               & Avatox_Cmd_Line.Argument_Strings.To_String
                                 (Item.Option_Argument));
                  end if;

               when Xsl_Extension =>
                  Xsl_Option_Selected := True;
                  exit when not XSL_Enabled;
                  Xsl_Transformation.Set_Xsl_Extension
                    (The_Node_Information.Xsl_Info,
                     Avatox_Cmd_Line.Argument_Strings.To_String
                       (Item.Option_Argument));
                  Xsl_Extensioned := True;

             end case;

         when Avatox_Cmd_Line.CLI_Option_Error =>
            Continue := False;

         when Avatox_Cmd_Line.CLI_Argument =>
            The_Unit_Name := To_Unbounded_String
              (Avatox_Cmd_Line.Argument_Strings.To_String (Item.Argument));
            Add_Files (The_Unit_Name, Unit_Names);

         when Avatox_Cmd_Line.CLI_Help_Requested =>
            return;

         end case;
      end loop;

      -- Now check that all selected options are compatible with one another.
      if Continue then
         -- Valid options were provided, so check the individual ones now...
         Continue := not (Multiple_Files and (Output_Filename /= Null_Unbounded_String));
         if not Continue then
            Put_Line ("Cannot specify both Multiple file output (-m) and an");
            Put_Line ("output filename (-o)");
         end if;

         if Continue then
            Continue := (Output_Filename = Null_Unbounded_String)
              or else (Element (Output_Filename, 1) /= '-');
            if not Continue then
               Put_Line ("No output filename provided with -o");
            end if;
         end if;

         if Continue then
            Continue := not ((Unit_Breadth = Single_Unit)
                                 and (Refed_Units = Predefined));
            if not Continue then
               Put_Line ("-p (predefined) is valid only with closure (-c) and supporting (-s)");
            end if;
         end if;

         if Continue then
            Continue := Unit_Breadth /= Error_Case;
            if not Continue then
               Put_Line ("Cannot specify both -c and -s");
            elsif (Unit_Breadth /= Single_Unit) and (Refed_Units = Unit_Only) then
               Refed_Units := App_Only;
            end if;
         end if;

         if Continue then
            Continue := The_Unit_Name /= Null_Unbounded_String;
            if not Continue then
               Put_Line ("No Unit Name provided");
            end if;
         end if;

         if Continue then
            Continue := Xsl_Enabled or else not Xsl_Option_Selected;
            if not Continue then
               Put_Line ("XSL not available, -xsl options invalid");
            end if;
         end if;

         -- XSL specific option checks
         if Continue then
            if Xsl_Enabled then
               Continue := not (Xsl_Extensioned
                                or Xsl_Target_File
                                or Xsl_Params_Given) or else Xsl_File_Given;
               if not Continue then
                  Put_Line ("No XSL stylesheet provided");
               end if;

               if Continue then
                  Continue := not Xsl_Extensioned or else Multiple_Files;
                  if not Continue then
                     Put_Line ("-xslext valid only with multiple file (-m) option");
                  end if;
               end if;

               if Continue then
                  Continue := not Xsl_Target_File or else not Multiple_Files;
                  if not Continue then
                     Put_Line ("-xslto valid only with -o or stdout");
                  end if;
               end if;
            elsif Xsl_Option_Selected then
               Continue := False;
               Put_Line ("XSL option(s) invalid, XSL is disabled");
            end if;
         end if;

         if not Continue then
            New_Line;
            Usage;
         end if;
      end if;

      ----------------------------------------------------------------------
      -- Full_Closure is not implemented due to bugs in the ASIS-for-GNAT
      -- distribution (GNAT GPL 2006) in the vicinity of the
      -- Semantic_Order_Dependence function.
      if Unit_Breadth = Vatox.Environment.Full_Closure then   -- -c disable
         Put_Line ("-c, Closure, is disabled. Perhaps -s,"    -- -c disable
                   & " Supporting units, will suffice.");     -- -c disable
         Continue := False;                                   -- -c disable
      end if;                                                 -- -c disable
      -- Full closure disabling block
      ----------------------------------------------------------------------

   end Process_Args_And_Options;

  -----------------------------------------------------------------------------

   function Passes_Filter (Unit_Name : String;
                           Filter    : Filtering_Entries) return Boolean is
      Filter_Info : Filter_Information;
   begin
      for I in Filter_Entry_Handling.First_Index (Filter)
        .. Filter_Entry_Handling.Last_Index (Filter) loop
         Filter_Info := Filter_Entry_Handling.Element (Filter, I);
         if Filter_Info.Match /=
           Gnat.Regexp.Match (Unit_Name, Filter_Info.F) then
            return False;
         end if;
      end loop;
      return True;
   end Passes_Filter;

   -----------------------------------------------------------------------------

   -- Given the list of filename specifications (which can be specific names or
   -- wildcarded), get the list of primary files that match the specification.
   procedure Collect_Primary_Files
     (File_Names        : in     File_Name_Entries;
      Primary_Filenames :    out File_Name_Entries) is

      use Ada;
      use File_Name_Handling;

      Fc : File_Name_Handling.Cursor := File_Names.First;
      Primary_File_Search : Directories.Search_Type;
      Primary_File_Entry  : Directories.Directory_Entry_Type;

   begin
      Primary_Filenames := Empty_Set;
      while Fc /= No_Element loop
         declare
            File_Spec : constant String := To_String(Element(Fc));
         begin
            Directories.Start_Search
              (Primary_File_Search,
               Directories.Current_Directory,
               File_Spec,
               (Directories.Ordinary_File => True, others => False));

            while Directories.More_Entries (Primary_File_Search) loop
               Directories.Get_Next_Entry (Primary_File_Search, Primary_File_Entry);
               Include (Primary_Filenames,
                        To_Unbounded_String (Directories.Simple_Name (Primary_File_Entry)));
            end loop;
            Directories.End_Search(Primary_File_Search);
         end;
         Next(Fc);
      end loop;
   end Collect_Primary_Files;

   -----------------------------------------------------------------------------

end Vatox.Environment;
