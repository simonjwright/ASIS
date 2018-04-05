------------------------------------------------------------------------------
--                                                                          --
--                      GNAT METRICS TOOLS COMPONENTS                       --
--                                                                          --
--                       M E T R I C S . O U T P U T                        --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                     Copyright (C) 2003-2016, AdaCore                     --
--                                                                          --
-- GNAT Metrics Toolset  is free software;  you can  redistribute it and/or --
-- modify it under terms of the  GNU General Public License as published by --
-- the Free Software Foundation;  either version 2, or (at your option) any --
-- later version.  GNAT Metrics Toolset is  distributed in the hope that it --
-- will be useful, but  WITHOUT ANY WARRANTY; without even the implied war- --
-- ranty of  MERCHANTABILITY or  FITNESS FOR A PARTICULAR PURPOSE.  See the --
-- GNU General Public License for more details.  You should have received a --
-- copy of the  GNU General Public License distributed with  GNAT; see file --
-- COPYING.  If not,  write to the  Free Software  Foundation,  51 Franklin --
-- Street, Fifth Floor, Boston, MA 02110-1301, USA.                         --
--                                                                          --
-- GNAT Metrics Toolset is maintained by AdaCore (http://www.adacore.com).  --
--                                                                          --
------------------------------------------------------------------------------

--  This package defines different output routines

with Asis;                        use Asis;

with ASIS_UL.Metrics.Definitions; use ASIS_UL.Metrics.Definitions;
with ASIS_UL.Source_Table;        use ASIS_UL.Source_Table;

package METRICS.Output is

   procedure Brief_Help;
   --  Prints out the brief help

   procedure Print_Gnatmetric_Usage;
   --  Similar to Brief_Help, but corresponds to the general format generated
   --  by other GNAT tools for '--help' option, and sends the output into
   --  Stdout.

   ----------------------------------------------
   -- Routines for outputting the metrics info --
   ----------------------------------------------

   --  The possibility to output the metric values in XML format was added to
   --  the metric tool when the tool itself was more or less completed. It was
   --  the first reason to add XML output to the existing output routines.
   --  Another reason was maintainability - to have the human-oriented and XML
   --  output code for the same thing together.

   --  To minimize the impact of adding XML output on the existing metrics
   --  tool, we have kept the existing names of the output routines and added
   --  to them the possibility to output XML text. In the documentation of
   --  these routines, the first part corresponds to human-oriented text
   --  output (referred simply as "text output" below"), and the last part
   --  describes XML-specific things.

   --  Note, that all the XML output is sent to a single file.

   function Source_Name_For_Output (SF : SF_Id) return String;
   --  Returns the file name to be included in the metrics output. Depending
   --  on the options set for the tool, it may or may not contain the path
   --  information

   function Source_Out_File (SF : SF_Id) return String;
   --  Depending on whether or not a different output directory is set, returns
   --  the appropriate name for metrics output file for the argument source.

   procedure Report_Global_Statistics;
   --  Report the global metrics statistics (the metric values summed up for
   --  all the sources for which these metrics are successfully computed. This
   --  global statistic is printed out only when there are at least two units
   --  to sum the metric values.
   --
   --  Puts global metrics values into XML file and closes <global> tag, and
   --  then closes the XML output file.
   --  ??? Should we output global values into XML file if we have only one
   --  Ada source file processed.

   procedure Set_Source_Out_File (SF : SF_Id);
   --  Creates and opens the output file for the detailed metrics information
   --  for the argument source. This file is set as the default output for
   --  text output.

   procedure Report
     (Message : String;
      Depth   :   Natural := 0);
   --  Sends the message into the current output file. The message is appended
   --  with platform-specific EOL. Depth is used to send the indentation if
   --  needed. This procedure is used for composing the textual output of the
   --  metrics tool, the current output should be set by the caller. Does
   --  nothing if the tool is called in "no text output" mode.

   procedure Report_Program_Unit
     (El           : Element;
      Depth        : Natural;
      Library_Item : Boolean := False);
   --  If Generate_Text_Output is set ON, prints into the text out file the
   --  heading info about the program unit represented by El.
   --  Depth is used to send the indentation in case of local bodies. If
   --  Library_Item is set ON, the note about that the unit is the library
   --  item of the enclosing compilation unit is added
   --
   --  If Generate_XML_Output is set ON, puts in XML output file string:
   --
   --   <unit name='unit_name" line="ll" col="cc">

   procedure Generate_Header (SF : SF_Id; CU : Compilation_Unit);
   --  Generates in the source output file the header containing the general
   --  information about the unit CU that is supposed to be in source SF
   --
   --  Puts in the XML output the string of the form:
   --
   --  <file name="file_name">

   procedure Generate_Line_Output (SF : SF_Id);
   --  Generates in the source output file the line metrics report. Collects
   --  the global statistics for line metrics.
   --
   --  Puts the file line metrics report into XML output file

   -------------------------
   -- XML output routines --
   -------------------------

   procedure Set_XML_Out_File;
   --  Tries to create the XML output file (if needed) and open it. In case
   --  if Generate_XML_Schema is ON, also tries to create (if needed) and open
   --  the file to write the schema in. If the XML output file (and the schema
   --  file) is (are) successfully open, opens <global> tag in XML output file.
   --  Otherwise sets Generate_XML_Output OFF and that prevents all further
   --  attempts to generate XML output.

   procedure Open_Tag (Tag_Name : String);
   procedure Close_Tag (Tag_Name : String; Depth : Natural := 0);
   --  Puts into XML file string <Tag_Name> or </Tag_Name>, using ??? to define
   --  the indentation. Does nothing if Generate_XML_Output is set OFF.

   procedure Output_XML_Metric
     (Metric : String;
      Value  : Metric_Count;
      Depth  : Natural := 0);
   --  Puts into XML file string
   --
   --   <metric name=Metric>Value</metric>
   --
   --  using Depth to define the indentation
   --  Does nothing if Generate_XML_Output is set OFF

   procedure Output_XML_Metric
     (Metric : String;
      Value  : String;
      Depth  : Natural := 0);
   --  Puts into XML file string
   --
   --   <metric name=Metric>Value</metric>
   --
   --  using Depth to define the indentation. If Value has leading and/or
   --  trailing spaces, they are cut off.
   --  Does nothing if Generate_XML_Output is set OFF

   procedure Report_XML
     (Message : String;
      Depth   :   Natural   := 0);
   --  Sends the message into the XML output file. The message is appended with
   --  platform-specific EOL. Depth is used to send the indentation if needed.
   --  Does nothing if Generate_XML_Output is set OFF

   procedure Write_XML_Schema;
   --  Writes down the XML schema. This routine assumes that the schema output
   --  file is already created and opened. When the schema is written down,
   --  the schema output file is closed.

end METRICS.Output;
