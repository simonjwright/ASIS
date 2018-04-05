------------------------------------------------------------------------------
--                                                                          --
--                      GNAT METRICS TOOLS COMPONENTS                       --
--                                                                          --
--                       M E T R I C S . C O M M O N                        --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                     Copyright (C) 2002-2009, AdaCore                     --
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
-- GNAT Metrics Toolset is maintained by Adacore (http://www.adacore.com).  --
--                                                                          --
------------------------------------------------------------------------------

--  This package contains some general-purpose entities that are used by many
--  Metrics Toolset components

with Ada.Command_Line;
with Ada.Text_IO; use Ada.Text_IO;
with GNAT.OS_Lib; use GNAT.OS_Lib;
with Asis;

package METRICS.Common is

   CU_Kind  :  Asis.Unit_Kinds;
   CU_Class : Asis.Unit_Classes;
   --  May be used to keep kind/class information about ASIS_UL.Common.The_CU.

   May_Have_Public_Subprograms : Boolean := False;
   May_Have_Subprogram_Bodies  : Boolean := False;
   --  These flags are used to define if a number-of-subprograms metrics should
   --  be computed for the given unit.

   May_Have_Public_Types     : Boolean := False;
   May_Have_Type_Definitions : Boolean := False;
   --  These flags are used to define if a number-of-types metrics should
   --  be computed for the given unit.

   --  We place these four flags here to compute them only once, because there
   --  is no sense to compute them for nested (non library-level) program
   --  units.

   The_Unit : Asis.Element;
   --  The top-level element of the program unit from the CU represented by
   --  The_CU

   Global_Output : File_Access;
   --  Output file for global metrics information

   Global_File_Name : String_Access;
   --  The name of the file to print the global metrics info in. If null, this
   --  output is sent to Stdout

   Global_Out_File : File_Type;
   --  File to place the global metrics summed up for all the files processed.

   Output_Dir : String_Access := new String'("");
   --  The name of the directory to place the detailed metrics info in. Is set
   --  by '-d' option.

   XML_File_Name : String_Access;
   --  The name of the file to print the XML form of the metrics info in.

   XML_Out_File : File_Type;
   --  File to place the XML output.

   XSD_File_Name : String_Access;
   --  The name of the file to put the schema for the XML the metrics info in.

   XSD_Out_File : File_Type;
   --  File to place the schema of the XML output.

   Generate_Text_Output : Boolean := True;
   --  If this flag is ON, the metrics output in textual form is generated

   Generate_XML_Output : Boolean := False;
   --  If this flag is ON, the metrics output in XML format is generated

   Generate_XML_Schema : Boolean := False;
   --  If this flag is ON, the XML Schema file that describes the syntax of the
   --  XML output is generated in the the same directory as the XML output
   --  file, and this schema is assigned to the XML output file. This schema
   --  file has the same name as XML output file, and '.xsd' syntax.

   Source_Output_File : File_Type;
   --  The output file containing detailed metrics for one source

   Indent_String : constant String := "   ";
   --  Indentation level in body complexity report. Should we set it from
   --  some parameter?

   Out_Suffix : String_Access;
   --  The suffix of the source out file

   GNATMETRIC_Exit_Status : Ada.Command_Line.Exit_Status :=
     Ada.Command_Line.Success;
   --  Sets to Failure if any problem is detected during the gnatmetric run,
   --  including the attempt to compile illegal source.

end METRICS.Common;
