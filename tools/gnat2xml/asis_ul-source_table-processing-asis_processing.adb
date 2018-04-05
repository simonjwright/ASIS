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
--                    Copyright (C) 2013-2016, AdaCore                      --
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

with Ada.Directories;       use Ada.Directories;
with Ada.Text_IO;           use Ada.Text_IO;

with Gnat2xml.Xml;          use Gnat2xml;

separate (ASIS_UL.Source_Table.Processing)
procedure ASIS_Processing (CU : Asis.Compilation_Unit; SF : SF_Id) is

   procedure Process (XML_File : File_Access);
   --  Helper routine that does all the work once the output file is opened

   procedure Process (XML_File : File_Access) is
      The_Node_Information : constant Xml.Info_Node :=
        (XML_File  => XML_File,
         Krunch    => False,
         Xml_Style => False,
         Verbose   => Verbose_Mode);
   begin
      Xml.Start_Representation (The_Node_Information);
      Xml.Process_Unit (CU, The_Node_Information);
   end Process;

--  Start of processing for ASIS_Processing

begin
   if Out_Dir = null then
      Process (Standard_Output); -- pipe output to standard output
   else
      declare
         XML_Name : constant String :=
           Compose
             (Containing_Directory => Out_Dir.all,
              Name                 => Short_Source_Name (SF),
              Extension            => "xml");
         XML_Out_File : aliased File_Type;
      begin
         if Verbose_Mode then
            Put_Line ("creating " & XML_Name);
         elsif Debug_Flag_V then
            Put_Line ("creating " & Short_Source_Name (SF) & ".xml");
         end if;

         Create (XML_Out_File, Out_File, Name => XML_Name);
         Process (XML_Out_File'Unchecked_Access);
         Close (XML_Out_File);
      end;
   end if;

   Set_Source_Status (SF, Processed);
end ASIS_Processing;
