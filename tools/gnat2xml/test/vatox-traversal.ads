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
-- Avatox is based off the Display_Source software distributed as part of   --
-- the ASIS implementation for GNAT, and therefore inherits its GPL         --
-- licensing.  Ada Core Technologies maintains the Display_Source program   --
-- and its copyright is held by the Free Software Foundation.               --
--                                                                          --
-- Avatox is now maintained by McKae Technologies (http://www.mckae.com)    --                                               --
------------------------------------------------------------------------------

with Asis.Elements;
with Text_Io;
with Vatox.Axf_Points.Options;
with Vatox.Xsl_Transformation;

package Vatox.Traversal is

   -- For maintaining the list of private declarations associated with a package
   type Private_Decls_List is access Asis.Declarative_Item_List;

   type Info_Node is -- ???Should be private?
      record
         XML_File      : Text_Io.File_Access;
         Axf_Points    : Vatox.Axf_Points.Options.Axf_Options_Selections;
         Krunch        : Boolean      := False;
         Xml_Style     : Boolean      := False;
         Xsl_Info      : Xsl_Transformation.Xsl_Information;
         Last_Element  : Asis.Element := Asis.Nil_Element;
         Verbose       : Boolean      := False;
         Private_Decls : Private_Decls_List;
      end record;

   procedure Start_Representation
     (State : in out Info_Node);

   procedure Start_Unit
     (Unit  : in     Asis.Compilation_Unit;
      State : in out Info_Node);

   procedure Start_Xml -- ???Not used
     (Unit  : in     Asis.Compilation_Unit;
      State : in out Info_Node);

   procedure End_Unit
     (Unit  : in     Asis.Compilation_Unit;
      State : in out Info_Node);

   procedure End_Representation
     (State : in out Info_Node);

   procedure End_Xml -- ???Not used
     (Unit  : in     Asis.Compilation_Unit;
      State : in out Info_Node);

   --------------------------------------------
   --                                        --
   --  Here is the pre procedure to provide  --
   --  to Traverse_Element to make a node    --
   --  display.                              --
   --                                        --
   --------------------------------------------

   procedure Pre_Procedure
     (Element : in     Asis.Element;
      Control : in out Asis.Traverse_Control;
      State   : in out Info_Node);

   procedure Post_Procedure
     (Element : in     Asis.Element;
      Control : in out Asis.Traverse_Control;
      State   : in out Info_Node);

   procedure Initiate_Node
     (Unit : in     Asis.Compilation_Unit;
      Control : in out Asis.Traverse_Control;
      State   : in out Info_Node);

   procedure Terminate_Node
     (Control : in out Asis.Traverse_Control;
      State   : in out Info_Node);

end Vatox.Traversal;
