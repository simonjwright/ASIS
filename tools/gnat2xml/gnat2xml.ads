------------------------------------------------------------------------------
--                                                                          --
--                            GNAT2XML COMPONENTS                           --
--                                                                          --
--                              G N A T 2 X M L                             --
--                                                                          --
--                                 S p e c                                  --
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

--  Root package for the Gnat2xml tools. There are two main
--  executables the schema (XSD) generator (gnat2xsd[.exe]) and the XML
--  generator (gnat2xml[.exe]). The main procedure for the former is
--  Gnat2xml.Gnat2xsd. The latter uses ASIS_UL, and the main procedure
--  is Gnat2xml.Driver.

with ASIS_UL.Debug;
with ASIS_UL.Utilities;

with A4G.Queries; use A4G;

with Asis.Text;

pragma Warnings (Off); -- imported for children
with Namet; use Namet;
with ASIS_UL.String_Utilities;   use ASIS_UL.String_Utilities;
with ASIS_UL.Dbg_Out;
with Asis.Extensions.Flat_Kinds; use Asis.Extensions.Flat_Kinds;
pragma Warnings (On);
with Ada_Trees;

package Gnat2xml is

   subtype Classes is Ada_Trees.Classes;
   subtype Opt_ASIS_Elems is Ada_Trees.Opt_ASIS_Elems;

   function Ekind
     (Element : Asis.Element)
      return    Opt_ASIS_Elems renames
     Ada_Trees.Ekind;

   use type A4G.Queries.Query_Index;

   function Span
     (Element : Asis.Element)
      return    Asis.Text.Span renames
     Ada_Trees.Span;

   subtype ASIS_Elems is Ada_Trees.ASIS_Elems;
   subtype Def_Names is Ada_Trees.Def_Names;
   subtype Usage_Names is Ada_Trees.Usage_Names;
   subtype Name_Elems is Ada_Trees.Name_Elems;
   subtype Boolean_Elems is Ada_Trees.Boolean_Elems;
   subtype Other_Elems is Ada_Trees.Other_Elems;

   subtype Unit_Kinds is Asis.Unit_Kinds;
   subtype Unit_Classes is Asis.Unit_Classes;
   subtype Unit_Origins is Asis.Unit_Origins;

   function Is_Leaf
     (E    : Asis.Element)
      return Boolean is
     (Queries.Num_Queries (Ekind (E)) = 0);

   Main_Done : Boolean renames ASIS_UL.Utilities.Main_Done;

   Debug_Mode : Boolean renames ASIS_UL.Debug.Debug_Flag_9;

end Gnat2xml;
