------------------------------------------------------------------------------
--                                                                          --
--                           AVATOX COMPONENTS                              --
--                                                                          --
--                        VATOX (Via Asis To Xml)                           --st
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

with Vatox.Xml_File_Output; use Vatox.Xml_File_Output;


package Vatox.Axf_Points is

   pragma Elaborate_Body;

   -- This is the root package for the various collections of AXF Points,
   -- i.e., AXF Points Of INformation for Transformation.
   -- AXF Points are used to provide supplemental context-dependent
   -- information within the Avatox XML Format (AXF) files.
   --
   -- AXF Points can provide different kinds of information and are expected
   -- to evolve as tranformations become more capable and complex.  The
   -- initial AXF Point collection is for operator nomenclature, since different
   -- programming languages can use different character(s) to represent
   -- operators, e.g. "not equals" can be "/=" (Ada) or "!=" (C, C++, Java).

   -- Tag for AXF Point elements
   AXF_Point_Tag          : constant String := "axfPoint";

   -- Pedigree information
   AXF_Points_Pedigree_Id : constant String := "axfPoints";

   AXF_Points_Pedigree    : constant Attribute_Value_Pairs
     := "pedigree" = AXF_Points_Pedigree_Id;

end Vatox.Axf_Points;
