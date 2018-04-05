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
-- Avatox is now maintained by McKae Technologies (http://www.mckae.com)    --                                               --
------------------------------------------------------------------------------

package Vatox.Axf_Pedigrees is

   -- Maintain the AXF element pedigrees that are providing elements to an
   -- AXF document.

   type Pedigree_Types is
     (Standard_Info, 	-- The pedigree indicates elements derived from
      			-- an actual or de facto standard.
      Processor_Info, 	-- The pedigree represents elements employed by an
      			-- AXF processor.
      Manual_Info	-- The predigree indicates elements that are manually
      			-- emplaced within an AXF document.
     );

   -- Register a pedigree
   procedure Register
     (Pedigree_Id         : in String;
      Pedigree_Type       : in Pedigree_Types;
      Pedigree_Producer   : in String;
      Implementor         : in String;
      Implementor_Version : in String);

   -- The reason that an iterative pedigree processor is used instead of a
   -- Write_Pedigree procedure is that it makes no assumption about how a
   -- pedigree is being output.  The handling of the pedigree is simply
   -- left to the application providing the Pedigree_Processor procedure.

   -- Procedure profile for procedures to iterate over the registered pedigrees.
   type Pedigree_Processors is
     access procedure (Pedigree_Id         : in     String;
                       Pedigree_Type       : in     Pedigree_Types;
                       Pedigree_Producer   : in     String;
                       Implementor         : in     String;
                       Implementor_Version : in     String;
                       Continue            :    out Boolean);

   -- Subject each registered pedigree to the processor until and unless the
   -- invoked processor returns a Continue parameter value of false.
   procedure Apply_Processor (P : in Pedigree_Processors);

end Vatox.Axf_Pedigrees;
