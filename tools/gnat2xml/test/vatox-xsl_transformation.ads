------------------------------------------------------------------------------
--                                                                          --
--                           AVATOX COMPONENTS                              --
--                                                                          --
--                        VATOX (Via Asis To Xml)                           --
--                                                                          --
--                                                                          --
--                Copyright (c) 2007, McKae Technologies.                   --
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

with Ada.Containers.Indefinite_Ordered_Maps;
with Ada.Strings.Unbounded;
use  Ada.Strings.Unbounded;

package Vatox.Xsl_Transformation is

   type Xsl_Information is private;

   -- Results of XSL transformation attempt
   type Transformation_Results is
     (Success,                  -- The XSL transformation was successfully performed
      Bad_Input_File,		-- The input file could not be opened/read
      Input_Not_Xml,		-- The input file does not contain valid XML
      Bad_Output_File, 		-- The output file could not be created/written
      Save_Failed,		-- Saving the transformed file failed
      Bad_Xsl_File,		-- The XSL file could not be opened/read
      Xsl_Not_Xsl,		-- The XSL file does not contain valid XSL
      Transformation_Failed	-- The application of the XSL stylesheet failed
     );

   -- Return whether this version of Avatox contains support for XSL
   -- transformations.
   function Is_Xsl_Available return Boolean;

   -- Return whether the XSL_Information provided so far indicates that an
   -- XSL transformation needs to be performed
   function XSL_Transformation_To_Be_Done
     (Xsl_Info : Xsl_Information
      -- XSL transformation configuration information
      ) return Boolean;

   -- Return a descriptive string corresponding to the transformation results
   function Xsl_Result_String (Result : Transformation_Results) return String;

   -- Specify the name of the XSL stylesheet
   procedure Set_Xsl_Filename
     (Xsl_Info : in out Xsl_Information;
      -- XSL transformation configuration information

      Filename : in     String
      -- Filename of XSL stylesheet to apply
     );

   -- Specify the name of the file into which to write the transformed output
   procedure Set_Output_Filename
     (Xsl_Info : in out Xsl_Information;
      -- XSL transformation configuration information

      Filename : in     String
      -- Filename into which to write the transformed output
     );

   -- Retreive the output filename, "" if none was specified
   function Get_Output_Filename
     (Xsl_Info : in Xsl_Information
      -- XSL transformation configuration information
      ) return String;


   -- Specify the extension to apply to individually transformed files
   procedure Set_XSL_Extension
     (Xsl_Info  : in out Xsl_Information;
      -- XSL transformation configuration information

      Extension : in     String
      -- File extension
     );

   -- Get the set XSL_extension
   function Get_XSL_Extension
     (Xsl_Info : Xsl_Information
      -- XSL transformatino configuration information
      ) return String;

   -- Add a parm/value parameter pair to the XSL transformation configuration.
   -- Param/value pairs must be in the format "param=value", this will be
   -- checked.
   procedure Add_Parameter_Pair
     (Xsl_Info         : in out Xsl_Information;
      -- XSL transformation configuration information

      Param_Value_Pair : in     String;
      -- String that is in the "param=value" format.

      Added            :    out Boolean
      -- Whether the parameter was successfully added to the list of parameters
     );

   -- Apply the XSL configuration to the given input file
   procedure Apply_Stylesheet
     (Input_Filename   : in     String;
      -- Filename to which to subject to an XSL stylesheet

      Output_Filename  : in     String;
      -- Filename to which to write the transformed XSL

      Xsl_Info         : in     Xsl_Information;
      -- XSL transformation configuration information

      Result          :    out Transformation_Results
      -- Result of attempting to apply the stylesheet
     );

private
   package Parameter_Lists is
     new Ada.Containers.Indefinite_Ordered_Maps (String, String);

   Default_AXF_XSL_Extension : constant String := "axt";

   type Xsl_Information is
      record
         -- Name of the XSL file to apply
         Xsl_Filename    : Unbounded_String;

         -- Name of the file in which to write the transformed output
         -- (if selected)
         Output_Filename : Unbounded_String;

         -- Extension to append to the transformed version of each AXF file.
         Xsl_Extension   : Unbounded_String
           := To_Unbounded_String (Default_AXF_XSL_Extension);

         -- XSL Parameter that will be passed to the stylesheet upon invocation
         Parameters      : Parameter_Lists.Map;
      end record;

end Vatox.Xsl_Transformation;
