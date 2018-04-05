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

package body Vatox.Xsl_Transformation is

   -- Stub version that does not perform XSL Transformation

   -----------------------------------------------------------------------------

   function Is_Xsl_Available return Boolean is
   begin
      return False;
   end Is_Xsl_Available;

   -----------------------------------------------------------------------------

   function XSL_Transformation_To_Be_Done (Xsl_Info : Xsl_Information)
                                           return Boolean is
   begin
      return False;
   end Xsl_Transformation_To_Be_Done;

   -----------------------------------------------------------------------------

   function Xsl_Result_String (Result : Transformation_Results) return String is
   begin
      return "";
   end Xsl_Result_String;

   -----------------------------------------------------------------------------

   procedure Set_Xsl_Filename
     (Xsl_Info : in out Xsl_Information;
      -- XSL transformation configuration information

      Filename : in     String
      -- Filename of XSL stylesheet to apply
     ) is
   begin
      null;
   end Set_Xsl_Filename;

   -----------------------------------------------------------------------------

   procedure Set_Output_Filename
     (Xsl_Info : in out Xsl_Information;
      -- XSL transformation configuration information

      Filename : in     String
      -- Filename into which to write the transformed output
     ) is
   begin
      null;
   end Set_Output_Filename;

   -----------------------------------------------------------------------------

   function Get_Output_Filename
     ( Xsl_Info : Xsl_Information) return String is
   begin
      return "";
   end Get_Output_Filename;

   -----------------------------------------------------------------------------

   procedure Set_XSL_Extension
     (Xsl_Info  : in out Xsl_Information;
      -- XSL transformation configuration information

      Extension : in     String
      -- File extension
     ) is
   begin
      null;
   end Set_XSL_Extension;

   -----------------------------------------------------------------------------

   function Get_XSL_Extension
     (Xsl_Info : Xsl_Information
      -- XSL transformatino configuration information
     ) return String is
   begin
      return "";
   end Get_Xsl_Extension;

   -----------------------------------------------------------------------------

   procedure Add_Parameter_Pair
     (Xsl_Info         : in out Xsl_Information;
      -- XSL transformation configuration information

      Param_Value_Pair : in     String;
      -- String that is in the "param=value" format.

      Added            :    out Boolean
      -- Whether the parameter was successfully added to the list of parameters
     ) is

      pragma Unreferenced(Added);
   begin
      null;
   end Add_Parameter_Pair;

   -----------------------------------------------------------------------------

   procedure Apply_Stylesheet
     (Input_Filename   : in     String;
      Output_Filename  : in     String;
      Xsl_Info         : in     Xsl_Information;
      Result           :    out Transformation_Results
     ) is
     pragma Unreferenced (Result);
   begin
      null;
   end Apply_Stylesheet;

end Vatox.Xsl_Transformation;
