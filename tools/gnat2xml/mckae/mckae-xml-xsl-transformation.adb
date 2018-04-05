------------------------------------------------------------------------
--                                                                    --
--                     McKae Software Utilities                       --
--                                                                    --
--           Copyright (C) 2007 McKae Technologies                    --
--                                                                    --
-- The  McKae   software  utilities   are  free  software;   you  can --
-- redistribute it  and/or modify it  under terms of the  GNU General --
-- Public  License  as published  by  the  Free Software  Foundation; --
-- either version  2, or (at  your option) any later  version.  McKae --
-- Software Utilities are  distributed in the hope that  they will be --
-- useful,  but  WITHOUT  ANY  WARRANTY;  without  even  the  implied --
-- warranty of  MERCHANTABILITY or FITNESS FOR  A PARTICULAR PURPOSE. --
-- See the GNU  General Public License for more  details.  You should --
-- have received a copy of the GNU General Public License distributed --
-- with DTraq; see file COPYING.   If not, write to the Free Software --
-- Foundation, 59  Temple Place -  Suite 330, Boston,  MA 02111-1307, --
-- USA.                                                               --
--                                                                    --
-- As a  special exception, if other files  instantiate generics from --
-- this unit,  or you link this  unit with other files  to produce an --
-- executable,  this unit  does  not by  itself  cause the  resulting --
-- executable to be covered by  the GNU General Public License.  This --
-- exception does  not however invalidate  any other reasons  why the --
-- executable file might be covered by the GNU Public License.        --
--                                                                    --
-- The McKae Software Utilities  are maintained by McKae Technologies --
-- (http://www.mckae.com).                                            --
------------------------------------------------------------------------

with Ada.Text_IO;
with Interfaces.C.Strings;
with McKae.XML.XSL.Transformation_Impl; use McKae.XML.XSL.Transformation_Impl;
with Ada.Text_IO;
with Ada.Text_IO;

package body McKae.XML.XSL.Transformation is

   use Interfaces;

   --  Apply the XSL stylesheet to the given file, writing the transformed XML
   --  into the provided file.

   procedure Apply_Xsl
     (Input_Filename  : String;
      Xsl_Filename    : String;
      Target_Filename : String;
      Status          : out Application_Statuses;
      Parameters : Parameter_Settings := No_Parameters)
   is

      use type C.size_t;

      Params : aliased Xslt_Params :=
        (0 .. Parameters'Length * 2 => C.Strings.Null_Ptr);
      Dummy       : C.int;
      Save_Result : C.int;

      Xsl_Handle    : Xslt_Stylesheet_Ptr;
      Xml_In_Handle : Xml_Doc_Ptr;
      Xsl_Result    : Xml_Doc_Ptr;

      Xsl_File    : C.Strings.chars_ptr := C.Strings.New_String (Xsl_Filename);
      Xml_In_File : C.Strings.chars_ptr :=
        C.Strings.New_String (Input_Filename);
      Xml_Out_File : C.Strings.chars_ptr :=
        C.Strings.New_String (Target_Filename);

      Test_File : Ada.Text_IO.File_Type;

      use type C.int;
      use type C.size_t;
      use type C.Strings.chars_ptr;

   begin
      Status := Success;

      --  Ensure we can access all the provided files

      begin
         Ada.Text_IO.Open (Test_File, Ada.Text_IO.In_File, Input_Filename);
         Ada.Text_IO.Close (Test_File);
      exception
         when others =>
            Status := Bad_Input_File;
      end;

      if Status = Success then
         begin
            Ada.Text_IO.Open (Test_File, Ada.Text_IO.In_File, Xsl_Filename);
            Ada.Text_IO.Close (Test_File);
         exception
            when others =>
               Status := Bad_Xsl_File;
               return;
         end;
      end if;

      if Status = Success then
         begin
            Ada.Text_IO.Create
              (Test_File,
               Ada.Text_IO.Out_File,
               Target_Filename);
            Ada.Text_IO.Delete (Test_File);
         exception
            when others =>
               Status := Bad_Output_File;
         end;
      end if;

      if Status = Success then
         --  Load up the parameters
         for I in Parameters'Range loop
            Params (C.size_t (I) * 2 - 2) :=
              C.Strings.New_String (To_String (Parameters (I).Key));
            Params (C.size_t (I) * 2 - 1) :=
              C.Strings.New_String (To_String (Parameters (I).Value));
         end loop;

         --  Initialize XSL stuff

         --  Genuinely don't care about result of this call:
         Dummy := Xml_Substitute_Entities_Default (1);

         Xml_Load_Ext_Dtd_Default_Value := 1;

         --  Start XSL processing
         Xsl_Handle := Xslt_Parse_Stylesheet_File (Xsl_File);
         if Xsl_Handle = null then
            Status := Xsl_Not_Xsl;
         end if;

         if Status = Success then
            Xml_In_Handle := Xml_Parse_File (Xml_In_File);
            if Xml_In_Handle = null then
               Status := Input_Not_Xml;
            end if;
         end if;

         if Status = Success then
            Xsl_Result :=
              Xslt_Apply_Stylesheet (Xsl_Handle, Xml_In_Handle, Params);
            if Xsl_Result = null then
               Status := Transformation_Failed;
            end if;
         end if;

         if Status = Success then
            Save_Result :=
              Xslt_Save_Result_To_Filename
                (Xml_Out_File,
                 Xsl_Result,
                 Xsl_Handle,
                 0);
            if Save_Result = -1 then
               Status := Save_Failed;
            end if;
         end if;
      end if;

      C.Strings.Free (Xsl_File);
      C.Strings.Free (Xml_In_File);
      C.Strings.Free (Xml_Out_File);

      for I in Params'Range loop
         if Params (I) /= C.Strings.Null_Ptr then
            C.Strings.Free (Params (I));
         end if;
      end loop;

      Xslt_Cleanup_Globals;
      Xml_Cleanup_Parser;
   end Apply_Xsl;

end McKae.XML.XSL.Transformation;
