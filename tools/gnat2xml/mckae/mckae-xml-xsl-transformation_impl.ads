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

with Interfaces.C.Strings;

private package McKae.XML.XSL.Transformation_Impl is

   use Interfaces;

   --  int xmlLoadExtDtdDefaultValue;	(globals.h)
   Xml_Load_Ext_Dtd_Default_Value : C.int;
   pragma Import
     (C,
      Xml_Load_Ext_Dtd_Default_Value,
      "xmlLoadExtDtdDefaultValue");

   --  typedef xsltStylesheet * xsltStylesheetPtr;  (xsltInternals.h)
   --  (Don't care what a stylesheet actually looks like, just need to hold a
   --  handle to it.)

   type Xslt_Stylesheet_Ptr is access all Integer;
   pragma Convention (C, Xslt_Stylesheet_Ptr);

   --  typedef xmlDoc *xmlDocPtr;	(tree.h)
   --  (Don't care what an xmlDoc actually looks like, just need to hold a
   --  handle to it.)

   type Xml_Doc_Ptr is access all Integer;
   pragma Convention (C, Xml_Doc_Ptr);

   --  XMLPUBFUN int XMLCALL		(parser.h)
   --         xmlSubstituteEntitiesDefault(int val);
   function Xml_Substitute_Entities_Default (Val : C.int) return C.int;
   pragma Import
     (C,
      Xml_Substitute_Entities_Default,
      "xmlSubstituteEntitiesDefault");

   --  XSLTPUBFUN XsltStylesheetPtr XSLTCALL	(xsltInternals.h)
   --               xsltParseStylesheetFile (const xmlChar* filename);
   function Xslt_Parse_Stylesheet_File
     (Filename : C.Strings.chars_ptr)
     return Xslt_Stylesheet_Ptr;
   pragma Import (C, Xslt_Parse_Stylesheet_File, "xsltParseStylesheetFile");

   --  XMLPUBFUN xmlDocPtr XMLCALL
   --              xmlParseFile            (const char *filename);
   function Xml_Parse_File (Filename : C.Strings.chars_ptr) return Xml_Doc_Ptr;
   pragma Import (C, Xml_Parse_File, "xmlParseFile");

   --  char **

   type Xslt_Params is new C.Strings.chars_ptr_array;
   pragma Convention (C, Xslt_Params);

   --  XSLTPUBFUN xmlDocPtr XSLTCALL		(transforms.h)
   --             xsltApplyStylesheet     (xsltStylesheetPtr style,
   --                                      xmlDocPtr doc,
   --                                      const char **params);
   function Xslt_Apply_Stylesheet
     (Style  : Xslt_Stylesheet_Ptr;
      Doc    : Xml_Doc_Ptr;
      Params : Xslt_Params)
     return Xml_Doc_Ptr;
   pragma Import (C, Xslt_Apply_Stylesheet, "xsltApplyStylesheet");

   --  XSLTPUBFUN int XSLTCALL			(xsltutils.h)
   --             xsltSaveResultToFilename        (const char *URI,
   --                                              xmlDocPtr result,
   --                                              xsltStylesheetPtr style,
   --                                              int compression);
   function Xslt_Save_Result_To_Filename
     (URI         : C.Strings.chars_ptr;
      Result      : Xml_Doc_Ptr;
      Style       : Xslt_Stylesheet_Ptr;
      Compression : C.int)
     return C.int;
   pragma Import (C, Xslt_Save_Result_To_Filename, "xsltSaveResultToFilename");

   --  XSLTPUBFUN void XSLTCALL			(xslutils.h)
   --                      xsltFreeStylesheet      (xsltStylesheetPtr sheet);
   procedure Xslt_Free_Stylesheet (Sheet : Xslt_Stylesheet_Ptr);
   pragma Import (C, Xslt_Free_Stylesheet, "xsltFreeStylesheet");

   --  XMLPUBFUN void XMLCALL			(tree.h)
   --              xmlFreeDoc              (xmlDocPtr cur);
   procedure Xml_Free_Doc (Cur : Xml_Doc_Ptr);
   pragma Import (C, Xml_Free_Doc, "xmlFreeDoc");

   --   XSLTPUBFUN void XSLTCALL		(xslt.h)
   --             xsltCleanupGlobals      (void);
   procedure Xslt_Cleanup_Globals;
   pragma Import (C, Xslt_Cleanup_Globals, "xsltCleanupGlobals");

   --  XMLPUBFUN void XMLCALL			(parser.h)
   --              xmlCleanupParser        (void);
   procedure Xml_Cleanup_Parser;
   pragma Import (C, Xml_Cleanup_Parser, "xmlCleanupParser");

end McKae.XML.XSL.Transformation_Impl;
