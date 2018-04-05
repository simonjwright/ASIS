------------------------------------------------------------------------
--                                                                    --
--                     McKae Software Utilities                       --
--                                                                    --
--           Copyright (C) 2005 McKae Technologies                    --
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
with Ada.Strings.Unbounded;
use  Ada.Strings.Unbounded;

generic

   type Output_Medium is limited private;

   with procedure Put (F : in Output_Medium;
                       S : in String) is <>;
   with procedure New_Line (F       : in Output_Medium;
                            Spacing : in Ada.Text_IO.Positive_Count := 1) is <>;

   -- DEPRECATED
   -- Control formatting by setting the Current_Format variable in the
   -- package spec.
   --
   -- Specify whether the XML that is created is to have indenting and
   -- line breaks.
   Format : Formatting_Options := Spread_Indented;
   -- DEPRECATED

   -- The maximum element nesting depth of an XML document
   Max_Element_Nesting : Positive := 200;

package McKae.XML.EZ_Out.Generic_Medium is

   -------------------------------------------------------------------
   -- This package provides the means to easily write XML elements and
   --  associated attributes to a provided medium that provides the
   --  required interface.

   -- Note that this package is designed in such a way that
   --  instantiations of it are meant to be "used" by the application.
   --  When accompanied with a "use" clause, specifying the XML to be
   --  produced is very simple and clear.  Insisting on using qualified
   --  names will make for very obtuse code.  Likewise, "named
   --  parameter" notation would obscure the complementarity of the Ada
   -- and XML, so apply for a waiver from any such style standards.
   -------------------------------------------------------------------

   -- The medium must be open and ready to accept content before
   --  invoking any of these XML output subprograms.

   -- If the medium raises any exceptions as a result of invoking the
   --  supplied Put or New_Line procedures, those exceptions will be
   --  passed through to the caller.

   -------------------------------------------------------------------
   -- The identation format of the XML that is output.  This can be
   -- altered at any time.
   Current_Format   : Formatting_Options := Format;

   -- Whether to output an attribute if it has a null value.
   Default_Output_Null_Attributes : Boolean := False;


   -------------------------------------------------------------------
   -- These procedures for outputting XML header elements cannot be
   --  invoked when there are any nested elements opening, i.e.,
   --  Start_Element has been called.  Doing so will result in the
   --  raising of an Invalid_Construction exception.

   -- Settings for the document header's standalone attribute.
   type Standalone_Values is (Yes, No, Omit);

   -- Output a standard XML header line, as amended by the supplied
   --  arguments.  To omit the attribute, pass an empty string.
   --   <?xml version="1.0" encoding="UTF-8" ?>
   procedure Output_XML_Header (F          : in Output_Medium;
                                Standalone : in Standalone_Values := Omit;
                                Encoding   : in String            := "UTF-8";
                                Version    : in String            := "1.0");

   -- Add a processing instruction to the XML document.
   procedure Output_Processing_Instruction (F      : in Output_Medium;
                                            Target : in String;
                                            Data   : in String);

   -------------------------------------------------------------------

   -- Representation of attribute/value pairs
   type Attribute_Value_Pairs is private;

   -- List of attributes and corresponding values to associated with
   --  an element
   type Attributes_List is array (Natural range <>) of Attribute_Value_Pairs;

-- Indicator that the element has no associated attributes
   No_Attributes : constant Attributes_List;

   -- Generate an entire element designated with the given tag and
   --  containing the provided content and list of attributes
   procedure Output_Element (F       : in Output_Medium;
                             Tag     : in String;
                             Content : in String;
                             Attrs   : in Attributes_List := No_Attributes;
                             Subst   : in Boolean         := True);

   -- Generate an entire element designated with the given tag and
   --  containing the provided content single attribute specification
   procedure Output_Element (F       : in Output_Medium;
                             Tag     : in String;
                             Content : in String;
                             Attrs   : in Attribute_Value_Pairs;
                             Subst   : in Boolean               := True);

   -- Generate an element tag containing zero or more attributes.  By
   --  default the element is created using the compact, no-end-tag
   --  notation; to force generation of an element that has both start
   --  and end tags and no content, set End_Tag to True.
   procedure Output_Tag (F       : in Output_Medium;
                         Tag     : in String;
                         Attrs   : in Attributes_List := No_Attributes;
                         End_Tag : in Boolean         := False;
                         Subst   : in Boolean         := True);

   -- Generate an element tag with a single attribute specification.
   --  By default the element is created using the compact, no-end-tag
   --  notation; to force generation of an element that has both start
   --  and end tags and no content, set End_Tag to True.
   procedure Output_Tag (F       : in Output_Medium;
                         Tag     : in String;
                         Attrs   : in Attribute_Value_Pairs;
                         End_Tag : in Boolean               := False;
                         Subst   : in Boolean               := True);

   -- Initiate the generation of an XML element with the given tag and
   --  zero or more attribute specifications using an Attributes_List
   --  initializing aggregate.  If there is only one attribute to be
   --  specified, the single attribute version of Start_Element may be
   --  used instead so as to avoid having to use named notation to
   --  specify the single element of the list.
   procedure Start_Element (F     : in Output_Medium;
                            Tag   : in String;
                            Attrs : in Attributes_List := No_Attributes;
                            Subst : in Boolean         := True);

   -- Initiate the generation of an XML element with the given tag and
   --  a single attribute specification.
   procedure Start_Element (F     : in Output_Medium;
                            Tag   : in String;
                            Attrs : in Attribute_Value_Pairs;
                            Subst : in Boolean               := True);

   -- Indicate the completion of the output of an XML element.  If a
   --  Tag is specified, compare it against the element tag that is
   --  currently open, and raise Element_End_Mismatch if the two do
   --  not match.  If there is no open element, then raise
   --  Element_Not_Open.
   procedure End_Element (F   : in Output_Medium;
                          Tag : in String        := "");

   -- Place the text, as is, as the content of the currently open XML
   --  element.  Output_Content can be called repeatedly, and will
   --  simply continue to append the additional content.  If there is
   --  no open element, raise Element_Not_Open.
   procedure Output_Content (F     : in Output_Medium;
                             S     : in String;
                             Subst : in Boolean       := True);

   -- Place the numeric value, as a base 10 text representation, as
   --  the content of the currently open XML element.  Output_Content
   --  can be called repeatedly, and will simply continue to append
   --  the additional content.  If there is no open element, raise
   --  Element_Not_Open.
   procedure Output_Content (F : in Output_Medium;
                            N : in Integer'Base);

   -- Place the text represenatation of the numeric value as the
   --  content of the currently open XML element.  Output_Content can
   --  be called repeatedly, and will simply continue to append the
   --  additional content.  If there is no open element, raise
   --  Element_Not_Open.
   procedure Output_Content (F : in Output_Medium;
                             N : in Float'Base);


   -- The following overloaded "=" functions are the only means by
   --  which to create attribute/value pairs.

   -- Attribute provided as String

   -- Associate an attribute with a string value.
   function "="(Attr  : String;
                Value : String)
               return Attribute_Value_Pairs;

   -- Associate an attribute with a character value.
   function "="(Attr  : String;
                Value : Character)
               return Attribute_Value_Pairs;

   -- Associate an attribute with a string value.
   function "="(Attr  : String;
                Value : Unbounded_String)
               return Attribute_Value_Pairs;

   -- Associate an attribute with an integral value.
   function "="(Attr  : String;
                Value : Integer'Base)
               return Attribute_Value_Pairs;

   -- Associate an attribute with a floating point value.
   function "="(Attr  : String;
                Value : Float'Base)
               return Attribute_Value_Pairs;

   -- Associate an attribute with a floating point value.
   function "="(Attr  : String;
                Value : Long_Float'Base)
                return Attribute_Value_Pairs;

   -- Attribute provided as Unbounded_String

   -- Associate an attribute with a string value.
   function "="(Attr  : Unbounded_String;
                Value : String)
               return Attribute_Value_Pairs;

   -- Associate an attribute with a character value.
   function "="(Attr  : Unbounded_String;
                Value : Character)
               return Attribute_Value_Pairs;

   -- Associate an attribute with a string value.
   function "="(Attr  : Unbounded_String;
                Value : Unbounded_String)
               return Attribute_Value_Pairs;

   -- Associate an attribute with an integral value.
   function "="(Attr  : Unbounded_String;
                Value : Integer'Base)
               return Attribute_Value_Pairs;

   -- Associate an attribute with a floating point value.
   function "="(Attr  : Unbounded_String;
                Value : Float'Base)
               return Attribute_Value_Pairs;

   -- Associate an attribute with a long floating point value.
   function "="(Attr  : Unbounded_String;
                Value : Long_Float'Base)
               return Attribute_Value_Pairs;

private
   type Attribute_Value_Pairs is
      record
         Attr  : Unbounded_String;
         Value : Unbounded_String;
      end record;

   No_Attributes : constant Attributes_List (1 .. 0)
     := (others => (others => Null_Unbounded_String));
end McKae.XML.EZ_Out.Generic_Medium;
