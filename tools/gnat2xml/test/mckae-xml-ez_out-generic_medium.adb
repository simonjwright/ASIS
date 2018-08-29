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

with Ada.Strings.Fixed;
use  Ada.Strings.Fixed;

package body McKae.XML.EZ_Out.Generic_Medium is

   ------------------------------------------------------------------------

   -- A very basic bounded stack implementation for keeping track of
   --  nested XML elements.

   type Stack_Size is new Natural range 0 .. Max_Element_Nesting;
   subtype Stack_Indices is Stack_Size range 1 .. Stack_Size'Last;

   type Element_Stacks is array (Stack_Indices) of Unbounded_String;

   Tag_Stack : Element_Stacks;
   Top_Of_Stack : Stack_Size := 0;

   procedure Push (Tag : Unbounded_String) is
   begin
      if Top_Of_Stack /= Stack_Size'Last then
         Top_Of_Stack := Top_Of_Stack + 1;
         Tag_Stack (Top_Of_Stack) := Tag;
      else
         raise Nesting_Too_Deep;
      end if;
   end Push;

   procedure Pop (Tag : out Unbounded_String) is
   begin
      if Top_Of_Stack /= 0 then
         Tag := Tag_Stack (Top_Of_Stack);
         Top_Of_Stack := Top_Of_Stack - 1;
      else
         raise Element_Not_Open;
      end if;
   end Pop;

   ------------------------------------------------------------------------

   type XML_Component_Kind is (Header_Component,
                               Start_Tag_Component,
                               Content_Component,
                               End_Tag_Component);

   ------------------------------------------------------------------------

   -- Constructed Put_Line from provided primitives
   procedure Put_Line (F : in Output_Medium;
                       S : in String) is
   begin
      Put (F, S);
      New_Line (F);
   end Put_Line;

   ------------------------------------------------------------------------

   procedure Replace_Special (C : in     String;
                              R : in     String;
                              S : in out Unbounded_String) is
      P : Natural := 0;
   begin
      if Index (R, C) /= 0 then
         -- The string to be replaced is present within the replacing
         --  string (e.g., "&" by "&amp;"), so the replacement has to
         --  take this into account.
         P := 1;
         while (P + C'Length - 1) <= Length (S) loop
            if Slice (S, P, P + C'Length - 1) = C then
               Replace_Slice (S, P, P + C'Length - 1, R);
               P := P + R'Length;  -- Skip over replacement string
            else
               P := P + 1;
            end if;
         end loop;
      else
         -- The string to be replaced is not present within the
         --  replacing string, so a simple find and replace can be
         --  done.
         loop
            P := Index (S, C);
            exit when P = 0;
            Replace_Slice (S, P, P + C'Length - 1, R);
         end loop;
      end if;
   end Replace_Special;

   ------------------------------------------------------------------------

   function Replace_Specials (S              : Unbounded_String;
                              Subst          : Boolean;
                              Replace_Quotes : Boolean          := False;
                              Replace_Apos   : Boolean          := False)
     return Unbounded_String is

      New_S : Unbounded_String := S;
   begin
      if Subst then
         -- Ampersands must be replaced first, since the replacement
         --  strings contain ampersands
         Replace_Special ("&", "&amp;", New_S);

         Replace_Special ("<", "&lt;", New_S);
         Replace_Special ("]]>", "]]&gt;", New_S);

         if Replace_Quotes then
            Replace_Special ("""", "&quot;", New_S);
         end if;
         if Replace_Apos then
            Replace_Special ("'", "&apos;", New_S);
         end if;
      end if;
      return New_S;
   end Replace_Specials;

   ------------------------------------------------------------------------

   -- Output the string in accordance with the specified format option.
   procedure Formatted_Put (F : in Output_Medium;
                           S : in Unbounded_String;
                           K : in XML_Component_Kind) is

      -- The number of items in the element nesting stack is directly
      --  proportional to the amount of required indenting
      Indentation : constant Natural := Natural (Top_Of_Stack) * 3;
      Value       : constant String := To_String (S);

   begin
      case K is
         when Header_Component =>
            pragma Assert (Top_Of_Stack = 0);
            case Current_Format is
               when Continuous_Stream =>
                  Put (F, Value);
               when Spread_Indented =>
                  Put_Line (F, Value);
            end case;
         when Start_Tag_Component | Content_Component | End_Tag_Component =>
            case Current_Format is
               when Continuous_Stream =>
                  Put (F, value);
               when Spread_Indented =>
                  Put (F, Indentation * ' ');
                  Put_Line (F, value);
            end case;
      end case;
   end Formatted_Put;

   ------------------------------------------------------------------------

   -- Output a standard XML header line, as amended by the supplied
   --  arguments.  To omit the attribute, pass an empty string.
   --   <?xml version="1.0" encoding="UTF-8" ?>
   procedure Output_XML_Header (F          : in Output_Medium;
                                Standalone : in Standalone_Values := Omit;
                                Encoding   : in String            := "UTF-8";
                                Version    : in String            := "1.0") is
      Header : Unbounded_String := To_Unbounded_String ("<?xml");
   begin
      if Top_Of_Stack = 0 then
         if Version /= "" then
            Append (Header, " version=""" & Version & """");
         end if;

         if Encoding /= "" then
            Append (Header, " encoding=""" & Encoding & """");
         end if;

         if Standalone /= Omit then
            Append (Header, " standalone=""");
            if Standalone = Yes then
               Append (Header, "yes""");
            else
               Append (Header, "no""");
            end if;
         end if;
         Append (Header, " ?>");
         Formatted_Put (F, Header, Header_Component);
      else
         raise Invalid_Construction;
      end if;
   end Output_XML_Header;

   ------------------------------------------------------------------------

   -- Add a processing instruction to the XML document.
   procedure Output_Processing_Instruction (F      : in Output_Medium;
                                            Target : in String;
                                            Data   : in String) is
   begin
      if Top_Of_Stack = 0 then
         Formatted_Put (F,
                       To_Unbounded_String ("<?" & Target & " " & Data & " ?>"),
                       Header_Component);
      else
         raise Invalid_Construction;
      end if;
   end Output_Processing_Instruction;

   ------------------------------------------------------------------------

   -- Generate an entire element designated with the given tag and
   --  containing the provided content and list of attributes
   procedure Output_Element (F       : in Output_Medium;
                             Tag     : in String;
                             Content : in String;
                             Attrs   : in Attributes_List := No_Attributes;
                             Subst   : in Boolean         := True) is
      Tag_Start : Unbounded_String := "<" & To_Unbounded_String (Tag);
      Tag_End   : constant Unbounded_String := "</" & To_Unbounded_String (Tag) & ">";
   begin
      if Attrs /= No_Attributes then
         for A in Attrs'Range loop
            if (Attrs (A).Value /= Null_Unbounded_String)
              or Default_Output_Null_Attributes then
               Append (Tag_Start, " " & Attrs (A).Attr
                      & "=""" & Replace_Specials (Attrs (A).Value, Subst,
                                                 Replace_Quotes => True,
                                                 Replace_Apos   => True) & """");
            end if;
         end loop;
      end if;
      Append (Tag_Start, ">");
      Formatted_Put (F, Tag_Start, Start_Tag_Component);
      Formatted_Put (F, Replace_Specials (To_Unbounded_String (Content), Subst),
                    Content_Component);
      Formatted_Put (F, Tag_End, End_Tag_Component);
   end Output_Element;

   ------------------------------------------------------------------------

   -- Generate an entire element designated with the given tag and
   --  containing the provided content single attribute specification
   procedure Output_Element (F       : in Output_Medium;
                             Tag     : in String;
                             Content : in String;
                             Attrs   : in Attribute_Value_Pairs;
                             Subst   : in Boolean               := True) is
   begin
      Output_Element (F, Tag, Content,
                     Attributes_List'(1 => Attrs),
                     Subst);
   end Output_Element;

   ------------------------------------------------------------------------

   -- Generate an entire element designated with the given tag and
   --  containing zero or more attributes.  By default the element is
   --  created using the compact, no-end-tag notation; to force
   --  generation of an element that has both start and end tags and
   --  no content, set End_Tag to True.
   procedure Output_Tag (F       : in Output_Medium;
                         Tag     : in String;
                         Attrs   : in Attributes_List := No_Attributes;
                         End_Tag : in Boolean         := False;
                         Subst   : in Boolean         := True) is
      Tag_Start : Unbounded_String := "<" & To_Unbounded_String (Tag);
      Tag_End   : constant Unbounded_String := "</" & To_Unbounded_String (Tag) & ">";
   begin
      if Attrs /= No_Attributes then
         for A in Attrs'Range loop
            if (Attrs (A).Value /= Null_Unbounded_String)
              or Default_Output_Null_Attributes then
               Append (Tag_Start, " " & Attrs (A).Attr
                      & "=""" & Replace_Specials (Attrs (A).Value, Subst,
                                                 Replace_Quotes => True,
                                                 Replace_Apos   => True) & """");
            end if;
         end loop;
      end if;

      if End_Tag then
         Append (Tag_Start, ">");
         Formatted_Put (F, Tag_Start, Start_Tag_Component);
         Formatted_Put (F, Tag_End, End_Tag_Component);
      else
         Append (Tag_Start, "/>");
         Formatted_Put (F, Tag_Start, Start_Tag_Component);
      end if;
   end Output_Tag;

   ------------------------------------------------------------------------

   -- Generate an element tag with a single attribute specElementification.
   --  By default the element is created using the compact, no-end-tag
   --  notation; to force generation of an element that has both start
   --  and end tags and no content, set End_Tag to True.
   procedure Output_Tag (F       : in Output_Medium;
                         Tag     : in String;
                         Attrs   : in Attribute_Value_Pairs;
                         End_Tag : in Boolean               := False;
                         Subst   : in Boolean               := True) is
   begin
      Output_Tag (F, Tag, Attributes_List'(1 => Attrs), End_Tag, Subst);
   end Output_Tag;

   ------------------------------------------------------------------------

   -- Initiate the generation of an XML element with the given tag and
   --  zero or more attribute specifications using an Attributes_List
   --  initializing aggregate.  If there is only one attribute to be
   --  specified, the single attribute version of Start_Element may be
   --  used instead so as to avoid having to use named notation to
   --  specify the single element of the list.
   procedure Start_Element (F     : in Output_Medium;
                            Tag   : in String;
                            Attrs : in Attributes_List := No_Attributes;
                            Subst : in Boolean         := True) is
      Tag_Start : Unbounded_String := "<" & To_Unbounded_String (Tag);
   begin
      -- First output the tag and any attributes.
      if Attrs /= No_Attributes then
         for A in Attrs'Range loop
            if (Attrs (A).Value /= Null_Unbounded_String)
              or Default_Output_Null_Attributes then
               Append (Tag_Start, " " & Attrs (A).Attr
                      & "=""" & Replace_Specials (Attrs (A).Value, Subst,
                                                 Replace_Quotes => True,
                                                 Replace_Apos   => True) & """");
            end if;
         end loop;
      end if;
      Append (Tag_Start, ">");
      Formatted_Put (F, Tag_Start, Start_Tag_Component);

      Push (To_Unbounded_String (Tag));
   end Start_Element;

   ------------------------------------------------------------------------

   -- Initiate the generation of an XML element with the given tag and
   --  a single attribute specification.
   procedure Start_Element (F     : in Output_Medium;
                            Tag   : in String;
                            Attrs : in Attribute_Value_Pairs;
                            Subst : in Boolean               := True) is
   begin
      Start_Element (F, Tag, Attributes_List'(1 => Attrs), Subst);
   end Start_Element;

   ------------------------------------------------------------------------

   -- Indicate the completion of the output of an XML element.  If a
   --  Tag is specified, compare it against the element tag that is
   --  currently open, and raise Element_End_Mismatch if the two do
   --  not match.  If there is no open element, then raise
   --  Element_Not_Open.
   procedure End_Element (F   : in Output_Medium;
                          Tag : in String        := "") is

      Open_Tag : Unbounded_String;

   begin
      Pop (Open_Tag);
      -- Validate the tag only if one was supplied
      if (Tag = "") or else (Tag = To_String (Open_Tag)) then
         Formatted_Put (F,
                       "</" & Open_Tag & ">",
                       End_Tag_Component);
      else
         raise Element_End_Mismatch;
      end if;
   end End_Element;

   ------------------------------------------------------------------------

   -- Place the text, as is, as the content of the currently open XML
   --  element.  Output_Content can be called repeatedly, and will
   --  simply continue to append the additional content.  If there is
   --  no open element, raise Element_Not_Open.
   procedure Output_Content (F     : in Output_Medium;
                             S     : in String;
                             Subst : in Boolean       := True) is
   begin
      Formatted_Put (F, Replace_Specials (To_Unbounded_String (S), Subst),
                    Content_Component);
   end Output_Content;

   ------------------------------------------------------------------------

   -- Place the numeric value, as a base 10 text representation, as
   --  the content of the currently open XML element.  Output_Content
   --  can be called repeatedly, and will simply continue to append
   --  the additional content.  If there is no open element, raise
   --  Element_Not_Open.
   procedure Output_Content (F : in Output_Medium;
                            N : in Integer'Base) is
      N_Rep : constant String := Integer'Base'Image (N);
   begin
      Output_Content (F, N_Rep (2 .. N_Rep'Length));
   end Output_Content;

   ------------------------------------------------------------------------

   -- Place the text represenatation of the numeric value as the
   --  content of the currently open XML element.  Output_Content can
   --  be called repeatedly, and will simply continue to append the
   --  additional content.  If there is no open element, raise
   --  Element_Not_Open.
   procedure Output_Content (F : in Output_Medium;
                             N : in Float'Base) is
      N_Rep : constant String := Float'Base'Image (N);
   begin
      Output_Content (F, N_Rep (2 .. N_Rep'Length));
   end Output_Content;

   ------------------------------------------------------------------------

   -- The following overloaded "=" functions are the only means by
   --  which to create attribute/value pairs.

   -- Attribute provided as String

   -- Associate an attribute with a string value.
   function "="(Attr  : String;
                Value : String)
               return Attribute_Value_Pairs is
   begin
      return To_Unbounded_String (Attr) = To_Unbounded_String (Value);
   end "=";

   -- Associate an attribute with a string value.
   function "="(Attr  : String;
                Value : Character)
               return Attribute_Value_Pairs is
   begin
      return To_Unbounded_String (Attr) = To_Unbounded_String ((1 => Value));
   end "=";

   -- Associate an attribute with a string value.
   function "="(Attr  : String;
                Value : Unbounded_String)
               return Attribute_Value_Pairs is
   begin
      return To_Unbounded_String (Attr) = Value;
   end "=";

   -- Associate an attribute with an integral value.
   function "="(Attr  : String;
                Value : Integer'Base)
               return Attribute_Value_Pairs is
      Value_Rep : constant String := Integer'Base'Image (Value);
      Is_Natural : constant Boolean := Value >= 0;
   begin
      if Is_Natural then
         return Attr = Value_Rep (2 .. Value_Rep'Last);
      else
         return Attr = Value_Rep;
      end if;
   end "=";

   -- Associate an attribute with a floating point value.
   function "="(Attr  : String;
                Value : Float'Base)
               return Attribute_Value_Pairs is
      Value_Rep : constant String := Float'Base'Image (Value);
      Is_Nonnegative : constant Boolean := Value >= 0.0;
   begin
      if Is_Nonnegative then
         return Attr = Value_Rep (2 .. Value_Rep'Last);
      else
         return Attr = Value_Rep;
      end if;
   end "=";

   -- Associate an attribute with a floating point value.
   function "="(Attr  : String;
                Value : Long_Float'Base)
               return Attribute_Value_Pairs is
      Value_Rep : constant String := Long_Float'Base'Image (Value);
      Is_Nonnegative : constant Boolean := Value >= 0.0;
   begin
      if Is_Nonnegative then
         return Attr = Value_Rep (2 .. Value_Rep'Last);
      else
         return Attr = Value_Rep;
      end if;
   end "=";

   -- Attribute provided as Unbounded_String

   -- Associate an attribute with a string value.
   function "="(Attr  : Unbounded_String;
                Value : String)
               return Attribute_Value_Pairs is
   begin
      return Attr = To_Unbounded_String (Value);
   end "=";

   -- Associate an attribute with a string value.
   function "="(Attr  : Unbounded_String;
                Value : Character)
               return Attribute_Value_Pairs is
   begin
      return Attr = To_Unbounded_String ((1 => Value));
   end "=";

   -- Associate an attribute with a string value.
   function "="(Attr  : Unbounded_String;
                Value : Unbounded_String)
               return Attribute_Value_Pairs is
   begin
      return (Attr, Value);
   end "=";

   -- Associate an attribute with an integral value.
   function "="(Attr  : Unbounded_String;
                Value : Integer'Base)
               return Attribute_Value_Pairs is
      Value_Rep : constant String := Integer'Base'Image (Value);
      Is_Natural : constant Boolean := Value >= 0;
   begin
      if Is_Natural then
         return Attr = To_Unbounded_String (Value_Rep (2 .. Value_Rep'Last));
      else
         return Attr = To_Unbounded_String (Value_Rep);
      end if;
   end "=";

   -- Associate an attribute with a floating point value.
   function "="(Attr  : Unbounded_String;
                Value : Float'Base)
               return Attribute_Value_Pairs is
      Value_Rep : constant String := Float'Base'Image (Value);
      Is_Nonnegative : constant Boolean := Value >= 0.0;
   begin
      if Is_Nonnegative then
         return Attr = To_Unbounded_String (Value_Rep (2 .. Value_Rep'Last));
      else
         return Attr = To_Unbounded_String (Value_Rep);
      end if;
   end "=";

   -- Associate an attribute with a floating point value.
   function "="(Attr  : Unbounded_String;
                Value : Long_Float'Base)
               return Attribute_Value_Pairs is
      Value_Rep : constant String := Long_Float'Base'Image (Value);
      Is_Nonnegative : constant Boolean := Value >= 0.0;
   begin
      if Is_Nonnegative then
         return Attr = To_Unbounded_String (Value_Rep (2 .. Value_Rep'Last));
      else
         return Attr = To_Unbounded_String (Value_Rep);
      end if;
   end "=";

end McKae.XML.EZ_Out.Generic_Medium;
