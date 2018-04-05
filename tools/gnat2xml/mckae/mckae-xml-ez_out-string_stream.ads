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

with McKae.XML.EZ_Out.Generic_Medium;
with Ada.Text_IO; use Ada.Text_IO;

package McKae.XML.EZ_Out.String_Stream is

   --  A basic in-memory string-based XML document construction
   --  utility.  This is not intended to be a robust, full-function
   --  memory buffering package.

   ---------------------------------------------------------------------------

   --  This nested package provides a basic string-based buffering
   --  capability.  The purpose of the EZ_Out String_Stream package is
   --  to provide a simple means of constructing an XML document in a
   --  memory buffer.  To do this, a memory buffering capability was
   --  needed--there were three approaches:
   --
   --  o Locate and employ an existing memory buffering component,
   --  modifying it as needed to provide the required Put and New_Line
   --  functions
   --  o Write a McKae component to do memory buffering and reference
   --  it in this package.
   --  o Embed a memory buffering capability within the String_Stream
   --  package.
   --
   --  The first option was discarded not because there's anything
   --  wrong with existing memory buffer components, but rather
   --  because of questions about which one should be chosen, what are
   --  its distribution and modification requirements, and so on.  The
   --  truth of the matter is that this approach is what a project
   --  using EZ_Out for a project actually ought to do--take their
   --  memory buffering capability, enhance it with Put and New_Line
   --  functionality, and then instantiate EZ_Out.Generic_Medium with
   --  it.
   --
   --  The second option was discarded because the focus of EZ_Out is
   --  on XML document generation, not providing a general purpose
   --  memory buffering component.  While a limited capability one
   --  could have been created, it would be over-specific to EZ_Out
   --  and its (intentional) limitations would distract from its
   --  intended use as an EZ_Out adjunct package.
   --
   --  This left the third option.  By embedding the above-mentioned
   --  limited capability memory buffering capability within
   --  String_Stream, it's clearly associated as just an aspect of the
   --  String_Stream implementation, as any relevant capabilities
   --  are simply exported by the String_Stream package.

   package String_Buffering is

      --  There's little point in having a small buffer for building XML
      --  documents, so the minimum size and expansion is 500 characters.

      subtype Buffer_Size_Range is Positive range 500 .. Positive'Last;

      type String_Buffer
        (Initial_Size : Buffer_Size_Range := 5000;
         Expansion    : Buffer_Size_Range := 5000) is limited private;

      --  Copy the given string into the buffer, expanding it if needed.
      procedure Put (F : String_Buffer; S : String);

      --  Insert a new line indicator into the buffer.  However, in
      --  this case do nothing, since this buffering package is being
      --  instantiated for Continuous_Stream formatting.
      procedure New_Line
        (F : String_Buffer;
         Spacing : Ada.Text_IO.Positive_Count := 1);

      --  Clear the buffer. Note that this does not free any allocated
      --  resources.
      procedure Clear (S : String_Buffer);

      --  Return the current contents of the string buffer
      function Get_String (S : String_Buffer) return String;

   private

      --  Handle to the buffer
      type Buffer_Ptr is access all String;

      --  String buffer self-access ("Rosen Trick");

      type String_Self_Access
        (SB : access String_Buffer) is limited null record;

      --  String buffer type definition.  By default, a newly created
      --  string buffer is initialized to be empty.

      type String_Buffer
        (Initial_Size : Buffer_Size_Range := 5000;
         Expansion    : Buffer_Size_Range := 5000) is limited record
         Allocation   : Buffer_Size_Range := Initial_Size;
         Size         : Natural           := 0;
         Buff         : Buffer_Ptr        := new String (1 .. Initial_Size);
         Self : String_Self_Access (String_Buffer'Access);
      end record;
   end String_Buffering;

   ---------------------------------------------------------------------------

   subtype Buffer_Size_Range is String_Buffering.Buffer_Size_Range;

   subtype String_Buffer is String_Buffering.String_Buffer;

   --  Clear the buffer. Note that this does not free any allocated
   --  resources.
   procedure Clear (S : String_Buffer) renames String_Buffering.Clear;

   --  Return the current contents of the string buffer
   function Get_String
     (S : String_Buffer) return String renames
     String_Buffering.Get_String;

   ---------------------------------------------------------------------------

   --  "Use" this XML_String_Buffer package for constructing an EZ_Out
   --  XML document.
   package XML_String_Buffer is new McKae.XML.EZ_Out.Generic_Medium
     (Output_Medium => String_Buffering.String_Buffer,
      Put           => String_Buffering.Put,
      New_Line      => String_Buffering.New_Line,
      Format        => Continuous_Stream);

   ---------------------------------------------------------------------------

end McKae.XML.EZ_Out.String_Stream;
