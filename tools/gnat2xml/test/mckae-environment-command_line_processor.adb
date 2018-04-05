------------------------------------------------------------------------
--                                                                    --
--                     McKae Software Utilities                       --
--                                                                    --
--           Copyright (C) 2003 McKae Technologies                    --
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
with Ada.Command_Line;
with Ada.Characters.Handling;
with Ada.Strings;
with Ada.Strings.Maps;
with Ada.Strings.Unbounded;
with Text_Io;

package body Mckae.Environment.Command_Line_Processor is

   use Ada.Command_Line;
   use Ada.Strings.Unbounded;

   -----------------------------------------------------------------------
   --
   -- Name:     Command_Line_Processor
   -- Author:   Marc A. Criley
   -- Date:     06 May 1996
   --
   -- Abstract: Associate command line options to members of an application-
   -- defined scalar type, along with providing simple access to arguments.
   -- Supports case (in)sensitivity, option arguments, and configurable option
   -- designator(s), usage description, and error handling.
   --
   -- Contact:  mcriley@acm.org
   --
   -- This software may be freely distributed.
   -----------------------------------------------------------------------

   package Cli renames Ada.Command_Line;
   package Char_Utils renames Ada.Characters.Handling;
   package Unbounded_Strings renames Ada.Strings.Unbounded;

   -----------------------------------------------------------------------

   Multi_Character_Option : Boolean := False;
   -- Set if an option consists of multiple characters.

   Single_Character_Option : Boolean := False;
   -- Set if there are single character options.  While perhaps unlikely
   -- that there would not be some, if all options are strings, knowing
   -- this will improve the handling of unrecognized options.

   Option_Designators : constant Ada.Strings.Maps.Character_Set :=
      Ada.Strings.Maps.To_Set (Option_Designation);
   -- Converted option designation characters; better suited to doing
   -- comparisons.

   Designator : Character;
   -- This is the designator for the block of options.  It will only take
   -- values supplied in the Option_Designation parameter.

   -----------------------------------------------------------------------

   function Is_In (Element : in Character;
                  Set     : in Ada.Strings.Maps.Character_Set) return Boolean
   renames Ada.Strings.Maps.Is_In;

   -----------------------------------------------------------------------

   function Set_Option (Item           : Command_Line_Components;
                        Identifier     : String;
                        Case_Sensitive : Boolean := True;
                        Argumented     : Boolean := False)
                                  return Item_Mappings is

      Item_Mapping : Item_Mappings;

   begin
      if Identifier'Length > 1 then
         Multi_Character_Option := True;
      end if;

      -- Initialize a local instance of the item mapping with settings
      -- corresponding to case-sensitive options.  This is not done during
      -- elaboration because a Length_Error exception could be raised if the
      -- option identifier is too long and such an exception must be handled
      -- locally.  If the option is not case-sensitive, convert the option
      -- identifier to upper case.
      Item_Mapping :=
            (Item           => Item,
             Identifier     => Option_Strings.To_Bounded_String (Identifier),
             Case_Sensitive => Case_Sensitive,
             Argumented     => Argumented);

      if not Case_Sensitive then
         Item_Mapping.Identifier := Option_Strings.To_Bounded_String
                                         (Char_Utils.To_Upper (Identifier));
      end if;

      return Item_Mapping;

   exception
      when Ada.Strings.Length_Error =>
         raise Option_Specification_Error;
   end Set_Option;

   -----------------------------------------------------------------------

   function Set_Option (Item           : Command_Line_Components;
                        Identifier     : Character;
                        Case_Sensitive : Boolean := True;
                        Argumented     : Boolean := False)
                                  return Item_Mappings is
   begin
      Single_Character_Option := True;
      return Set_Option (Item, (1 => Identifier),
                        Case_Sensitive, Argumented);
   end Set_Option;

   -----------------------------------------------------------------------

   function "=" (L, R : Item_Mappings) return Boolean is
      -- Check for the equivalence of two Item_Mappings, taking into
      -- account case sensitivity.

      use type Option_Strings.Bounded_String;

   begin
      if L.Case_Sensitive and R.Case_Sensitive then
         return L.Identifier = R.Identifier;
      else
         return
            Char_Utils.To_Upper (Option_Strings.To_String (L.Identifier)) =
            Char_Utils.To_Upper (Option_Strings.To_String (R.Identifier));
      end if;
   end "=";

   -----------------------------------------------------------------------

   Command_Line_Mapping : Command_Line_Item_Mappings (1 .. Max_Options);
   Valid_Options        : Natural := 0;

   -----------------------------------------------------------------------

   procedure Install_Option_Mappings
                (Mappings : in     Command_Line_Item_Mappings) is
   begin
      if Mappings'Last > Max_Options then
         raise Option_Specification_Error;
      else
         -- Ensure all option strings are unique, taking into account case
         -- insensitivity.
       Verify_Mappings :
         for Mapped_Item in Mappings'Range loop
            for M in 1 .. Mapped_Item - 1 loop
               if Mappings (Mapped_Item) = Mappings (M) then
                  raise Option_Specification_Error;
               end if;
            end loop;
         end loop Verify_Mappings;
      end if;

      Valid_Options := Mappings'Length;
      Command_Line_Mapping (1 .. Valid_Options) := Mappings;
   end Install_Option_Mappings;

   -----------------------------------------------------------------------

   Usage : Usage_Procedure_Profile;

   procedure Set_Usage (Usage_Procedure : Usage_Procedure_Profile) is
   begin
      Usage := Usage_Procedure;
   end Set_Usage;

   -----------------------------------------------------------------------

   Usage_Directions : Unbounded_Strings.Unbounded_String;

   procedure Set_Usage (Usage_String : String := "USAGE:  No information.") is
   begin
      Usage_Directions := Unbounded_Strings.To_Unbounded_String (Usage_String);
      Usage := null;
   end Set_Usage;

   -----------------------------------------------------------------------

   type Option_Error_Data is record
      Error_Message : Unbounded_Strings.Unbounded_String;
      Display_Usage : Boolean := True;
   end record;

   Invalid_Option : Option_Error_Data;

   -----------------------------------------------------------------------

   procedure Set_Invalid_Options_Message (Error_Message : String :=
                                            "Unrecognized option(s)";
                                         Display_Usage : Boolean := True) is
   begin
      Invalid_Option :=
        (Error_message => Unbounded_Strings.To_Unbounded_String (Error_Message),
         Display_Usage => Display_Usage);
   end Set_Invalid_Options_Message;

   -----------------------------------------------------------------------

   Ignore_Option_Errors : Boolean;

   procedure Ignore_Unrecognized_Options (Ignore : Boolean := True) is
   begin
      Ignore_Option_Errors := Ignore;
   end Ignore_Unrecognized_Options;

   -----------------------------------------------------------------------

   Omitted_Argument : Option_Error_Data;

   procedure Set_Omitted_Argument_Message (Error_Message : String :=
                                            "Missing argument";
                                          Display_Usage : Boolean := True) is
   begin
      Omitted_Argument :=
        (Error_message => Unbounded_Strings.To_Unbounded_String (Error_Message),
         Display_Usage => Display_Usage);
   end Set_Omitted_Argument_Message;

   -----------------------------------------------------------------------

   type CLI_States is record
      Item_Block : Natural := 1;
      -- The block of space-delimited options being processed
      Option_Item  : Natural := 1;
      -- The option within an item block
   end record;

   CLI_State : CLI_States;

   -----------------------------------------------------------------------

   procedure Show_Usage is
   begin
      if Usage /= null then
         Usage.all;
      else
         Text_IO.Put_Line (Unbounded_Strings.To_String (Usage_Directions));
      end if;
   end Show_Usage;

   -----------------------------------------------------------------------

   function Help_Requested (Item : String) return Boolean is

      -----------------------------------------------------------
      -- Determine whether help is being requested in some form:
      --   An option:
      --      -h, -help, -HELP, or -?
      --   An argument:
      --      help, HELP, or ?
      -----------------------------------------------------------

      CLI_Item : constant String := Char_Utils.To_Upper (Item);

   begin
      if (Item'Length > 1) and Is_In (Item (Item'First), Option_Designators) then
         return (CLI_Item (2 .. CLI_Item'Length) = "HELP") or
                ((CLI_Item'Length = 2) and
                    ((CLI_Item (2) = '?') or (CLI_Item (2) = 'H')));
      else
         return (CLI_Item = "HELP") or
                ((CLI_Item'Length = 1) and (CLI_Item (1) = '?'));
      end if;
   end Help_Requested;

   -----------------------------------------------------------------------

   procedure Locate_Option (Item         : in     String;
                           Found        :    out Boolean;
                           Valid_Option :    out Command_Line_Components;
                           Argumented   :    out Boolean) is
      use type Option_Strings.Bounded_String;

      Option_String : String (Item'Range);

   begin
      Found := False;
      Argumented := False;
      for I in 1 .. Valid_Options loop
         Option_String := Item;
         if not Command_Line_Mapping (I).Case_Sensitive then
            Option_String := Char_Utils.To_Upper (Option_String);
         end if;

         if Command_Line_Mapping (I).Identifier = Option_String then
            Valid_Option := Command_Line_Mapping (I).Item;
            Argumented := Command_Line_Mapping (I).Argumented;
            Found := True;
            exit;
         end if;
      end loop;
   end Locate_Option;

   -----------------------------------------------------------------------

   procedure Locate_Option (Item         : in     Character;
                           Found        :    out Boolean;
                           Valid_Option :    out Command_Line_Components;
                           Argumented   :    out Boolean) is
   begin
      Locate_Option ((1 => Item), Found, Valid_Option, Argumented);
   end Locate_Option;

   -----------------------------------------------------------------------

   procedure Extract_Argument
                     (Item            : in     String;
                      Option_Argument :    out Argument_Strings.Bounded_String;
                      Found           :    out Boolean) is
   begin
      Found := True;
      -- Offset Option_Item for the option designator and the option char
      if (CLI_State.Option_Item + 2) <= Item'Length then
         Option_Argument := Argument_Strings.To_Bounded_String
            (Item (CLI_State.Option_Item + 2 .. Item'Last));
         -- Force skipping of the rest of the option block
         CLI_State.Option_Item := Item'Length - 1;

      elsif (CLI_State.Item_Block + 1) <= CLI.Argument_Count then
         -- Use the succeeding block as the argument
         CLI_State.Item_Block := CLI_State.Item_Block + 1;
         Option_Argument := Argument_Strings.To_Bounded_String
                                (CLI.Argument (CLI_State.Item_Block));
      else
         Found := False;
      end if;
   end Extract_Argument;

   -----------------------------------------------------------------------

   procedure Handle_Error (Error_Data   : in     Option_Error_Data;
                           Item         : in     String;
                           Error_Result :    out Command_Line_Items) is
   begin
      if Ignore_Option_Errors then
         Error_Result :=
           (Item_Kind      => CLI_Option_Error,
            Unknown_Option => Option_Strings.To_Bounded_String
                                 (Item (Item'First + 1 .. Item'Last)),
            Unknown_Option_Designator => Designator);
         -- Continue processing with the next block
         CLI_State.Item_Block := CLI_State.Item_Block + 1;
         CLI_State.Option_Item := 1;

      else
         Text_IO.Put_Line (Unbounded_Strings.To_String
                          (Error_Data.Error_Message));
         if Error_Data.Display_Usage then
            Show_Usage;
         end if;

         -- After an option error, prevent any further processing of
         -- options or arguments.
         CLI_State.Item_Block := CLI.Argument_Count + 1;

         if Single_Character_Option then
            Error_Result :=
              (Item_Kind      => CLI_Option_Error,
               Unknown_Option => Option_Strings.To_Bounded_String
                                    ((1 => Item (CLI_State.Option_Item + 1))),
               Unknown_Option_Designator => Designator);
         else
            Error_Result :=
              (Item_Kind      => CLI_Option_Error,
               Unknown_Option => Option_Strings.To_Bounded_String
                                    (Item (Item'First + 1 .. Item'Last)),
               Unknown_Option_Designator => Designator);
         end if;
      end if;
   end Handle_Error;

   -----------------------------------------------------------------------

   function Next_Item return Command_Line_Items is
   -- Returns the next item supplied on the command line.  The Item_Kind
   -- is 'CLI_Complete' if there are no more.
      CLI_Item     : Command_Line_Items;
      Error_Result : Command_Line_Items;
   begin
      if CLI_State.Item_Block > CLI.Argument_Count then
         return (Item_Kind => CLI_Complete);
      end if;

      declare
         Item            : constant String :=
                                       CLI.Argument (CLI_State.Item_Block);
         Valid_Option    : Command_Line_Components;
         Found           : Boolean := False;
         Argumented      : Boolean := False;
         Option_Argument : Argument_Strings.Bounded_String;
      begin
         -- Determine whether this item is an option.  If it is an option
         -- designator alone, then treat it as an argument.
         if Is_In (Item (1), Option_Designators) and (Item'Length > 1) then
            Designator := Item (1);
            if CLI_State.Option_Item = 1 then
               -- Check, starting from the first character after the option
               -- designator to see if this is a request for a help or the
               -- start of a multi-character option.
               if Help_Requested (Item) then
                  Show_Usage;
                  CLI_State.Item_Block := CLI.Argument_Count + 1;
                  return (Item_Kind => CLI_Help_Requested);

               elsif Multi_Character_Option then
                  Locate_Option (Item (2 .. Item'Length), Found,
                                Valid_Option, Argumented);
                  if Found then
                     CLI_State.Option_Item := Item'Length - 1;
                  end if;
               end if;
            end if;

            if not Found then
               -- All options are single character or there is no match with
               -- any of the multi-character (string) options.  Locate the
               -- option indicated by the character,
               Locate_Option (Item (CLI_State.Option_Item + 1),
                             Found, Valid_Option, Argumented);
            end if;

            if Found then
               if Argumented then
                  Extract_Argument (Item, Option_Argument, Found);
                  if Found then
                     CLI_Item := (Item_Kind         => CLI_Option,
                                  Option            => Valid_Option,
                                  Option_Argument   => Option_Argument,
                                  Option_Designator => Designator);
                  else
                     Handle_Error (Omitted_Argument, Item, Error_Result);
                     return Error_Result;
                  end if;
               else
                  CLI_Item := (Item_Kind         => CLI_Option,
                               Option            => Valid_Option,
                               Option_Argument   =>
                                    Argument_Strings.Null_Bounded_String,
                               Option_Designator => Designator);
               end if;

               -- Advance the pointer within the option block.  If all
               -- options within the block have been processed, advance
               -- to the next option block.
               CLI_State.Option_Item := CLI_State.Option_Item + 1;
               if CLI_State.Option_Item = Item'Length then
                  CLI_State.Item_Block := CLI_State.Item_Block + 1;
                  CLI_State.Option_Item := 1;
               end if;

               return CLI_Item;

            else
               Handle_Error (Invalid_Option, Item, Error_Result);
               return Error_Result;
            end if;

         else
            if (CLI_State.Item_Block = 1) and Help_Requested (Item) then
               Show_Usage;
               CLI_State.Item_Block := CLI.Argument_Count + 1;
               return (Item_Kind => CLI_Help_Requested);
            end if;

            CLI_State.Item_Block := CLI_State.Item_Block + 1;
            return (Item_Kind => CLI_Argument,
                    Argument  => Argument_Strings.To_Bounded_String (Item));

         end if;
      end;

   exception
      when Ada.Strings.Length_Error =>
         raise Option_Specification_Error;
   end Next_Item;

   -----------------------------------------------------------------------

   function Command_Name return String
   renames Ada.Command_Line.Command_Name;
   -- Returns a value corresponding to the name of the program.

   -----------------------------------------------------------------------

   function Command_Line return String
   is
      Temp : Unbounded_String;
   begin
      for A in 1 .. Argument_Count loop
         Append (Temp, Argument (A));
         if A /= Argument_Count then
            Append (Temp, " ");
         end if;
      end loop;

      return To_String (Temp);
   end Command_Line;

   -----------------------------------------------------------------------

begin
   Set_Usage;
   Set_Invalid_Options_Message;
   Ignore_Unrecognized_Options (Ignore => False);
   Set_Omitted_Argument_Message;
end Mckae.Environment.Command_Line_Processor;
