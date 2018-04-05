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
with Ada.Strings.Bounded;

generic

   type Command_Line_Components is (<>);
   --  The discrete type to which options correspond.

   Max_Options : Natural := 100;
   --  This should be altered only if an excessive number of options
   --  are required.

   Max_Option_Length : Positive := 20;
   --  If options may take the form of strings, this is the maximum
   --  length of such a string.

   Max_Argument_Length : Positive := 255;
   --  The maximum length of an argument.

   Option_Designation : String := "-";
   --  Character(s) designating an option string.  Add to or modify as needed.
   --  Note: "+-" are commonly used option designators to signify options that
   --  can be turned on or off.

package McKae.Environment.Command_Line_Processor is

   -----------------------------------------------------------------------
   --
   --  Name:     Command_Line_Processor
   --  Author:   Marc A. Criley
   --  Date:     06 May 1996
   --
   --  Abstract: Associate command line options to members of an application-
   --  defined scalar type, along with providing simple access to arguments.
   --  Supports case (in)sensitivity, option arguments, and configurable option
   --  designator(s), usage description, and error handling.
   --
   --  Contact:  mcriley@acm.org
   --
   --  This software may be freely distributed.
   -----------------------------------------------------------------------
   --
   --  Usage (see simple_cli.adb for a working example):
   --
   --  1) Define a scalar type with which command line options will be
   --  associated.
   --
   --  2) Instantiate this package, supplying that type and specifying any
   --  other applicable settings accessible in the generic parameters.
   --
   --  3a) Declare a variable or constant of type Command_Line_Item_Mappings
   --  with the initializing aggregate that will implicitly supply the index
   --  range for the array, or,
   --
   --  3b) Declare a variable of type Command_Line_Item_Mappings, specifying an
   --  index range corresponding to the number of options that will be
   --  recognized and, optionally, an initializing aggregate.
   --
   --  4) Supply values for the initializing aggregate (or if an initializing
   --  aggregate was not used, initialize the variable in the body of the code)
   --  by calling the Set_Option function for each of the options to be
   --  recognized. Specify the member of the "option type" to be associated
   --  with a command line option, the option character or string, whether
   --  recognizing that option is case sensitive, and whether the option
   --  requires an argument. Note that more than one option representation can
   --  be associated with the same member of the option type.
   --
   --  5) Install the command line item mapping by calling
   --  Install_Option_Mappings with the variable or constant.
   --
   --  6) Set any special USAGE or error handling by calling the appropriate
   --  procedures.
   --
   --  7) In the body of the application where the command line options and
   --  arguments are processed, set up a loop in which Next_Item is called in
   --  each iteration.
   --
   --  8) Process the response from each call to Next_Item. Next_Item returns
   --  an object of type Command_Line_Items, the discriminant of which,
   --  Item_Kind, indicates whether an option or argument is being returned, if
   --  help was requested, if an error occurred, or if all command line items
   --  have been processed. See the definition of Command_Line_Items regarding
   --  the format of the data it returns.
   --
   -----------------------------------------------------------------------
   --
   --  These types and subprograms implement the mapping between option
   --  identifiers and the corresponding type members.
   --

   type Item_Mappings is private;

   type Command_Line_Item_Mappings is
     array (Natural range <>) of Item_Mappings;

   --  Associate a member item of the type defining the command line options
   --  with the character indicating that option.  Set Case_Sensitive as
   --  appropriate and Argumentable if the option requires an argument.
   function Set_Option
     (Item       : Command_Line_Components;
      Identifier : Character;
      Case_Sensitive : Boolean := True;
      Argumented : Boolean := False)
     return Item_Mappings;

   --  As above, but with an identifier string being associated with the
   --  option.
   function Set_Option
     (Item       : Command_Line_Components;
      Identifier : String;
      Case_Sensitive : Boolean := True;
      Argumented : Boolean := False)
     return Item_Mappings;

   --  Install the mapping.  Successive calls to this procedure replace
   --  previous mappings.  If there is a problem during installation, the
   --  Option_Specification_Error exception is raised.
   procedure Install_Option_Mappings (Mappings : Command_Line_Item_Mappings);

   Option_Specification_Error : exception;
   --  Raised if there is a problem setting up an option configuration or
   --  installing it.  This would be raised by mapping the same option
   --  identifier to two different option type members, or if one letter case
   --  of an option was specified as case-sensitive and the other as
   --  case-insensitive, if the length of an option string exceeded the
   --  Max_Option_Length, or if more than Max_Options were defined.

   -----------------------------------------------------------------------
   --
   --  The following procedures adjust the command line processor's interaction
   --  with users.
   --
   -----------------------------------------------------------------------

   -----------------------------------------------------------------------
   --
   --  The Set_Usage procedures permit supplying either a string specifying the
   --  program's proper usage profile (which is the default action), or a
   --  parameterless procedure for more extensive interaction.  The string is
   --  displayed, or the usage procedure invoked, upon encountering the option
   --  "-h", "-help", or "-?"; or the single argument "help" or "?".
   --
   -----------------------------------------------------------------------

   procedure Set_Usage (Usage_String : String := "USAGE:  No information.");

   type Usage_Procedure_Profile is access procedure;

   procedure Set_Usage (Usage_Procedure : Usage_Procedure_Profile);

   -----------------------------------------------------------------------
   --
   --  Set_Invalid_Options_Message and Set_Omitted_Argument_Message permit the
   --  specification of a message appropriate to encountering invalid options
   --  or finding that no argument was supplied for an option that requires
   --  one.  To specify that no message at all is to be supplied for errors,
   --  invoke this procedure with a null string as the Error_Message.
   --
   --  Normally, processing of the command line ends after encountering an
   --  error.  To continue, as in the case when unrecognized options are to be
   --  passed on to another program, call Ignore_Unrecognized_Options.  When an
   --  unrecognized option is encountered, no further processing within that
   --  block of options will occur, processing will resume with the subsequent
   --  block.  Upon encountering an unrecognized option, Next_Item (see below)
   --  will return CLI_Error and the Unknown_Option field will contain that
   --  entire option block, even if some of the initial characters did
   --  correspond to recognized options.  PROCEED WITH CAUTION AFTER TURNING
   --  OFF OPTION ERROR RECOGNITION.
   --
   -----------------------------------------------------------------------

   procedure Set_Invalid_Options_Message
     (Error_Message : String := "Unrecognized option(s)";
      Display_Usage : Boolean := True);

   procedure Ignore_Unrecognized_Options (Ignore : Boolean := True);

   procedure Set_Omitted_Argument_Message
     (Error_Message : String := "Missing argument";
      Display_Usage : Boolean := True);

   -----------------------------------------------------------------------
   --
   --  The following type declarations define the format in which the options
   --  and arguments are returned to the calling routine.
   --
   --  Arguments, supplied as such or as an argument associated with an option,
   --  are stored as a Bounded_Strings whose maximum length may be adjusted
   --  during instantiation.
   --
   --  The Command_Line_Item_Kinds indicates what is being returned to the
   --  caller:
   --
   --    'CLI_Option' is accompanied by the value of the
   --       Command_Line_Components type corresponding to that option.  If the
   --       option has been designated as one which itself has an argument,
   --       that argument is returned as well.
   --
   --    'CLI_Argument' returns a bounded string containing the argument.
   --
   --    'CLI_Help_Requested' indicates that usage help was requested, so it is
   --       likely that the application should simply exit.
   --
   --    'CLI_Complete' indicates there are no further options or arguments,
   --       including indicating that none were supplied at all.
   --
   --    'CLI_Option_Error' is returned when a supplied option is unrecognized.
   --       The unrecognized option is returned as well.
   --       Immediately prior to returning this result, the displaying of
   --       the invalid options message is triggered.
   --
   -----------------------------------------------------------------------

   package Argument_Strings is new Ada.Strings.Bounded.Generic_Bounded_Length
     (Max => Max_Argument_Length);

   package Option_Strings is new Ada.Strings.Bounded.Generic_Bounded_Length
     (Max => Max_Option_Length);

   type Command_Line_Item_Kinds is
     (CLI_Option,
     CLI_Argument,
     CLI_Help_Requested,
     CLI_Complete,
     CLI_Option_Error);

   type Command_Line_Items
     (Item_Kind : Command_Line_Item_Kinds := CLI_Complete) is record
      case Item_Kind is
         when CLI_Option =>
            Option            : Command_Line_Components;
            Option_Argument   : Argument_Strings.Bounded_String;
            Option_Designator : Character;
         --  The Option_Designator is the character preceding the
         --  option block.  As such, it will take on only those
         --  values supplied by the Option_Designation parameter.

         when CLI_Argument =>
            Argument : Argument_Strings.Bounded_String;

         when CLI_Help_Requested | CLI_Complete =>
            null;

         when CLI_Option_Error =>
            Unknown_Option            : Option_Strings.Bounded_String;
            Unknown_Option_Designator : Character;

      end case;
   end record;

   -----------------------------------------------------------------------
   --
   --  These functions return the components of the command line invocation.
   --
   -----------------------------------------------------------------------

   --  Returns a value corresponding to the name of the program.
   function Command_Name return String;

   --  Returns the next option or argument supplied on the command line.
   --  Options and arguments may be specified in any order on the command line,
   --  although whatever follows an option that requires an argument is taken
   --  to be the argument.
   function Next_Item return Command_Line_Items;

   --  Return the entire command line as submitted to the application
   function Command_Line return String;

   -----------------------------------------------------------------------

private
   type Item_Mappings is record
      Item       : Command_Line_Components;
      Identifier : Option_Strings.Bounded_String;
      Case_Sensitive : Boolean := True;
      Argumented     : Boolean := False;
   end record;

   function "=" (L, R : Item_Mappings) return Boolean;

end McKae.Environment.Command_Line_Processor;
