------------------------------------------------------------------------------
--                                                                          --
--                          GNATCHECK COMPONENTS                            --
--                                                                          --
--                 G N A T C H E C K . R U L E S . T E X T                  --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                     Copyright (C) 2010-2017, AdaCore                     --
--                                                                          --
-- GNATCHECK  is  free  software;  you can redistribute it and/or modify it --
-- under terms of the  GNU  General Public License as published by the Free --
-- Software Foundation;  either version 3, or ( at your option)  any  later --
-- version.  GNATCHECK  is  distributed in the hope that it will be useful, --
-- but  WITHOUT  ANY  WARRANTY;   without  even  the  implied  warranty  of --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General --
-- Public License for more details.  You should have received a copy of the --
-- GNU General Public License distributed with GNAT; see file  COPYING3. If --
-- not,  go  to  http://www.gnu.org/licenses  for  a  complete  copy of the --
-- license.                                                                 --
--                                                                          --
-- GNATCHECK is maintained by AdaCore (http://www.adacore.com).             --
--                                                                          --
------------------------------------------------------------------------------

pragma Ada_2012;

with Ada.Characters.Conversions; use Ada.Characters.Conversions;
with Ada.Strings.Fixed;          use Ada.Strings.Fixed;

with ASIS_UL.Misc;               use ASIS_UL.Misc;
with ASIS_UL.Output;             use ASIS_UL.Output;

package body Gnatcheck.Rules.Text is

   ------------------------
   -- Annotated_Comments --
   ------------------------

   --  Data structures needed for rule implementation.

   package Annotations is new Simple_String_Dictionary ("comment annotations");
   --  Keeps definitions of comment annotations to be flagged by the rule.

   ------------------------------------------------
   -- Activate_In_Test_Mode (Annotated_Comments) --
   ------------------------------------------------

   overriding procedure Activate_In_Test_Mode
     (Rule : in out Annotated_Comments_Rule_Type)
   is
   begin
      Process_Rule_Parameter
        (Rule       => Rule,
         Param      => "#hide",
         Enable     => True,
         Defined_At => "");

      Process_Rule_Parameter
        (Rule       => Rule,
         Param      => "#accept",
         Enable     => True,
         Defined_At => "");

      Process_Rule_Parameter
        (Rule       => Rule,
         Param      => "%foo",
         Enable     => True,
         Defined_At => "");

   end Activate_In_Test_Mode;

   ------------------------------------
   -- Init_Rule (Annotated_Comments) --
   ------------------------------------

   procedure Init_Rule
     (Rule : in out Annotated_Comments_Rule_Type)
   is
   begin
      Init_Rule (Rule_Template (Rule));

      Rule.Name        := new String'("Annotated_Comments");
      Rule.Rule_Status := Fully_Implemented;
      Rule.Help_Info   := new String'("use of comment annotations");
      Rule.Diagnosis   := new String'("annotated comment: %1%");
   end Init_Rule;

   -------------------------------------
   -- Line_Check (Annotated_Comments) --
   -------------------------------------

   procedure Line_Check
     (Rule               : in out Annotated_Comments_Rule_Type;
      Line_Num           :        Line_Number_Positive;
      Full_Line_Image    :        Program_Text_Access;
      Ada_Line_Image     :        Program_Text_Access;
      Comment_Line_Image :        Program_Text_Access;
      State              : in out Rule_Traversal_State)
   is
      pragma Unreferenced (Rule, Full_Line_Image, Ada_Line_Image);

   begin

      if Comment_Line_Image.all /= "" then
         declare
            String_Commment_Image : constant String :=
              To_String (Comment_Line_Image.all);

            Comment_Start : constant Positive :=
              Index (String_Commment_Image, "--");

            Last : constant Positive := String_Commment_Image'Last;

            Word_Start, Word_End : Natural := 0;
         begin
            if Comment_Start + 2 <= Last
              and then
               not Is_White_Space (String_Commment_Image (Comment_Start + 2))
            then
               for J in Comment_Start + 3 .. Last loop
                  if not Is_White_Space (String_Commment_Image (J)) then
                     Word_Start := J;
                     exit;
                  end if;
               end loop;

               if Word_Start > 0 then
                  for J in Word_Start .. Last - 1 loop
                     if Is_White_Space (String_Commment_Image (J + 1)) then
                        Word_End := J;
                        exit;
                     end if;
                  end loop;
               end if;

               if Word_Start > 0 and then Word_End = 0 then
                  Word_End := Last;
               elsif Word_Start = 0 and then Word_End = 0 then
                  --  here we need a null range
                  Word_Start := 1;
               end if;

               if Annotations.Is_In_Dictionary
                    (String_Commment_Image (Comment_Start + 2) &
                     String_Commment_Image (Word_Start .. Word_End))
               then
                  State.Detected    := True;
                  State.Line        := Positive (Line_Num);
                  State.Column      := Comment_Start;
                  State.Diag_Params := Enter_String
                    ("%1%--"                                   &
                     String_Commment_Image (Comment_Start + 2) &
                     ' '                                       &
                     String_Commment_Image (Word_Start .. Word_End));
               end if;

            end if;

         end;
      end if;

   end Line_Check;

   --------------------------------------------
   -- More_Rule_Comment (Annotated_Comments) --
   --------------------------------------------

   function More_Rule_Comment
     (Rule          : Annotated_Comments_Rule_Type;
      Template_Kind : Template_Coding_Standard_Kinds)
      return String
   is
      pragma Unreferenced (Rule);
   begin
      if Template_Kind = Template_All_ON then
         return "possibly meaningless default parameter used!";
      else
         return "provide a proper comment marker as a parameter value " &
                "if the rule is enabled!";
      end if;
   end More_Rule_Comment;

   ------------------------
   -- Print_Rule_To_File --
   ------------------------

   overriding procedure Print_Rule_To_File
     (Rule         : Annotated_Comments_Rule_Type;
      Rule_File    : File_Type;
      Indent_Level : Natural := 0)
   is
      First_Par : Boolean := True;
      Rule_Name_Padding : constant String :=
        (1 .. Rule.Name'Length + 4 => ' ');
   begin
      Print_Rule_To_File (Rule_Template (Rule), Rule_File, Indent_Level);
      Annotations.Reset_Iterator;

      while not Annotations.Done loop
         if First_Par then
            Put (Rule_File, ": " & Annotations.Next_Entry);
            First_Par := False;
         else
            Put_Line (Rule_File, ",");

            for J in 1 .. Indent_Level loop
               Put (Rule_File, Get_Indent_String);
            end loop;

            Put (Rule_File,
                 Rule_Name_Padding & Annotations.Next_Entry);
         end if;
      end loop;
   end Print_Rule_To_File;

   -------------------------------------------------
   -- Process_Rule_Parameter (Annotated_Comments) --
   -------------------------------------------------

   procedure Process_Rule_Parameter
     (Rule       : in out Annotated_Comments_Rule_Type;
      Param      :        String;
      Enable     :        Boolean;
      Defined_At : String)
   is
      pragma Unreferenced (Defined_At);
   begin
      if Param = "" then

         if Enable then
            Error ("(" & Rule.Name.all & ") parameter is required for +R");
         else
            Annotations.Clear;
            Rule.Rule_State := Disabled;
         end if;

      else

         if Enable then
            --  Check if there is no white spaces in the parameter

            for J in Param'Range loop
               if Param (J) = ' ' or else Param (J) = ASCII.HT then
                  Error ("(" & Rule.Name.all & ") parameter cannot contain " &
                         "white spaces");
                  return;
               end if;
            end loop;

            Annotations.Add_To_Dictionary (Param);
            Rule.Rule_State := Enabled;

         else
            Error ("(" & Rule.Name.all & ") no parameter allowed for -R");
         end if;

      end if;
   end Process_Rule_Parameter;

   --------------------------------------
   -- Rule_Option (Annotated_Comments) --
   --------------------------------------

   function Rule_Option
     (Rule          : Annotated_Comments_Rule_Type;
      Template_Kind : Template_Coding_Standard_Kinds)
      return          String
   is
   begin
      if Template_Kind = Template_All_ON then
         return Rule_Option (Rule_Template (Rule), Template_Kind) & " : #";
      else
         return Rule_Option (Rule_Template (Rule), Template_Kind);
      end if;
   end Rule_Option;

   -----------------------------------------
   -- XML_Print_Rule (Annotated_Comments) --
   -----------------------------------------

   overriding procedure XML_Print_Rule
     (Rule         : Annotated_Comments_Rule_Type;
      Indent_Level : Natural := 0)
   is
   begin
      XML_Report
        ("<rule id=""" & Rule_Name (Rule) & """>",
         Indent_Level);

      Annotations.Reset_Iterator;

      while not Annotations.Done loop
         XML_Report
           ("<parameter>" & Annotations.Next_Entry & "</parameter>",
            Indent_Level + 1);
      end loop;

      XML_Report ("</rule>", Indent_Level);
   end XML_Print_Rule;

   ----------------------------------------
   -- XML_Rule_Help (Annotated_Comments) --
   ----------------------------------------

   procedure XML_Rule_Help
     (Rule  : Annotated_Comments_Rule_Type;
      Level : Natural)
   is
   begin
      Info (Level * Ident_String                                    &
            "<field switch=""+R"                                    &
            Rule.Name.all                                           &
            """ label="""                                           &
            "detect specified annotations (use ',' as separator)""" &
            " separator="":"""                                      &
            "/>");
   end XML_Rule_Help;

   ---------------------
   -- Printable_ASCII --
   ---------------------

   function Bad_Symbol_Description (Ch : Wide_Character) return String;
   --  Provides short description of a bad symbol for diagnoses

   ----------------------------
   -- Bad_Symbol_Description --
   ----------------------------

   function Bad_Symbol_Description (Ch : Wide_Character) return String is
      Ch_Code   : Natural;
      ASCII_Str : constant String := " (ASCII.";
   begin
      if not Is_Character (Ch) then
         return " (outside Character type range)";
      end if;

      Ch_Code := Character'Pos (To_Character (Ch));

      case Ch_Code is
         when 127 .. 255 =>
            return " (outside ASCII range)";

         when  0 => return ASCII_Str & "NUL)";
         when  1 => return ASCII_Str & "SOH)";
         when  2 => return ASCII_Str & "STX)";
         when  3 => return ASCII_Str & "ETX)";
         when  4 => return ASCII_Str & "EOT)";
         when  5 => return ASCII_Str & "ENQ)";
         when  6 => return ASCII_Str & "ACK)";
         when  7 => return ASCII_Str & "BEL)";
         when  8 => return ASCII_Str & "BS)";
         when  9 => return ASCII_Str & "HT)";
         when 11 => return ASCII_Str & "VT)";
         when 12 => return ASCII_Str & "FF)";
         when 14 => return ASCII_Str & "SO)";
         when 15 => return ASCII_Str & "SI)";
         when 16 => return ASCII_Str & "DLE)";
         when 17 => return ASCII_Str & "DC1)";
         when 18 => return ASCII_Str & "DC2)";
         when 19 => return ASCII_Str & "DC3)";
         when 20 => return ASCII_Str & "DC4)";
         when 21 => return ASCII_Str & "NAK)";
         when 22 => return ASCII_Str & "SYN)";
         when 23 => return ASCII_Str & "ETB)";
         when 24 => return ASCII_Str & "CAN)";
         when 25 => return ASCII_Str & "EM)";
         when 26 => return ASCII_Str & "SUB)";
         when 27 => return ASCII_Str & "ESC)";
         when 28 => return ASCII_Str & "FS)";
         when 29 => return ASCII_Str & "GS)";
         when 30 => return ASCII_Str & "RS)";
         when 31 => return ASCII_Str & "US)";

         when others =>
            pragma Assert (False);
            return " !!!(report problem with the rule!!!)";
      end case;
   end Bad_Symbol_Description;

   ---------------------------------
   -- Init_Rule (Printable_ASCII) --
   ---------------------------------

   overriding procedure Init_Rule (Rule : in out Printable_ASCII_Rule_Type) is
   begin
      Init_Rule (Rule_Template (Rule));

      Rule.Name        := new String'("Printable_ASCII");
      Rule.Rule_Status := Fully_Implemented;
      Rule.Help_Info   := new String'("non-printable characters");
      Rule.Diagnosis   := new String'("#1#symbol is not from printable ASCII" &
                                      "%1%"                                   &
                                      "#2#symbol is not from printable ASCII" &
                                      "%1% (more occurrences on this line)");
   end Init_Rule;

   ----------------------------------
   -- Line_Check (Printable_ASCII) --
   ----------------------------------

   overriding procedure Line_Check
     (Rule               : in out Printable_ASCII_Rule_Type;
      Line_Num           :        Line_Number_Positive;
      Full_Line_Image    :        Program_Text_Access;
      Ada_Line_Image     :        Program_Text_Access;
      Comment_Line_Image :        Program_Text_Access;
      State              : in out Rule_Traversal_State)
   is

      pragma Unreferenced (Rule, Comment_Line_Image, Ada_Line_Image);
      Ch_Code        : Natural;
      Bad_Symbol_N   : Natural := 0;
      First_Bad_Sym  : Boolean := True;
   begin
      for J in Full_Line_Image'Range loop
         if not Is_Character (Full_Line_Image (J)) then
            State.Detected := True;
            Bad_Symbol_N   := Bad_Symbol_N + 1;
         else
            Ch_Code        :=
              Character'Pos (To_Character (Full_Line_Image (J)));

            if Ch_Code not in 10 | 13 | 32 .. 126 then
               State.Detected := True;
               Bad_Symbol_N   := Bad_Symbol_N + 1;
            end if;
         end if;

         if State.Detected and then First_Bad_Sym then
            State.Line        := Positive (Line_Num);
            State.Column      := J;
            State.Diag_Params := Enter_String
              ("%1%" & Bad_Symbol_Description (Full_Line_Image (J)));
            First_Bad_Sym     := False;
         end if;

         exit when Bad_Symbol_N = 2;
      end loop;

      if State.Detected then
         State.Diagnosis := Bad_Symbol_N;
      end if;

   end Line_Check;

end Gnatcheck.Rules.Text;
