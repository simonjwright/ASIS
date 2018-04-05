------------------------------------------------------------------------------
--                                                                          --
--                          GNATCHECK COMPONENTS                            --
--                                                                          --
--              G N A T C H E C K . S O U R C E _ C H E C K S               --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                       Copyright (C) 2010, AdaCore                        --
--                                                                          --
-- GNATCHECK  is  free  software;  you can redistribute it and/or modify it --
-- under terms of the  GNU  General Public License as published by the Free --
-- Software Foundation;  either version 2, or ( at your option)  any  later --
-- version.  GNATCHECK  is  distributed in the hope that it will be useful, --
-- but  WITHOUT  ANY  WARRANTY;   without  even  the  implied  warranty  of --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General --
-- Public License for more details.  You should have received a copy of the --
-- GNU  General Public License distributed with GNAT; see file  COPYING. If --
-- not,  write to the  Free Software Foundation,  51 Franklin Street, Fifth --
-- Floor, Boston, MA 02110-1301, USA.                                       --
--                                                                          --
-- GNATCHECK is maintained by AdaCore (http://www.adacore.com).             --
--                                                                          --
------------------------------------------------------------------------------

with Asis.Extensions;            use Asis.Extensions;
with Asis.Text;                  use Asis.Text;

with Gnatcheck.Diagnoses;        use Gnatcheck.Diagnoses;
with Gnatcheck.Ids;              use Gnatcheck.Ids;
with Gnatcheck.Rules.Output;     use Gnatcheck.Rules.Output;
with Gnatcheck.Rules.Rule_Table; use Gnatcheck.Rules.Rule_Table;
with Gnatcheck.Rules.Traversing; use Gnatcheck.Rules.Traversing;

package body Gnatcheck.Source_Checks is

   First_Non_Analyzed_Line : Line_Number_Positive;
   Last_Unit_Line          : Line_Number_Positive;

   Full_Line_Image    : Program_Text_Access;
   Ada_Line_Image     : Program_Text_Access;
   Comment_Line_Image : Program_Text_Access;

   procedure Check_Lines
     (From    :      Line_Number_Positive;
      To      :      Line_Number_Positive;
      Element :      Asis.Element;
      State : in out Rule_Traversal_State);
   --  Checks all the enabled source code rules for the source lines of the
   --  currently checked unit in the From .. To range. Element is used as a
   --  means to get access to the source code representation of the analyzed
   --  unit

   -----------------
   -- Check_Lines --
   -----------------

   procedure Check_Lines
     (From    :      Line_Number_Positive;
      To      :      Line_Number_Positive;
      Element :      Asis.Element;
      State : in out Rule_Traversal_State)
   is
   begin
      if From <= To then

         declare
            Lines_To_Check : constant Line_List (From .. To) :=
              Lines (Element    => Element,
                     First_Line => From,
                     Last_Line  => To);
         begin

            for J in From .. To loop

               Full_Line_Image :=
                 new Program_Text'(Line_Image (Lines_To_Check (J)));
               Ada_Line_Image  :=
                 new Program_Text'(Non_Comment_Image (Lines_To_Check (J)));
               Comment_Line_Image :=
                 new Program_Text'(Comment_Image (Lines_To_Check (J)));

               for R in First_Rule .. All_Rules.Last loop
                  if Is_Enable (All_Rules.Table (R).all)
                       and then
                        All_Rules.Table (R).all in Text_Rule_Template'Class
                  then
                     Reset_State (State);

                     Line_Check
                       (Rule => Text_Rule_Template'Class
                                  (All_Rules.Table (R).all),
                        Line_Num           => J,
                        Full_Line_Image    => Full_Line_Image,
                        Ada_Line_Image     =>  Ada_Line_Image,
                        Comment_Line_Image => Comment_Line_Image,
                        State              => State);

                        if State.Detected then
                           Report_Detection
                             (For_Rule      => R,
                              On            => Element,
                              In_SF         => State.SF,
                              Justification => Exemption_Justification (R),
                              Diagnosis_Num => State.Diagnosis,
                              Diag_Actuals  => State.Diag_Params,
                              Diag_Line     => State.Line,
                              Diag_Column   => State.Column);
                        end if;
                  end if;
               end loop;

               Free (Full_Line_Image);
               Free (Ada_Line_Image);
               Free (Comment_Line_Image);

            end loop;
         end;
      end if;
   end Check_Lines;

   ----------------------
   -- Check_Text_Rules --
   ----------------------

   procedure Check_Text_Rules
     (Up_To :        Asis.Element;
      State : in out Rule_Traversal_State)
   is
      Last_Line_To_Check : constant Line_Number_Positive :=
        Element_Span (Up_To).First_Line;
   begin
      Check_Lines
        (From    => First_Non_Analyzed_Line,
         To      => Last_Line_To_Check,
         Element => Up_To,
         State   => State);

      First_Non_Analyzed_Line := Last_Line_To_Check + 1;
   end Check_Text_Rules;

   ------------------------------------------
   -- Check_Text_Rules_For_Remaining_Lines --
   ------------------------------------------

   procedure Check_Text_Rules_For_Remaining_Lines
     (Unit  :        Asis.Element;
      State : in out Rule_Traversal_State)
   is
   begin
      Check_Lines
        (From    => First_Non_Analyzed_Line,
         To      => Last_Unit_Line,
         Element => Unit,
         State   => State);
   end Check_Text_Rules_For_Remaining_Lines;

   -----------------------------
   -- Init_Source_Text_Checks --
   -----------------------------

   procedure Init_Source_Text_Checks (Unit : Asis.Element) is
      CU_Span : constant Span := Compilation_Span (Unit);
   begin
      First_Non_Analyzed_Line := CU_Span.First_Line;
      Last_Unit_Line          := CU_Span.Last_Line;
   end Init_Source_Text_Checks;

end Gnatcheck.Source_Checks;
