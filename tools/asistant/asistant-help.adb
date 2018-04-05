------------------------------------------------------------------------------
--                                                                          --
--             ASIS Tester And iNTerpreter (ASIStant) COMPONENTS            --
--                                                                          --
--                        A S I S T A N T . H E L P                         --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1997-2012, Free Software Foundation, Inc.         --
--                                                                          --
-- ASIStant  is  free  software;  you can  redistribute it and/or modify it --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software Foundation;  either version 2,  or  (at your option)  any later --
-- version.  ASIStant is  distributed  in the hope  that it will be useful, --
-- but  WITHOUT  ANY  WARRANTY;  without  even the implied warranty of MER- --
-- CHANTABILITY or  FITNESS FOR  A PARTICULAR PURPOSE.  See the GNU General --
-- Public License for more details.  You should have received a copy of the --
-- GNU General Public License  distributed with GNAT;  see file COPYING. If --
-- not,  write to the  Free Software Foundation,  51 Franklin Street, Fifth --
-- Floor, Boston, MA 02110-1301, USA.                                       --
--                                                                          --
-- ASIStant  is an evolution of  ASIStint tool that was created by  Vasiliy --
-- Fofanov  as  part  of  a  collaboration  between  Software   Engineering --
-- Laboratory  of the  Swiss  Federal Institute of Technology in  Lausanne, --
-- Switzerland,  and the Scientific Research Computer Center of the  Moscow --
-- University, Russia,  supported by the  Swiss National Science Foundation --
-- grant #7SUPJ048247, "Development of ASIS for GNAT with industry quality" --
--                                                                          --
-- ASIStant  is  distributed as a part of the  ASIS implementation for GNAT --
-- (ASIS-for-GNAT) and is maintained by AdaCore (http://www.adacore.com).   --
--                                                                          --
------------------------------------------------------------------------------

with Asis;                       use Asis;
with Asis.Implementation;

with ASIStant.Ambiguous_Mapping; use ASIStant.Ambiguous_Mapping;
with ASIStant.Call;              use ASIStant.Call;
with ASIStant.Common;            use ASIStant.Common;
with ASIStant.FuncEnum;          use ASIStant.FuncEnum;
with ASIStant.Help.Queries;
with ASIStant.String_Handling;   use ASIStant.String_Handling;
with ASIStant.Text_IO;           use ASIStant.Text_IO;

package body ASIStant.Help is

   -----------------------
   -- Local subprograms --
   -----------------------

   procedure Display_Query_Help (SW : Switch_Index);
   --  Displays help for a given ASIStant or ASIS query

   procedure Display_Syntax (SW : Switch_Index; SYNT : Func_Syntax);
   --  Displays the text representation of a given syntax structure

   -----------------------
   -- Display_Kind_Help --
   -----------------------

   procedure Display_Kind_Help (K : Flat_Element_Kinds) is
      use ASIStant.Help.Queries;
      Q : constant Query_List := Appropriate_Queries (K);
   begin

      ATIPut_Line (
         "Appropriate ASIS structural queries for " &
         To_Proper (Flat_Element_Kinds'Wide_Image (K)) & ":");

      if Q (1) = Invalid_Index then
         ATIPut_Line ("   None.");
      else
         for I in Q'Range loop
            exit when Q (I) = Invalid_Index;
            ATIPut_Line ("   " & To_Proper (Switch_Index'Wide_Image (Q (I))));
         end loop;
      end if;

   end Display_Kind_Help;

   ------------------------
   -- Display_Query_Help --
   ------------------------

   procedure Display_Query_Help (SW : Switch_Index) is
      T :  Func_Syntax;
      Amb : Amb_Index;
   begin

      ATIPut_Line (To_Proper (Switch_Index'Wide_Image (SW)) & " syntax:");

      for I in Switch_Info'Range loop

         if SW in Switch_Info (I).From .. Switch_Info (I).To then
            T := Switch_Info (I).Synt;
            exit;
         end if;

      end loop;

      Amb := To_Amb_Index (SW);
      --  This will return Not_Ambiguous if SW is not an overloaded query

      if Amb = Not_Ambiguous then
         Display_Syntax (SW, T);

      else
         for i in 1 .. AI_LENGTH loop
            exit when Amb_Info (Amb, i).New_Index = Invalid_Index;
            Display_Syntax (SW, Amb_Info (Amb, i).Synt);
         end loop;
      end if;

   end Display_Query_Help;

   --------------------
   -- Display_Syntax --
   --------------------

   procedure Display_Syntax (SW : Switch_Index; SYNT : Func_Syntax) is
   begin
      ATIPut ("   ");
      ATIPut (To_Proper (Switch_Index'Wide_Image (SW)));

      if SYNT (1) /= Par_Absent then
         ATIPut (" (");
      end if;

      for i in Parameter_Range loop
         exit when SYNT (i) = Par_Absent;
         if i /= 1 then
            ATIPut (", ");
         end if;

         ATIPut (Skip_Prefix (Func_Param'Wide_Image (SYNT (i))));
      end loop;

      if SYNT (1) /= Par_Absent then
         ATIPut (")");
      end if;

      if SYNT (0) /= Par_Absent then
         ATIPut (" return ");
         ATIPut (Skip_Prefix (Func_Param'Wide_Image (SYNT (0))));
      end if;

      ATINew_Line;
   end Display_Syntax;

   --------------
   -- ASISHelp --
   --------------

   procedure ASISHelp is
   begin
      Print_ASIStant_Header;
      ATINew_Line;
      ATIPut_Line ("ASIS and ASIStant Technical and Copyright Information :");
      ATINew_Line;
      ATIPut ("  Asis Version :              ");
      ATIPut_Line (Asis.Implementation.ASIS_Version);
      ATIPut ("  Asis Implementor Version :  ");
      ATIPut_Line (Asis.Implementation.ASIS_Implementor_Version);
      ATIPut ("  Asis Implementor :          ");
      ATIPut_Line (Asis.Implementation.ASIS_Implementor);
      ATIPut ("  Asis Implementor Info :     ");
      ATIPut_Line (Asis.Implementation.ASIS_Implementor_Information);
      ATINew_Line;
      ATIPut_Line ("  ASIStant author Info :      "
                 & "Vasiliy Fofanov, ASIS-for-GNAT team at AdaCore");
      ATIPut_Line (
         "                              Email : fofanov@adacore.com");
      ATIPut_Line ("  ASIStant status :");

      if Log then
         ATIPut_Line ("          Log enabled. Output Level : "
                      & Integer'Wide_Image (OutputLevel));
      else
         ATIPut_Line ("          Log disabled.");
      end if;

      if Script > 0 then
         ATIPut_Line ("          Script enabled (level " &
                      Integer'Wide_Image (Script) & ") and is in " &
                      Script_Mode'Wide_Image (ScriptMode) & " mode.");
      else
         ATIPut_Line ("          Script disabled.");
      end if;
   end ASISHelp;

   ----------
   -- Help --
   ----------

   procedure Help (NP : Node_Position) is

      N :  Node := CurStat.Tree (NP);

   begin

      if N.NValue = 0 then --  general help

         Print_ASIStant_Header;
         ATINew_Line;
         ATIPut_Line ("List of ASIStant utilities :");
         ATINew_Line;
         ATIPut_Line ("  HELP (<topic-name>)    "
                      & " Displays help for a specific <topic-name>");
         ATIPut_Line ("  IF (<bool-expr>,<expr1>[,<expr2>]) Executes <expr1> "
                      & "if <bool-expr> is true,");
         ATIPut_Line ("                                     otherwise, "
                      & "executes <expr2> if present");
         ATIPut_Line ("  INFO                    Displays ASIS and ASIStant "
                      & "technical info");
         ATIPut_Line ("  LOG [(""<filename>"")]    Opens file as a current "
                      & "log/closes current log");
         ATIPut_Line ("  LOGLEVEL (<level>)      Sets a logging level to "
                      & "integer <level>, 0..5");
         ATIPut_Line ("  PAUSE                   Pauses current script");
         ATIPut_Line ("  PRINT (<variable>)      Prints a value "
                      & "of a variable");
         ATIPut_Line ("  PRINTDETAIL (""{D|d}{T|t}"") Changes PRINT settings");
         ATIPut_Line ("  QUIT [(<exit-status>)]  Exits program returning "
                      & "<exit-status> or 0");
         ATIPut_Line ("  RUN [(""<filename>"")]    Launches script "
                      & "<filename>/resumes paused script");
         ATIPut_Line ("                          (IRUN = interactively)");
         ATIPut_Line ("  SET ( <ID>, <expr> )    Declares or changes "
                      & "variable <ID>");
         ATINew_Line;

         ATIPut_Line ("Please refer to ASIStant User's Guide for more "
                      & "information.");

      else --  specific help

         N := CurStat.Tree (N.NValue); --  receive parameter list
         N := CurStat.Tree (N.NValue); --  receive first element

         --  Determine type of requested help

         --  Is query help requested?
         declare
            sw : Switch_Index;
         begin
            sw := Switch_Index'Wide_Value (N.SValue.all);
            Display_Query_Help (sw);
            return;
         exception
            when others =>
               null; -- trap exception if query unknown
         end;

         --  Is Flat_Element_Kind help requested?
         declare
            K : Flat_Element_Kinds;
         begin
            K := Flat_Element_Kinds'Wide_Value (N.SValue.all);
            Display_Kind_Help (K);
            return;
         exception
            when others =>
               null; -- trap exception if Flat_Element_Kind unknown
         end;

         --  If we are here, the topic wasn't recognized
         Error (ERR_UNKNOWNQUERY, N.SValue.all);

      end if;

   end Help;

end ASIStant.Help;
