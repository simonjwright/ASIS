------------------------------------------------------------------------------
--                                                                          --
--                 ASIS-for-GNAT IMPLEMENTATION COMPONENTS                  --
--                                                                          --
--                          A 4 G . A _ T Y P E S                           --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--            Copyright (C) 1995-2015, Free Software Foundation, Inc.       --
--                                                                          --
-- ASIS-for-GNAT is free software; you can redistribute it and/or modify it --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software Foundation;  either version 2,  or  (at your option)  any later --
-- version. ASIS-for-GNAT is distributed  in the hope  that it will be use- --
-- ful, but WITHOUT ANY WARRANTY; without even the implied warranty of MER- --
-- CHANTABILITY or  FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General  --
-- Public License for more details. You should have received a copy of the  --
-- GNU  General  Public  License  distributed with ASIS-for-GNAT;  see file --
-- COPYING.  If not,  write  to the  Free Software Foundation,  51 Franklin --
-- Street, Fifth Floor, Boston, MA 02110-1301, USA.                         --
--                                                                          --
--                                                                          --
--                                                                          --
--                                                                          --
--                                                                          --
--                                                                          --
--                                                                          --
--                                                                          --
-- ASIS-for-GNAT was originally developed  by the ASIS-for-GNAT team at the --
-- Software  Engineering  Laboratory  of  the Swiss  Federal  Institute  of --
-- Technology (LGL-EPFL) in Lausanne,  Switzerland, in cooperation with the --
-- Scientific  Research  Computer  Center of  Moscow State University (SRCC --
-- MSU), Russia,  with funding partially provided  by grants from the Swiss --
-- National  Science  Foundation  and  the  Swiss  Academy  of  Engineering --
-- Sciences.  ASIS-for-GNAT is now maintained by  AdaCore                   --
-- (http://www.adacore.com).                                                --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Unchecked_Conversion;

package body A4G.A_Types is

   ---------------
   -- A_OS_Time --
   ---------------

   function A_OS_Time return ASIS_OS_Time is
   begin
      return ASIS_Clock;
   end A_OS_Time;

   ---------------------------
   -- Increase_ASIS_OS_Time --
   ---------------------------

   procedure Increase_ASIS_OS_Time is
   begin
      ASIS_Clock := ASIS_Clock + 1;
   end Increase_ASIS_OS_Time;

   -----------
   -- Later --
   -----------

   function Later (L, R : ASIS_OS_Time) return Boolean is
   begin
      return L <= R;
   end Later;

   ------------------------------
   -- Parameter_String_To_List --
   ------------------------------

   function Parameter_String_To_List
     (Par_String : String)
      return       Argument_List_Access
   is
      Max_Pars : constant Integer := Par_String'Length;
      New_Parv : Argument_List (1 .. Max_Pars);
      New_Parc : Natural := 0;
      Idx      : Integer;
      Old_Idx  : Integer;

      function Move_To_Next_Par (Ind : Integer) return Integer;
      --  Provided that Ind points somewhere inside Par_String, moves
      --  it ahead to point to the beginning of the next parameter if
      --  Ind points to the character considering as a parameter separator,
      --  otherwise returns Ind unchanged. If Ind points to a separator and
      --  there is no more parameters ahead, Par_String'Last + 1 is returned.
      --  (See the definition of the syntax of the Parameters string in the
      --  ASIS Reference Manual)

      function Move_To_Par_End (Ind : Integer) return Integer;
      --  Provided that Ind points to some character of a separate parameters
      --  being a part of Par_String, returns the index of the last character
      --  of this parameter

      function Move_To_Next_Par (Ind : Integer) return Integer is
         Result : Integer := Ind;
      begin

         while Result <=             Par_String'Last and then
              (Par_String (Result) = ' '      or else
               Par_String (Result) = ASCII.HT or else
               Par_String (Result) = ASCII.LF or else
               Par_String (Result) = ASCII.CR)
         loop
            Result := Result + 1;
         end loop;

         return Result;

      end Move_To_Next_Par;

      function Move_To_Par_End (Ind : Integer) return Integer is
         Result : Integer := Ind;
         Quoted : Boolean := False;
      begin

         loop

            --  Am unquoted white space or EOL is the end of an argument
            if not Quoted
              and then
              (Par_String (Result) = ' '      or else
               Par_String (Result) = ASCII.HT or else
               Par_String (Result) = ASCII.LF or else
               Par_String (Result) = ASCII.CR)
            then
               exit;

            --  Start of quoted string
            elsif not Quoted
              and then Par_String (Result) = '"'
            then
               Quoted := True;

            --  End of a quoted string and end of an argument
            elsif Quoted
              and then Par_String (Result) = '"'
            then
               Result := Result + 1;
               exit;
            end if;

            Result := Result + 1;
            exit when Result > Par_String'Last;
         end loop;

         Result := Result - 1;

         return Result;

      end Move_To_Par_End;

   begin
      Idx := Move_To_Next_Par (Par_String'First);

      while Idx <= Par_String'Last loop
         Old_Idx := Idx;
         Idx     := Move_To_Par_End (Idx);

         New_Parc := New_Parc + 1;
         New_Parv (New_Parc) :=
           new String'(Par_String (Old_Idx .. Idx));

         Idx := Move_To_Next_Par (Idx + 1);
      end loop;

      return new Argument_List'(New_Parv (1 .. New_Parc));
   end Parameter_String_To_List;

end A4G.A_Types;
