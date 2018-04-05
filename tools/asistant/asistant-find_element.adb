------------------------------------------------------------------------------
--                                                                          --
--             ASIS Tester And iNTerpreter (ASIStant) COMPONENTS            --
--                                                                          --
--                 A S I S T A N T . F I N D _ E L E M E N T                --
--                                                                          --
--                         F u n c t i o n   B o d y                        --
--                                                                          --
--          Copyright (C) 1997-2013, Free Software Foundation, Inc.         --
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
-- The original version of this function was created by Ilia Chentsov.      --
--                                                                          --
-- ASIStant  is  distributed as a part of the  ASIS implementation for GNAT --
-- (ASIS-for-GNAT) and is maintained by AdaCore (http://www.adacore.com).   --
------------------------------------------------------------------------------

with Asis;      use Asis;
with Asis.Text; use Asis.Text;
with Asis.Compilation_Units;
with Asis.Elements;
with Asis.Extensions.Iterator;

--  Find_Element searches for the 'smallest' element at position Line:Col in
--  the unit Unit (that is, the element that contains this text position but
--  is not a parent to any other element containing this text position).
--  If the Unit is Nil_Compilation_Unit or if no element in this unit includes
--  position Line:Col, the function returns Nil_Element.

function ASIStant.Find_Element
  (Unit      : Compilation_Unit;
   Line, Col : Integer)
   return      Element
is
   --  Type Info carries information for procedure Traverse_The_Unit.
   type Trav_Modes is (Line_Col, Line_Only);

   type Info is record
      Line             :  Line_Number;
      Col              :  Character_Position;
      The_Element      :  Element;
      Trav_Mode        :  Trav_Modes;
      Tmp_El_First_Col :  Character_Position;
   end record;

   procedure Pre
     (Element :        Asis.Element;
      Control : in out Traverse_Control;
      State   : in out Info);
   --  ??? Documentation!!!

   procedure Pre (Element :        Asis.Element;
                  Control : in out Traverse_Control;
                  State   : in out Info)
   is
      Elspan : Span;
   begin

      if Asis.Elements.Is_Nil (Element) then
         Control := Abandon_Children;
      else
         Elspan := Element_Span (Element);
         if Elspan.First_Line <= State.Line and then
            Elspan.Last_Line  >= State.Line and then
            ((State.Trav_Mode     =  Line_Col and then
              Elspan.First_Column <= State.Col and then
              Elspan.Last_Column  >= State.Col) or else
             (State.Trav_Mode = Line_Only and then
              ((Elspan.First_Line   =  State.Line and then
                Elspan.Last_Line    =  State.Line and then
                Elspan.First_Column <= State.Tmp_El_First_Col) or else
              Elspan.First_Line   < State.Line or else
                 Elspan.Last_Line    > State.Line)))
         then

            State.The_Element := Element;

            if Elspan.First_Line   =  State.Line and then
               Elspan.Last_Line    =  State.Line and then
               Elspan.First_Column <= State.Tmp_El_First_Col
            then
               State.Tmp_El_First_Col :=
                  Element_Span (Element).First_Column;
            end if;

         end if;

         Control := Continue;
      end if;

   end Pre;

   procedure Post
     (Element :        Asis.Element;
      Control : in out Traverse_Control;
      State   : in out Info);
   --  ??? Documentation!!!

   procedure Post (Element :        Asis.Element;
                   Control : in out Traverse_Control;
                   State   : in out Info)
   is
   begin
      pragma Unreferenced (Element);
      pragma Unreferenced (Control);
      pragma Unreferenced (State);
      null;
   end Post;

   --  Instantiates procedure Asis.Extensions.Traverse_Unit
   procedure Traverse_Unit is new Asis.Extensions.Iterator.Traverse_Unit
     (State_Information => Info,
      Pre_Operation     => Pre,
      Post_Operation    => Post);

   --  Declarations for Find_Element
   The_Unit : constant Compilation_Unit := Unit;
   The_Control : Traverse_Control := Continue;
   --  Initial State
   The_State : Info :=
      (Col => Line_Number (Col),
      Line => Character_Position (Line),
      The_Element => Nil_Element,
      Trav_Mode => Line_Col,
      Tmp_El_First_Col => Maximum_Line_Length);

   Temp_Elem : Element;

begin --  Find_Element

   if Asis.Compilation_Units.Is_Nil (The_Unit) then
      return Nil_Element;
   end if;

   Traverse_Unit (The_Unit, The_Control, The_State);
   Temp_Elem := The_State.The_Element;

   if Asis.Elements.Is_Nil (Temp_Elem) then
      The_State.Trav_Mode := Line_Only;
      The_State.Tmp_El_First_Col := Maximum_Line_Length;
      Traverse_Unit (The_Unit, The_Control, The_State);
   end if;

   return The_State.The_Element;

end ASIStant.Find_Element;
