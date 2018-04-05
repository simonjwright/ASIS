------------------------------------------------------------------------------
--                                                                          --
--             ASIS Tester And iNTerpreter (ASIStant) COMPONENTS            --
--                                                                          --
--             A S I S T A N T . B R O W S E R . I T E R A T O R            --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1997-2013, Free Software Foundation, Inc.         --
--                                                                          --
-- ASIStant is free software; you can redistribute it and/or modify it      --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software Foundation;  either version 2,  or  (at your option)  any later --
-- version. ASIStant is distributed  in the hope  that it will be useful,   --
-- but WITHOUT ANY WARRANTY; without even the implied warranty of MER-      --
-- CHANTABILITY or  FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General  --
-- Public License for more details. You should have received a copy of the  --
-- GNU General Public License distributed with GNAT; see file COPYING. If   --
-- not, write to the Free Software Foundation, 59 Temple Place Suite 330,   --
-- Boston, MA 02111-1307, USA.                                              --
--                                                                          --
-- ASIStant is an evolution of ASIStint tool that was created by            --
-- Vasiliy Fofanov as part of a collaboration between Software Engineering  --
-- Laboratory of the Swiss Federal Institute of Technology in Lausanne,     --
-- Switzerland, and the Scientific Research Computer Center of the Moscow   --
-- University, Russia, supported by the Swiss National Science Foundation   --
-- grant #7SUPJ048247, "Development of ASIS for GNAT with industry quality" --
--                                                                          --
-- The original version of the Browser tool was created by Alain Le Guennec --
--                                                                          --
-- ASIStant is distributed as a part of the ASIS implementation for GNAT    --
-- (ASIS-for-GNAT) and is maintained by AdaCore (http://www.adacore.com).   --
--                                                                          --
------------------------------------------------------------------------------

with Asis;          use Asis;
with Asis.Elements; use Asis.Elements;
with Asis.Extensions;

package body ASIStant.Browser.Iterator is

   ------------
   --  Next  --
   ------------

   function Next (E : Asis.Element) return Asis.Element is
      Comp : constant Asis.Element_List := Asis.Extensions.Components (Up (E));
   begin
      for I in Comp'Range loop

         if Is_Identical (E, Comp (I)) then

            if I = Comp'Last then
               return Nil_Element;
            else
               return Comp (I + 1);
            end if;

         end if;

      end loop;

      return Nil_Element;
   end Next;

   ----------------
   --  Previous  --
   ----------------

   function Previous (E : Asis.Element) return Asis.Element is
      Comp : constant Asis.Element_List := Asis.Extensions.Components (Up (E));
   begin
      for I in Comp'Range loop

         if Is_Identical (E, Comp (I)) then

            if I = 1 then
               return Nil_Element;
            else
               return Comp (I - 1);
            end if;

         end if;

      end loop;

      return Nil_Element;
   end Previous;

   ----------
   --  Up  --
   ----------

   function Up (E : Asis.Element) return Asis.Element
      renames Asis.Elements.Enclosing_Element;

   ------------
   --  Down  --
   ------------

   function Down (E : Asis.Element) return Asis.Element is
      Comp : constant Asis.Element_List := Asis.Extensions.Components (E);
   begin

      if Comp = Nil_Element_List then
         return Nil_Element;
      else
         return Comp (1);
      end if;

   end Down;

end ASIStant.Browser.Iterator;
