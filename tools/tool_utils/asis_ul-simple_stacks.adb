------------------------------------------------------------------------------
--                                                                          --
--                                                                          --
--                     ASIS UTILITY LIBRARY COMPONENTS                      --
--                                                                          --
--                A S I S _ U L . S I M P L E _ S T A C K S                 --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                    Copyright (C) 2012-2014, AdaCore                      --
--                                                                          --
-- Asis Utility Library (ASIS UL) is free software; you can redistribute it --
-- and/or  modify  it  under  terms  of  the  GNU General Public License as --
-- published by the Free Software Foundation; either version 3, or (at your --
-- option)  any later version.  ASIS UL  is distributed in the hope that it --
-- will  be  useful,  but  WITHOUT  ANY  WARRANTY; without even the implied --
-- warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the --
-- GNU  General Public License for more details. You should have received a --
-- copy of the  GNU General Public License  distributed with GNAT; see file --
-- COPYING3. If not,  go to http://www.gnu.org/licenses for a complete copy --
-- of the license.                                                          --
--                                                                          --
-- ASIS UL is maintained by AdaCore (http://www.adacore.com).               --
--                                                                          --
------------------------------------------------------------------------------
pragma Ada_2012;

package body ASIS_UL.Simple_Stacks is

   --------------------
   -- Is_Empty_Stack --
   --------------------

   function Is_Empty_Stack (S : in out Simple_Stack) return Boolean is
   begin
      return Is_Empty (S);
   end Is_Empty_Stack;

   ---------
   -- Pop --
   ---------

   function Pop (S : in out Simple_Stack) return Element_Type is
      Result : Element_Type := No_Element;
   begin

      if not Is_Empty (S) then
         Result := Last_Element (S);
         Delete_Last (S);
      end if;

      return Result;
   end Pop;

   ---------
   -- Pop --
   ---------

   procedure Pop (S : in out Simple_Stack) is
   begin

      if not Is_Empty (S) then
         Delete_Last (S);
      end if;

   end Pop;

   ----------
   -- Push --
   ----------

   procedure Push (S : in out Simple_Stack; Elem : Element_Type) is
   begin
      Append (S, Elem);
   end Push;

   ----------
   -- Size --
   ----------

   function Size  (S : in out Simple_Stack) return Natural is
   begin
      return Natural (Length (S));
   end Size;

   ---------
   -- Top --
   ---------

   function Top
     (S         : in out Simple_Stack;
      Step_Down :        Natural := 0)
      return             Element_Type
   is
      Depth : constant Natural := Size (S);
   begin

      if Depth = 0 or else Step_Down > Depth then
         return No_Element;
      else
         return Element (S,  Last_Index (S) - Step_Down);
      end if;

   end Top;

end ASIS_UL.Simple_Stacks;
