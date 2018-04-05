------------------------------------------------------------------------------
--                                                                          --
--                          GNATCHECK COMPONENTS                            --
--                                                                          --
--   A S I S _ U L . G L O B A L _ S T A T E . C G. C O N D I T I O N S     --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                       Copyright (C) 2009, AdaCore                        --
--                                                                          --
-- Asis Utility Library (ASIS UL) is free software; you can redistribute it --
-- and/or  modify  it  under  terms  of  the  GNU General Public License as --
-- published by the Free Software Foundation; either version 2, or (at your --
-- option)  any later version.  ASIS UL  is distributed in the hope that it --
-- will  be  useful,  but  WITHOUT  ANY  WARRANTY; without even the implied --
-- warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the --
-- GNU  General Public License for more details. You should have received a --
-- copy of the  GNU General Public License  distributed with GNAT; see file --
-- COPYING. If not,  write  to the  Free Software Foundation,  51 Franklin  --
-- Street, Fifth Floor, Boston, MA 02110-1301, USA.                         --
--                                                                          --
-- ASIS UL is maintained by AdaCore (http://www.adacore.com).               --
--                                                                          --
------------------------------------------------------------------------------

with ASIS_UL.Utilities;

package body ASIS_UL.Global_State.CG.Conditions is

   Unconditional_CG : Boolean := True;

   ---------------------------------------------------------
   --  Conditions to select specific nodes for call graph --
   ---------------------------------------------------------

   --  ...

   ------------------------------
   --  Condition look-up table --
   ------------------------------

   type Condition_Access is access function (E : Asis.Element) return Boolean;

   type Condition is record
      On    : Boolean := False;
      Check : Condition_Access;
   end record;

   Conditions : array (Check_Kinds) of Condition :=
     (Inlined_Subprograms =>
        (On    => False,
         Check => ASIS_UL.Utilities.Has_Pragma_Inline'Access));

   -------------------
   -- Set_Condition --
   -------------------

   procedure Set_Condition (Cond : Check_Kinds) is
   begin
      Conditions (Cond).On := True;
   end Set_Condition;

   ----------------------------------
   -- Set_Unconditional_Call_Graph --
   ----------------------------------

   procedure Set_Unconditional_Call_Graph (On : Boolean) is
   begin
      Unconditional_CG := On;
   end Set_Unconditional_Call_Graph;

   ---------------------
   -- Should_Be_In_CG --
   ---------------------

   function Should_Be_In_CG (E : Asis.Element) return Boolean is
      Result : Boolean := Unconditional_Call_Graph;
   begin
      for J in Conditions'Range loop
         exit when Result;

         if Conditions (J).On then
            Result := Conditions (J).Check (E);
         end if;

      end loop;

      return Result;
   end Should_Be_In_CG;

   ------------------------------
   -- Unconditional_Call_Graph --
   ------------------------------

   function Unconditional_Call_Graph return Boolean is
   begin
      return Unconditional_CG;
   end Unconditional_Call_Graph;

end ASIS_UL.Global_State.CG.Conditions;
