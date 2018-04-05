------------------------------------------------------------------------------
--                                                                          --
--                     ASIS UTILITY LIBRARY COMPONENTS                      --
--                                                                          --
--                 A S I S _ U L . W I D E _ S T R I N G S                  --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                    Copyright (C) 2008-2017, AdaCore                      --
--                                                                          --
-- Asis Utility Library (ASIS UL) is free software; you can redistribute it --
-- and/or  modify  it  under  terms  of  the  GNU General Public License as --
-- published by the Free Software Foundation; either version 2, or (at your --
-- option)  any later version.  ASIS UL  is distributed in the hope that it --
-- will  be  useful,  but  WITHOUT  ANY  WARRANTY; without even the implied --
-- warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the --
-- GNU  General Public License for more details. You should have received a --
-- copy of the  GNU General Public License  distributed with GNAT; see file --
-- COPYING.  If  not,  write  to  the Free Software Foundation, 51 Franklin --
-- Street, Fifth Floor, Boston, MA 02110-1301, USA.                         --
--                                                                          --
-- ASIS UL is maintained by AdaCore (http://www.adacore.com).               --
--                                                                          --
------------------------------------------------------------------------------

with GNAT.Table;

package body ASIS_UL.Wide_Strings is

   package Wide_Chars is new GNAT.Table (
     Table_Component_Type => Wide_Character,
     Table_Index_Type     => Integer,
     Table_Low_Bound      => 1,
     Table_Initial        => 10000,
     Table_Increment      => 1000,
     Table_Name           => "Wide_Character container");

   Table : Wide_Chars.Table_Ptr renames Wide_Chars.Table;

   -----------------------
   -- Enter_Wide_String --
   -----------------------

   function Enter_Wide_String (S : Wide_String) return Wide_String_Loc is
      Len   : constant Integer := S'Length;
      F     :          Integer;
   begin

      if Len = 0 then
         return Nil_Wide_String_Loc;
      else
         Wide_Chars.Increment_Last;
         F := Wide_Chars.Last;
         Wide_Chars.Set_Last (F + Len - 1);

         Table (F .. F + Len - 1) := Wide_Chars.Table_Type (S);

         return (F, F + Len - 1);
      end if;

   end Enter_Wide_String;

   ---------------------
   -- Get_Wide_String --
   ---------------------

   function Get_Wide_String (SL : Wide_String_Loc) return Wide_String is
   begin

      if SL = Nil_Wide_String_Loc then
         return "";
      else
         return Wide_String (Table (SL.First .. SL.Last));
      end if;

   end Get_Wide_String;

   ----------
   -- Init --
   ----------

   procedure Init is
   begin
      Wide_Chars.Init;
   end Init;

   --------------
   -- Is_Equal --
   --------------

   function Is_Equal (S : Wide_String; SL : Wide_String_Loc) return Boolean is
      Result : Boolean := False;
   begin

      if SL.First in 1 .. Wide_Chars.Last
        and then
         SL.Last in 1 .. Wide_Chars.Last
        and then
         SL.Last - SL.First + 1 = S'Length
      then
         Result := S = Get_Wide_String (SL);
      end if;

      return Result;

   end Is_Equal;

end ASIS_UL.Wide_Strings;
