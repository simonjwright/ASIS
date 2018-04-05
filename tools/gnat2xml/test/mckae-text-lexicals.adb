------------------------------------------------------------------------
--                                                                    --
--                     McKae Software Utilities                       --
--                                                                    --
--           Copyright (C) 2006 McKae Technologies                    --
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

with Ada.Strings.Fixed;
use  Ada.Strings.Fixed;
with Ada.Strings.Maps.Constants;
use  Ada.Strings.Maps.Constants;

package body McKae.Text.Lexicals is

   ---------------
   -- Transform --
   ---------------

   function Transform
     (S                  : String;
      Capitalization     : Capitalizations;
      Remove_Underscores : Boolean := False)
      return String
   is
      use Ada.Strings.Maps;

      T : String (S'Range);
      P : Positive := S'Last;
      Cap_Next : Boolean := Capitalization = Capitalized;

   begin
      case Capitalization is
         when No_Change =>
            T := S;
         when Lower =>
            T := Translate (S, Lower_Case_Map);
         when Upper =>
            T := Translate (S, Upper_Case_Map);
         when Capitalized | Camelback  =>
            T := Translate (S, Lower_Case_Map);
            for I in T'Range loop
               if Cap_Next then
                  T (I) := Value (Upper_Case_Map, T (I));
                  Cap_Next := False;
               end if;
               Cap_Next := T (I) = '_';
            end loop;
      end case;

      if Remove_Underscores then
         P := T'First;
         for I in T'Range loop
            if T (I) /= '_' then
               T (P) := T (I);
               P := P + 1;
            end if;
         end loop;
         P := P - 1;
      end if;
      return T (T'First .. P);
   end Transform;

end McKae.Text.Lexicals;
