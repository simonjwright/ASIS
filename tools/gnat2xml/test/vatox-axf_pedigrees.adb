------------------------------------------------------------------------------
--                                                                          --
--                           AVATOX COMPONENTS                              --
--                                                                          --
--                        VATOX (Via Asis To Xml)                           --st
--                                                                          --
--                                                                          --
--                Copyright (c) 2006, McKae Technologies.                   --
--                                                                          --
-- Avatox is free software; you can redistribute it and/or modify it        --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software Foundation;  either version 2,  or  (at your option)  any later --
-- version. Avatox is distributed in the hope  that it will be useful,      --
-- but WITHOUT ANY WARRANTY; without even the implied warranty of MER-      --
-- CHANTABILITY or  FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General  --
-- Public License for more details. You should have received a copy of the  --
-- GNU General Public License distributed with GNAT; see file COPYING. If   --
-- not, write to the Free Software Foundation, 59 Temple Place Suite 330,   --
-- Boston, MA 02111-1307, USA.                                              --
--                                                                          --
-- Avatox is now maintained by McKae Technologies (http://www.mckae.com)    --                                               --
------------------------------------------------------------------------------

with Ada.Containers.Vectors;
with Ada.Strings.Unbounded;
use  Ada.Strings.Unbounded;

package body Vatox.Axf_Pedigrees is

   type Registered_Pedigrees is
      record
         Pedigree_Id         : Unbounded_String;
         Pedigree_Type       : Pedigree_Types;
         Pedigree_Producer   : Unbounded_String;
         Implementor         : Unbounded_String;
         Implementor_Version : Unbounded_String;
      end record;

   function Equal_Pedigrees (L, R : Registered_Pedigrees) return Boolean;

   package Pedigree_List is new Ada.Containers.Vectors
     (Positive, Registered_Pedigrees, Equal_Pedigrees);

   Pedigrees : Pedigree_List.Vector;

   function Equal_Pedigrees (L, R : Registered_Pedigrees) return Boolean is
   begin
      return L.Pedigree_Id = R.Pedigree_Id;
   end Equal_Pedigrees;

   --------------
   -- Register --
   --------------

   procedure Register
     (Pedigree_Id         : in String;
      Pedigree_Type       : in Pedigree_Types;
      Pedigree_Producer   : in String;
      Implementor         : in String;
      Implementor_Version : in String)
   is
      Pedigree_Key : Registered_Pedigrees;
   begin
      Pedigree_Key.Pedigree_Id := To_Unbounded_String(Pedigree_Id);
      if not Pedigree_List.Contains (Pedigrees, Pedigree_Key) then
         Pedigree_List.Append
           (Pedigrees,
            (To_Unbounded_String (Pedigree_Id),
             Pedigree_Type,
             To_Unbounded_String (Pedigree_Producer),
             To_Unbounded_String (Implementor),
             To_Unbounded_String (Implementor_Version)));
      end if;
   end Register;

   ---------------------
   -- Apply_Processor --
   ---------------------

   procedure Apply_Processor (P : in Pedigree_Processors) is
      Continue            : Boolean := False;
      Registered_Pedigree : Registered_Pedigrees;
   begin
      for I in Pedigrees.First_Index .. Pedigrees.Last_Index loop
         Registered_Pedigree := Pedigrees.Element (I);
         P.all (To_String (Registered_Pedigree.Pedigree_Id),
                Registered_Pedigree.Pedigree_Type,
                To_String (Registered_Pedigree.Pedigree_Producer),
                To_String (Registered_Pedigree.Implementor),
                To_String (Registered_Pedigree.Implementor_Version),
                Continue);
         exit when not Continue;
      end loop;
   end Apply_Processor;

end Vatox.Axf_Pedigrees;
