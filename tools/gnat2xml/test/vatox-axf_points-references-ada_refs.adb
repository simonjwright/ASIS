------------------------------------------------------------------------------
--                                                                          --
--                           AVATOX COMPONENTS                              --
--                                                                          --
--                        VATOX (Via Asis To Xml)                           --
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
-- Avatox is based off the Display_Source software distributed as part of   --
-- the ASIS implementation for GNAT, and therefore inherits its GPL         --
-- licensing.  Ada Core Technologies maintains the Display_Source program   --
-- and its copyright is held by the Free Software Foundation.               --
--                                                                          --
-- Avatox is now maintained by McKae Technologies (http://www.mckae.com)    --                                               --
------------------------------------------------------------------------------

with Ada.Containers.Indefinite_Ordered_Maps;
with Ada.Strings.Wide_Unbounded;
with Asis.Declarations;
with Asis.Elements;

package body Vatox.Axf_Points.References.Ada_Refs is

   use Ada.Strings.Wide_Unbounded;

   -----------------------------------------------------------------------------

   -- Indicate an empty ref entry
   Null_Ref_Entry : Ref_List (1 .. 0);

   -----------------------------------------------------------------------------
   function Enclosing_Declaration
     (Element : in Asis.Element) return Asis.Declaration is
      Parent : Asis.Declaration;
   begin
      Parent := Asis.Elements.Enclosing_Element (Element);
      case Asis.Elements.Element_Kind (Parent) is
         when Asis.A_Declaration =>
            return Parent;

         when Asis.Not_An_Element =>
            return Parent;

         when others =>
            return Enclosing_Declaration (Parent);

      end case;
   end Enclosing_Declaration;

   --------------------------------------------------------------------

   function Get_Scope_Refs
     (Asis_Decl : Asis.Declaration
      -- The declaration whose list of scope references is needed
     ) return Ref_List is

      Parent : Asis.Declaration;

      use Ada.Strings.Wide_Unbounded;

   begin
      case Asis.Elements.Declaration_Kind (Asis_Decl) is
         when Asis.Not_A_Declaration =>
            return Null_Ref_Entry;

         when others =>
            Parent := Enclosing_Declaration (Asis_Decl);
            case Asis.Elements.Element_Kind (Parent) is
               when Asis.A_Declaration =>
                  return Get_Scope_Refs (Parent)
                    & (1 => To_Unbounded_Wide_String
                         (Asis.Declarations.Defining_Name_Image
                          (Asis.Declarations.Names (Asis_Decl) (1))));
               when Asis.Not_An_Element =>
                  return (1 => To_Unbounded_Wide_String
                            (Asis.Declarations.Defining_Name_Image
                             (Asis.Declarations.Names (Asis_Decl) (1))));
               when others =>
                  null;  -- not happen
            end case;
      end case;
      return Null_Ref_Entry;
   end Get_Scope_Refs;

   -----------------------------------------------------------------------------

   function Get_Scope_Sequence
     (Asis_Decl        : Asis.Declaration
     ) return Wide_String is

      Scope_Refs : Ref_List := Get_Scope_Refs (Asis_Decl);

      Sequence_Id : Unbounded_Wide_String := Scope_Refs(Scope_Refs'First);

   begin
      for I in Scope_Refs'First + 1 .. Scope_Refs'Last loop
         Append(Sequence_Id, " " & Scope_Refs(I));
      end loop;
      return To_Wide_String(Sequence_ID);
    end Get_Scope_Sequence;

   -----------------------------------------------------------------------------

end Vatox.Axf_Points.References.Ada_Refs;
