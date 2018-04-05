------------------------------------------------------------------------------
--                                                                          --
--                 ASIS-for-GNAT IMPLEMENTATION COMPONENTS                  --
--                                                                          --
--                         A 4 G . E E _ C A C H E                          --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2014-2015, Free Software Foundation, Inc.          --
--                                                                          --
-- ASIS-for-GNAT is free software; you can redistribute it and/or modify it --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software  Foundation;  either version 3,  or (at your option)  any later --
-- version.  ASIS-for-GNAT  is  distributed  in  the  hope  that it will be --
-- useful,  but  WITHOUT ANY WARRANTY; without even the implied warranty of --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     --
--                                                                          --
--                                                                          --
--                                                                          --
--                                                                          --
--                                                                          --
-- You should have  received  a copy of the  GNU General Public License and --
-- a copy of the  GCC Runtime Library Exception  distributed with GNAT; see --
-- the files COPYING3 and COPYING.RUNTIME respectively.  If not, see        --
-- <http://www.gnu.org/licenses/>.                                          --
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

pragma Ada_2012;

with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Containers.Vectors;

with Asis.Elements; use Asis.Elements;

with Output;        use Output;

with A4G.A_Opt;

package body A4G.EE_Cache is

   type Cache_Idx is new Natural;
   No_EE_Index    : constant Cache_Idx := Cache_Idx'First;
   First_EE_Index : constant Cache_Idx := No_EE_Index + 1;

   subtype Existing_EE_Idx is Cache_Idx range
     First_EE_Index .. Cache_Idx'Last;

   type EE_Cache_Record is record
      Element   : Asis.Element;
      Enclosing : Cache_Idx := No_EE_Index;
      Hash_Link : Cache_Idx := No_EE_Index;
   end record;

   function "=" (L, R : EE_Cache_Record) return Boolean is
     (Is_Equal (L.Element, R.Element));

   package EE_Cache is new Ada.Containers.Vectors
     (Index_Type   => Existing_EE_Idx,
      Element_Type => EE_Cache_Record);

   use EE_Cache;

   EE_Cache_Table : EE_Cache.Vector;

   --------------------------------------------
   -- Hash table for Enclosing_Element Cache --
   --------------------------------------------

   Hash_Num : constant Integer := 2**16;
   --  Number of headers in the hash table. There is no special reason in this
   --  choice.

   Hash_Max : constant Integer := Hash_Num - 1;
   --  Indexes in the hash header table run from 0 to Hash_Num - 1

   subtype Hash_Index_Type is Integer range 0 .. Hash_Max;
   --  Range of hash index values

   Hash_Table : array (Hash_Index_Type) of Cache_Idx;
   --  The hash table is used to locate existing entries in the table. The
   --  entries point to the first table entry whose hash value matches the hash
   --  code. Then subsequent nodes table entries with the same hash code value
   --  are linked through the Hash_Link fields.

   function Hash (El : Asis.Element) return Hash_Index_Type;
   --  Compute hash code for its argument.

   ------------------------
   --  Local subprograms --
   ------------------------

   function Find_Element (E : Asis.Element) return Cache_Idx;
   --  Thies to locate an argument Element in EE_Cache_Table. Returns the
   --  corresponding index if the attempt is succesful. Returns No_EE_Index if
   --  the argument Element is not stored in EE_Cache_Table.

   function Locate_Element
     (E          :     Asis.Element;
      Already_In : out Boolean)
      return           Cache_Idx;
   --  Tries to locate the argument in EE_Cache_Table. If the attempt is
   --  successful, sets Already_In ON. Otherwise sets Already_In OFF and
   --  appens the element to the cache. Returns index pointing to the argument
   --  Element, it can never be No_EE_Index.

   procedure Print_EE_Cache_Record (Idx : Existing_EE_Idx);
   --  Debug procedure. Prints into Stderr the debug image of the information
   --  stored under index Idx. The caller if responsibe for calling this
   --  procedure only for arguments that are in
   --  First_EE_Index .. Last_Index (EE_Cache_Table).

   ------------------
   -- Find_Element --
   ------------------

   function Find_Element (E : Asis.Element) return Cache_Idx is
      Result : Cache_Idx    := Hash_Table (Hash (E));
   begin
      while Result /= No_EE_Index loop
         exit when Is_Equal (EE_Cache_Table (Result).Element, E);
         Result := EE_Cache_Table (Result).Hash_Link;
      end loop;

      return Result;
   end Find_Element;

   ---------------------------
   -- Get_Enclosing_Element --
   ---------------------------

   function Get_Enclosing_Element
     (E       :     Asis.Element;
      Success : out Boolean)
      return Asis.Element
   is
      Result : Asis.Element := Nil_Element;
      Idx    : Cache_Idx    := Find_Element (E);
   begin
      Success := False;

      if Idx /= No_EE_Index then
         Idx := EE_Cache_Table (Idx).Enclosing;

         if Idx /= No_EE_Index then
            Success := True;
            Result  := EE_Cache_Table (Idx).Element;
         end if;

      end if;

      return Result;
   end Get_Enclosing_Element;

   ----------
   -- Hash --
   ----------

   function Hash (El : Asis.Element) return Hash_Index_Type is
   begin
      return Asis.Elements.Hash (El) mod Hash_Num;
   end Hash;

   -------------------
   -- Init_EE_Cache --
   -------------------

   procedure Init_EE_Cache is
   begin
      Hash_Table := (others => No_EE_Index);
      Clear (EE_Cache_Table);
   end Init_EE_Cache;

   --------------------
   -- Locate_Element --
   --------------------

   function Locate_Element
     (E          :     Asis.Element;
      Already_In : out Boolean)
      return           Cache_Idx
   is
      Hash_Value : constant Hash_Index_Type := Hash (E);
      Result     : Cache_Idx := Hash_Table (Hash_Value);
      Last_Hash  : Cache_Idx := No_EE_Index;
   begin
      while Result /= No_EE_Index loop
         exit when Is_Equal (EE_Cache_Table (Result).Element, E);
         Last_Hash := Result;
         Result    := EE_Cache_Table (Result).Hash_Link;
      end loop;

      if Result /= No_EE_Index then
         Already_In := True;
      else
         Already_In := False;

         Append
           (Container => EE_Cache_Table,
            New_Item  =>
              (Element   => E,
               Enclosing => No_EE_Index,
               Hash_Link => No_EE_Index));

         Result := Last_Index (EE_Cache_Table);

         if Last_Hash = No_EE_Index then
            Hash_Table (Hash_Value) := Result;
         else
            EE_Cache_Table (Last_Hash).Hash_Link := Result;
         end if;
      end if;

      return Result;
   end Locate_Element;

   --------------------
   -- Print_EE_Cache --
   --------------------

   procedure Print_EE_Cache is
      Last_Idx : Existing_EE_Idx;
      use Ada.Containers;
   begin
      if Length (EE_Cache_Table) = 0 then
         Write_Line ("Enclosig_Element Cache is empty");
         return;
      end if;

      Last_Idx := Last_Index (EE_Cache_Table);

      Write_Line ("*** Enclosig_Element Cache dump start ***");

      for J in Existing_EE_Idx'First .. Last_Idx loop
         Print_EE_Cache_Record (J);
      end loop;

      Write_Line ("*** Enclosig_Element Cache dump end ***");

   end Print_EE_Cache;

   ---------------------------
   -- Print_EE_Cache_Record --
   ---------------------------

   procedure Print_EE_Cache_Record (Idx : Existing_EE_Idx) is
      El : constant Asis.Element := EE_Cache_Table (Idx).Element;
   begin
      Write_Line ("Idx =" & Idx'Img);
      Write_Line (To_String (Debug_Image (El)));
--      Write_Line ("at " & Build_GNAT_Location (El));
      Write_Line ("  Enclosing -" & EE_Cache_Table (Idx).Enclosing'Img);
      Write_Line ("  Hash_Link -" & EE_Cache_Table (Idx).Hash_Link'Img);

   end Print_EE_Cache_Record;

   ---------------------------------
   -- Print_EE_Cache_Summary_Info --
   ---------------------------------

   procedure Print_EE_Cache_Summary_Info is
      Total : Cache_Idx;
      No_EE_Set : Natural := 0;

      Hash_Entries         : Natural := 0;
      Max_Hash_Chain       : Natural := 0;
      Average_Hash_Chain   : Natural := 0;
      Chain_Len            : Natural;

      function Get_Chain_Len (Idx : Existing_EE_Idx) return Natural;

      function Get_Chain_Len (Idx : Existing_EE_Idx) return Natural is
         Result : Natural   := 0;
         H_Idx  : Cache_Idx := EE_Cache_Table (Idx).Hash_Link;
      begin
         while H_Idx /= No_EE_Index loop
            Result := Result + 1;
            H_Idx  := EE_Cache_Table (H_Idx).Hash_Link;
         end loop;

         return Result;
      end Get_Chain_Len;

      use Ada.Containers;
   begin
      if Length (EE_Cache_Table) = 0 then
         Write_Line ("Enclosig_Element Cache is empty");
         return;
      end if;

      Total := Last_Index (EE_Cache_Table);

      Write_Line ("** EE Cache Summary Info **");
      Write_Line ("Total Number of records              :" & Total'Img);

      for J in Existing_EE_Idx'First .. Total loop
         if EE_Cache_Table (J).Enclosing = No_EE_Index then
            No_EE_Set := No_EE_Set + 1;
         end if;
      end loop;

      Write_Line ("Number of top Elements with no EE set:" & No_EE_Set'Img);

      for J in Hash_Index_Type loop
         if Hash_Table (J) /= No_EE_Index then
            Hash_Entries       := Hash_Entries + 1;
            Chain_Len          := Get_Chain_Len (Hash_Table (J));
            Average_Hash_Chain := Average_Hash_Chain + Chain_Len;

            if Max_Hash_Chain < Chain_Len then
               Max_Hash_Chain := Chain_Len;
            end if;
         end if;
      end loop;

      Average_Hash_Chain := Average_Hash_Chain / Hash_Entries;

      Write_Line ("Number of Hash table entries         :" & Hash_Entries'Img);
      Write_Line ("Max hash chain lenght                :" &
                  Max_Hash_Chain'Img);
      Write_Line ("Average hash chain lenght            :" &
                  Average_Hash_Chain'Img);

   end Print_EE_Cache_Summary_Info;

   -----------------------------
   -- Store_Enclosing_Element --
   -----------------------------

   procedure Store_Enclosing_Element
     (Parent : Asis.Element;
      Child  : Asis.Element)
   is
      Already_Stored : Boolean;
      Parent_Idx     : Cache_Idx;
      Child_Idx      : Cache_Idx;
   begin
      if A4G.A_Opt.Cache_EE_Results then

         Child_Idx := Locate_Element (Child, Already_Stored);

         if Already_Stored then
            if EE_Cache_Table (Child_Idx).Enclosing /= No_EE_Index then
               --  nothing to do, Parent should also be in EE_Cache_Table
               return;
            end if;
         end if;

         Parent_Idx := Locate_Element (Parent, Already_Stored);
         EE_Cache_Table (Child_Idx).Enclosing := Parent_Idx;
      end if;

   end Store_Enclosing_Element;

   -----------------------------
   -- Store_Enclosing_Element --
   -----------------------------

   procedure Store_Enclosing_Element
     (Parent   : Asis.Element;
      Children : Asis.Element_List)
   is
      Already_Stored : Boolean;
      Parent_Idx     : Cache_Idx;
      Child_Idx      : Cache_Idx;
   begin

      if A4G.A_Opt.Cache_EE_Results then
         Child_Idx :=
           Locate_Element (Children (Children'First), Already_Stored);

         if Already_Stored then
            if EE_Cache_Table (Child_Idx).Enclosing /= No_EE_Index then
               --  nothing to do, Parent should also be in EE_Cache_Table,
               --  and the Enclosing_Element link should be stored for all
               --  Children
               return;
            end if;
         end if;

         Parent_Idx := Locate_Element (Parent, Already_Stored);
         EE_Cache_Table (Child_Idx).Enclosing := Parent_Idx;

         for J in Children'First + 1 .. Children'Last loop
            Child_Idx := Locate_Element (Children (J), Already_Stored);
            EE_Cache_Table (Child_Idx).Enclosing := Parent_Idx;
         end loop;
      end if;

   end Store_Enclosing_Element;

end A4G.EE_Cache;
