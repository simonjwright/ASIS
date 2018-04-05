------------------------------------------------------------------------------
--                                                                          --
--                  GNATMETRIC METRIC TESTING COMPONENTS                    --
--                                                                          --
--                       METRIC : Extra Exit Points                         --
--                                                                          --
--                       Copyright (C) 2008, AdaCore                        --
--                                                                          --
------------------------------------------------------------------------------

package body Extra_Exit_Points.Test1 is

   -----------
   -- Proc1 --
   -----------

   procedure Proc1 (X : in out Integer) is
      --  EXPECTED METRIC VALUE: 2
   begin

      if X = 1 then
         return;    --  Exit point for Proc1
      end if;

      X := X + 1;

      if X = 3 then
         raise My_Exception; -- Exit point for Proc1
      end if;

      begin
         if X = 100 then
            raise Constraint_Error; -- Not an exit point for Proc1,
            --  exception is handled locally
         end if;
      exception
         when others =>
            X := 13;
      end;

   exception
      when others =>
         if X = 200 then
            X := 1;
         else
            return; --  not an exit point - located in exception handler.
         end if;
   end Proc1;

   ---------------
   -- Gen_Proc1 --
   ---------------

   procedure Gen_Proc1 (X : in out Integer) is
      --  EXPECTED METRIC VALUE: 0

      procedure Local_Proc (Y : in  out Integer);

      procedure Local_Proc (Y : in  out Integer) is
         --  EXPECTED METRIC VALUE: 1
      begin
         if Y > 0 then
            return; -- Exit point for Local_Proc, but not for Gen_Proc1
         end if;

         Y := Y + 1;
      end Local_Proc;

   begin
      Local_Proc (X);
      X := X + 1;
   end Gen_Proc1;

   ----------
   -- Fun1 --
   ----------

   function Fun1 (X : Integer) return Integer is
      --  EXPECTED METRIC VALUE: 1
   begin
      if X > 0 then
         return 1;    --  Exit point for Fun1
      else
         return -1;   --  Exit point for Fun
      end if;
   end Fun1;

   --------------
   -- Gen_Fun1 --
   --------------

   function Gen_Fun1 (X : Integer) return Integer is
      --  EXPECTED METRIC VALUE: 0
      Res : Integer := X;
   begin
      begin
         Res := Res ** Res;
      exception
         when Constraint_Error =>
            Res := Integer'Last;
            return Res; --  Not an extra exit point -
            --  located in exception handler
      end;

      Res := Res * Res;

      return Res; --  Exit point for Gen_Fun1
   exception
      when Constraint_Error =>
         return Integer'Last;  --  Not an extra exit point -
         --  located in exception handler
   end Gen_Fun1;

end Extra_Exit_Points.Test1;
