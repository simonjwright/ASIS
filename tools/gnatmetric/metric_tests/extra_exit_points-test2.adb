------------------------------------------------------------------------------
--                                                                          --
--                  GNATMETRIC METRIC TESTING COMPONENTS                    --
--                                                                          --
--                       METRIC : Extra Exit Points                         --
--                                                                          --
--                       Copyright (C) 2008, AdaCore                        --
--                                                                          --
------------------------------------------------------------------------------

package body Extra_Exit_Points.Test2 is

   -----------
   -- Proc1 --
   -----------

   procedure Proc1 (X : in out My_Rec) is
      --  EXPECTED METRIC VALUE: 3
   begin

      begin
         case X.I is
            when 1 =>
               return;  -- Exit point for Proc1
            when 2 =>
               raise My_Exception; -- Exit point for Proc1
               --  When raised here, this exception is not handled locally
            when others =>
               null;
         end case;
      exception
         when others =>
            X := (0, False);
            raise;
      end;

      X.I := X.I * (X.I - 1);

      begin
         begin
            if X.I > 1000 then
               return; -- Exit point for Proc1
            elsif X.I > 0 then
               raise My_Exception; -- Not an exit point for Proc1,
               --  exception is handled locally in enclosing block
            else
               X.B := True;
            end if;

         exception
            when others =>
               X := (1, True);
               raise;
         end;

         X.I := X.I + 100;

      exception
         when My_Exception =>
            X.I := X.I + 1;
      end;

   end Proc1;

   ----------
   -- Fun1 --
   ----------

   function Fun1 (X : Integer) return My_Rec is
      --  EXPECTED METRIC VALUE: 4
   begin
      case X is
         when 0 =>
            return (0, False);      -- Exit point for Fun1
         when 1 =>
            return (1, True);       -- Exit point for Fun1
         when 2 =>
            raise My_Exception;     -- Exit point for Fun1
         when 3 =>
            raise Constraint_Error; -- Exit point for Fun1
         when others =>
            return Res : My_Rec do  -- Exit point for Fun1
               Res.I := X * 2;
               Res.B := X > 1000;
            end return;
      end case;

   end Fun1;

end Extra_Exit_Points.Test2;
