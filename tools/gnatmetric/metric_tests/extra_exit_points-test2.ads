------------------------------------------------------------------------------
--                                                                          --
--                  GNATMETRIC METRIC TESTING COMPONENTS                    --
--                                                                          --
--                       METRIC : Extra Exit Points                         --
--                                                                          --
--                       Copyright (C) 2008, AdaCore                        --
--                                                                          --
------------------------------------------------------------------------------

--  This test unit implements the following test case:
--
--  2. Typical case: reasonable level of construct nesting and reasonable
--     number of (extra) exit points
--
--  OBJECTIVE:
--     Check combinations of different kinds of exit points and different
--     contexts.

package Extra_Exit_Points.Test2 is

   My_Exception : exception;

   type My_Rec is record
      I : Integer;
      B : Boolean;
   end record;

   procedure Proc1 (X : in out My_Rec);

   function Fun1 (X : Integer) return My_Rec;

end Extra_Exit_Points.Test2;
