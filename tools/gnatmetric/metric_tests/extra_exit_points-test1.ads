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
--  1. Simple case: different kinds of extra exit points in simple contexts
--
--  OBJECTIVE:
--     Check different kinds of exit points in different contexts.

package Extra_Exit_Points.Test1 is

   My_Exception : exception;

   procedure Proc1 (X : in out Integer);

   generic
   procedure Gen_Proc1 (X : in out Integer);

   function Fun1 (X : Integer) return Integer;

   generic
   function Gen_Fun1 (X : Integer) return Integer;

end Extra_Exit_Points.Test1;
