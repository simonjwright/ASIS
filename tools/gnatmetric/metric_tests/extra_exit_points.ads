------------------------------------------------------------------------------
--                                                                          --
--                  GNATMETRIC METRIC TESTING COMPONENTS                    --
--                                                                          --
--                       METRIC : Extra Exit Points                         --
--                                                                          --
--                       Copyright (C) 2008, AdaCore                        --
--                                                                          --
------------------------------------------------------------------------------

--  This is the root package for the test suite for extra exit points metric

--  This test suite contains the following test cases:
--
--  1. Simple case: different kinds of extra exit points in simple contexts
--
--  2. Typical case: reasonable level of construct nesting and reasonable
--     number of (extra) exit points

package Extra_Exit_Points is
   pragma Pure (Extra_Exit_Points);
end Extra_Exit_Points;
