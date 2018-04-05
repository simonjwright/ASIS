--  Leading comment

with Text_IO;
with Ada.Text_IO, Ada.Strings.Unbounded;
use Ada;
use Ada.Text_IO;

with Ada;         use Ada;
with Ada.Text_IO; use Ada.Text_IO;
with Ada;         use Ada;

with Ada.Text_IO; use Ada.Text_IO;
with Ada;         use Ada;
with Ada;         use Ada;

with Ada;         use Ada;
with Ada;         use Ada;
with Ada.Text_IO; use Ada.Text_IO;

with Ada; use Ada;
--  A comment in the middle.
with Ada.Text_IO; use Ada.Text_IO;
with Ada;         use Ada;

with Ada.Text_IO; use Ada.Text_IO;
with Ada;         use Ada;
--  A comment in the middle.
with Ada; use Ada;

with Ada; use Ada;
--  A comment in the middle.
with Ada;         use Ada;
with Ada.Text_IO; use Ada.Text_IO;

--  Another comment

package Basic_Decl is -- An end-of-line comment.

   type Short_Range -- An end-of-line comment in an odd place.
   is range 1 .. 10;

   subtype Same_Short_Range is Short_Range;

   type Mid_Range is range Integer'First .. Natural'Last;

   type A_Record is record
      A : Integer;
      S : Short_Range;
      M : Mid_Range range 10 .. Mid_Range (Short_Range'Last);
   end record;

   A_Very_Very_Very_Very_Very_Very_Long_Identifier,
   Another_Very_Very_Very_Very_Very_Very_Long_Identifier,
   Still_Another_Very_Very_Very_Very_Very_Very_Long_Identifier : Integer;

end Basic_Decl;
