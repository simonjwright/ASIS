-- Leading comment

with Text_IO;
with Ada.Text_IO, Ada.Strings.Unbounded;
use Ada;
use Ada.Text_IO;

-- Another comment
package Basic_Decl is

   type Short_Range is range 1 .. 10;

   subtype Same_Short_Range is Short_Range;

   type Mid_Range is range Integer'First .. Natural'Last;

   type A_Record is record
      A : Integer; -- line ending comment
      S : Short_Range;
      M : Mid_Range range 10 .. Mid_Range(Short_Range'Last);
   end record;

end Basic_Decl;
-- Trailing comment
