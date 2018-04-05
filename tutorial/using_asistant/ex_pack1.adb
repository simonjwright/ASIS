with GNAT.IO; use GNAT.IO;
package body Ex_Pack1 is
   procedure Put_Factorial
     (I   : in Natural;
      Res : in Positive)
   is
   begin
      New_Line;
      Put ("For ");
      Put (I);
      Put (" the factorial is ");
      put (Res);
      New_Line;
   end Put_Factorial;
end Ex_Pack1;