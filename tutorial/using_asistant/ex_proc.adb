--  this is an example procedure to be used in the
--  ASIS exercises
--
with Ex_Pack1; use Ex_Pack1;
procedure Ex_Proc is
   --  we have something to do in this example...
   --  Well, let's compute some factorials:

   subtype Test_Subtype is Integer range 1 .. 5;
   Res_Var  : Positive;
   Test_Var : Natural;

   function Factorial (N : Natural) return Positive;
   function Factorial (N : Natural) return Positive is
   begin
      if N = 0 then
         return 1;
      else
         return N * Factorial (N-1);
      end if;
   end Factorial;

begin
   for I in Test_Subtype loop
      Res_Var := Factorial (I);
      Put_Factorial (I, Res_Var);
   end loop;

   --  and now just try some value:
   Test_Var := 6;
   Res_Var  := Factorial (Test_Var);
   Put_Factorial (Test_Var, Res_Var);

   --  and anothe one:
   Put_Factorial (0, Factorial (0));

end Ex_Proc;