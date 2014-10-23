--  pragma Profile (Ravenscar);
with PCA9685;

procedure Ravn is
   B : aliased PCA9685.Bus (Address => 0);
   C : PCA9685.Chip (Address => 16#20#, On_Bus => B'Access);
begin
   C.Reset;
end Ravn;
